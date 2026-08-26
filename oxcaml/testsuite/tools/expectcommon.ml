(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-69"]

open StdLabels

(* representation of: {tag|str|tag} *)
type string_constant =
  { str : string
  ; tag : string
  }

type expectation_filter =
  | Principal
  | Structured
  | X86_64
  | Raw
  | Simplify
  | Reaper

type expectation_kind =
  | Expect_toplevel
  (* When [whole_function] is [true], the capture extends past the hot body
     and also shows the trailing out-of-line code: GC jump pads, safety-error
     calls and the stack-realloc handler. *)
  | Expect_asm of { whole_function : bool }
  | Expect_fexpr

type expectation =
  { extid_loc       : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc     : Location.t (* Location of the whole payload *)
  ; expected_output : (expectation_filter list * string_constant) list
  ; kind            : expectation_kind
  }

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases      : Parsetree.toplevel_phrase list
  ; expectations : expectation list
  }

let register_assembly_callback :
  ((string -> unit) -> unit) option ref = ref None

(* Set before compiling a chunk to tell the backend whether the [%%expect_asm]
   capture should extend to the whole function (see [Expect_asm]). *)
let set_assembly_whole_function : (bool -> unit) option ref = ref None

let register_compilation_unit_callback :
  ((Compilation_unit.t -> unit) -> unit) option ref = ref None

type correction =
  { corrected_expectations : expectation list
  ; trailing_output        : string
  }

let filter_of_string = function
  | "Principal" -> Some Principal
  | "Structured" -> Some Structured
  | "X86_64" -> Some X86_64
  | "Raw" -> Some Raw
  | "Simplify" -> Some Simplify
  | "Reaper" -> Some Reaper
  | _ -> None

let string_of_filter = function
  | Principal -> "Principal"
  | Structured -> "Structured"
  | X86_64 -> "X86_64"
  | Raw -> "Raw"
  | Simplify -> "Simplify"
  | Reaper -> "Reaper"

let match_expect_extension (ext : Parsetree.extension) =
  let match_ext_name = function
    | "expect" | "ocaml.expect" -> Some Expect_toplevel
    | "expect_asm" | "ocaml.expect_asm" ->
      Some (Expect_asm { whole_function = false })
    | "expect_asm_full" | "ocaml.expect_asm_full" ->
      Some (Expect_asm { whole_function = true })
    | "expect_fexpr" | "ocaml.expect_fexpr" -> Some Expect_fexpr
    | _ -> None
  in
  match ext with
  | ({Asttypes.txt; loc = extid_loc}, payload) ->
    match match_ext_name txt with
    | None -> None
    | Some kind ->
    let invalid_payload
        ?(loc=extid_loc)
        ?(msg = (Printf.sprintf "invalid [%%%%%s payload]" txt))
        () =
      Location.raise_errorf ~loc "%s" msg
    in
    if
      Option.is_none !register_assembly_callback
      && (match kind with Expect_asm _ -> true | _ -> false)
    then invalid_payload ~msg:"expect_asm is only supported by expect.opt" ();
    if Option.is_none !register_compilation_unit_callback && kind = Expect_fexpr
    then invalid_payload ~msg:"expect_fexpr is only supported by expect.opt" ();
    let string_constant (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_constant {pconst_desc = Pconst_string (str, _, Some tag); _} ->
        { str; tag }
      | _ -> invalid_payload ~loc:e.pexp_loc ()
    in
    let ident (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_construct ({ txt = Lident txt; }, None) -> txt
      | Pexp_ident {txt = Lident txt; } -> txt
      | _ -> invalid_payload ~loc:e.pexp_loc ()
    in
    (* Parse elements: filters followed by {|...|} *)
    let parse_element (e : Parsetree.expression) =
      let filters, txt =
        match e.pexp_desc with
        | Pexp_constant {pconst_desc = Pconst_string _; _} ->
            (* Bare string constant - no filter *)
            ([], string_constant e)
        | Pexp_construct ({txt = Lident filter; }, Some txt) ->
            ([filter], string_constant txt)
        | Pexp_apply (e, args) -> (
        (* filter1 ... filterN {|content|} - filters with string content *)
            match List.rev args with
            | [] -> invalid_payload
                      ~loc:e.pexp_loc
                      ~msg:"expected filter* {|...|}" ()
            | (_, txt)::filters ->
                ident e :: (List.rev_map ~f:(fun (_, e) -> ident e) filters),
                string_constant txt)
        | _ ->
            invalid_payload
              ~loc:e.pexp_loc
              ~msg:("expected {|...|} or Filter{|...|}")
              ()
      in
      let filters =
        List.map ~f:(fun f ->
            match filter_of_string f with
            | None ->
                invalid_payload
                  ~msg:(Printf.sprintf "unexpected filter \"%s\"" f)
                  ~loc:e.pexp_loc
                  ()
            | Some f -> f
          )
          filters
      in
      (filters, txt)
    in
    let is_arch_filter = function
      | X86_64 -> true
      | Principal | Structured | Raw | Simplify | Reaper -> false
    in
    let is_pass_filter = function
      | Raw | Simplify | Reaper -> true
      | Principal | Structured | X86_64 -> false
    in
    let validate_expect_toplevel entries =
      match entries with
      | [([], _)] -> entries
      | [([], _); ([Principal], _)]
      | [([], _); ([Structured], _)]
      | [([], _); ([Principal], _); ([Structured], _)]
      | [([], _); ([Structured], _); ([Principal], _)] -> entries
      | _ ->
        let msg = "expected [%%expect {|...|}] or \
                   [%%expect {|...|}, Principal{|...|}, \
                    Structured{|...|}]"
        in
        invalid_payload ~msg ()
    in
    let validate_expect_asm entries =
      (* All entries must have architecture tags *)
      if List.for_all ~f:(fun (f, _) ->
        match f with
        | [ filter ] -> is_arch_filter filter
        | [] | _::_::_ -> false
      ) entries
      then entries
      else
        invalid_payload
          ~msg:"expected [%%expect_asm Arch1{|...|}, Arch2{|...|}, ...]"
          ()
    in
    let validate_expect_fexpr entries =
      (* All entries must have pass tags *)
      if List.for_all ~f:(fun (f, _) ->
          List.for_all ~f:is_pass_filter f
      ) entries
      then entries
      else
        invalid_payload
          ~msg:"expected [%%expect_fexpr Pass1{|...|}, Pass2{|...|}, ...]"
          ()
    in
    let expectation =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (e, _attrs) }] ->
        let expected_output =
          match e.pexp_desc with
          | Pexp_tuple elements ->
            (* Multiple elements *)
            List.map ~f:(fun (_, elem) -> parse_element elem) elements
          | _ ->
            (* Single element *)
            [parse_element e]
        in
        let expected_output =
          match kind with
          | Expect_toplevel -> validate_expect_toplevel expected_output
          | Expect_asm _ -> validate_expect_asm expected_output
          | Expect_fexpr -> validate_expect_fexpr expected_output
        in
        { extid_loc
        ; payload_loc = e.pexp_loc
        ; expected_output
        ; kind
        }
      | PStr [] ->
        (* Empty payload - only valid for expect_toplevel *)
        (match kind with
        | Expect_toplevel ->
          { extid_loc
          ; payload_loc = { extid_loc with loc_start = extid_loc.loc_end }
          ; expected_output = [([], { tag = ""; str = "" })]
          ; kind
          }
        | Expect_asm _ | Expect_fexpr -> invalid_payload ())
      | _ -> invalid_payload ()
    in
    Some expectation

(* Split a list of phrases from a .ml file *)
let split_chunks phrases =
  let rec loop
      (phrases : Parsetree.toplevel_phrase list) code_acc expect_acc acc =
    match phrases with
    | [] ->
      let acc =
        match expect_acc with
        | [] -> acc
        | _ ->
          { phrases = List.rev code_acc
          ; expectations = List.rev expect_acc
          } :: acc
      in
      if code_acc = [] || expect_acc <> [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc))
    | Ptop_def [] :: phrases -> loop phrases code_acc expect_acc acc
    | phrase :: phrases ->
      let expectation = match phrase with
        | Ptop_def [{pstr_desc = Pstr_extension(ext, [])}] ->
          match_expect_extension ext
        | _ -> None
      in match expectation with
        | Some expectation ->
          loop phrases code_acc (expectation :: expect_acc) acc
        | None -> match expect_acc with
          | [] -> loop phrases (phrase :: code_acc) [] acc
          | _ ->
            let chunk =
              { phrases = List.rev code_acc
              ; expectations = List.rev expect_acc
              }
            in
            loop phrases [phrase] [] (chunk :: acc)
  in
  loop phrases [] [] []

module Compiler_messages = struct
  let capture ppf ~f =
    Misc.protect_refs
      [ R (Location.formatter_for_warnings, ppf) ]
      f
end

let collect_formatters buf pps ~f =
  let ppb = Format.formatter_of_buffer buf in
  let out_functions = Format.pp_get_formatter_out_functions ppb () in

  List.iter ~f:(fun pp -> Format.pp_print_flush pp ()) pps;
  let save =
    List.map ~f:(fun pp -> Format.pp_get_formatter_out_functions pp ()) pps
  in
  let restore () =
    List.iter2
      ~f:(fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  List.iter
    ~f:(fun pp -> Format.pp_set_formatter_out_functions pp out_functions)
    pps;
  match f () with
  | x             -> restore (); x
  | exception exn -> restore (); raise exn

(* Invariant: ppf = Format.formatter_of_buffer buf *)
let capture_everything buf ppf ~f =
  collect_formatters buf [Format.std_formatter; Format.err_formatter]
                     ~f:(fun () -> Compiler_messages.capture ppf ~f)

let exec_phrase ppf phrase ~execute_phrase =
  Location.reset ();
  if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  execute_phrase true ppf phrase

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf fname;
  Location.input_name := fname;
  Location.input_lexbuf := Some lexbuf;
  Parse.use_file lexbuf

let current_arch_filter () =
  match Target_system.architecture () with
  | X86_64 -> Some X86_64
  | AArch64 | IA32 | ARM | POWER | Z | Riscv -> None

(* For [%%expect]:
   - {|...|} alone: used for both principal and non-principal
   - {|...|}, Principal{|...|}: first for non-principal, second for principal
   - {|...|}, Structured{|...|}: first for normal output, second for structured
     diagnostics
   - Structured Principal{|...|}: not allowed

   With [structured_diagnostics = "true"] in TEST, Structured entries are
   added for failing phrases and removed for successful phrases.
*)
let eval_toplevel_expectation expectation ~output =
  let is_structured (filters, _) = filters = [Structured] in
  let structured =
    List.filter ~f:is_structured expectation.expected_output
  in
  let ordinary =
    List.filter
      ~f:(fun entry -> not (is_structured entry))
      expectation.expected_output
  in
  let corrected s = { s with str = output } in
  let combine if_not_principal if_principal =
    if if_not_principal.str = if_principal.str
    then [([], if_not_principal)]
    else [([], if_not_principal); ([Principal], if_principal)]
  in
  let expected_output =
    match ordinary, !Clflags.principal with
    | [([], if_not_principal)], false -> [([], corrected if_not_principal)]
    | [([], expected)], true -> combine expected (corrected expected)
    | [([], if_not_principal); ([Principal], if_principal)], false ->
      combine (corrected if_not_principal) if_principal
    | [([], if_not_principal); ([Principal], if_principal)], true ->
      combine if_not_principal (corrected if_principal)
    | _ -> Misc.fatal_error "impossible: already validated"
  in
  let expected_output = expected_output @ structured in
  if expected_output = expectation.expected_output
  then None
  else Some { expectation with expected_output }

(* For [%%expect_asm]:
   - All entries must have architecture tags (X86_64)
*)
let eval_filtered_expectation expectation ~output ~to_update =
  match to_update with
  | [(filters, s)] when s.str <> output ->
    let s = { s with str = output } in
    Some { expectation with expected_output =
      List.map
        ~f:(fun (f, e) -> (f, if List.equal ~eq:(=) f filters then s else e))
    }
  | _ :: _ :: _ ->
    Location.raise_errorf
      ~loc:expectation.payload_loc
      "duplicate architectures in [%%%%expect_asm]"
  | _ -> None

let update_automatic_structured_expectation expectation ~output ~success =
  match expectation.kind with
  | Expect_asm _ | Expect_fexpr -> None
  | Expect_toplevel ->
    let is_structured (filters, _) = filters = [Structured] in
    let structured =
      List.filter ~f:is_structured expectation.expected_output
    in
    if success || String.equal output "" then
      match structured with
      | [] -> None
      | _ ->
        Some
          { expectation with
            expected_output =
              List.filter
                ~f:(fun entry -> not (is_structured entry))
                expectation.expected_output
          }
    else
      match structured with
      | [] ->
        let tag =
          match expectation.expected_output with
          | [] -> ""
          | (_, output) :: _ -> output.tag
        in
        Some
          { expectation with
            expected_output =
              expectation.expected_output
              @ [[Structured], { str = output; tag }]
          }
      | [(filters, expected)] when not (String.equal expected.str output) ->
        let expected = { expected with str = output } in
        Some
          { expectation with
            expected_output =
              List.map
                ~f:(fun (candidate, previous) ->
                  if candidate = filters then candidate, expected
                  else candidate, previous)
                expectation.expected_output
          }
      | [_] -> None
      | _ :: _ :: _ ->
        Location.raise_errorf
          ~loc:expectation.payload_loc
          "duplicate Structured fields in [%%%%expect]"

let eval_expectation expectation ~output ~success
    ~automatic_structured_diagnostics =
  if !Clflags.structured_diagnostics && automatic_structured_diagnostics then
    update_automatic_structured_expectation expectation ~output ~success
  else
    match expectation.kind with
    | Expect_toplevel when !Clflags.structured_diagnostics ->
      let to_update =
        List.filter
          ~f:(fun (filters, _) -> filters = [Structured])
          expectation.expected_output
      in
      eval_filtered_expectation expectation ~output ~to_update
    | Expect_toplevel -> eval_toplevel_expectation expectation ~output
    | (Expect_asm _ | Expect_fexpr) when !Clflags.structured_diagnostics -> None
    | Expect_asm _ ->
      let to_update =
      List.filter
        ~f:(fun (f, _) ->
            match current_arch_filter () with
            | None -> false
            | Some arch -> List.mem ~set:f arch)
        expectation.expected_output
      in
      eval_filtered_expectation expectation ~output ~to_update
    | Expect_fexpr ->
      eval_filtered_expectation expectation ~output
        ~to_update:expectation.expected_output

let shift_lines delta phrases =
  let position (pos : Lexing.position) =
    { pos with pos_lnum = pos.pos_lnum + delta }
  in
  let location _this (loc : Location.t) =
    { loc with
      loc_start = position loc.loc_start
    ; loc_end   = position loc.loc_end
    }
  in
  let mapper = { Ast_mapper.default_mapper with location } in
  List.map phrases ~f:(function
    | Parsetree.Ptop_dir _ as p -> p
    | Parsetree.Ptop_def st ->
      Parsetree.Ptop_def (mapper.structure mapper st))

let rec min_line_number : Parsetree.toplevel_phrase list -> int option =
function
  | [] -> None
  | (Ptop_dir _  | Ptop_def []) :: l -> min_line_number l
  | Ptop_def (st :: _) :: _ -> Some st.pstr_loc.loc_start.pos_lnum


let visible_inline_code () =
  let open Misc.Style in
  let default = get_styles () in
  let inline_code = { ansi = []; text_open = {|"|}; text_close={|"|} } in
  set_styles { default with inline_code }

let format_structured_diagnostic ppf
    (diagnostic : Structured_diagnostic.t) =
  let module Diagnostic = Structured_diagnostic in
  let rec render_inlines ppf inlines =
    List.iter inlines ~f:(render_inline ppf)
  and render_inline ppf (inline : Diagnostic.Inline.t) =
    match inline with
    | Text text -> Format.pp_print_string ppf text
    | Annotated { annotation; content } -> (match annotation with
      | Code -> Format.fprintf ppf "`%a`" render_inlines content
      | Source _ -> Format.fprintf ppf "[%a]" render_inlines content
      | Mention { entity; form } ->
        Format.fprintf ppf "{%d%s:%a}"
          (Diagnostic.Entities.Id.to_int entity)
          (match form with Name -> "" | Pronoun -> "*")
          render_inlines content
      | Term term ->
        Format.fprintf ppf "%a#%d" render_inlines content
          (Diagnostic.Glossary.Id.to_int term))
  in
  let rec inlines_are_empty inlines =
    List.for_all inlines ~f:(fun (inline : Diagnostic.Inline.t) ->
      match inline with
      | Text text -> String.equal text ""
      | Annotated { annotation = _; content } -> inlines_are_empty content)
  in
  let kind_marker (kind : Diagnostic.Kind.t) =
    match kind with Explanation -> "-" | Background -> "=" | Suggestion -> "+"
  in
  let relation_marker (relation : Diagnostic.Relation.t) =
    match relation with Claim -> "" | Elaboration -> "~"
  in
  let render_list render ppf items =
    Format.pp_print_list ~pp_sep:Format.pp_print_cut render ppf items
  in
  let rec render_block ppf ~relation (block : Diagnostic.Block.t) =
    if inlines_are_empty block.content then
      render_children ppf block.children
    else
      Format.fprintf ppf "@[<v 2>%s%s %a%a@]" relation
        (kind_marker block.kind) render_inlines block.content
        render_nested_children block.children
  and render_child ppf (relation, block) =
    render_block ppf ~relation:(relation_marker relation) block
  and render_children ppf children =
    render_list render_child ppf children
  and render_nested_children ppf = function
    | [] -> ()
    | children ->
      Format.pp_print_cut ppf ();
      render_children ppf children
  in
  let render_entity ppf (entity, (loc : Location.t)) =
    Format.fprintf ppf "entity %d: %a"
      (Diagnostic.Entities.Id.to_int entity) Location.print_loc loc
  in
  let render_entities ppf = function
    | [] -> ()
    | entities ->
      Format.pp_print_cut ppf ();
      Format.pp_print_cut ppf ();
      render_list render_entity ppf entities
  in
  Format.fprintf ppf "@[<v 0>%a%a@]"
    (render_list (render_block ~relation:"")) diagnostic.body
    render_entities (Diagnostic.Entities.to_list diagnostic.entities)

let format_structured_output output =
  let diagnostics =
    String.split_on_char ~sep:'\n' output
    |> List.filter_map ~f:(fun line ->
      if String.equal line "" || not (Char.equal line.[0] '{') then None
      else
      match Structured_diagnostic.of_json line with
      | Ok diagnostic ->
        Some
          (Format.asprintf "%a" format_structured_diagnostic diagnostic
           |> Misc.replace_substring ~before:".corrected" ~after:""
           |> Misc.delete_eol_spaces)
      | Error message ->
        Misc.fatal_errorf "invalid structured diagnostic JSON: %s" message)
  in
  match diagnostics with
  | [] -> ""
  | diagnostics -> "\n" ^ String.concat ~sep:"\n" diagnostics ^ "\n"

let has_structured_expectation expectation =
  match expectation.kind with
  | Expect_toplevel ->
    List.exists expectation.expected_output ~f:(fun (filters, _) ->
      filters = [Structured])
  | Expect_asm _ | Expect_fexpr -> false

let eval_expect_file fname ~file_contents ~execute_phrase
    ~automatic_structured_diagnostics =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents
      ~fname:(if !Clflags.structured_diagnostics then fname else "")
      file_contents
    |> split_chunks
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let saw_error = ref false in
  let () =
    visible_inline_code ();
    Misc.Style.set_tag_handling ppf in
  let exec_phrases phrases =
    let last_asm = ref None in
    let last_unit = ref None in
    let phrases =
      match min_line_number phrases with
      | None -> phrases
      | Some lnum -> shift_lines (1 - lnum) phrases
    in
    (* For formatting purposes *)
    Buffer.add_char buf '\n';
    let exec_one phrase =
      Option.iter
        (fun register -> register (fun asm_out -> last_asm := Some asm_out))
        !register_assembly_callback;
      Option.iter
        (fun register -> register (fun cu -> last_unit := Some cu))
        !register_compilation_unit_callback;
      let snap = Btype.snapshot () in
      try
        Sys.with_async_exns
          (fun () -> exec_phrase ppf phrase ~execute_phrase)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        begin try Location.report_exception ppf exn
        with _ ->
          Format.fprintf ppf "Uncaught exception: %s\n%s\n"
            (Printexc.to_string exn)
            (Printexc.raw_backtrace_to_string bt)
        end;
        Btype.backtrack snap;
        false
    in
    let (success : bool) =
      List.fold_left phrases ~init:true ~f:(fun acc phrase ->
          if acc then exec_one phrase else acc)
    in
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* For formatting purposes *)
      Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    (success, Misc.delete_eol_spaces s, !last_asm, !last_unit)
  in
  let fexpr_outputs unit filters =
    let read_dump_file pass =
      let file =
        let fname =
          match unit with
          | Some unit ->
              (* CR keryan: works only as long as we keep generating a
                 capitalized filename, and with the -S flag *)
              Compilation_unit.(name unit |> Name.to_string) ^ "."
          | None -> Filename.chop_extension fname
        in
        fname ^ pass ^ ".fl"
      in
      if Sys.file_exists file then
        let ic = open_in_bin file in
        try In_channel.input_all ic with
        | _ -> close_in ic; "Unexpected error"
      else Printf.sprintf "Error: No fexpr file '%s' found" file
    in
    List.map filters ~f:(function
        | Raw -> read_dump_file "raw"
        | Simplify -> read_dump_file "simplify"
        | Reaper -> read_dump_file "reaper"
        | Principal | Structured | X86_64 -> "")
    |> String.concat ~sep:"\n"
    |> (^) "\n"
  in
  let corrected_expectations =
    capture_everything buf ppf ~f:(fun () ->
      List.concat_map chunks ~f:(fun chunk ->
            let has_fexpr =
              List.exists ~f:(fun exp ->
                  match exp.kind with
                  | Expect_fexpr -> true | _ -> false)
                chunk.expectations
            in
            let asm_expectations =
              List.filter_map chunk.expectations ~f:(fun exp ->
                  match exp.kind with
                  | Expect_asm { whole_function } -> Some (exp, whole_function)
                  | Expect_toplevel | Expect_fexpr -> None)
            in
            let whole_function = List.exists ~f:snd asm_expectations in
            (if whole_function then
               match
                 List.find_opt asm_expectations
                   ~f:(fun (_, whole_function) -> not whole_function)
               with
               | Some (exp, _) ->
                 Location.raise_errorf ~loc:exp.extid_loc
                   "[%%%%expect_asm] and [%%%%expect_asm_full] cannot be \
                    attached to the same code block"
               | None -> ());
            let keep_asm = !Clflags.keep_asm_file in
            if has_fexpr then Clflags.keep_asm_file := true;
            Option.iter (fun set -> set whole_function)
              !set_assembly_whole_function;
        let (success, toplevel_output, asm_output, last_unit) =
          Fun.protect
            ~finally:(fun () ->
              Option.iter (fun set -> set false) !set_assembly_whole_function;
              Clflags.keep_asm_file := keep_asm)
            (fun () -> exec_phrases chunk.phrases)
        in
        let has_toplevel_expectation =
          List.exists chunk.expectations ~f:(fun expectation ->
            match expectation.kind with
            | Expect_toplevel -> true
            | Expect_asm _ | Expect_fexpr -> false)
        in
        if not success && has_toplevel_expectation then saw_error := true;
        let toplevel_output =
          if
            !Clflags.structured_diagnostics
            && (List.exists
                  chunk.expectations
                  ~f:has_structured_expectation
                || automatic_structured_diagnostics && not success)
          then
            format_structured_output toplevel_output
          else toplevel_output
        in
        List.filter_map chunk.expectations ~f:(fun expectation ->
          let output = match expectation.kind with
            | Expect_toplevel -> toplevel_output
            | Expect_asm _ ->
                if success then
                  Option.value asm_output
                    ~default:"\nNo assembly: compilation failed\n"
                else toplevel_output
            | Expect_fexpr ->
                if success then
                  let filters =
                    List.concat_map ~f:fst expectation.expected_output
                  in
                  fexpr_outputs last_unit filters
                else toplevel_output
          in
          eval_expectation expectation ~output ~success
            ~automatic_structured_diagnostics)))
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf
        ~f:(fun () ->
          let (_, output, _, _) = exec_phrases phrases in
          if !Clflags.structured_diagnostics then "" else output)
  in
  let needs_principal =
    Option.is_some trailing_code
    ||
    List.exists chunks ~f:(fun chunk ->
      List.exists chunk.expectations ~f:(fun expectation ->
        match expectation.kind with
        | Expect_toplevel -> true
        | Expect_asm _ | Expect_fexpr -> false))
  in
  let needs_structured =
    List.exists chunks ~f:(fun chunk ->
      List.exists chunk.expectations ~f:has_structured_expectation)
    || automatic_structured_diagnostics && !saw_error
  in
  { corrected_expectations; trailing_output }, ~needs_principal,
  ~needs_structured

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

let output_corrected oc ~file_contents correction =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  let output_entry oc (filter, str_const) =
    List.iter ~f:(fun f -> output_string oc (string_of_filter f)) filter;
    output_body oc str_const
  in
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        (match c.expected_output with
        | [] -> ()
        | entry :: rest ->
          output_entry oc entry;
          List.iter ~f:(fun e ->
            output_string oc ", ";
            output_entry oc e
          ) rest);
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  match correction.trailing_output with
  | "" -> ()
  | s  -> Printf.fprintf oc "\n[%%%%expect{|%s|}]\n" s

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

let process_expect_file fname ~execute_phrase
    ~automatic_structured_diagnostics =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let correction, ~needs_principal, ~needs_structured =
    eval_expect_file fname ~file_contents ~execute_phrase
      ~automatic_structured_diagnostics
  in
  write_corrected ~file:corrected_fname ~file_contents correction;
  needs_principal, needs_structured

let repo_root = ref None
let keep_original_error_size = ref false
let preload_objects = ref []
let main_file = ref None

module type Toplevel = sig
  val override_sys_argv : string array -> unit
  val initialize_toplevel_env : unit -> unit
  val load_file : Format.formatter -> string -> bool
  val execute_phrase :
    bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool
end

let is_object_file ~object_extensions fname =
  List.exists ~f:(fun ext -> Filename.check_suffix fname ext) object_extensions

let read_anonymous_arg ~object_extensions fname =
  if is_object_file ~object_extensions fname
  then preload_objects := fname :: !preload_objects
  else
  match !main_file with
  | None -> main_file := Some fname
  | Some _ ->
    Printf.eprintf "expect_test: multiple input source files\n";
    exit 2

let main (module Toplevel : Toplevel) fname
    ~automatic_structured_diagnostics =
  if not !keep_original_error_size then
    Clflags.error_size := 0;
  Toplevel.override_sys_argv
    (Array.sub Sys.argv ~pos:!Arg.current
       ~len:(Array.length Sys.argv - !Arg.current));
  (* Ignore OCAMLRUNPARAM=b to be reproducible *)
  Printexc.record_backtrace false;
  if not !Clflags.no_std_include then begin
    match !repo_root with
    | None -> ()
    | Some dir ->
        (* If we pass [-repo-root], use the stdlib from inside the
           compiler, not the installed one. We use
           [Compenv.last_include_dirs] to make sure that the stdlib
           directory is the last one. *)
        Clflags.no_std_include := true;
        Compenv.last_include_dirs :=
          [{ Clflags.path = Filename.concat dir "stdlib";
             cmx_guaranteed = true }]
  end;
  Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
  Toplevel.initialize_toplevel_env ();
  let objects = List.rev (!preload_objects) in
  List.iter objects ~f:(fun obj_fname ->
    match Toplevel.load_file Format.err_formatter obj_fname with
    | true -> ()
    | false ->
      Printf.eprintf "expect_test: failed to load object %s\n" obj_fname;
      exit 2);
  (* We are in interactive mode and should record directive error on stdout *)
  Sys.interactive := true;
  let needs_principal, needs_structured =
    process_expect_file fname ~execute_phrase:Toplevel.execute_phrase
      ~automatic_structured_diagnostics
  in
  if !Clflags.structured_diagnostics then exit 0;
  exit
    ((if needs_principal then 3 else 0)
     + if needs_structured then 4 else 0)

let run ~read_anonymous_arg ~extra_args ~extra_init toplevel =
  let automatic_structured_diagnostics = ref false in
  let args =
    Arg.align
      ( [ "-repo-root", Arg.String (fun s -> repo_root := Some s),
          "<dir> root of the OCaml repository. This causes the tool to use \
           the stdlib from the current source tree rather than the installed \
           one."
        ; "-keep-original-error-size", Arg.Set keep_original_error_size,
          " truncate long error messages as the compiler would"
        ; "-structured-diagnostics",
          Arg.Unit Mode_diagnostics.enable_structured_diagnostics,
          " report diagnostics as JSON"
        ; "-automatic-structured-diagnostics",
          Arg.Set automatic_structured_diagnostics,
          " add structured expectations for errors"
        ] @ extra_args
      )
  in
  let usage = "Usage: expect_test <options> [script-file [arguments]]\n\
               options are:"
  in
  (* Some tricky typing tests cause stack overflows in the compiler.
     Bounding the compiler's stack size makes that happen faster. *)
  Gc.set {(Gc.get ()) with stack_limit = 1_000_000};
  extra_init ();
  (* Early disabling of colors in any output *)
  let () =
    Clflags.color := Some Misc.Color.Never;
    Misc.Style.(setup @@ Some Never)
  in
  try
    Arg.parse args read_anonymous_arg usage;
    if !Clflags.structured_diagnostics && !Clflags.principal then begin
      Printf.eprintf "expect: %s cannot be combined with -principal\n"
        "-structured-diagnostics";
      exit 2
    end;
    match !main_file with
    | Some fname ->
      main toplevel fname
        ~automatic_structured_diagnostics:!automatic_structured_diagnostics
    | None ->
      Printf.eprintf "expect: no input file\n";
      exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
