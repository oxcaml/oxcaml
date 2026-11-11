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

(* Compiler settings that can change the output of [%%expect] blocks. The
   tool runs the whole file once for each set of parameters in
   [parameter_sets], and an [%%expect] block records the output of each of
   these runs. *)
module Parameter = struct
  type t =
    | Principal
    | Rectypes
    | Nolabel

  let to_string = function
    | Principal -> "Principal"
    | Rectypes -> "Rectypes"
    | Nolabel -> "Nolabel"

  let of_string = function
    | "Principal" -> Some Principal
    | "Rectypes" -> Some Rectypes
    | "Nolabel" -> Some Nolabel
    | _ -> None

  let all = [Principal; Rectypes; Nolabel]

  let is_set = function
    | Principal -> !Clflags.principal
    | Rectypes -> !Clflags.recursive_types
    | Nolabel -> !Clflags.classic

  let set parameter value =
    match parameter with
    | Principal -> Clflags.principal := value
    | Rectypes -> Clflags.recursive_types := value
    | Nolabel -> Clflags.classic := value

  module Set = struct
    include Set.Make (struct
        type nonrec t = t
        let compare = compare
      end)

    module Map = Map.Make (struct
        type nonrec t = t
        let compare = compare
      end)

    (* Written [Principal.Rectypes] in [%%expect] payloads *)
    let to_string t =
      String.concat ~sep:"." (List.map ~f:to_string (elements t))

    let rec of_longident ~loc : Longident.t -> t = function
      | Lident name ->
        (match of_string name with
         | Some parameter -> singleton parameter
         | None -> Location.raise_errorf ~loc "unknown parameter \"%s\"" name)
      | Ldot (lid, name) ->
        union
          (of_longident ~loc:lid.loc lid.txt)
          (of_longident ~loc:name.loc (Longident.Lident name.txt))
      | Lapply _ ->
        Location.raise_errorf ~loc "unexpected functor application"

    (* The flags passed on the command line stay set in every run. *)
    let set_flags t ~command_line =
      List.iter all ~f:(fun parameter ->
        set parameter (mem parameter t || mem parameter command_line))

    let of_flags () = of_list (List.filter ~f:is_set all)
  end
end

(* The file is always run without any parameters first. *)
let parameter_sets =
  [ Parameter.Set.empty; Parameter.Set.singleton Parameter.Principal ]

type expectation_filter = X86_64 | Raw | Simplify | Reaper

type expectation_kind =
  | Expect_toplevel
  (* When [whole_function] is [true], the capture extends past the hot body
     and also shows the trailing out-of-line code: GC jump pads, safety-error
     calls and the stack-realloc handler. *)
  | Expect_asm of { whole_function : bool }
  | Expect_fexpr

type expected_output =
  (* The output of each run, for [%%expect]. There is always an entry for the
     empty set of parameters. *)
  | Toplevel of string_constant Parameter.Set.Map.t
  (* For [%%expect_asm] and [%%expect_fexpr], which only look at the run
     without parameters. *)
  | Filtered of (expectation_filter list * string_constant) list

type expectation =
  { extid_loc       : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc     : Location.t (* Location of the whole payload *)
  ; expected_output : expected_output
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

(* The outputs of one run of the file *)
type run =
  { expectation_outputs : (expectation * string) list
  ; trailing_output     : string
  }

type correction =
  { corrected_expectations : expectation list
  ; trailing_output        : string_constant Parameter.Set.Map.t
  }

let filter_of_string = function
  | "X86_64" -> Some X86_64
  | "Raw" -> Some Raw
  | "Simplify" -> Some Simplify
  | "Reaper" -> Some Reaper
  | _ -> None

let string_of_filter = function
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
      | Raw | Simplify | Reaper -> false
    in
    let is_pass_filter = function
      | Raw | Simplify | Reaper -> true
      | X86_64 -> false
    in
    (* Valid formats:
       - [{|...|}]
       - [{|...|}, Params{|...|}, ...], where [Params] is a set of parameters,
         such as [Principal] or [Principal.Rectypes], or a tuple of sets that
         have the same output, such as [(Principal, Rectypes)] *)
    let parse_expect_toplevel (e : Parsetree.expression) =
      let add_output ~loc parameters output outputs =
        if Parameter.Set.Map.mem parameters outputs then
          invalid_payload ~loc
            ~msg:(Printf.sprintf "duplicate output for %s"
                    (Parameter.Set.to_string parameters))
            ();
        Parameter.Set.Map.add parameters output outputs
      in
      let parameters (e : Parsetree.expression) =
        match e.pexp_desc with
        | Pexp_construct ({ txt; loc }, None) ->
          Parameter.Set.of_longident ~loc txt
        | _ ->
          invalid_payload ~loc:e.pexp_loc ~msg:"expected parameters" ()
      in
      let add_entry outputs (label, (e : Parsetree.expression)) =
        match label, e.pexp_desc with
        | None, Pexp_construct ({ txt; loc }, Some output) ->
          add_output ~loc
            (Parameter.Set.of_longident ~loc txt)
            (string_constant output)
            outputs
        | None,
          Pexp_apply
            ({ pexp_desc = Pexp_tuple parameter_sets; _ }, [Nolabel, output])
          ->
          let output = string_constant output in
          List.fold_left parameter_sets ~init:outputs
            ~f:(fun outputs (label, parameter_set) ->
              match label with
              | Some _ -> invalid_payload ~loc:e.pexp_loc ()
              | None ->
                add_output ~loc:parameter_set.Parsetree.pexp_loc
                  (parameters parameter_set) output outputs)
        | _ ->
          invalid_payload ~loc:e.pexp_loc
            ~msg:"expected Params{|...|}" ()
      in
      match e.pexp_desc with
      | Pexp_tuple ((None, without_parameters) :: entries) ->
        List.fold_left entries ~f:add_entry
          ~init:(Parameter.Set.Map.singleton Parameter.Set.empty
                   (string_constant without_parameters))
      | _ ->
        Parameter.Set.Map.singleton Parameter.Set.empty (string_constant e)
    in
    let parse_filtered (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_tuple elements ->
        (* Multiple elements *)
        List.map ~f:(fun (_, elem) -> parse_element elem) elements
      | _ ->
        (* Single element *)
        [parse_element e]
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
          match kind with
          | Expect_toplevel -> Toplevel (parse_expect_toplevel e)
          | Expect_asm _ -> Filtered (validate_expect_asm (parse_filtered e))
          | Expect_fexpr ->
            Filtered (validate_expect_fexpr (parse_filtered e))
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
          ; expected_output =
              Toplevel
                (Parameter.Set.Map.singleton Parameter.Set.empty
                   { tag = ""; str = "" })
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

let filters expectation =
  match expectation.expected_output with
  | Toplevel _ -> []
  | Filtered expected -> List.concat_map ~f:fst expected

let current_arch_filter () =
  match Target_system.architecture () with
  | X86_64 -> Some X86_64
  | AArch64 | IA32 | ARM | POWER | Z | Riscv -> None

(* Outputs that are the same as the output without parameters are omitted. *)
let omit_redundant_outputs outputs =
  let without_parameters =
    Parameter.Set.Map.find Parameter.Set.empty outputs
  in
  Parameter.Set.Map.filter (fun parameters output ->
    Parameter.Set.is_empty parameters
    || output.str <> without_parameters.str)
    outputs

(* For [%%expect]: [outputs] has the output of each run, the run without
   parameters first. Outputs for parameters that were not run are dropped. *)
let eval_toplevel_expectation expectation expected ~outputs =
  let tag parameters =
    match Parameter.Set.Map.find_opt parameters expected with
    | Some s -> s.tag
    | None -> (Parameter.Set.Map.find Parameter.Set.empty expected).tag
  in
  let corrected =
    Parameter.Set.Map.of_list
      (List.map outputs ~f:(fun (parameters, str) ->
         parameters, { str; tag = tag parameters }))
    |> omit_redundant_outputs
  in
  if Parameter.Set.Map.equal (=) corrected expected
  then None
  else Some { expectation with expected_output = Toplevel corrected }

(* For [%%expect_asm]:
   - All entries must have architecture tags (X86_64)
*)
let eval_filtered_expectation expectation expected ~output ~to_update =
  match to_update with
  | [(filters, s)] when s.str <> output ->
    let s = { s with str = output } in
    Some { expectation with expected_output = Filtered (
      List.map
        ~f:(fun (f, e) -> (f, if List.equal ~eq:(=) f filters then s else e))
        expected)
    }
  | _ :: _ :: _ ->
    Location.raise_errorf
      ~loc:expectation.payload_loc
      "duplicate architectures in [%%%%expect_asm]"
  | _ -> None

let eval_expectation expectation ~outputs =
  match expectation.expected_output with
  | Toplevel expected ->
    eval_toplevel_expectation expectation expected ~outputs
  | Filtered expected ->
    let output = List.assoc Parameter.Set.empty outputs in
    let to_update =
      match expectation.kind with
      | Expect_asm _ ->
        List.filter
          ~f:(fun (f, _) ->
              match current_arch_filter () with
              | None -> false
              | Some arch -> List.mem ~set:f arch)
          expected
      | Expect_toplevel | Expect_fexpr -> expected
    in
    eval_filtered_expectation expectation expected ~output ~to_update

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

let eval_expect_file fname ~file_contents ~execute_phrase =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents ~fname:"" file_contents |> split_chunks
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
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
        | X86_64 -> "")
    |> String.concat ~sep:"\n"
    |> (^) "\n"
  in
  let expectation_outputs =
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
        List.map chunk.expectations ~f:(fun expectation ->
          let output = match expectation.kind with
            | Expect_toplevel -> toplevel_output
            | Expect_asm _ ->
                if success then
                  Option.value asm_output
                    ~default:"\nNo assembly: compilation failed\n"
                else toplevel_output
            | Expect_fexpr ->
                if success then
                  fexpr_outputs last_unit (filters expectation)
                else toplevel_output
          in
          expectation, output)))
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf
        ~f:(fun () -> let (_, r, _, _) = exec_phrases phrases in r)
  in
  { expectation_outputs; trailing_output }

(* [runs] has the run without parameters first. *)
let correct runs =
  let expectations =
    List.map (snd (List.hd runs)).expectation_outputs
      ~f:(fun (expectation, _) -> expectation, [])
  in
  let corrected_expectations =
    List.fold_right runs ~init:expectations
      ~f:(fun (parameters, run) expectations ->
        List.map2 expectations run.expectation_outputs
          ~f:(fun (expectation, outputs) (_, output) ->
            expectation, (parameters, output) :: outputs))
    |> List.filter_map ~f:(fun (expectation, outputs) ->
      eval_expectation expectation ~outputs)
  in
  let trailing_output =
    Parameter.Set.Map.of_list
      (List.map runs ~f:(fun (parameters, (run : run)) ->
         parameters, { str = run.trailing_output; tag = "" }))
  in
  { corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

let output_body oc { str; tag } =
  Printf.fprintf oc "{%s|%s|%s}" tag str tag

(* Parameter sets with the same output are grouped together. *)
let output_toplevel oc outputs =
  let outputs = omit_redundant_outputs outputs in
  output_body oc (Parameter.Set.Map.find Parameter.Set.empty outputs);
  let rec group = function
    | [] -> []
    | (parameters, output) :: rest ->
      let same, different =
        List.partition rest ~f:(fun (_, output') -> output'.str = output.str)
      in
      (parameters :: List.map ~f:fst same, output) :: group different
  in
  Parameter.Set.Map.remove Parameter.Set.empty outputs
  |> Parameter.Set.Map.bindings
  |> group
  |> List.iter ~f:(fun (parameter_sets, output) ->
    output_string oc ", ";
    (match parameter_sets with
     | [parameters] -> output_string oc (Parameter.Set.to_string parameters)
     | _ ->
       output_string oc "(";
       output_string oc
         (String.concat ~sep:", "
            (List.map ~f:Parameter.Set.to_string parameter_sets));
       output_string oc ")");
    output_body oc output)

let output_corrected oc ~file_contents correction =
  let output_entry oc (filter, str_const) =
    List.iter ~f:(fun f -> output_string oc (string_of_filter f)) filter;
    output_body oc str_const
  in
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        (match c.expected_output with
        | Toplevel outputs -> output_toplevel oc outputs
        | Filtered [] -> ()
        | Filtered (entry :: rest) ->
          output_entry oc entry;
          List.iter ~f:(fun e ->
            output_string oc ", ";
            output_entry oc e
          ) rest);
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  if Parameter.Set.Map.exists (fun _ output -> output.str <> "")
       correction.trailing_output
  then begin
    output_string oc "\n[%%expect";
    output_toplevel oc correction.trailing_output;
    output_string oc "]\n"
  end

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

module type Toplevel = sig
  val override_sys_argv : string array -> unit
  val initialize_toplevel_env : unit -> unit
  val load_file : Format.formatter -> string -> bool
  val max_printer_depth : int ref
  val max_printer_steps : int ref
  val isolate_run : (unit -> 'a) -> 'a
  val execute_phrase :
    bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool
end

(* Each run starts from a fresh toplevel, so that one run cannot affect the
   output of the next. This includes the settings that phrases can change
   with attributes and directives, and the state of [Random]. *)
let with_fresh_toplevel (module Toplevel : Toplevel) ~random_state f =
  Env.reset_required_globals ();
  Lambda.reset_raise_count ();
  Out_type.reset ();
  Out_type.reset_weak_names ();
  Lexer.reset_syntax_mode ();
  Random.set_state (Random.State.copy random_state);
  Misc.protect_refs
    [ R (Clflags.zero_alloc_assert, !Clflags.zero_alloc_assert)
    ; R (Clflags.include_dirs, !Clflags.include_dirs)
    ; R (Clflags.hidden_include_dirs, !Clflags.hidden_include_dirs)
    ; R (Clflags.debug, !Clflags.debug)
    ; R (Clflags.all_ppx, !Clflags.all_ppx)
    ; R (Toplevel.max_printer_depth, !Toplevel.max_printer_depth)
    ; R (Toplevel.max_printer_steps, !Toplevel.max_printer_steps)
    ]
    (fun () ->
      Local_store.with_store (Local_store.fresh ()) (fun () ->
        Toplevel.initialize_toplevel_env ();
        f ()))

let process_expect_file fname toplevel =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let module Toplevel = (val toplevel : Toplevel) in
  let command_line = Parameter.Set.of_flags () in
  let warnings = Warnings.backup () in
  let random_state = Random.get_state () in
  let runs =
    List.map parameter_sets ~f:(fun parameters ->
      Parameter.Set.set_flags parameters ~command_line;
      let run =
        Toplevel.isolate_run (fun () ->
          Warnings.with_state warnings (fun () ->
            with_fresh_toplevel toplevel ~random_state (fun () ->
              eval_expect_file fname ~file_contents
                ~execute_phrase:Toplevel.execute_phrase)))
      in
      parameters, run)
  in
  write_corrected ~file:corrected_fname ~file_contents (correct runs)

let repo_root = ref None
let keep_original_error_size = ref false
let preload_objects = ref []
let main_file = ref None

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

let main (module Toplevel : Toplevel) fname =
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
  process_expect_file fname (module Toplevel);
  exit 0

let run ~read_anonymous_arg ~extra_args ~extra_init toplevel =
  let args =
    Arg.align
      ( [ "-repo-root", Arg.String (fun s -> repo_root := Some s),
          "<dir> root of the OCaml repository. This causes the tool to use \
           the stdlib from the current source tree rather than the installed \
           one."
        ; "-keep-original-error-size", Arg.Set keep_original_error_size,
          " truncate long error messages as the compiler would"
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
    match !main_file with
    | Some fname -> main toplevel fname
    | None ->
      Printf.eprintf "expect: no input file\n";
      exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
