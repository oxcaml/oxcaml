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

type expectation_filter = Principal | X86_64 | AArch64

type expectation_kind = Expect_toplevel | Expect_asm

type expectation =
  { extid_loc       : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc     : Location.t (* Location of the whole payload *)
  ; expected_output : (expectation_filter option * string_constant) list
  ; kind            : expectation_kind
  }

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases     : Parsetree.toplevel_phrase list
  ; expectations : expectation list
  }

let register_assembly_callback :
  ((string -> unit) -> unit) option ref = ref None

type correction =
  { corrected_expectations : expectation list
  ; trailing_output        : string
  }

let filter_of_string = function
  | "Principal" -> Some Principal
  | "X86_64" -> Some X86_64
  | "AArch64" -> Some AArch64
  | _ -> None

let string_of_filter = function
  | Principal -> "Principal"
  | X86_64 -> "X86_64"
  | AArch64 -> "AArch64"

let match_expect_extension (ext : Parsetree.extension) =
  let match_ext_name = function
    | "expect" | "ocaml.expect" -> Some Expect_toplevel
    | "expect_asm" | "ocaml.expect_asm" -> Some Expect_asm
    | _ -> None
  in
  match ext with
  | ({Asttypes.txt; loc = extid_loc}, payload) ->
    match match_ext_name txt with
    | None -> None
    | Some kind ->
    let invalid_payload ?(loc=extid_loc) ?(msg = "invalid [%%expect payload]") () =
      Location.raise_errorf ~loc "%s" msg
    in
    if Option.is_none !register_assembly_callback && kind = Expect_asm
    then invalid_payload ~msg:"expect_asm is only supported by expect.opt" ();
    let string_constant (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (str, _, Some tag)) ->
        { str; tag }
      | _ -> invalid_payload ~loc:e.pexp_loc ()
    in
    (* Parse a single element: either {|...|} or Filter{|...|} *)
    let parse_element (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string _) ->
        (* Bare string constant - no filter *)
        (None, string_constant e)
      | Pexp_construct ({ txt = Lident name; }, Some arg) -> (
        (* Filter{|content|} - filter with string content *)
        match filter_of_string name with
        | Some filter -> (Some filter, string_constant arg)
        | None -> invalid_payload ~msg:("unexpected filter \""^name^"\"") ~loc:e.pexp_loc ())
      | _ -> invalid_payload ~loc:e.pexp_loc ~msg:("expected {|...|} or Filter{|...|}") ()
    in
    let is_arch_filter = function
      | X86_64 | AArch64 -> true
      | Principal -> false
    in
    let validate_expect_toplevel entries =
      (* Valid formats:
         - [{|...|}] (one untagged)
         - [{|...|}, Principal{|...|}] (untagged + Principal)
         - [Principal{|...|}] (Principal only) *)
      match entries with
      | [(None, _)] -> entries
      | [(None, _); (Some Principal, _)]-> entries
      | _ -> invalid_payload ~msg:"expected [%%expect {|...|}] or [%%expect {|...|}, Principal{|...|}]" ()
    in
    let validate_expect_asm entries =
      (* All entries must have architecture tags *)
      if List.for_all ~f:(fun (f, _) ->
        match f with
        | Some filter -> is_arch_filter filter
        | None -> false
      ) entries
      then entries
      else invalid_payload ~msg:"expected [%%expect_asm Arch1{|...|}, Arch2{|...|}, ...]" ()
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
          | Expect_asm -> validate_expect_asm expected_output
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
          ; expected_output = [(None, { tag = ""; str = "" })]
          ; kind
          }
        | Expect_asm -> invalid_payload ())
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
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc expect_acc acc
      | Ptop_def [{pstr_desc = Pstr_extension(ext, [])}] -> begin
          match match_expect_extension ext with
          | None -> loop phrases (phrase :: code_acc) expect_acc acc
          | Some expectation ->
            loop phrases code_acc (expectation :: expect_acc) acc
        end
      | _ -> begin
        match expect_acc with
        | [] -> loop phrases (phrase :: code_acc) [] acc
        | _ ->
          let chunk =
            { phrases = List.rev code_acc
            ; expectations = List.rev expect_acc
            }
          in
          loop phrases [phrase] [] (chunk :: acc)
      end
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
  | AArch64 -> Some AArch64
  | IA32 | ARM | POWER | Z | Riscv -> None

(* For [%%expect]:
   - {|...|} alone: used for both principal and non-principal
   - {|...|}, Principal{|...|}: first for non-principal, second for principal

   For [%%expect_asm]:
   - All entries must have architecture tags (X86_64, AArch64)
*)
let eval_expectation expectation ~output =
  let to_update = match expectation.kind with
  | Expect_toplevel ->
    (match expectation.expected_output with
      | [(None, expected)] -> [(None, expected)]
      | [(None, if_not_principal); (Some Principal, if_principal)] ->
        if !Clflags.principal
        then [(Some Principal, if_principal)]
        else [(None, if_not_principal)]
      | _ -> Misc.fatal_error "impossible: already validated")
  | Expect_asm -> List.filter ~f:(fun (f, _) -> f = current_arch_filter ()) expectation.expected_output in
  match to_update with
  | [(filter, s)] when s.str <> output ->
    let s = { s with str = output } in
    Some { expectation with expected_output =
      List.map ~f:(fun (f, e) -> (f, if f = filter then s else e)) expectation.expected_output
    }
  | _ :: _ :: _ -> Location.raise_errorf ~loc:expectation.payload_loc "duplicate architectures in [%%%%expect_asm]"
  | _ -> None

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

let eval_expect_file _fname ~file_contents ~execute_phrase =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents ~fname:"" file_contents |> split_chunks
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let () = Misc.Style.set_tag_handling ppf in
  let last_asm = ref None in
  let exec_phrases phrases =
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
    let _ : bool =
      List.fold_left phrases ~init:true ~f:(fun acc phrase ->
        acc && exec_one phrase)
    in
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* For formatting purposes *)
      Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    (Misc.delete_eol_spaces s, !last_asm)
  in
  let corrected_expectations =
    capture_everything buf ppf ~f:(fun () ->
      List.concat_map chunks ~f:(fun chunk ->
        let (toplevel_output, asm_output) = exec_phrases chunk.phrases in
        List.filter_map chunk.expectations ~f:(fun expectation ->
          let output = match expectation.kind with
            | Expect_toplevel -> toplevel_output
            | Expect_asm -> Option.value asm_output ~default:""
          in
          eval_expectation expectation ~output)))
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf
        ~f:(fun () -> fst (exec_phrases phrases))
  in
  { corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

let output_corrected oc ~file_contents correction =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  let output_entry oc (filter, str_const) =
    Option.iter (fun f -> output_string oc (string_of_filter f)) filter;
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

let process_expect_file fname ~execute_phrase =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let correction = eval_expect_file fname ~file_contents ~execute_phrase in
  write_corrected ~file:corrected_fname ~file_contents correction

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
        Compenv.last_include_dirs := [Filename.concat dir "stdlib"]
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
  process_expect_file fname ~execute_phrase:Toplevel.execute_phrase;
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
