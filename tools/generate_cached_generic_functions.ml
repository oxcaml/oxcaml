(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2023 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

open Printf
open Misc
open Config

module CU = Compilation_unit

(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

let make_cached_generic_functions unix ~ppf_dump ~id genfns =
  let name = Generic_fns.Partition.name id in
  Location.input_name := name; (* set name of "current" input *)
  let startup_comp_unit = Generic_fns.Partition.to_cu id in
  let startup_unit_info =
    Unit_info.make_dummy ~input_name:name startup_comp_unit
  in
  Compilenv.reset startup_unit_info;
  Emit.begin_assembly unix;
  Asmgen.emit_codegen_variant_guard_reference ();
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Profile.record_call "genfns" (fun () ->
    List.iter compile_phrase (
      Generic_fns.compile ~cache:true ~shared:true genfns));
 Emit.end_assembly ()

let cached_generic_functions unix ~ppf_dump ~id output_name genfns =
  Profile.record_call output_name (fun () ->
    let startup = output_name ^ ext_asm in
    Profile.record_call "compile_unit" (fun () ->
      let obj_filename = output_name ^ ext_obj in
      Asmgen.compile_unit unix ~output_prefix:output_name
        ~asm_filename:startup ~keep_asm:false
        ~obj_filename
        ~may_reduce_heap:true
        ~ppf_dump
        (fun () ->
          make_cached_generic_functions unix ~ppf_dump ~id genfns);
      obj_filename
    );
  )

let generate filename =
  let unix = (module Unix_for_owee : Compiler_owee.Unix_intf.S) in
  Clflags.set_target Clflags.Target.Native;
  (* The archive is always emitted by this backend, whatever the environment
     selects for ordinary compilations. *)
  Clflags.llvm_backend := false;
  Clflags.use_linscan := true;
  Clflags.function_sections := Config.function_sections;
  Compmisc.init_path ();
  let file_prefix = Filename.remove_extension filename ^ ext_lib in
  let genfns_partitions = Generic_fns.Cache.all () in
  let objects = ref [] in
  let temporary_files = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter remove_file !temporary_files)
    (fun () ->
       Hashtbl.iter (fun name partition ->
         let output_name = Filename.temp_file ("cached-generated-" ^ Generic_fns.Partition.to_string name) "" in
         temporary_files := output_name :: !temporary_files;
         let obj =
           cached_generic_functions
             unix ~ppf_dump:Format.std_formatter ~id:name output_name partition
         in
         temporary_files := obj :: !temporary_files;
         objects := obj :: !objects
       ) genfns_partitions;
       if Ccomp.create_archive file_prefix !objects <> 0
       then begin
         remove_file file_prefix;
         Location.input_name := file_prefix;
         raise (Linkenv.Error (Archiver_error file_prefix))
       end)

let arg_usage =
  Printf.sprintf
    "%s [options] FILE : Generate the archive of cached generic functions \
     FILE (with the extension of archives) for a codegen variant: the one \
     that -codegen-variant names, whatever the other options and OCAMLPARAM \
     select, and without that option the one that ocamlopt selects with the \
     same options and the same OCAMLPARAM. The archive is always emitted by \
     the native backend: llvm-backend in OCAMLPARAM is not followed"
    Sys.argv.(0)

(* The variant that -codegen-variant names is the archive to generate (the
   compiler prints such a command when the archive of a variant is missing):
   it is applied after everything else, so that neither another option nor
   the part of OCAMLPARAM that overrides the command line changes it. *)
let named_codegen_variant = ref None

let set_codegen_variant name =
  match Codegen_variant.of_string name with
  | Some variant -> named_codegen_variant := Some variant
  | None ->
    raise (Arg.Bad (Printf.sprintf "unknown codegen variant %S" name))

(* An archive is for one frame-pointer, stack-check and AddressSanitizer
   setting: its members refer to the link-time guard of that setting
   ([Asmgen.emit_codegen_variant_guard_reference]), so the linker refuses
   the archive in a program built for another one.  Without
   -codegen-variant the setting is selected as for ocamlopt, so that a build
   that selects its codegen variant with flags or with OCAMLPARAM gets the
   matching archive by running this tool the same way.  Poll insertion does
   not change the archive; its options are accepted so that all the flags of
   a variant can be passed. *)
let arg_spec =
  [ ( "-codegen-variant",
      Arg.String set_codegen_variant,
      "<name>  Generate the archive for the named codegen variant (e.g. \
       base or fp), whatever the other options and OCAMLPARAM select" );
    ( "-frame-pointers",
      Arg.Set Clflags.frame_pointers,
      " Generate the archive for code with frame pointers" );
    ( "-no-frame-pointers",
      Arg.Clear Clflags.frame_pointers,
      " Generate the archive for code without frame pointers" );
    ( "-stack-checks",
      Arg.Set Clflags.stack_checks,
      " Generate the archive for code with stack checks" );
    ( "-no-stack-checks",
      Arg.Clear Clflags.stack_checks,
      " Generate the archive for code without stack checks" );
    ( "-asan",
      Arg.Set Clflags.address_sanitizer,
      " Generate the archive for code with AddressSanitizer" );
    ( "-no-asan",
      Arg.Clear Clflags.address_sanitizer,
      " Generate the archive for code without AddressSanitizer" );
    ( "-enable-poll-insertion",
      Arg.Set Clflags.poll_insertion,
      " Accepted as for ocamlopt; does not change the archive" );
    ( "-disable-poll-insertion",
      Arg.Clear Clflags.poll_insertion,
      " Accepted as for ocamlopt; does not change the archive" ) ]

(* The output file is only recorded during parsing and the generation runs
   once parsing is complete, so the variant flags take effect regardless of
   whether they appear before or after FILE. *)
let output_file = ref None

let set_output_file filename =
  match !output_file with
  | None -> output_file := Some filename
  | Some _ -> raise (Arg.Bad "only one FILE may be given")

let main () =
  Memtrace.trace_if_requested ~context:"ocamlopt" ();
  (try
     (* OCAMLPARAM is read as by ocamlopt: what precedes "_" before the
        options, what follows it after them. *)
     Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
     Compenv.readenv Format.err_formatter Before_args;
     Arg.parse_expand arg_spec set_output_file arg_usage;
     Compenv.readenv Format.err_formatter Before_link;
     Option.iter Codegen_variant.apply !named_codegen_variant;
     match !output_file with
     | None ->
       prerr_endline arg_usage;
       exit 2
     | Some filename -> generate filename
   with
   | Compenv.Exit_with_status status -> exit status
   | exn ->
     Location.report_exception Format.err_formatter exn;
     exit 2);
  exit 0

let _ = main ()
