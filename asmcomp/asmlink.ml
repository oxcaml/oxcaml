(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Link a set of .cmx/.o files and produce an executable *)

open Misc
open Config
open Cmx_format
open Compilenv
module CU = Compilation_unit

type error =
  | Dwarf_fission_objcopy_on_macos
  | Dwarf_fission_dsymutil_not_macos
  | Dsymutil_error of int
  | Objcopy_error of int

exception Error of error

type unit_link_info = Linkenv.unit_link_info =
  { name : Compilation_unit.t;
    defines : Compilation_unit.t list;
    file_name : string;
    crc : Digest.t;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

let runtime_lib () =
  let variant =
    if Config.runtime5 && !Clflags.runtime_variant = "nnp"
    then ""
    else !Clflags.runtime_variant
  in
  let libname = "libasmrun" ^ variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime
    then []
    else [Load_path.find libname]
  with Not_found -> raise (Linkenv.Error (File_not_found libname))

(* Second pass: generate the startup file and link it with everything else *)

let named_startup_file () =
  !Clflags.keep_startup_file || !Emitaux.binary_backend_available

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata [Cmm.Csymbol_address (Cmm.global_symbol "caml_startup")])

let sourcefile_for_dwarf ~named_startup_file filename =
  (* Ensure the name emitted into the DWARF is stable, for build reproducibility
     purposes. *)
  if named_startup_file then filename else ".startup"

let emit_ocamlrunparam ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata
       [ Cmm.Cdefine_symbol
           { sym_name = "caml_ocamlrunparam"; sym_global = Global };
         Cmm.Cstring (!Clflags.ocamlrunparam ^ "\000") ])

let make_startup_file unix ~ppf_dump ~sourcefile_for_dwarf genfns units
    cached_gen =
  Location.input_name := "caml_startup";
  (* set name of "current" input *)
  let startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_startup")
  in
  let startup_unit_info =
    Unit_info.make_dummy ~input_name:"caml_startup" startup_comp_unit
  in
  Compilenv.reset startup_unit_info;
  Emitaux.Dwarf_helpers.init ~ppf_dump
    ~disable_dwarf:(not !Dwarf_flags.dwarf_for_startup_file)
    ~sourcefile:sourcefile_for_dwarf;
  if !Clflags.llvm_backend
  then Llvmize.begin_assembly ~is_startup:true ~sourcefile:sourcefile_for_dwarf
  else Emit.begin_assembly unix;
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  let name_list = List.flatten (List.map (fun u -> u.defines) units) in
  emit_ocamlrunparam ~ppf_dump;
  List.iter compile_phrase (Cmm_helpers.entry_point name_list);
  List.iter compile_phrase
    (* Emit the GC roots table, for dynlink. *)
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
       (Generic_fns.compile ~cache:false ~shared:false genfns));
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table name_list);
  let globals_map = Linkenv.make_globals_map units in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  compile_phrase
    (Cmm_helpers.data_segment_table (startup_comp_unit :: name_list));
  (* CR mshinwell: We should have a separate notion of "backend compilation
     unit" really, since the units here don't correspond to .ml source files. *)
  let hot_comp_unit = CU.create CU.Prefix.empty (CU.Name.of_string "_hot") in
  let system_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_system")
  in
  let code_comp_units =
    if !Clflags.function_sections
    then hot_comp_unit :: startup_comp_unit :: name_list
    else startup_comp_unit :: name_list
  in
  let code_comp_units =
    if !Oxcaml_flags.use_cached_generic_functions
    then Generic_fns.imported_units cached_gen @ code_comp_units
    else code_comp_units
  in
  compile_phrase (Cmm_helpers.code_segment_table code_comp_units);
  let all_comp_units = startup_comp_unit :: system_comp_unit :: name_list in
  let all_comp_units =
    if !Oxcaml_flags.use_cached_generic_functions
    then Generic_fns.imported_units cached_gen @ all_comp_units
    else all_comp_units
  in
  compile_phrase (Cmm_helpers.frame_table all_comp_units);
  if !Clflags.output_complete_object then force_linking_of_startup ~ppf_dump;
  if !Clflags.llvm_backend
  then Llvmize.end_assembly ()
  else Emit.end_assembly ()

let make_shared_startup_file unix ~ppf_dump ~sourcefile_for_dwarf genfns units =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  let shared_startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_shared_startup")
  in
  let shared_startup_unit_info =
    Unit_info.make_dummy ~input_name:"caml_startup" shared_startup_comp_unit
  in
  Compilenv.reset shared_startup_unit_info;
  Emitaux.Dwarf_helpers.init ~ppf_dump
    ~disable_dwarf:(not !Dwarf_flags.dwarf_for_startup_file)
    ~sourcefile:sourcefile_for_dwarf;
  if !Clflags.llvm_backend
  then Llvmize.begin_assembly ~is_startup:true ~sourcefile:sourcefile_for_dwarf
  else Emit.begin_assembly unix;
  emit_ocamlrunparam ~ppf_dump;
  List.iter compile_phrase
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
       (Generic_fns.compile ~cache:false ~shared:true genfns));
  let dynunits = List.map (fun u -> Option.get u.dynunit) units in
  compile_phrase (Cmm_helpers.plugin_header dynunits);
  compile_phrase
    (Cmm_helpers.global_table (List.map (fun unit -> unit.name) units));
  if !Clflags.output_complete_object then force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker might drop
     some of them (in case of libraries) *)
  if !Clflags.llvm_backend
  then Llvmize.end_assembly ()
  else Emit.end_assembly ()

let call_linker_shared ?(native_toplevel = false) file_list output_name =
  let exitcode =
    Ccomp.call_linker ~native_toplevel Ccomp.Dll output_name file_list ""
  in
  if not (exitcode = 0) then raise (Linkenv.Error (Linking_error exitcode))

(* The compiler allows [-o /dev/null], which can be used for testing linking. In
   this case, we should not use the DWARF fission workflow during linking. *)
let not_output_to_dev_null output_name =
  not (String.equal output_name "/dev/null")

let link_shared unix ml_objfiles output_name ~genfns ~units_tolink ~ppf_dump =
  if !Oxcaml_flags.use_cached_generic_functions
  then
    (* When doing shared linking do not use the shared generated startup file.
       Frametables for the imported functions needs to be initialized, which is
       a bit tricky to do in the context of shared libraries as the frametables
       are initialized at runtime. *)
    Oxcaml_flags.use_cached_generic_functions := false;
  if !Oxcaml_flags.internal_assembler
  then
    (* CR-soon gyorsh: workaround to turn off internal assembler temporarily,
       until it is properly tested for shared library linking. *)
    Emitaux.binary_backend_available := false;
  let objfiles = List.rev ml_objfiles @ List.rev !Clflags.ccobjs in
  let named_startup_file = named_startup_file () in
  let startup =
    if named_startup_file
    then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm
  in
  let startup_obj = output_name ^ ".startup" ^ ext_obj in
  let sourcefile_for_dwarf = sourcefile_for_dwarf ~named_startup_file startup in
  Asmgen.compile_unit ~output_prefix:output_name ~asm_filename:startup
    ~keep_asm:!Clflags.keep_startup_file ~obj_filename:startup_obj
    ~may_reduce_heap:true ~ppf_dump (fun () ->
      make_shared_startup_file unix ~ppf_dump
        ~sourcefile_for_dwarf:(Some sourcefile_for_dwarf) genfns units_tolink);
  call_linker_shared (startup_obj :: objfiles) output_name;
  if !Oxcaml_flags.internal_assembler
  then
    (* CR gyorsh: restore after workaround. *)
    Emitaux.binary_backend_available := true;
  remove_file startup_obj

let call_linker file_list_rev startup_file output_name =
  let main_dll =
    !Clflags.output_c_object && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object in
  let file_list_rev =
    if !Oxcaml_flags.use_cached_generic_functions
    then !Oxcaml_flags.cached_generic_functions_path :: file_list_rev
    else file_list_rev
  in
  let files = startup_file :: List.rev file_list_rev in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime
    then
      ( files @ List.rev !Clflags.ccobjs @ runtime_lib (),
        if !Clflags.nopervasives || (main_obj_runtime && not main_dll)
        then ""
        else Config.native_c_libraries )
    else files, ""
  in
  let mode =
    if main_dll
    then Ccomp.MainDll
    else if !Clflags.output_c_object
    then Ccomp.Partial
    else Ccomp.Exe
  in
  (* Determine if we need to use a temporary file for objcopy workflow *)
  (* We disable the objcopy workflow if the output is piped to /dev/null. *)
  let needs_objcopy_workflow =
    not_output_to_dev_null output_name
    && !Clflags.dwarf_fission = Clflags.Fission_objcopy
    && (not (Target_system.is_macos ()))
    && mode = Ccomp.Exe
    && not !Dwarf_flags.restrict_to_upstream_dwarf
  in
  let link_output_name =
    if needs_objcopy_workflow
    then Filename.temp_file (Filename.basename output_name) ".tmp"
    else output_name
  in
  let exitcode = Ccomp.call_linker mode link_output_name files c_lib in
  if not (exitcode = 0)
  then (
    if needs_objcopy_workflow then Misc.remove_file link_output_name;
    raise (Linkenv.Error (Linking_error exitcode)))
  else
    (* Handle DWARF fission if requested and linking succeeded *)
    match !Clflags.dwarf_fission with
    | Fission_none -> ()
    | Fission_objcopy ->
      if Target_system.is_macos ()
      then raise (Error Dwarf_fission_objcopy_on_macos)
      else if needs_objcopy_workflow
      then (
        (* Run objcopy to extract debug info into .debug file *)
        let debug_file = output_name ^ ".debug" in
        let compression_flag =
          match Dwarf_flags.get_dwarf_objcopy_compression_format () with
          | Some compression ->
            Printf.sprintf " %s=%s" Config.objcopy_compress_debug_sections_flag
              compression
          | None -> ""
        in
        let objcopy_cmd_create_debug =
          Printf.sprintf
            "%s --enable-deterministic-archives --only-keep-debug%s %s %s"
            Config.objcopy compression_flag
            (Filename.quote link_output_name)
            (Filename.quote debug_file)
        in
        let objcopy_exit = Ccomp.command objcopy_cmd_create_debug in
        if objcopy_exit <> 0
        then (
          Misc.remove_file link_output_name;
          raise (Error (Objcopy_error objcopy_exit)));
        let objcopy_cmd_create_stripped_exe =
          Printf.sprintf
            "%s --enable-deterministic-archives --strip-debug \
             --add-gnu-debuglink=%s %s %s"
            Config.objcopy
            (Filename.quote debug_file)
            (Filename.quote link_output_name)
            (Filename.quote output_name)
        in
        let objcopy_exit = Ccomp.command objcopy_cmd_create_stripped_exe in
        Misc.remove_file link_output_name;
        if objcopy_exit <> 0 then raise (Error (Objcopy_error objcopy_exit)))
    | Fission_dsymutil ->
      if not (Target_system.is_macos ())
      then raise (Error Dwarf_fission_dsymutil_not_macos)
      else if not_output_to_dev_null output_name
              && mode = Ccomp.Exe
              && not !Dwarf_flags.restrict_to_upstream_dwarf
      then
        (* Run dsymutil on the executable *)
        let dsymutil_cmd =
          Printf.sprintf "dsymutil %s" (Filename.quote output_name)
        in
        let dsymutil_exit = Ccomp.command dsymutil_cmd in
        if dsymutil_exit <> 0 then raise (Error (Dsymutil_error dsymutil_exit))

(* Main entry point *)

let link unix ml_objfiles output_name ~cached_genfns_imports ~genfns
    ~units_tolink ~ppf_dump : unit =
  if !Oxcaml_flags.internal_assembler
  then Emitaux.binary_backend_available := true;
  let named_startup_file = named_startup_file () in
  let startup =
    if named_startup_file
    then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm
  in
  let sourcefile_for_dwarf = sourcefile_for_dwarf ~named_startup_file startup in
  let startup_obj = Filename.temp_file "camlstartup" ext_obj in
  Asmgen.compile_unit ~output_prefix:output_name ~asm_filename:startup
    ~keep_asm:!Clflags.keep_startup_file ~obj_filename:startup_obj
    ~may_reduce_heap:true ~ppf_dump (fun () ->
      make_startup_file unix ~ppf_dump
        ~sourcefile_for_dwarf:(Some sourcefile_for_dwarf) genfns units_tolink
        cached_genfns_imports);
  Emitaux.reduce_heap_size ~reset:(fun () -> Linkenv.reset ());
  Misc.try_finally
    (fun () -> call_linker ml_objfiles startup_obj output_name)
    ~always:(fun () -> remove_file startup_obj)

(* Error report *)

open Format

let report_error ppf = function
  | Dwarf_fission_objcopy_on_macos ->
    fprintf ppf
      "Error: -gdwarf-fission=objcopy is not supported on macOS systems.@ \
       Please use -gdwarf-fission=dsymutil instead."
  | Dwarf_fission_dsymutil_not_macos ->
    fprintf ppf
      "Error: -gdwarf-fission=dsymutil is only supported on macOS systems."
  | Dsymutil_error exitcode ->
    fprintf ppf "Error running dsymutil (exit code %d)" exitcode
  | Objcopy_error exitcode ->
    fprintf ppf "Error running objcopy (exit code %d)" exitcode

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
