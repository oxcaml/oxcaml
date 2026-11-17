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
  | Cm_bundle_error of Cm_bundle.error

exception Error of error

type unit_link_info = Linkenv.unit_link_info =
  { name : Compilation_unit.t;
    defines : Compilation_unit.t list;
    file_name : string;
    crc : Digest.t;
    imports_cmx : Import_info.t list;
    codegen_fingerprint : Codegen_variant.fingerprint;
    polled : bool;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

let cached_generic_functions_path () =
  match !Oxcaml_flags.cached_generic_functions_path with
  | Some path ->
    (* An explicit -cached-generic-functions-path is passed through
       untouched. *)
    path
  | None ->
    (* The default path points into the library tree that serves the selected
       variant, and installations do not necessarily ship the archive (make
       installs never do); fail clearly rather than with a linker error. *)
    let path = Oxcaml_flags.get_cached_generic_functions_path () in
    let variant = Codegen_variant.current () in
    let tree = Codegen_variant.selected_tree () in
    let root_archive =
      Filename.concat Config.standard_library (Filename.basename path)
    in
    let trusted =
      (* The archive carries no fingerprint of its own. The root is taken at its
         word for the default variant, and so is a tree under variants/ for its
         own. When the root serves the default variant with the other
         poll-insertion setting it is checked like a root that has no tree for
         the selected variant: OCAMLLIB can make it anything. *)
      match tree with
      | Some (tree_variant, _root) ->
        Codegen_variant.equal tree_variant variant
        || (not (Codegen_variant.is_default tree_variant))
        || Linkenv.standard_library_is_built_for variant
      | None -> false
    in
    if trusted && Sys.file_exists path
    then path
    else if
      (* The root has no tree that serves the selected variant, but it may
         itself be such a tree. The archive carries no fingerprint that the
         compiler can read (the linker refuses one generated for other
         frame-pointer, stack-check or AddressSanitizer settings, through the
         link-time guard, but less clearly than the error below), so its root's
         archive is only used when the root is verifiably built for the
         fingerprint of the selected variant. *)
      Option.is_none tree
      && Sys.file_exists root_archive
      && Linkenv.standard_library_is_built_for variant
    then root_archive
    else
      let variant =
        match tree with Some (variant, _root) -> variant | None -> variant
      in
      raise
        (Linkenv.Error
           (Cached_generic_functions_not_found
              { filename = path; variant = Codegen_variant.to_string variant }))

let runtime_lib () =
  let variant =
    if !Clflags.runtime_variant = "nnp" then "" else !Clflags.runtime_variant
  in
  (* Archives for the install's default codegen variant have unsuffixed names;
     other variants are suffixed with the projection of the variant name onto
     the runtime axes. The naming rule is described in stdlib/dune_rules/gen.ml
     (write_runtime_dune). *)
  let codegen_variant = Codegen_variant.current () in
  let codegen_suffix = Codegen_variant.runtime_archive_suffix codegen_variant in
  let libname = "libasmrun" ^ variant ^ codegen_suffix ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime
    then []
    else [Load_path.find libname]
  with Not_found ->
    let unsuffixed_exists =
      (not (String.equal codegen_suffix ""))
      &&
      match Load_path.find ("libasmrun" ^ variant ^ ext_lib) with
      | (_ : string) -> true
      | exception Not_found -> false
    in
    (* Blame the codegen variant only when the runtime variant itself exists for
       the install's default codegen mode and the installation has no library
       tree for the selected variant (and is not one itself). With such a tree,
       what is missing is this file. *)
    if
      unsuffixed_exists
      && Option.is_none (Codegen_variant.selected_tree ())
      && not (Linkenv.standard_library_is_built_for codegen_variant)
    then
      raise
        (Linkenv.Error
           (Codegen_variant_runtime_not_found
              { filename = libname;
                variant = Codegen_variant.to_string codegen_variant
              }))
    else raise (Linkenv.Error (File_not_found libname))

(* Second pass: generate the startup file and link it with everything else *)

(* The startup file contains only generated code (entry point, generic
   functions, tables), which is not worth recording coverage for. *)
let without_trace_pc_guard f =
  Misc.protect_refs [Misc.R (Clflags.gated_trace_pc_guard, false)] f

let named_startup_file () =
  !Clflags.keep_startup_file || !Emitaux.binary_backend_available

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata
       [ Cmm.Csymbol_address
           (Cmm.global_symbol ~ty:Cmm.typ_code_pointer "caml_startup") ])

let sourcefile_for_dwarf ~named_startup_file filename =
  (* Ensure the name emitted into the DWARF is stable, for build reproducibility
     purposes. *)
  if named_startup_file then filename else ".startup"

let emit_ocamlrunparam ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata
       [ Cmm.Cdefine_symbol
           (Cmm.global_symbol ~ty:Cmm.typ_out_of_heap_data_pointer
              "caml_ocamlrunparam");
         Cmm.Cstring (!Clflags.ocamlrunparam ^ "\000") ])

(* Whether every OCaml unit being linked was compiled with poll insertion: the
   AND of the per-unit [polled] bits over [units]. The poll-insertion setting of
   the link itself is not part of it: it only chooses the library tree (see
   [Codegen_variant.selected_tree]), whose units have their own bits.

   Generic functions take part in the link without a [unit_link_info], the
   cached ones from the archive of the selected tree and the others compiled
   into this startup file, and none of them ever gets polls, under either
   setting (see [Cfg_polling.function_is_assumed_to_never_poll]). This does not
   weaken what the bit stands for, because none of them can run unboundedly
   between two polls of the code that it calls: [caml_apply*], [caml_curry*] and
   [caml_tuplify*] have no loop, and the one loop, the method lookup of
   [caml_send*] on a cache miss ([Cmm_helpers.cache_public_method]), is a binary
   search bounded by the size of the method table. A function added to
   [Generic_fns] must keep that property.

   The runtime reads the symbol at startup to pick the default domain cap (see
   runtime/startup_aux.c); it is emitted only into the main startup object
   ([make_startup_file], which also serves -output-obj), never into the
   shared-startup object of a .cmxs, which must not carry a second definition
   into the host program. *)
let emit_code_fully_polled linkenv ~ppf_dump units =
  let fully_polled = List.for_all (fun u -> u.polled) units in
  (* A link with poll insertion asks for the libraries that have it, and gets
     the others where the installation has none. A program that links a unit of
     those is limited to one domain however its own units were compiled, which
     nothing else tells whoever builds it. This is an alert and not a warning:
     such a link has always been accepted, and must not fail where warnings are
     errors. It is about the units that are linked and what their unit infos
     say, not about the tree that was searched or its name: a program that takes
     nothing from the libraries is fully polled when its own units are. And it
     is about the standard library: the file that holds the unit Stdlib, of
     which a link may take other units only, and the unit Std_exit, which is a
     file of its own, wherever the link found them (a directory given with -I
     comes before the library root) and however their paths are spelled. A unit
     of the program without polls is the program's own business, wherever its
     file is, unless it is named Std_exit or its file holds the unit Stdlib. *)
  (if (Codegen_variant.current ()).poll_insertion
   then
     let of_the_standard_library u =
       Linkenv.file_holds_unit linkenv u.file_name
         (Compilation_unit.of_string "Stdlib")
       || String.equal (Compilation_unit.full_path_as_string u.name) "Std_exit"
     in
     match
       List.find_opt
         (fun u -> (not u.polled) && of_the_standard_library u)
         units
     with
     | None -> ()
     | Some unit ->
       Location.prerr_alert Location.none
         { Warnings.kind = "no_polled_libraries";
           use = Location.none;
           def = Location.none;
           message =
             Printf.sprintf
               "the libraries in %s, which this link uses for codegen variant \
                %s, were compiled without poll insertion (%s, for one), so the \
                program is not fully polled and is limited to one domain by \
                default."
               (Filename.dirname unit.file_name)
               (Codegen_variant.to_string (Codegen_variant.current ()))
               (Compilation_unit.full_path_as_string unit.name)
         });
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata
       [ Cmm.Cdefine_symbol
           (Cmm.global_symbol ~ty:Cmm.typ_out_of_heap_data_pointer
              "caml_code_fully_polled");
         Cmm.Cint (if fully_polled then 1n else 0n) ]);
  (* The object files of units without polls refer to this definition
     ([Asmgen.emit_unpolled_code_reference]): the linker then refuses one in a
     program that is fully polled by its unit infos, also when another image of
     the process defines the symbol. *)
  if fully_polled
  then Asmgen.emit_unpolled_code_exclusion ()
  else Asmgen.emit_unpolled_code_definition ~exported:true

(* With [-gated-trace-pc-guard-define-gate], the startup file defines the gate
   of [Cfg_trace_pc_guard]. This definition takes precedence over the weak ones
   of instrumented units. *)
let emit_trace_pc_guard_gate () =
  if !Clflags.gated_trace_pc_guard_define_gate
  then (
    if !Clflags.llvm_backend
    then
      Misc.fatal_error
        "-gated-trace-pc-guard-define-gate is not supported with -llvm-backend";
    Emit_sancov_common.emit_gate_definition ~weak:false)

let make_startup_file linkenv unix ~ppf_dump ~sourcefile_for_dwarf genfns units
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
  Asmgen.emit_codegen_variant_guard_of_startup ();
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  let name_list = List.flatten (List.map (fun u -> u.defines) units) in
  (* In manual module init mode, entry_point and global_table should be empty *)
  let init_name_list =
    if !Oxcaml_flags.manual_module_init then [] else name_list
  in
  emit_ocamlrunparam ~ppf_dump;
  emit_code_fully_polled linkenv ~ppf_dump units;
  List.iter compile_phrase (Cmm_helpers.entry_point init_name_list);
  List.iter compile_phrase
    (* Emit the GC roots table, for dynlink. *)
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
       (Generic_fns.compile ~cache:false ~shared:false genfns));
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table init_name_list);
  let globals_map = Linkenv.make_globals_map linkenv units in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  (* CR mshinwell: We should have a separate notion of "backend compilation
     unit" really, since the units here don't correspond to .ml source files. *)
  let system_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_system")
  in
  let code_comp_units = startup_comp_unit :: name_list in
  let code_comp_units =
    if !Oxcaml_flags.use_cached_generic_functions
    then Generic_fns.imported_units cached_gen @ code_comp_units
    else code_comp_units
  in
  (* The data of every object file whose code is in the code segment table, the
     cached generic functions included: an object file's reference to the
     link-time guard is at the start of its data, and a linker that discards
     unreferenced sections must keep it wherever it keeps the code. *)
  compile_phrase (Cmm_helpers.data_segment_table code_comp_units);
  compile_phrase (Cmm_helpers.code_segment_table code_comp_units);
  (* The runtime reads the code segment table and not the data segment table.
     This word follows the former and belongs to it for a linker that discards
     unreferenced symbols, which then keeps the latter. *)
  compile_phrase
    (Cmm.Cdata
       [ Cmm.Csymbol_address
           (Cmm.global_symbol ~ty:Cmm.typ_out_of_heap_data_pointer
              "caml_data_segments") ]);
  let all_comp_units = startup_comp_unit :: system_comp_unit :: name_list in
  let all_comp_units =
    if !Oxcaml_flags.use_cached_generic_functions
    then Generic_fns.imported_units cached_gen @ all_comp_units
    else all_comp_units
  in
  (* In manual module init mode, exclude user modules from the static frame
     table; their frametables will be registered dynamically when each module is
     initialized via caml_init_module. *)
  let frame_table_units =
    if !Oxcaml_flags.manual_module_init
    then
      let base = [startup_comp_unit; system_comp_unit] in
      if !Oxcaml_flags.use_cached_generic_functions
      then Generic_fns.imported_units cached_gen @ base
      else base
    else all_comp_units
  in
  compile_phrase (Cmm_helpers.frame_table frame_table_units);
  (* Always emit unit_deps_table; empty when not in manual module init mode *)
  let unit_deps =
    if !Oxcaml_flags.manual_module_init
    then List.map (fun u -> u.name, u.imports_cmx) units
    else []
  in
  compile_phrase (Cmm_helpers.unit_deps_table unit_deps);
  if !Clflags.output_complete_object then force_linking_of_startup ~ppf_dump;
  emit_trace_pc_guard_gate ();
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
  (* A plugin vouches for its units, whose headers tell the loader what they
     were compiled for. A plugin without units has no such header: it refers to
     the guard like other generated code, so not with the LLVM backend, and with
     that reference it can only be opened where the guard is defined. *)
  (match units with
  | [] -> Asmgen.emit_codegen_variant_guard_reference ()
  | _ :: _ -> Asmgen.emit_codegen_variant_guard_definition ());
  emit_ocamlrunparam ~ppf_dump;
  List.iter compile_phrase
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
       (Generic_fns.compile ~cache:false ~shared:true genfns));
  let dynunits = List.map (fun u -> Option.get u.dynunit) units in
  compile_phrase (Cmm_helpers.plugin_header dynunits);
  (* A plugin whose header says that one of its units has no polls defines, for
     itself, what the object files of such units refer to. One whose header says
     that every unit has polls does not, and must not get the definition from
     anywhere else: the link of the plugin fails on an object file without polls
     in it. (Where the object format cannot say that, such an object file needs
     a definition that is visible to the plugin when it is loaded, which a fully
     polled program does not provide.) A plugin without units says nothing of
     the object files that it is made of, with or without polls: it does
     neither, and an object file without polls in it needs a definition that is
     visible to the plugin when it is loaded. *)
  (match units with
  | [] -> ()
  | _ :: _ ->
    if List.for_all (fun u -> u.polled) units
    then Asmgen.emit_unpolled_code_exclusion ()
    else Asmgen.emit_unpolled_code_definition ~exported:false);
  (* In manual module init mode, global_table should be empty *)
  (* CR mshinwell: why does caml_globals need to be populated in this case?
     Note that the frametables are not registered here; they are done
     dynamically by the natdynlink code, together with the GC roots. *)
  let init_name_list =
    if !Oxcaml_flags.manual_module_init
    then []
    else List.map (fun unit -> unit.name) units
  in
  compile_phrase (Cmm_helpers.global_table init_name_list);
  (* Always emit unit_deps_table; empty when not in manual module init mode *)
  let unit_deps =
    if !Oxcaml_flags.manual_module_init
    then List.map (fun u -> u.name, u.imports_cmx) units
    else []
  in
  compile_phrase (Cmm_helpers.unit_deps_table unit_deps);
  emit_trace_pc_guard_gate ();
  if !Clflags.output_complete_object then force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker might drop
     some of them (in case of libraries) *)
  if !Clflags.llvm_backend
  then Llvmize.end_assembly ()
  else Emit.end_assembly ()

let call_linker_shared ?(native_toplevel = false) file_list output_name =
  let exitcode =
    Profile.record_call "link_object" (fun () ->
        Ccomp.call_linker ~native_toplevel Ccomp.Dll output_name file_list "")
  in
  if not (exitcode = 0) then raise (Linkenv.Error (Linking_error exitcode))

(* The compiler allows [-o /dev/null], which can be used for testing linking. In
   this case, we should not use the DWARF fission workflow during linking. *)
let not_output_to_dev_null output_name =
  not (String.equal output_name "/dev/null")

let link_shared_actual unix ml_objfiles output_name ~genfns ~units_tolink
    ~ppf_dump =
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
  (* The generic functions that the archives on the command line need are left
     out of a plugin when the link selects no unit at all (as it may from an
     archive given alone without -linkall): the plugin would hold code compiled
     for this invocation's settings and no unit whose header tells the loader
     what these were. *)
  let genfns =
    match units_tolink with [] -> Generic_fns.Tbl.make () | _ :: _ -> genfns
  in
  let named_startup_file = named_startup_file () in
  let startup =
    if named_startup_file
    then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm
  in
  let startup_obj = output_name ^ ".startup" ^ ext_obj in
  let sourcefile_for_dwarf = sourcefile_for_dwarf ~named_startup_file startup in
  Asmgen.compile_unit unix ~output_prefix:output_name ~asm_filename:startup
    ~keep_asm:!Clflags.keep_startup_file ~obj_filename:startup_obj
    ~may_reduce_heap:true ~ppf_dump (fun () ->
      Profile.record_call "make_shared_startup_file" (fun () ->
          without_trace_pc_guard @@ fun () ->
          make_shared_startup_file unix ~ppf_dump
            ~sourcefile_for_dwarf:(Some sourcefile_for_dwarf) genfns
            units_tolink));
  call_linker_shared (startup_obj :: objfiles) output_name;
  if !Oxcaml_flags.internal_assembler
  then
    (* CR gyorsh: restore after workaround. *)
    Emitaux.binary_backend_available := true;
  remove_file startup_obj

let link_shared unix ml_objfiles output_name ~genfns ~units_tolink ~ppf_dump =
  Profile.record_call "link_shared" (fun () ->
      link_shared_actual unix ml_objfiles output_name ~genfns ~units_tolink
        ~ppf_dump)

let call_linker ?dissector_args file_list_rev startup_file output_name =
  let main_dll =
    !Clflags.output_c_object && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object in
  let files, c_lib =
    match dissector_args with
    | Some args ->
      (* Dissector mode: partition files contain everything (startup,
         ml_objfiles, ccobjs, runtime_lib). Don't add them again. Add linker
         script flag. However, linker flags (starting with -l) need to be passed
         through as they can't be baked into partition files. *)
      Clflags.all_ccopts
        := Build_linker_args.linker_script_flag args :: !Clflags.all_ccopts;
      (* When assuming LLD without 64-bit EH frame support, suppress LLD's
         broken .eh_frame_hdr generation - we provide our own in the linker
         script. *)
      if !Oxcaml_flags.dissector_assume_lld_without_64_bit_eh_frames
      then Clflags.all_ccopts := "-Wl,--no-eh-frame-hdr" :: !Clflags.all_ccopts;
      let c_lib =
        if !Clflags.nopervasives || (main_obj_runtime && not main_dll)
        then ""
        else Config.native_c_libraries
      in
      (* Filter ccobjs to only include linker flags (starting with -l) *)
      let linker_flags =
        List.filter
          (fun s -> String.starts_with ~prefix:"-l" s)
          (List.rev !Clflags.ccobjs)
      in
      Build_linker_args.object_files args @ linker_flags, c_lib
    | None ->
      (* Normal mode: combine startup + ml_objfiles + ccobjs + runtime_lib *)
      let file_list_rev = List.map (fun f -> f.Linkenv.path) file_list_rev in
      let file_list_rev =
        if !Oxcaml_flags.use_cached_generic_functions
        then cached_generic_functions_path () :: file_list_rev
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
      files, c_lib
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
    && not !Clflags.restrict_to_upstream_dwarf
  in
  let link_output_name =
    if needs_objcopy_workflow
    then Filename.temp_file (Filename.basename output_name) ".tmp"
    else output_name
  in
  let exitcode =
    Profile.record_call "link_object" (fun () ->
        Ccomp.call_linker mode link_output_name files c_lib)
  in
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
        let objcopy_exit =
          Profile.record_call "objcopy_create_debug" (fun () ->
              Ccomp.command objcopy_cmd_create_debug)
        in
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
        let objcopy_exit =
          Profile.record_call "objcopy_create_stripped_exe" (fun () ->
              Ccomp.command objcopy_cmd_create_stripped_exe)
        in
        Misc.remove_file link_output_name;
        if objcopy_exit <> 0 then raise (Error (Objcopy_error objcopy_exit)))
    | Fission_dsymutil ->
      if not (Target_system.is_macos ())
      then raise (Error Dwarf_fission_dsymutil_not_macos)
      else if
        not_output_to_dev_null output_name
        && mode = Ccomp.Exe
        && not !Clflags.restrict_to_upstream_dwarf
      then
        (* Run dsymutil on the executable *)
        let dsymutil_cmd =
          Printf.sprintf "dsymutil %s" (Filename.quote output_name)
        in
        let dsymutil_exit =
          Profile.record_call "dsymutil" (fun () -> Ccomp.command dsymutil_cmd)
        in
        if dsymutil_exit <> 0 then raise (Error (Dsymutil_error dsymutil_exit))

(* Main entry point *)

let entry_symbols units =
  List.map
    (fun compilation_unit ->
      Cmm_helpers.entry_symbol_name ~compilation_unit ()
      |> Asm_targets.Asm_symbol.create_global)
    units

let link_actual unix linkenv ml_objfiles output_name ~cached_genfns_imports
    ~genfns ~units_tolink ~uses_eval ~quoted_cmi ~quoted_cmx ~ppf_dump : unit =
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
  let bundled_cm_obj =
    if not uses_eval
    then None
    else
      match
        Cm_bundle.make_bundled_cm_file unix ~ppf_dump ~quoted_cmi ~quoted_cmx
          ~named_startup_file ~output_name
      with
      | exception Cm_bundle.Error error -> raise (Error (Cm_bundle_error error))
      | bundled_cm_obj -> Some bundled_cm_obj
  in
  let ml_objfiles =
    match bundled_cm_obj with
    | None -> ml_objfiles
    | Some bundled_cm_obj ->
      { Linkenv.path = bundled_cm_obj; units = [] } :: ml_objfiles
  in
  Asmgen.compile_unit unix ~output_prefix:output_name ~asm_filename:startup
    ~keep_asm:!Clflags.keep_startup_file ~obj_filename:startup_obj
    ~may_reduce_heap:true ~ppf_dump (fun () ->
      Profile.record_call "make_startup_file" (fun () ->
          without_trace_pc_guard @@ fun () ->
          make_startup_file linkenv unix ~ppf_dump
            ~sourcefile_for_dwarf:(Some sourcefile_for_dwarf) genfns
            units_tolink cached_genfns_imports));
  Emitaux.reduce_heap_size ~reset:(fun () -> ());
  (* Dissector pass: partitions all object files and rewrites them *)
  let dissector_args, dissector_temp_dir =
    if !Clflags.dissector
    then (
      let cached_genfns =
        if !Oxcaml_flags.use_cached_generic_functions
        then Some (cached_generic_functions_path ())
        else None
      in
      let temp_dir = mk_temp_dir "camldissector" "" in
      let ml_objfiles_with_symbols =
        List.map
          (fun { Linkenv.path; units } -> path, entry_symbols units)
          ml_objfiles
      in
      let result =
        Profile.record_call "dissector" (fun () ->
            Dissector.run ~unix ~temp_dir ~ml_objfiles:ml_objfiles_with_symbols
              ~startup_obj ~ccobjs:(List.rev !Clflags.ccobjs)
              ~runtime_libs:(runtime_lib ()) ~cached_genfns)
      in
      let linker_args = Build_linker_args.build result in
      (* Add EH frame registration object if the dissector generated one. This
         handles runtime frame registration when using LLD without 64-bit EH
         frame support. *)
      (match Dissector.Result.eh_frame_registration_obj result with
      | Some eh_frame_obj -> Clflags.ccobjs := eh_frame_obj :: !Clflags.ccobjs
      | None -> ());
      (* Print partition file paths if requested *)
      if !Clflags.ddissector_partitions
      then (
        Printf.eprintf "Dissector partition files (in %s):\n" temp_dir;
        List.iter
          (fun file -> Printf.eprintf "  %s\n" file)
          (Build_linker_args.object_files linker_args);
        Printf.eprintf "  %s\n%!" (Build_linker_args.linker_script linker_args));
      Some linker_args, Some temp_dir)
    else None, None
  in
  let cleanup_dissector_temp_dir () =
    match dissector_temp_dir with
    | None -> ()
    | Some dir ->
      if !Clflags.ddissector_partitions
      then () (* Keep partition files for debugging *)
      else Misc.remove_dir_contents dir
  in
  Misc.try_finally
    (fun () -> call_linker ?dissector_args ml_objfiles startup_obj output_name)
    ~always:(fun () ->
      remove_file startup_obj;
      Option.iter remove_file bundled_cm_obj;
      cleanup_dissector_temp_dir ())

let link unix linkenv ml_objfiles output_name ~cached_genfns_imports ~genfns
    ~units_tolink ~uses_eval ~quoted_cmi ~quoted_cmx ~ppf_dump : unit =
  Profile.record_call "link" (fun () ->
      link_actual unix linkenv ml_objfiles output_name ~cached_genfns_imports
        ~genfns ~units_tolink ~uses_eval ~quoted_cmi ~quoted_cmx ~ppf_dump)

(* Error report *)

open Format_doc

let report_error_doc ppf = function
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
  | Cm_bundle_error (Missing_intf_for_quote intf) ->
    fprintf ppf "Missing interface for module %a which is required by quote"
      CU.Name.print_as_inline_code intf
  | Cm_bundle_error (Missing_impl_for_quote impl) ->
    fprintf ppf
      "Missing implementation for module %a which is required by quote"
      CU.print_as_inline_code impl

let report_error = Format_doc.compat report_error_doc

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)
