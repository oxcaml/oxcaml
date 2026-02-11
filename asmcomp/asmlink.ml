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
<<<<<<< oxcaml
  | Dwarf_fission_objcopy_on_macos
  | Dwarf_fission_dsymutil_not_macos
  | Dsymutil_error of int
  | Objcopy_error of int
  | Cm_bundle_error of Cm_bundle.error
||||||| upstream-base
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname
=======
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Missing_cmx of filepath * modname
  | Link_error of Linkdeps.error
>>>>>>> upstream-incoming

exception Error of error

<<<<<<< oxcaml
type unit_link_info = Linkenv.unit_link_info =
  { name : Compilation_unit.t;
    defines : Compilation_unit.t list;
    file_name : string;
    crc : Digest.t;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }
||||||| upstream-base
(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = ref ([] : string list)

module Cmx_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    let source = List.assoc unit.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc -> Cmi_consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  implementations := unit.ui_name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.ui_name crc file_name;
  implementations_defined :=
    (unit.ui_name, file_name) :: !implementations_defined;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Cmi_consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end
=======
(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = ref ([] : string list)

module Cmx_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : string list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc -> Cmi_consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  implementations := unit.ui_name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.ui_name crc file_name;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Cmi_consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end
>>>>>>> upstream-incoming

let runtime_lib () =
  let variant =
    if Config.runtime5 && !Clflags.runtime_variant = "nnp"
    then ""
    else !Clflags.runtime_variant
  in
  let libname = "libasmrun" ^ variant ^ ext_lib in
  try
<<<<<<< oxcaml
    if !Clflags.nopervasives || not !Clflags.with_runtime
    then []
    else [Load_path.find libname]
  with Not_found -> raise (Linkenv.Error (File_not_found libname))
||||||| upstream-base
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let object_file_name_of_file = function
  | Unit (fname, _, _) -> Some (Filename.chop_suffix fname ".cmx" ^ ext_obj)
  | Library (fname, infos) ->
      let obj_file = Filename.chop_suffix fname ".cmxa" ^ ext_lib in
      (* MSVC doesn't support empty .lib files, and macOS struggles to make
         them (#6550), so there shouldn't be one if the .cmxa contains no
         units. The file_exists check is added to be ultra-defensive for the
         case where a user has manually added things to the .a/.lib file *)
      if infos.lib_units = [] && not (Sys.file_exists obj_file) then None else
      Some obj_file

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file file tolink = match file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required file_name) info.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
           || !Clflags.link_everything
           || is_required info.ui_name
           then begin
             remove_required info.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.ui_name))
               info.ui_imports_cmx;
             (info, file_name, crc) :: reqd
           end else
           reqd)
        infos.lib_units tolink
=======
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let object_file_name_of_file = function
  | Unit (fname, _, _) -> Some (Filename.chop_suffix fname ".cmx" ^ ext_obj)
  | Library (fname, infos) ->
      let obj_file = Filename.chop_suffix fname ".cmxa" ^ ext_lib in
      (* MSVC doesn't support empty .lib files, and macOS struggles to make
         them (#6550), so there shouldn't be one if the .cmxa contains no
         units. The file_exists check is added to be ultra-defensive for the
         case where a user has manually added things to the .a/.lib file *)
      if infos.lib_units = [] && not (Sys.file_exists obj_file) then None else
      Some obj_file

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file ldeps file tolink = match file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      Linkdeps.add ldeps
        ~filename:file_name ~compunit:info.ui_name
        ~provides:[info.ui_name]
        ~requires:(List.map fst info.ui_imports_cmx);
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
           || !Clflags.link_everything
           || Linkdeps.required ldeps info.ui_name
           then begin
             Linkdeps.add ldeps
               ~filename:file_name ~compunit:info.ui_name
               ~provides:[info.ui_name]
               ~requires:(List.map fst info.ui_imports_cmx);
             (info, file_name, crc) :: reqd
           end else
           reqd)
        infos.lib_units tolink
>>>>>>> upstream-incoming

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
  let globals_map = Linkenv.make_globals_map linkenv units in
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
<<<<<<< oxcaml
  let shared_startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_shared_startup")
||||||| upstream-base
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ();
  List.iter compile_phrase
    (Cmm_helpers.emit_preallocated_blocks [] (* add gc_roots (for dynlink) *)
      (Cmm_helpers.generic_functions true (List.map fst units)));
  compile_phrase (Cmm_helpers.plugin_header units);
  compile_phrase
    (Cmm_helpers.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared file_list output_name =
  let exitcode = Ccomp.call_linker Ccomp.Dll output_name file_list "" in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let obj_infos = List.map read_file objfiles in
    let units_tolink = List.fold_right scan_file obj_infos [] in
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles =
      List.rev (List.filter_map object_file_name_of_file obj_infos) @
      (List.rev !Clflags.ccobjs) in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () ->
         make_shared_startup_file ~ppf_dump
           (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink)
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
=======
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ();
  List.iter compile_phrase
    (Cmm_helpers.emit_preallocated_blocks [] (* add gc_roots (for dynlink) *)
      (Cmm_helpers.generic_functions true (List.map fst units)));
  compile_phrase (Cmm_helpers.plugin_header units);
  compile_phrase
    (Cmm_helpers.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared file_list output_name =
  let exitcode = Ccomp.call_linker Ccomp.Dll output_name file_list "" in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let obj_infos = List.map read_file objfiles in
    let ldeps = Linkdeps.create ~complete:false in
    let units_tolink = List.fold_right (scan_file ldeps) obj_infos [] in
    (match Linkdeps.check ldeps with
     | None -> ()
     | Some e -> raise (Error (Link_error e)));
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles =
      List.rev (List.filter_map object_file_name_of_file obj_infos) @
      (List.rev !Clflags.ccobjs) in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () ->
         make_shared_startup_file ~ppf_dump
           (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink)
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
>>>>>>> upstream-incoming
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
    && not !Dwarf_flags.restrict_to_upstream_dwarf
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
        && not !Dwarf_flags.restrict_to_upstream_dwarf
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

<<<<<<< oxcaml
let link_actual unix linkenv ml_objfiles output_name ~cached_genfns_imports
    ~genfns ~units_tolink ~uses_eval ~quoted_globals ~ppf_dump : unit =
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
  let ml_objfiles =
    if not uses_eval
    then ml_objfiles
    else
      match
        Cm_bundle.make_bundled_cm_file unix ~ppf_dump ~quoted_globals
          ~named_startup_file ~output_name
      with
      | exception Cm_bundle.Error error -> raise (Error (Cm_bundle_error error))
      | bundled_cm_obj -> bundled_cm_obj :: ml_objfiles
  in
  Asmgen.compile_unit unix ~output_prefix:output_name ~asm_filename:startup
    ~keep_asm:!Clflags.keep_startup_file ~obj_filename:startup_obj
    ~may_reduce_heap:true ~ppf_dump (fun () ->
      Profile.record_call "make_startup_file" (fun () ->
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
        then Some !Oxcaml_flags.cached_generic_functions_path
        else None
      in
      let temp_dir = mk_temp_dir "camldissector" "" in
      let result =
        Profile.record_call "dissector" (fun () ->
            Dissector.run ~unix ~temp_dir ~ml_objfiles ~startup_obj
              ~ccobjs:(List.rev !Clflags.ccobjs) ~runtime_libs:(runtime_lib ())
              ~cached_genfns)
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
      cleanup_dissector_temp_dir ())

let link unix linkenv ml_objfiles output_name ~cached_genfns_imports ~genfns
    ~units_tolink ~uses_eval ~quoted_globals ~ppf_dump : unit =
  Profile.record_call "link" (fun () ->
      link_actual unix linkenv ml_objfiles output_name ~cached_genfns_imports
        ~genfns ~units_tolink ~uses_eval ~quoted_globals ~ppf_dump)
||||||| upstream-base
let link ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let obj_infos = List.map read_file objfiles in
    let units_tolink = List.fold_right scan_file obj_infos [] in
    Array.iter remove_required Runtimedef.builtin_exceptions;
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    let crc_interfaces = extract_crc_interfaces () in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () -> make_startup_file ~ppf_dump units_tolink ~crc_interfaces);
    Misc.try_finally
      (fun () ->
         call_linker (List.filter_map object_file_name_of_file obj_infos)
           startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )
=======
let link ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let obj_infos = List.map read_file objfiles in
    let ldeps = Linkdeps.create ~complete:true in
    let units_tolink = List.fold_right (scan_file ldeps) obj_infos [] in
    (match Linkdeps.check ldeps with
     | None -> ()
     | Some e -> raise (Error (Link_error e)));
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    let crc_interfaces = extract_crc_interfaces () in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () -> make_startup_file ~ppf_dump units_tolink ~crc_interfaces);
    Misc.try_finally
      (fun () ->
         call_linker (List.filter_map object_file_name_of_file obj_infos)
           startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )
>>>>>>> upstream-incoming

(* Error report *)

<<<<<<< oxcaml
||||||| upstream-base
open Format
module Style = Misc.Style
=======
module Style = Misc.Style
>>>>>>> upstream-incoming
open Format_doc

let report_error_doc ppf = function
<<<<<<< oxcaml
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
      CU.Name.print_as_inline_code impl

let report_error = Format_doc.compat report_error_doc
||||||| upstream-base
let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        (Style.as_inline_code Location.print_filename) name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           Style.inline_code ppf r1;
           List.iter (fun r -> fprintf ppf ",@ %a" Style.inline_code r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
           fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
             Style.inline_code md
             print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %a@]"
       (Style.as_inline_code Location.print_filename) file1
       (Style.as_inline_code Location.print_filename) file2
       Style.inline_code intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %a@]"
       (Style.as_inline_code Location.print_filename) file1
       (Style.as_inline_code Location.print_filename) file2
       Style.inline_code intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a"
        (Style.as_inline_code Location.print_filename) file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %a@]"
        (Style.as_inline_code Location.print_filename) file1
        (Style.as_inline_code Location.print_filename) file2
        Style.inline_code modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the %a file@ for module %a,@ \
         which was produced by %a.@ \
         Please recompile %a@ with the correct %a option@ \
         so that %a@ is found.@]"
        (Style.as_inline_code Location.print_filename) filename
        Style.inline_code ".cmx"
        Style.inline_code name
        Style.inline_code "ocamlopt -for-pack"
        (Style.as_inline_code Location.print_filename) filename
        Style.inline_code "-I"
        Style.inline_code (name^".cmx")
=======
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.Doc.quoted_filename name
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %a@]"
       Location.Doc.quoted_filename file1
       Location.Doc.quoted_filename file2
       Style.inline_code intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %a@]"
       Location.Doc.quoted_filename file1
       Location.Doc.quoted_filename file2
       Style.inline_code intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a"
        Location.Doc.quoted_filename file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the %a file@ for module %a,@ \
         which was produced by %a.@ \
         Please recompile %a@ with the correct %a option@ \
         so that %a@ is found.@]"
        Location.Doc.quoted_filename filename
        Style.inline_code ".cmx"
        Style.inline_code name
        Style.inline_code "ocamlopt -for-pack"
        Location.Doc.quoted_filename filename
        Style.inline_code "-I"
        Style.inline_code (name^".cmx")
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e
>>>>>>> upstream-incoming

let () =
<<<<<<< oxcaml
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)
||||||| upstream-base
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  implementations_defined := [];
  cmx_required := [];
  interfaces := [];
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []
=======
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  cmx_required := [];
  interfaces := [];
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []
>>>>>>> upstream-incoming
