(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Misc
open Compile_common

module type S = sig
  include Optcomp_intf.File_extensions

  val interface : source_file:string -> output_prefix:string -> unit

  val implementation :
    start_from:Clflags.Compiler_pass.t ->
    source_file:string ->
    output_prefix:string ->
    keep_symbol_tables:bool ->
    unit

  val instance :
    source_file:string ->
    output_prefix:string ->
    compilation_unit:Compilation_unit.t ->
    runtime_args:Translmod.runtime_arg list ->
    main_module_block_size:int ->
    arg_descr:Lambda.arg_descr option ->
    keep_symbol_tables:bool ->
    unit

  val link : ppf_dump:Format.formatter -> string list -> string -> unit

  val link_shared : ppf_dump:Format.formatter -> string list -> string -> unit

  val create_archive : string list -> string -> unit

  val instantiate : src:string -> args:string list -> string -> unit

  val package_files :
    ppf_dump:Format.formatter -> Env.t -> string list -> string -> unit
end

module Make (Backend : Optcomp_intf.Backend) : S = struct
  let tool_name = "ocamlopt"

  include (Backend : Optcomp_intf.File_extensions)

  let with_info ~dump_ext =
    let dump_ext =
      if StringLabels.starts_with dump_ext ~prefix:"."
      then
        StringLabels.sub dump_ext ~pos:1 ~len:(StringLabels.length dump_ext - 1)
      else dump_ext
    in
    Compile_common.with_info ~backend:(Opt Backend.backend) ~tool_name ~dump_ext

  let interface ~source_file ~output_prefix =
    with_info ~source_file ~output_prefix ~dump_ext:"cmi"
      ~compilation_unit:Inferred_from_output_prefix ~kind:Intf
    @@ fun info ->
    Compile_common.interface
      ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_intf)
      ~hook_typed_tree:(Compiler_hooks.execute Compiler_hooks.Typed_tree_intf)
      info

  (** Native compilation backend for .ml files. *)

  let make_arg_descr ~param ~arg_block_idx : Lambda.arg_descr option =
    match param, arg_block_idx with
    | Some arg_param, Some arg_block_idx -> Some { arg_param; arg_block_idx }
    | None, None -> None
    | Some _, None -> Misc.fatal_error "No argument field"
    | None, Some _ -> Misc.fatal_error "Unexpected argument field"

  let compile_from_raw_lambda i raw_lambda ~keep_symbol_tables ~as_arg_for =
    raw_lambda
    |> print_if i.ppf_dump Clflags.dump_debug_uid_tables (fun ppf _ ->
           Type_shape.print_debug_uid_tables ppf)
    |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
    |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
    |> Profile.(record generate) (fun (program : Lambda.program) ->
           Builtin_attributes.warn_unused ();
           let code = Simplif.simplify_lambda program.Lambda.code in
           { program with Lambda.code }
           |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
           |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
           |> fun (program : Lambda.program) ->
           if Clflags.(should_stop_after Compiler_pass.Lambda)
           then ()
           else (
             Backend.compile_implementation ~keep_symbol_tables
               ~sourcefile:(Some (Unit_info.original_source_file i.target))
               ~prefixname:(Unit_info.prefix i.target)
               ~ppf_dump:i.ppf_dump program;
             let arg_descr =
               make_arg_descr ~param:as_arg_for
                 ~arg_block_idx:program.arg_block_idx
             in
             Compilenv.save_unit_info
               (Unit_info.Artifact.filename
                  (Unit_info.artifact i.target
                     ~extension:Backend.ext_flambda_obj))
               ~main_module_block_format:program.main_module_block_format
               ~arg_descr))

  let compile_from_typed i typed ~keep_symbol_tables ~as_arg_for =
    typed
    |> Profile.(record transl) (Translmod.transl_implementation i.module_name)
    |> compile_from_raw_lambda i ~keep_symbol_tables ~as_arg_for

  type starting_point =
    | Parsing
    | Emit of Optcomp_intf.emit
    | Instantiation of
        { runtime_args : Translmod.runtime_arg list;
          main_module_block_size : int;
          arg_descr : Lambda.arg_descr option
        }

  let starting_point_of_compiler_pass start_from =
    match (start_from : Clflags.Compiler_pass.t), Backend.emit with
    | Parsing, _ -> Parsing
    | Emit, Some emit -> Emit emit
    | _ ->
      Misc.fatal_errorf "Cannot start from %s"
        (Clflags.Compiler_pass.to_string start_from)

  let implementation_aux ~start_from ~source_file ~output_prefix
      ~keep_symbol_tables
      ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
    with_info ~source_file ~output_prefix ~dump_ext:Backend.ext_flambda_obj
      ~compilation_unit ~kind:Impl
    @@ fun info ->
    if !Oxcaml_flags.internal_assembler
    then Emitaux.binary_backend_available := true;
    Compilenv.reset info.target;
    match start_from with
    | Parsing ->
      let backend info
          ({ structure; coercion; argument_interface; _ } :
            Typedtree.implementation) =
        let argument_coercion =
          match argument_interface with
          | Some { ai_coercion_from_primary; ai_signature = _ } ->
            Some ai_coercion_from_primary
          | None -> None
        in
        let typed = structure, coercion, argument_coercion in
        let as_arg_for =
          !Clflags.as_argument_for
          |> Option.map Global_module.Parameter_name.of_string
        in
        if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
        compile_from_typed info typed ~as_arg_for ~keep_symbol_tables
      in
      Compile_common.implementation
        ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
        ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
          Compiler_hooks.execute Compiler_hooks.Typed_tree_impl impl)
        info ~backend
    | Emit emit -> emit info (* Emit assembly directly from Linear IR *)
    | Instantiation { runtime_args; main_module_block_size; arg_descr } ->
      (match !Clflags.as_argument_for with
      | Some _ ->
        (* CR lmaurer: Needs nicer error message (this is a user error) *)
        Misc.fatal_error
          "-as-argument-for is not allowed (and not needed) with -instantiate"
      | None -> ());
      let as_arg_for, arg_block_idx =
        match (arg_descr : Lambda.arg_descr option) with
        | Some { arg_param; arg_block_idx } ->
          Some arg_param, Some arg_block_idx
        | None -> None, None
      in
      let impl =
        Translmod.transl_instance info.module_name ~runtime_args
          ~main_module_block_size ~arg_block_idx
      in
      if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
      compile_from_raw_lambda info impl ~as_arg_for ~keep_symbol_tables

  let implementation ~start_from ~source_file ~output_prefix ~keep_symbol_tables
      =
    let start_from = start_from |> starting_point_of_compiler_pass in
    implementation_aux ~start_from ~source_file ~output_prefix
      ~keep_symbol_tables ~compilation_unit:Inferred_from_output_prefix

  let instance ~source_file ~output_prefix ~compilation_unit ~runtime_args
      ~main_module_block_size ~arg_descr ~keep_symbol_tables =
    let start_from =
      Instantiation { runtime_args; main_module_block_size; arg_descr }
    in
    implementation_aux ~start_from ~source_file ~output_prefix
      ~keep_symbol_tables ~compilation_unit:(Exactly compilation_unit)

  module Link = Optlink.Make (Backend)

  module Link_input = struct
    include Backend
    include Link
  end

  include Link
  include Optpackager.Make (Link_input)
  include Optlibrarian.Make (Link_input)

  let read_unit_info file : Instantiator.unit_info =
    let unit_info, _crc = Compilenv.read_unit_info file in
    let { Cmx_format.ui_unit; ui_arg_descr; ui_format; _ } = unit_info in
    { Instantiator.ui_unit; ui_arg_descr; ui_format }

  let instantiate ~src ~args targetcmx =
    Instantiator.instantiate ~src ~args targetcmx
      ~expected_extension:ext_flambda_obj ~read_unit_info
      ~compile:(instance ~keep_symbol_tables:false)
end

let native unix
    ~flambda2:
      (lambda_to_cmm :
        ppf_dump:Format.formatter ->
        prefixname:string ->
        machine_width:Target_system.Machine_width.t ->
        keep_symbol_tables:bool ->
        Lambda.program ->
        Cmm.phrase list) =
  (module Make (struct
    let backend = Compile_common.Native

    let supports_metaprogramming = true

    let ext_asm = Config.ext_asm

    let ext_obj = Config.ext_obj

    let ext_lib = Config.ext_lib

    let ext_flambda_obj = ".cmx"

    let ext_flambda_lib = ".cmxa"

    let default_executable_name = Config.default_executable_name

    let link = Asmlink.link unix

    let link_shared target objfiles ~genfns ~units_tolink ~ppf_dump =
      Asmlink.link_shared unix target objfiles ~genfns ~units_tolink ~ppf_dump

    let emit : Optcomp_intf.emit option =
      Some
        (fun info ->
          Asmgen.compile_implementation_linear unix
            (Unit_info.prefix info.target)
            ~progname:(Unit_info.original_source_file info.target)
            ~ppf_dump:info.ppf_dump)

    let link_partial target objfiles =
      let exitcode = Ccomp.call_linker Ccomp.Partial target objfiles "" in
      if exitcode <> 0 then raise (Linkenv.Error (Linking_error exitcode))

    let create_archive archive_name objfile_list =
      if Ccomp.create_archive archive_name objfile_list <> 0
      then raise (Linkenv.Error (Archiver_error archive_name))

    let compile_implementation ~keep_symbol_tables ~sourcefile ~prefixname
        ~ppf_dump program =
      let machine_width = Target_system.Machine_width.Sixty_four in
      Asmgen.compile_implementation unix
        ~pipeline:
          (Direct_to_cmm (lambda_to_cmm ~machine_width ~keep_symbol_tables))
        ~sourcefile ~prefixname ~ppf_dump program

    let extra_load_paths_for_eval =
      ["unix"; "compiler-libs"; "ocaml-jit"; "eval"]

    let extra_libraries_for_eval =
      [ "unix/unix";
        "compiler-libs/ocamlcommon";
        "compiler-libs/ocamloptcomp";
        "ocaml-jit/jit";
        "eval/camlinternaleval" ]

    let support_files_for_eval () =
      List.iter
        (fun lib ->
          Load_path.add_dir ~hidden:false
            (Misc.expand_directory Config.standard_library ("+" ^ lib)))
        extra_load_paths_for_eval;
      List.map (fun lib -> lib ^ ext_flambda_lib) extra_libraries_for_eval
  end) : S)
