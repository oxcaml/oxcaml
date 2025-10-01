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

let tool_name = "ocamlopt"

let with_info = Compile_common.with_info ~native:true ~tool_name
let with_js_info = Compile_common.with_info ~native:false ~tool_name

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

let compile_from_raw_lambda i raw_lambda ~unix ~pipeline ~as_arg_for =
  raw_lambda
  |> print_if i.ppf_dump Clflags.dump_debug_uid_tables
        (fun ppf _ -> Type_shape.print_debug_uid_tables ppf)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
  |> Profile.(record generate)
   (fun (program : Lambda.program) ->
      Builtin_attributes.warn_unused ();
      let code = Simplif.simplify_lambda program.Lambda.code in
      { program with Lambda.code }
      |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
      |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
      |> (fun (program : Lambda.program) ->
           if Clflags.(should_stop_after Compiler_pass.Lambda) then ()
           else begin
             Asmgen.compile_implementation
               unix
               ~pipeline
               ~sourcefile:(Some (Unit_info.original_source_file i.target))
               ~prefixname:(Unit_info.prefix i.target)
               ~ppf_dump:i.ppf_dump
               program;
             let arg_descr =
               make_arg_descr ~param:as_arg_for
                 ~arg_block_idx:program.arg_block_idx
             in
             Compilenv.save_unit_info
               (Unit_info.Artifact.filename (Unit_info.cmx i.target))
               ~main_module_block_format:program.main_module_block_format
               ~arg_descr
           end))

let compile_from_typed i typed ~unix ~pipeline ~as_arg_for =
  typed
  |> Profile.(record transl)
    (* OCaml upstream no longer exposes a [~style] argument here; the
       representation choice is internal to [transl_implementation]. *)
    (Translmod.transl_implementation i.module_name)
  |> compile_from_raw_lambda i ~unix ~pipeline ~as_arg_for

type flambda2 =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  machine_width:Target_system.Machine_width.t ->
  keep_symbol_tables:bool ->
  Lambda.program ->
  Cmm.phrase list

(* Emit assembly directly from Linear IR *)
let emit unix i =
  Compilenv.reset i.target;
  Asmgen.compile_implementation_linear unix
    (Unit_info.prefix i.target)
    ~progname:(Unit_info.original_source_file i.target)

type starting_point =
  | Parsing
  | Emit
  | Instantiation of {
      runtime_args : Translmod.runtime_arg list;
      main_module_block_size : int;
      arg_descr : Lambda.arg_descr option;
  }

type lambda_to_jsir =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  machine_width:Target_system.Machine_width.t ->
  Lambda.program ->
  Jsoo_imports.Js_backend.program

let js_machine_width = Target_system.Machine_width.Thirty_two_no_gc_tag_bit

let starting_point_of_compiler_pass start_from  =
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing -> Parsing
  | Emit -> Emit
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)

type js_backend_result = {
  js_program : Jsoo_imports.Js_backend.program;
  main_module_block_format : Lambda.main_module_block_format;
  arg_descr : Lambda.arg_descr option;
}

let js_of_raw_lambda info raw_lambda ~lambda_to_jsir ~as_arg_for =
  Jsoo_imports.Targetint.set_num_bits 32;
  raw_lambda
  |> Profile.(record ~accumulate:true generate)
       (fun (program : Lambda.program) ->
          Builtin_attributes.warn_unused ();
          let code =
            program.code
            |> print_if info.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
            |> Simplif.simplify_lambda
            |> print_if info.ppf_dump Clflags.dump_lambda Printlambda.lambda
          in
          let program = { program with code } in
          let arg_descr =
            make_arg_descr ~param:as_arg_for ~arg_block_idx:program.arg_block_idx
          in
          let js_program =
            lambda_to_jsir
              ~ppf_dump:info.ppf_dump
              ~prefixname:(Unit_info.prefix info.target)
              ~machine_width:js_machine_width
              program
          in
          let _ =
            print_if info.ppf_dump Clflags.dump_jsir
              (fun ppf program ->
                 Jsoo_imports.Jsir.Print.program ppf (fun _ _ -> "")
                   program.Jsoo_imports.Js_backend.program)
              js_program
          in
          {
            js_program;
            main_module_block_format = program.main_module_block_format;
            arg_descr;
          })

let js_of_typedtree info
    (Typedtree.{ structure; coercion; argument_interface; _ })
    ~lambda_to_jsir ~as_arg_for =
  let argument_coercion =
    match argument_interface with
    | Some { ai_coercion_from_primary; ai_signature = _ } ->
        Some ai_coercion_from_primary
    | None -> None
  in
  let raw_lambda =
    (structure, coercion, argument_coercion)
    |> Profile.(record transl) (Translmod.transl_implementation info.module_name)
      in
      js_of_raw_lambda info raw_lambda ~lambda_to_jsir ~as_arg_for

let emit_js info (js_program : Jsoo_imports.Js_backend.program) =
  let jsir = Unit_info.jsir info.target in
  let compilation_unit : Jsoo_imports.Jsir.compilation_unit =
    let info_record : Jsoo_imports.Unit_info.t =
      {
        provides =
          Jsoo_imports.StringSet.singleton
            (Compilation_unit.full_path_as_string info.module_name);
        requires =
          Compilation_unit.Set.elements js_program.imported_compilation_units
          |> List.map Compilation_unit.full_path_as_string
          |> Jsoo_imports.StringSet.of_list;
        primitives = [];
        aliases = [];
        force_link = false;
        effects_without_cps = false;
      }
    in
    {
      info = info_record;
      contents =
        {
          code = js_program.program;
          cmis = Jsoo_imports.StringSet.empty;
          debug = Jsoo_imports.Jsir.Debug.default_summary;
        };
    }
  in
  let filename = Unit_info.Artifact.filename jsir in
  Jsoo_imports.Jsir.save compilation_unit ~filename;
  Misc.try_finally
    ~always:(fun () -> Misc.remove_file filename)
    (fun () ->
       let debug_flag = if !Clflags.debug then [ "--debug-info" ] else [] in
       Jscomp.run_jsoo_exn
         ~args:
           ([
              "compile";
              "--enable=effects,with-js-error";
              Unit_info.Artifact.filename (Unit_info.jsir info.target);
              "-o";
              Unit_info.Artifact.filename (Unit_info.cmjo info.target);
            ]
           @ debug_flag
           @ List.rev !Clflags.all_ccopts))

let native_implementation_aux ~machine_width unix ~(flambda2 : flambda2)
      ~start_from ~source_file ~output_prefix ~keep_symbol_tables
      ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  let pipeline : Asmgen.pipeline =
    Direct_to_cmm (flambda2 ~machine_width ~keep_symbol_tables)
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" ~compilation_unit
    ~kind:Impl
  @@ fun info ->
  if !Oxcaml_flags.internal_assembler then
    Emitaux.binary_backend_available := true;
  match start_from with
  | Parsing ->
    let backend info ({ structure; coercion; argument_interface; _ }
                      : Typedtree.implementation) =
      Compilenv.reset info.target;
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
      compile_from_typed info typed ~unix ~pipeline ~as_arg_for
    in
    Compile_common.implementation
      ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
      ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
        Compiler_hooks.execute Compiler_hooks.Typed_tree_impl impl)
      info ~backend
  | Emit -> emit unix info ~ppf_dump:info.ppf_dump
  | Instantiation { runtime_args; main_module_block_size; arg_descr } ->
    Compilenv.reset info.target;
    begin
      match !Clflags.as_argument_for with
      | Some _ ->
        (* CR lmaurer: Needs nicer error message (this is a user error) *)
        Misc.fatal_error
          "-as-argument-for is not allowed (and not needed) with -instantiate"
      | None -> ()
    end;
    let as_arg_for, arg_block_idx =
      match (arg_descr : Lambda.arg_descr option) with
      | Some { arg_param; arg_block_idx } -> Some arg_param, Some arg_block_idx
      | None -> None, None
    in
    let impl =
      Translmod.transl_instance info.module_name ~runtime_args
        ~main_module_block_size ~arg_block_idx
    in
    if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
    compile_from_raw_lambda info impl ~unix ~pipeline ~as_arg_for

let js_implementation_aux unix ~(lambda_to_jsir : lambda_to_jsir) ~start_from
      ~source_file ~output_prefix ~keep_symbol_tables:(_ : bool)
      ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  let module _ = (val unix : Compiler_owee.Unix_intf.S) in
  let dump_ext = "cmjo" in
  with_js_info ~source_file ~output_prefix ~dump_ext ~compilation_unit
    ~kind:Impl
  @@ fun info ->
  match start_from with
  | Emit ->
      Misc.fatal_error "-stop-after Emit is not supported for the JavaScript backend"
  | Parsing ->
      let backend info (typed : Typedtree.implementation) =
        Compilenv.reset info.target;
        let as_arg_for =
          !Clflags.as_argument_for
          |> Option.map Global_module.Parameter_name.of_string
        in
        let result =
          js_of_typedtree info typed ~lambda_to_jsir ~as_arg_for
        in
        emit_js info result.js_program;
        Compilenv.save_unit_info
          (Unit_info.Artifact.filename (Unit_info.cmjx info.target))
          ~main_module_block_format:result.main_module_block_format
          ~arg_descr:result.arg_descr;
        Warnings.check_fatal ()
      in
      Compile_common.implementation
        ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
        ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
          Compiler_hooks.execute Compiler_hooks.Typed_tree_impl impl)
        info ~backend
  | Instantiation { runtime_args; main_module_block_size; arg_descr } ->
      Compilenv.reset info.target;
      begin
        match !Clflags.as_argument_for with
        | Some _ ->
          Misc.fatal_error
            "-as-argument-for is not allowed (and not needed) with -instantiate"
        | None -> ()
      end;
      let as_arg_for, arg_block_idx =
        match (arg_descr : Lambda.arg_descr option) with
        | Some { arg_param; arg_block_idx } ->
            (Some arg_param, Some arg_block_idx)
        | None -> (None, None)
      in
      let raw_lambda =
        Translmod.transl_instance info.module_name ~runtime_args
          ~main_module_block_size ~arg_block_idx
      in
      let result =
        js_of_raw_lambda info raw_lambda ~lambda_to_jsir ~as_arg_for
      in
      emit_js info result.js_program;
      let arg_descr_to_save =
        match arg_descr with
        | Some arg_descr -> Some arg_descr
        | None -> result.arg_descr
      in
      Compilenv.save_unit_info
        (Unit_info.Artifact.filename (Unit_info.cmjx info.target))
        ~main_module_block_format:result.main_module_block_format
        ~arg_descr:arg_descr_to_save;
      Warnings.check_fatal ()

let implementation_aux ~machine_width unix ~(flambda2 : flambda2)
      ~(lambda_to_jsir : lambda_to_jsir) ~start_from
      ~source_file ~output_prefix ~keep_symbol_tables
      ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  let backend =
    match Clflags.backend_target () with
    | Some Clflags.Backend.Js_of_ocaml -> `Js
    | _ -> `Native
  in
  match backend with
  | `Native ->
      native_implementation_aux ~machine_width unix ~flambda2 ~start_from
        ~source_file ~output_prefix ~keep_symbol_tables ~compilation_unit
  | `Js ->
      js_implementation_aux unix ~lambda_to_jsir ~start_from ~source_file
        ~output_prefix ~keep_symbol_tables ~compilation_unit

let implementation ~machine_width unix ~flambda2 ~lambda_to_jsir ~start_from
      ~source_file ~output_prefix ~keep_symbol_tables =
  let start_from = start_from |> starting_point_of_compiler_pass in
  implementation_aux ~machine_width unix ~flambda2 ~lambda_to_jsir ~start_from
    ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:Inferred_from_output_prefix

let instance ~machine_width unix ~flambda2 ~lambda_to_jsir ~source_file
      ~output_prefix ~compilation_unit ~runtime_args ~main_module_block_size
      ~arg_descr ~keep_symbol_tables =
  let start_from =
    Instantiation { runtime_args; main_module_block_size; arg_descr }
  in
  implementation_aux ~machine_width unix ~flambda2 ~lambda_to_jsir ~start_from
    ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:(Exactly compilation_unit)
