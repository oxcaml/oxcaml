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

open Misc
open Compile_common

let tool_name = "ocamlj"
let with_info = Compile_common.with_info ~native:false ~tool_name

type backend_result = {
  emit : Compile_common.info -> unit;
  main_module_block_format : Lambda.main_module_block_format;
  arg_descr : Lambda.arg_descr option;
}

type backend = {
  of_typedtree :
    Compile_common.info ->
    Typedtree.implementation ->
    as_arg_for:Global_module.Parameter_name.t option ->
    backend_result;
  of_lambda :
    Compile_common.info ->
    Lambda.program ->
    as_arg_for:Global_module.Parameter_name.t option ->
    backend_result;
}

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi"
    ~compilation_unit:Inferred_from_output_prefix ~kind:Intf
  @@ fun info ->
  Compile_common.interface
    ~hook_parse_tree:(fun _ -> ())
    ~hook_typed_tree:(fun _ -> ())
    info

let run_jsoo_exn ~args =
  let prog =
    (* Use jsoo from our PATH when we're bootstrapping *)
    match Sys.ocaml_release with
    | { extra = Some (Plus, "ox"); _ } ->
        Filename.concat Config.bindir "js_of_oxcaml"
    | _ ->
        (* Try to find js_of_oxcaml in the same directory as the current executable *)
        let exe_dir = Filename.dirname Sys.executable_name in
        let jsoo_path = Filename.concat exe_dir "js_of_oxcaml" in
        if Sys.file_exists jsoo_path then jsoo_path else "js_of_oxcaml"
  in
  let cmdline = Filename.quote_command prog args in
  match Ccomp.command cmdline with 0 -> () | _ -> raise (Sys_error cmdline)

type starting_point =
  | Parsing
  | Instantiation of {
      runtime_args : Translmod.runtime_arg list;
      main_module_block_size : int;
      arg_descr : Lambda.arg_descr option;
    }

let starting_point_of_compiler_pass start_from =
  match (start_from : Clflags.Compiler_pass.t) with
  | Parsing -> Parsing
  | _ ->
      Misc.fatal_errorf "Cannot start from %s"
        (Clflags.Compiler_pass.to_string start_from)

let implementation_aux backend ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables:_
    ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  with_info ~source_file ~output_prefix ~dump_ext:"cmjo" ~compilation_unit
    ~kind:Impl
  @@ fun info ->
  match start_from with
  | Parsing ->
      let process info typed =
        Compilenv.reset info.target;
        Jsoo_imports.Targetint.set_num_bits 32;
        let as_arg_for =
          !Clflags.as_argument_for
          |> Option.map Global_module.Parameter_name.of_string
        in
        let result = backend.of_typedtree info typed ~as_arg_for in
        result.emit info;
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
        info ~backend:process
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
            (Some arg_param, Some arg_block_idx)
        | None -> (None, None)
      in
      Compilenv.reset info.target;
      let impl =
        Translmod.transl_instance info.module_name ~runtime_args
          ~main_module_block_size ~arg_block_idx
      in
      let result = backend.of_lambda info impl ~as_arg_for in
      result.emit info;
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

let implementation ~backend ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables =
  let start_from = start_from |> starting_point_of_compiler_pass in
  implementation_aux backend ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables
    ~compilation_unit:Inferred_from_output_prefix

let instance ~backend ~source_file ~output_prefix ~compilation_unit
    ~runtime_args ~main_module_block_size ~arg_descr ~keep_symbol_tables =
  let start_from =
    Instantiation { runtime_args; main_module_block_size; arg_descr }
  in
  implementation_aux backend ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables
    ~compilation_unit:(Exactly compilation_unit)
