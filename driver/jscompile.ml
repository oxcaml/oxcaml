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
let with_info = Compile_common.with_info ~backend:(Opt Js_of_ocaml) ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi"
    ~compilation_unit:Inferred_from_output_prefix ~kind:Intf
  @@ fun info ->
  Compile_common.interface
    ~hook_parse_tree:(fun _ -> ())
    ~hook_typed_tree:(fun _ -> ())
    info

(** Js_of_ocaml IR compilation backend for .ml files. *)

let make_arg_descr ~param ~arg_block_idx : Lambda.arg_descr option =
  match (param, arg_block_idx) with
  | Some arg_param, Some arg_block_idx -> Some { arg_param; arg_block_idx }
  | None, None -> None
  | Some _, None -> Misc.fatal_error "No argument field"
  | None, Some _ -> Misc.fatal_error "Unexpected argument field"

let raw_lambda_to_jsir i raw_lambda ~as_arg_for =
  raw_lambda
  |> Profile.(record ~accumulate:true generate)
       (fun (program : Lambda.program) ->
         Builtin_attributes.warn_unused ();
         program.code
         |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
         |> Simplif.simplify_lambda ~restrict_to_upstream_dwarf:true
              ~gdwarf_may_alter_codegen:false
         |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
         |> fun lambda ->
         let arg_descr =
           make_arg_descr ~param:as_arg_for ~arg_block_idx:program.arg_block_idx
         in
         lambda |> fun code ->
         Flambda2.lambda_to_flambda ~machine_width:Thirty_two_no_gc_tag_bit
           ~ppf_dump:i.ppf_dump
           ~prefixname:(Unit_info.prefix i.target)
           { program with code }
         |> fun (flambda_result : Flambda2.flambda_result) ->
         let jsir =
           Flambda2_to_jsir.To_jsir.unit ~offsets:flambda_result.offsets
             ~all_code:flambda_result.all_code
             ~reachable_names:flambda_result.reachable_names
             flambda_result.flambda
           |> print_if i.ppf_dump Clflags.dump_jsir
                (fun ppf (jsir : Flambda2_to_jsir.To_jsir_result.program) ->
                  Jsoo_imports.Code.Print.program ppf
                    (fun _ _ -> "")
                    jsir.program)
         in
         (jsir, program.main_module_block_format, arg_descr))

let emit_jsir i
    ({ program; imported_compilation_units } :
      Flambda2_to_jsir.To_jsir_result.program) =
  let cmj = Unit_info.cmj i.target in
  let oc = open_out_bin (Unit_info.Artifact.filename cmj) in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () ->
      Misc.remove_file (Unit_info.Artifact.filename cmj))
    (fun () ->
      output_string oc Config.cmj_magic_number;
      (* We include the highest used variable in the translation, so that Js_of_ocaml
         can read this number and update its own state accordingly. *)
      let cmj_body : Jsoo_imports.Code.cmj_body =
        {
          program;
          last_var = Jsoo_imports.Code.Var.idx (Jsoo_imports.Code.Var.last ());
          imported_compilation_units =
            Compilation_unit.Set.elements imported_compilation_units;
          exported_compilation_unit = i.module_name;
        }
      in
      output_value oc cmj_body)

let to_jsir i Typedtree.{ structure; coercion; argument_interface; _ }
    ~as_arg_for =
  let argument_coercion =
    match argument_interface with
    | Some { ai_coercion_from_primary; ai_signature = _ } ->
        Some ai_coercion_from_primary
    | None -> None
  in
  let raw_lambda =
    (structure, coercion, argument_coercion)
    |> Profile.(record transl) (Translmod.transl_implementation i.module_name)
  in
  let jsir, main_module_block_format, arg_descr =
    raw_lambda_to_jsir i raw_lambda ~as_arg_for
  in
  Compilenv.save_unit_info
    (Unit_info.Artifact.filename (Unit_info.cmjx i.target))
    ~main_module_block_format ~arg_descr;
  jsir

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

let implementation_aux ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables:_
    ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" ~compilation_unit
    ~kind:Impl
  @@ fun info ->
  match start_from with
  | Parsing ->
      let backend info typed =
        Compilenv.reset info.target;
        let as_arg_for =
          !Clflags.as_argument_for
          |> Option.map Global_module.Parameter_name.of_string
        in
        let jsir = to_jsir info typed ~as_arg_for in
        emit_jsir info jsir
      in
      Compile_common.implementation
        ~hook_parse_tree:(fun _ -> ())
        ~hook_typed_tree:(fun _ -> ())
        info ~backend
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
      let jsir, main_module_block_format, arg_descr_computed =
        raw_lambda_to_jsir info impl ~as_arg_for
      in
      emit_jsir info jsir;
      Compilenv.save_unit_info
        (Unit_info.Artifact.filename (Unit_info.cmx info.target))
        ~main_module_block_format
        ~arg_descr:
          (match arg_descr with
          | None -> arg_descr_computed
          | Some _ -> arg_descr)

let implementation ~start_from ~source_file ~output_prefix ~keep_symbol_tables =
  let start_from = start_from |> starting_point_of_compiler_pass in
  implementation_aux ~start_from ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:Inferred_from_output_prefix

let instance ~source_file ~output_prefix ~compilation_unit ~runtime_args
    ~main_module_block_size ~arg_descr ~keep_symbol_tables =
  let start_from =
    Instantiation { runtime_args; main_module_block_size; arg_descr }
  in
  implementation_aux ~start_from ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:(Exactly compilation_unit)
