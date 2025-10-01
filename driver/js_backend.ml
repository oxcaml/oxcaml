open Misc
open Compile_common

let make_arg_descr ~param ~arg_block_idx : Lambda.arg_descr option =
  match (param, arg_block_idx) with
  | Some arg_param, Some arg_block_idx -> Some { arg_param; arg_block_idx }
  | None, None -> None
  | Some _, None -> Misc.fatal_error "No argument field"
  | None, Some _ -> Misc.fatal_error "Unexpected argument field"

let raw_lambda_to_jsir (info : Compile_common.info) raw_lambda ~as_arg_for =
  raw_lambda
  |> Profile.(record ~accumulate:true generate)
       (fun (program : Lambda.program) ->
         Builtin_attributes.warn_unused ();
         let program =
           program.code
           |> print_if info.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
           |> Simplif.simplify_lambda
           |> print_if info.ppf_dump Clflags.dump_lambda Printlambda.lambda
           |> fun code -> { program with code }
         in
         let arg_descr =
           make_arg_descr ~param:as_arg_for ~arg_block_idx:program.arg_block_idx
         in
         let jsir_result =
           Flambda2.lambda_to_flambda ~machine_width:Thirty_two_no_gc_tag_bit
             ~ppf_dump:info.ppf_dump
             ~prefixname:(Unit_info.prefix info.target)
             program
           |> fun (flambda_result : Flambda2.flambda_result) ->
           Flambda2_to_jsir.To_jsir.unit ~offsets:flambda_result.offsets
             ~all_code:flambda_result.all_code
             ~reachable_names:flambda_result.reachable_names
             flambda_result.flambda
         in
         let jsir_result =
           print_if info.ppf_dump Clflags.dump_jsir
             (fun ppf (jsir : Flambda2_to_jsir.To_jsir_result.program) ->
               Jsoo_imports.Jsir.Print.program ppf (fun _ _ -> "") jsir.program)
             jsir_result
         in
         (jsir_result, program.main_module_block_format, arg_descr))

let emit_jsir (info : Compile_common.info)
    ({ program; imported_compilation_units } :
      Flambda2_to_jsir.To_jsir_result.program) =
  let jsir = Unit_info.jsir info.target in
  let compilation_unit : Jsoo_imports.Jsir.compilation_unit =
    let info : Jsoo_imports.Unit_info.t =
      {
        provides =
          Jsoo_imports.StringSet.singleton
            (Compilation_unit.full_path_as_string info.module_name);
        requires =
          Compilation_unit.Set.elements imported_compilation_units
          |> List.map Compilation_unit.full_path_as_string
          |> Jsoo_imports.StringSet.of_list;
        primitives = [];
        aliases = [];
        force_link = false;
        effects_without_cps = false;
      }
    in
    {
      info;
      contents =
        {
          code = program;
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
      Jscompile.run_jsoo_exn
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

let backend : Jscompile.backend =
  let rec of_lambda (info : Compile_common.info) raw_lambda ~as_arg_for =
    let jsir, main_module_block_format, arg_descr =
      raw_lambda_to_jsir info raw_lambda ~as_arg_for
    in
    let result : Jscompile.backend_result =
      {
        emit = (fun info -> emit_jsir info jsir);
        main_module_block_format;
        arg_descr;
      }
    in
    result
  and of_typedtree
      (info : Compile_common.info)
      Typedtree.{ structure; coercion; argument_interface; _ }
      ~as_arg_for =
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
    of_lambda info raw_lambda ~as_arg_for
  in
  { of_typedtree; of_lambda }
