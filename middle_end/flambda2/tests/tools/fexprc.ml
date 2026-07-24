open Import

let check_invariants program =
  (* CR Keryan : do we want to restore this at some point ? *)
  try () (* Flambda_unit.invariant program *)
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

let parse_flambda filename =
  match Parse_flambda.parse_fexpr filename with
  | Ok unit ->
    let unit_info = Parse_flambda.make_unit_info ~filename in
    let comp_unit = Unit_info.modname unit_info in
    Env.set_current_unit unit_info;
    let fl2 = Fexpr_to_flambda.conv comp_unit unit in
    check_invariants fl2.unit;
    Flambda2.flambda_to_flambda ~ppf_dump:Format.std_formatter
      ~prefixname:(Filename.chop_extension filename)
      ~machine_width:Sixty_four ~code_slot_offsets:fl2.code_slot_offsets
      fl2.unit
  | Error e ->
    Test_utils.dump_error e;
    exit 1

(* Compile all the way to an executable and run it, so that the runtime
   behaviour of fexpr code can be observed. This exercises To_cmm and the
   backend, unlike [parse_flambda] above, which stops after Simplify. The
   program's output goes to fexprc's stdout/stderr (we exec the program), so the
   check-program-output ocamltest action can compare it to a reference. *)
let compile_and_run filename =
  match Parse_flambda.parse_fexpr filename with
  | Error e ->
    Test_utils.dump_error e;
    exit 1
  | Ok unit ->
    let unit_info = Parse_flambda.make_unit_info ~filename in
    let comp_unit = Unit_info.modname unit_info in
    Env.set_current_unit unit_info;
    Compilenv.reset unit_info;
    let fl2 = Fexpr_to_flambda.conv comp_unit unit in
    check_invariants fl2.unit;
    let pipeline ~ppf_dump ~prefixname (_ : Lambda.program) =
      let { Flambda2.flambda; all_code; offsets; reachable_names } =
        Flambda2.flambda_to_flambda ~ppf_dump ~prefixname
          ~machine_width:Sixty_four ~code_slot_offsets:fl2.code_slot_offsets
          fl2.unit
      in
      Flambda2_to_cmm.To_cmm.unit flambda ~all_code ~offsets ~reachable_names
    in
    let main_module_block_format : Lambda.main_module_block_format =
      Mb_struct { mb_repr = Module_value_only { field_count = 0 } }
    in
    let program : Lambda.program =
      { compilation_unit = comp_unit;
        main_module_block_format;
        arg_block_idx = None;
        required_globals = Compilation_unit.Set.empty;
        code = Lambda.lambda_unit
      }
    in
    let prefixname = Filename.chop_extension filename in
    Asmgen.compile_implementation
      (module Unix : Compiler_owee.Unix_intf.S)
      ~pipeline:(Direct_to_cmm pipeline) ~sourcefile:(Some filename) ~prefixname
      ~ppf_dump:Format.std_formatter program;
    Compilenv.save_unit_info (prefixname ^ ".cmx") ~main_module_block_format
      ~arg_descr:None;
    Compmisc.init_path ();
    let module Compiler =
      (val Optcompile.native
             (module Unix : Compiler_owee.Unix_intf.S)
             ~flambda2:Flambda2.lambda_to_cmm)
    in
    let exe = prefixname ^ ".exe" in
    Compiler.link ~ppf_dump:Format.err_formatter [prefixname ^ ".cmx"] exe;
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush stdout;
    flush stderr;
    Unix.execv exe [| exe |]

module Options = Oxcaml_args.Make_optcomp_options (Oxcaml_args.Default.Optmain)

let _ =
  (* Setting [Clflags.native_code := true] is how we tell the option parsing
     machinery that we are going to use flambda2. *)
  Clflags.native_code := true;
  let run_mode = ref false in
  let file_action = ref (fun () -> Misc.fatal_error "Missing a flambda file") in
  let defer_file file =
    let ext = Filename.extension file in
    match ext with
    | ".fl" ->
      file_action
        := fun () ->
             if !run_mode
             then compile_and_run file
             else ignore (parse_flambda file : Flambda2.flambda_result)
    | _ -> Misc.fatal_errorf "Unrecognized extension %s" ext
  in
  Compenv.warnings_for_discarded_params := true;
  Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
  Compenv.readenv Format.err_formatter Before_args;
  Clflags.add_arguments __LOC__
    (("-run", Arg.Set run_mode, " Compile to an executable and run it")
    :: (Arch.command_line_options @ Options.list));
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Compenv.parse_arguments (ref Sys.argv) defer_file "fexprc";
  Location.read_clflags_from_env ();
  !file_action ()
