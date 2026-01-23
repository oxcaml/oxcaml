open Import

let check_invariants program =
  try () (* Flambda_unit.invariant program *)
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

let parse_flambda filename =
  match Parse_flambda.parse_fexpr filename with
  | Ok unit ->
    let comp_unit =
      Parse_flambda.make_compilation_unit ~extension:".fl" ~filename ()
    in
    let unit_info = Unit_info.make_dummy ~input_name:filename comp_unit in
    Env.set_unit_name (Some unit_info);
    let fl2 = Fexpr_to_flambda.conv comp_unit unit in
    check_invariants fl2;
    (* CR gbury/lmaurer/bclement: add a proper traversal to compute the actual
       code_slot_offsets here (as well as free_names) *)
    Flambda2.flambda_to_flambda ~ppf_dump:Format.std_formatter ~prefixname:""
      ~machine_width:Sixty_four fl2
  | Error e ->
    Test_utils.dump_error e;
    exit 1

module Options = Oxcaml_args.Make_optcomp_options (Oxcaml_args.Default.Optmain)

let _ =
  let file_action = ref (fun () -> assert false) in
  let defer_file file =
    let ext = Filename.extension file in
    match ext with
    | ".fl" -> file_action := fun () -> parse_flambda file
    | _ -> Misc.fatal_errorf "Unrecognized extension %s" ext
  in
  Compenv.warnings_for_discarded_params := true;
  Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
  Compenv.readenv Format.err_formatter Before_args;
  Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Compenv.parse_arguments (ref Sys.argv) defer_file "fexprc";
  !file_action ()
