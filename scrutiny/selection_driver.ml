module Options =
  Oxcaml_args.Make_optcomp_options (Oxcaml_args.Default.Optmain)

let () =
  try
    Compenv.warnings_for_discarded_params := true;
    Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
    Compenv.readenv Format.err_formatter Before_args;
    Clflags.add_arguments __LOC__ Options.list;
    Compenv.parse_arguments (ref Sys.argv)
      (fun argument -> raise (Arg.Bad ("Unexpected argument: " ^ argument)))
      "selection_driver";
    Compenv.readenv Format.err_formatter (Before_compile "input.ml");
    List.iter
      (fun feature ->
        Printf.printf "%s=%b "
          (Experimental_feature_registry.flag_name feature)
          (Oxcaml_flags.Experimental.is_enabled feature))
      Experimental_feature_registry.all;
    Printf.printf "cfg-merge-blocks=%b\n" !Oxcaml_flags.cfg_merge_blocks
  with Compenv.Exit_with_status status -> exit status
