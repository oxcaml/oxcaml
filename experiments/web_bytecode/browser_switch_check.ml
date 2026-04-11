let check_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  Browser_switch_common.capture_diagnostics (fun _ppf ->
      Browser_switch_common.prepare_compiler environment ~filename;
      let lexbuf = Browser_switch_common.prepare_lexbuf ~filename source in
      let ast = Parse.implementation lexbuf in
      let compilation_unit = Browser_switch_common.compilation_unit filename in
      let unit_info = Unit_info.make_dummy ~input_name:filename compilation_unit in
      Env.set_unit_name (Some unit_info);
      let env = Compmisc.initial_env () in
      ignore (Typemod.type_implementation unit_info compilation_unit env ast);
      Warnings.check_fatal ();
      true)
