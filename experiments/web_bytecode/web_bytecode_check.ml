let check_string ~browser ~filename ~source =
  let environment =
    if browser then Web_bytecode_common.Browser
    else Web_bytecode_common.Native
  in
  Web_bytecode_common.capture_diagnostics (fun _ppf ->
      Web_bytecode_common.prepare_compiler environment ~filename;
      let lexbuf = Web_bytecode_common.prepare_lexbuf ~filename source in
      let ast = Parse.implementation lexbuf in
      let compilation_unit = Web_bytecode_common.compilation_unit filename in
      let unit_info =
        Unit_info.make_dummy ~input_name:filename compilation_unit
      in
      Env.set_unit_name (Some unit_info);
      let env = Compmisc.initial_env () in
      ignore (Typemod.type_implementation unit_info compilation_unit env ast);
      Warnings.check_fatal ();
      true)
