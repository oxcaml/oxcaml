let interface_string ~browser ~filename ~source =
  let environment =
    if browser then Web_bytecode_common.Browser
    else Web_bytecode_common.Native
  in
  let buffer, ppf = Web_bytecode_common.make_formatter_buffer () in
  (try
     Web_bytecode_common.prepare_compiler environment ~filename;
     let lexbuf = Web_bytecode_common.prepare_lexbuf ~filename source in
     let ast = Parse.implementation lexbuf in
     let compilation_unit = Web_bytecode_common.compilation_unit filename in
     let unit_info =
       Unit_info.make_dummy ~input_name:filename compilation_unit
     in
     Env.set_unit_name (Some unit_info);
     let env = Compmisc.initial_env () in
     let implementation =
       Typemod.type_implementation unit_info compilation_unit env ast
     in
     Warnings.check_fatal ();
     Printtyp.wrap_printing_env ~error:false env (fun () ->
         Format.fprintf ppf "%a@."
           (Printtyp.printed_signature filename)
           implementation.Typedtree.signature)
   with exn -> Location.report_exception ppf exn);
  Web_bytecode_common.flush_formatter ppf;
  Buffer.contents buffer
