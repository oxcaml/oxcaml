let interface_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
      let buffer, ppf = Browser_switch_common.make_formatter_buffer () in
      (try
         Browser_switch_common.prepare_compiler environment ~filename;
         let lexbuf = Browser_switch_common.prepare_lexbuf ~filename source in
         let ast = Parse.implementation lexbuf in
         let compilation_unit = Browser_switch_common.compilation_unit filename in
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
       with
       | Browser_switch_common.Missing_cmi _ as exn -> raise exn
       | exn ->
         Location.report_exception ppf exn;
         let backtrace = Printexc.get_backtrace () in
         if not (String.equal backtrace "")
         then Format.fprintf ppf "@.%s" backtrace);
      Browser_switch_common.flush_formatter ppf;
      Buffer.contents buffer)
