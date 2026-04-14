let check_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  let suffix = "_" ^ Filename.basename filename in
  let source_path = Filename.temp_file "browser_switch_check_" suffix in
  let output_prefix = source_path ^ ".build" in
  Browser_switch_common.write_source_file ~source_path ~source;
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
    Browser_switch_common.capture_diagnostics (fun _ppf ->
        Browser_switch_common.prepare_compiler environment ~filename;
        Fun.protect
          (fun () ->
            ignore
              (Browser_switch_common.compile_source_file ~source_path ~output_prefix);
            true)
          ~finally:(fun () ->
            Browser_switch_common.cleanup_build_artifacts ~source_path ~output_prefix)))
  |> Browser_switch_common.replace_all ~pattern:source_path ~with_:filename
