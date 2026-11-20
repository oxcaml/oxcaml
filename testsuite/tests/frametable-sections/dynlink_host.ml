(* TEST
 readonly_files = "host_api.ml plugin.ml";
 include dynlink;
 libraries = "";
 native-compiler;
 native-dynlink;
 frametable_sections;
 unset OCAMLPARAM;
 setup-ocamlopt.byte-build-env;
 flags = "-g";
 module = "host_api.ml";
 ocamlopt.byte;
 flags = "-g -function-sections -frametable-sections";
 module = "plugin.ml";
 ocamlopt.byte;
 flags = "-g";
 module = "dynlink_host.ml";
 ocamlopt.byte;
 module = "";
 program = "plugin.cmxs";
 flags = "-g -shared";
 all_modules = "plugin.cmx";
 ocamlopt.byte;
 libraries = "dynlink";
 all_modules = "host_api.cmx dynlink_host.cmx";
 program = "${test_build_directory}/dynlink_host.exe";
 flags = "-g";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* A host loads a plugin compiled with -frametable-sections. Natdynlink finds
   the plugin's frametable by symbol and registers it like any other, so this
   works without dynlink-specific support. Frametable sections offer no benefit
   to shared code, though: if this test ever fails, the strong default fix is
   to make -frametable-sections an error with -shared, not to repair it.

   The plugin runs a major GC under a deep stack of its own frames, and
   registers a callback that calls [Host_api.boom]; the host then runs it and
   prints the backtrace, which crosses the plugin's frames. *)

let[@inline never] run_callback () =
  !Host_api.callback ();
  print_endline "no exception"

let () =
  Printexc.record_backtrace true;
  (try Dynlink.loadfile "plugin.cmxs" with
  | Dynlink.Error e -> print_endline (Dynlink.error_message e));
  try run_callback () with
  | Host_api.Boom n ->
    Printf.printf "caught Boom %d\n" n;
    print_string (Printexc.get_backtrace ())
