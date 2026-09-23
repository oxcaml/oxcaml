(* TEST
 readonly_files = "host_api.ml plugin.ml";
 include dynlink;
 libraries = "";
 flags = "-g -function-sections";
 native-compiler;
 native-dynlink;
 function_sections;
 link_order_frametables;
 setup-ocamlopt.byte-build-env;
 module = "host_api.ml";
 ocamlopt.byte;
 module = "plugin.ml";
 ocamlopt.byte;
 module = "dynlink_host.ml";
 ocamlopt.byte;
 module = "";
 program = "plugin.cmxs";
 flags = "-g -shared";
 all_modules = "plugin.cmx";
 ocamlopt.byte;
 libraries = "dynlink";
 all_modules = "host_api.cmx dynlink_host.cmx";
 {
   (* Natdynlink needs the executable's dynamic symbol table, so linking
      Dynlink with -no-export-dynamic is an error. *)
   program = "${test_build_directory}/dynlink_host_noexport.exe";
   flags = "-g -no-export-dynamic";
   ocamlopt_byte_exit_status = "2";
   compiler_reference =
     "${test_source_directory}/dynlink_host.noexport.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   program = "${test_build_directory}/dynlink_host.exe";
   flags = "-g";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The plugin registers a callback that calls [Host_api.boom]; the host then
   runs it and prints the backtrace, which crosses the plugin's frames and so
   needs the plugin's frametable to have been registered by natdynlink. *)

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
