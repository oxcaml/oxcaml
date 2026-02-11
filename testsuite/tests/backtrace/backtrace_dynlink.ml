(* TEST_BELOW
(* Blank lines added here to preserve locations. *)























*)

(* test for backtrace and stack unwinding with dynlink. *)
(* https://github.com/ocaml-multicore/ocaml-multicore/issues/440 *)
(* https://github.com/ocaml-multicore/ocaml-multicore/pull/499 *)


[@@@ocaml.warning "-52"]

let () =
  Dynlink.allow_unsafe_modules true;
  try
    (Dynlink.loadfile [@inlined never]) "backtrace_dynlink_plugin.cmxs"
  with
  | Dynlink.Error ((Library's_module_initializers_failed (
      Failure "SUCCESS")) as err) ->
    print_endline (Dynlink.error_message err);
    let bt = Printexc.get_backtrace () in
    let bt_list = String.split_on_char '\n' bt in
    if List.length bt_list > 5 then print_endline "Backtrace sufficiently long"
    else (
      print_endline "Failure: Backtrace too short:";
      print_string bt
    )
  | exn ->
    Printexc.to_string exn |> print_endline;
    print_endline "ERROR"

(* TEST
 include dynlink;
 readonly_files = "backtrace_dynlink_plugin.ml";
 libraries = "";
 shared-libraries;
 native-dynlink;
 setup-ocamlopt.byte-build-env;

 module = "backtrace_dynlink.ml";
 flags = "-g";
 ocamlopt.byte;

 unset module;
 program = "backtrace_dynlink_plugin.cmxs";
 flags = "-shared -g";
 all_modules = "backtrace_dynlink_plugin.ml";
 ocamlopt.byte;

 program = "${test_build_directory}/main.exe";
 unset flags;
 libraries = "dynlink";
 all_modules = "backtrace_dynlink.cmx";
 ocamlopt.byte;

 ocamlrunparam += ",b=1";
 run;
 {
   no-flambda;
   check-program-output;
 }{
<<<<<<< HEAD
   program = "backtrace_dynlink_plugin.cmxs";
   flags = "-shared -g";
   all_modules = "backtrace_dynlink_plugin.ml";
   ocamlopt.byte;
 }{
   program = "${test_build_directory}/main.exe";
   libraries = "dynlink";
   all_modules = "backtrace_dynlink.cmx";
   ocamlopt.byte;
   ocamlrunparam += ",b=1";
   run;
||||||| 23e84b8c4d
   program = "backtrace_dynlink_plugin.cmxs";
   flags = "-shared -g";
   all_modules = "backtrace_dynlink_plugin.ml";
   ocamlopt.byte;
 }{
   program = "${test_build_directory}/main.exe";
   libraries = "dynlink";
   all_modules = "backtrace_dynlink.cmx";
   ocamlopt.byte;
   ocamlrunparam += ",b=1";
   run;
   {
     no-flambda;
     check-program-output;
   }{
     reference = "${test_source_directory}/backtrace_dynlink.flambda.reference";
     flambda;
     check-program-output;
   }
=======
   reference = "${test_source_directory}/backtrace_dynlink.flambda.reference";
   flambda;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
   check-program-output;
 }
*)
