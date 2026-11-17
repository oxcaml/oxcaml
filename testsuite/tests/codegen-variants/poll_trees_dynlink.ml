(* TEST
 codegen_variant = "poll";
 variant_available;
 codegen_variant = "base";
 variant_available;
 shared-libraries;
 native-dynlink;
 readonly_files = "poll_trees_plugin.ml";
 {
   compiler_directory_suffix = ".unpolled";
   reference = "${test_source_directory}/poll_trees_dynlink.unpolled.reference";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "base";
   flags = "-shared";
   program = "poll_trees_plugin.cmxs";
   all_modules = "poll_trees_plugin.ml";
   ocamlopt.byte;
   codegen_variant = "poll";
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/poll_trees_dynlink.exe";
   all_modules = "poll_trees_dynlink.ml";
   ocamlopt.byte;
   {
     ocamlrunparam = "d=2";
     run;
     check-program-output;
   }{
     reference = "${test_source_directory}/poll_trees_dynlink.polled.reference";
     ocamlrunparam = "d=1";
     run;
     check-program-output;
   }
 }{
   compiler_directory_suffix = ".polled";
   reference = "${test_source_directory}/poll_trees_dynlink.polled.reference";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "poll";
   flags = "-shared";
   program = "poll_trees_plugin.cmxs";
   all_modules = "poll_trees_plugin.ml";
   ocamlopt.byte;
   codegen_variant = "base";
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/poll_trees_dynlink.exe";
   all_modules = "poll_trees_dynlink.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".override";
   reference = "${test_source_directory}/poll_trees_dynlink.override.reference";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "base";
   flags = "-shared";
   program = "poll_trees_plugin.cmxs";
   all_modules = "poll_trees_plugin.ml";
   ocamlopt.byte;
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/poll_trees_dynlink.exe";
   all_modules = "poll_trees_dynlink.ml";
   ocamlopt.byte;
   ocamlrunparam = "d=2";
   run;
   check-program-output;
 }
*)

(* A plugin and the program that loads it need not have the same
   poll-insertion setting, in an installation that has a library tree for
   each setting: the plugin was compiled against the standard library of
   the other tree than the program's, and the two trees have the same
   implementation CRCs. So a plugin with polls loads into a program
   without, and a plugin without polls is only refused by a fully polled
   program that may run more than one domain, as it is when the two were
   compiled against one tree (see poll_dynlink.ml). A program that is not
   fully polled loads a plugin without polls whatever its domain cap is:
   with OCAMLRUNPARAM=d=2 the runtime warns about the program, and the
   plugin adds nothing to that. The plugin uses Printf, whose code has
   symbols named after the path of its source. *)

let () =
  match Dynlink.loadfile "poll_trees_plugin.cmxs" with
  | () -> print_endline "plugin loaded"
  | exception Dynlink.Error (Library_not_fully_polled { unit_name; _ }) ->
    Printf.printf "load rejected (not fully polled): %s\n" unit_name
  | exception Dynlink.Error e ->
    Printf.printf "unexpected dynlink error: %s\n" (Dynlink.error_message e)
