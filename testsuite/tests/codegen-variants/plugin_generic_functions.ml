(* TEST
 lib = "plugin_generic_functions_lib";
 readonly_files = "${lib}.ml";
 symbols = "sh ${test_source_directory}/link_guard.sh generic-functions";
 shared-libraries;
 native-dynlink;
 setup-ocamlopt.byte-build-env;
 all_modules = "${lib}.ml";
 flags = "-a";
 program = "${lib}.cmxa";
 ocamlopt.byte;
 all_modules = "${lib}.cmxa";
 flags = "-shared -linkall";
 program = "whole.cmxs";
 ocamlopt.byte;
 flags = "-shared";
 program = "empty.cmxs";
 ocamlopt.byte;
 script = "${symbols} whole.cmxs";
 output = "whole.output";
 script;
 script = "${symbols} empty.cmxs";
 output = "empty.output";
 script;
 script = "cat whole.output empty.output";
 output = "symbols.output";
 script;
 check-program-output;
*)

(* A plugin made from an archive alone, of which the link takes no unit, as
   here (no -linkall, and no other file to need one), has nothing in it that
   tells Dynlink which codegen variant it was built for: it refers to the
   link-time guard of that variant instead, like the plugin made from an
   object file in link_guard_plugin.ml, and can open only where that guard
   is visible. The generic functions that the units of the archive need
   are compiled into the plugin that contains those units, and not into
   this one, where a plugin loaded later would find them. *)

let () = print_endline "not run"
