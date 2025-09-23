(* TEST
 {
   setup-ocamlc.byte-build-env;
   module = "empty.ml";
   ocamlc.byte;
   module = "";
   flags = "-a";
   all_modules = "";
   program = "empty.cma";
   ocamlc.byte;
   flags = "";
   program = "${test_build_directory}/empty.byte";
   all_modules = "empty.cma empty.cmo";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   module = "empty.ml";
   ocamlopt.byte;
   module = "";
   flags = "-a";
   all_modules = "";
   program = "empty.cmxa";
   ocamlopt.byte;
   flags = "";
   program = "${test_build_directory}/empty.native";
   all_modules = "empty.cmxa empty.cmx";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlj.opt-build-env;
   module = "empty.ml";
   ocamlj.opt;
   module = "";
   flags = "-a";
   all_modules = "";
   program = "empty.cmjxa";
   ocamlj.opt;
   flags = "";
   program = "${test_build_directory}/empty.js";
   all_modules = "empty.cmjxa empty.cmjx";
   ocamlj.opt;
   check-ocamlj.opt-output;
 }
*)
