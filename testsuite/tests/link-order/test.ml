(* TEST
 (* Top-level side-effects should happen in link-order. *)
 readonly_files = "counter.ml increment.ml triple.ml print.ml";
 {
   setup-ocamlopt.byte-build-env;
   module = "counter.ml";
   ocamlopt.byte;
   module = "increment.ml";
   ocamlopt.byte;
   module = "print.ml";
   ocamlopt.byte;
   module = "triple.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "counter.cmx increment.cmx triple.cmx print.cmx";
   program = "${test_build_directory}/test.opt";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   setup-ocamlc.byte-build-env;
   module = "counter.ml";
   ocamlc.byte;
   module = "increment.ml";
   ocamlc.byte;
   module = "print.ml";
   ocamlc.byte;
   module = "triple.ml";
   ocamlc.byte;
   module = "";
   all_modules = "counter.cmo increment.cmo triple.cmo print.cmo";
   program = "${test_build_directory}/test.byte";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlc.byte-build-env;
   flags = "-thunkify-compilation-unit-initialization";
   module = "counter.ml";
   ocamlc.byte;
   module = "increment.ml";
   ocamlc.byte;
   module = "print.ml";
   ocamlc.byte;
   module = "triple.ml";
   ocamlc.byte;
   module = "";
   all_modules = "counter.cmo increment.cmo triple.cmo print.cmo";
   program = "${test_build_directory}/test.thunkified.byte";
   ocamlc.byte;
   run;
   check-program-output;
 }
*)
