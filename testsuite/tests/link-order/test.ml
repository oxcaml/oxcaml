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
   program = "${test_build_directory}/test.opt";
   {
     all_modules = "counter.cmx increment.cmx triple.cmx print.cmx";
     reference = "${test_source_directory}/test_order_1.reference";
     ocamlopt.byte;
     run;
     check-program-output;
   }{
     all_modules = "counter.cmx triple.cmx increment.cmx print.cmx";
     reference = "${test_source_directory}/test_order_2.reference";
     ocamlopt.byte;
     run;
     check-program-output;
   }
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
   program = "${test_build_directory}/test.byte";
   {
     all_modules = "counter.cmo increment.cmo triple.cmo print.cmo";
     reference = "${test_source_directory}/test_order_1.reference";
     ocamlc.byte;
     run;
     check-program-output;
   }{
     all_modules = "counter.cmo triple.cmo increment.cmo print.cmo";
     reference = "${test_source_directory}/test_order_2.reference";
     ocamlc.byte;
     run;
     check-program-output;
   }
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
   program = "${test_build_directory}/test.byte";
   {
     all_modules = "counter.cmo increment.cmo triple.cmo print.cmo";
     reference = "${test_source_directory}/test_order_1.reference";
     ocamlc.byte;
     run;
     check-program-output;
   }{
     all_modules = "counter.cmo triple.cmo increment.cmo print.cmo";
     reference = "${test_source_directory}/test_order_2.reference";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)
