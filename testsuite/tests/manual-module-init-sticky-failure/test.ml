(* TEST
 readonly_files = "fail_config.ml fail_a.ml fail_b.ml fail_c.ml fail_d.ml sticky_failure_test.ml driver.c";
 runtime5;
 {
   setup-ocamlopt.byte-build-env;

   flags = "-manual-module-init";
   module = "fail_config.ml";
   ocamlopt.byte;
   module = "fail_c.ml";
   ocamlopt.byte;
   module = "fail_b.ml";
   ocamlopt.byte;
   module = "fail_a.ml";
   ocamlopt.byte;
   module = "fail_d.ml";
   ocamlopt.byte;
   module = "sticky_failure_test.ml";
   ocamlopt.byte;

   module = "";
   flags = "-manual-module-init -output-complete-obj";
   program = "test_modules.${objext}";
   all_modules = "fail_config.cmx fail_c.cmx fail_b.cmx fail_a.cmx fail_d.cmx sticky_failure_test.cmx";
   ocamlopt.byte;

   script = "${mkexe} -I${ocamlsrcdir}/${runtime_dir} -o test_driver test_modules.${objext} ${bytecc_libs} driver.c";
   script;

   program = "./test_driver";
   run;
   check-program-output;
 }
*)

(* Placeholder for ocamltest. Test logic is in sticky_failure_test.ml. *)
