(* TEST
 readonly_files = "base.ml dep_a.ml dep_b.ml diamond.ml gc_test.ml closures.ml gc_compact_test.ml driver.c";
 {
   setup-ocamlopt.byte-build-env;

   (* Compile each module with -manual-module-init *)
   flags = "-manual-module-init";
   module = "base.ml";
   ocamlopt.byte;
   module = "dep_a.ml";
   ocamlopt.byte;
   module = "dep_b.ml";
   ocamlopt.byte;
   module = "diamond.ml";
   ocamlopt.byte;
   module = "gc_test.ml";
   ocamlopt.byte;
   module = "closures.ml";
   ocamlopt.byte;
   module = "gc_compact_test.ml";
   ocamlopt.byte;

   (* Create complete object with all modules *)
   module = "";
   flags = "-manual-module-init -output-complete-obj";
   program = "test_modules.${objext}";
   all_modules = "base.cmx dep_a.cmx dep_b.cmx diamond.cmx gc_test.cmx closures.cmx gc_compact_test.cmx";
   ocamlopt.byte;

   (* Link with C driver *)
   script = "${mkexe} -I${ocamlsrcdir}/${runtime_dir} -o test_driver test_modules.${objext} ${bytecc_libs} driver.c";
   output = "${compiler_output}";
   script;

   (* Run the test *)
   program = "./test_driver";
   run;
   check-program-output;
 }
*)

(* This file is just a placeholder for the ocamltest harness.
   The actual test is run via driver.c which calls caml_init_module. *)
