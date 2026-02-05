(* TEST
 readonly_files = "base.ml dep_a.ml dep_b.ml diamond.ml gc_test.ml closures.ml gc_compact_test.ml reentrant_a.ml reentrant_b.ml reentrant_c.ml reentrant_stubs.c driver.c";
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
   module = "reentrant_a.ml";
   ocamlopt.byte;
   module = "reentrant_c.ml";
   ocamlopt.byte;
   module = "reentrant_b.ml";
   ocamlopt.byte;

   (* Compile the C stubs *)
   module = "";
   script = "${cc} ${cflags} -I${ocamlsrcdir}/${runtime_dir} -c reentrant_stubs.c -o reentrant_stubs.${objext}";
   script;

   (* Create complete object with all modules *)
   flags = "-manual-module-init -output-complete-obj";
   program = "test_modules.${objext}";
   all_modules = "base.cmx dep_a.cmx dep_b.cmx diamond.cmx gc_test.cmx closures.cmx gc_compact_test.cmx reentrant_a.cmx reentrant_c.cmx reentrant_b.cmx";
   ocamlopt.byte;

   (* Link with C driver and stubs *)
   script = "${mkexe} -I${ocamlsrcdir}/${runtime_dir} -o test_driver test_modules.${objext} reentrant_stubs.${objext} ${bytecc_libs} driver.c";
   script;

   (* Run the test *)
   program = "./test_driver";
   run;
   check-program-output;
 }
*)

(* This file is just a placeholder for the ocamltest harness.
   The actual test is run via driver.c which calls caml_init_module. *)
