(* TEST
 readonly_files = "cycle_module.ml cycle_stubs.c driver.c";
 {
   setup-ocamlopt.byte-build-env;

   (* Compile module with -manual-module-init *)
   flags = "-manual-module-init";
   module = "cycle_module.ml";
   ocamlopt.byte;

   (* Compile the C stubs *)
   module = "";
   script = "${cc} ${cflags} -I${ocamlsrcdir}/${runtime_dir} -c cycle_stubs.c -o cycle_stubs.${objext}";
   script;

   (* Create complete object with module *)
   flags = "-manual-module-init -output-complete-obj";
   program = "test_modules.${objext}";
   all_modules = "cycle_module.cmx";
   ocamlopt.byte;

   (* Link with C driver and stubs *)
   script = "${mkexe} -I${ocamlsrcdir}/${runtime_dir} -o test_driver test_modules.${objext} cycle_stubs.${objext} ${bytecc_libs} driver.c";
   script;

   (* Run the test - expect abort due to cycle detection *)
   program = "./test_driver";
   exit_status = "-6";
   run;
   check-program-output;
 }
*)

(* This file is just a placeholder for the ocamltest harness.
   The actual test is run via driver.c which calls caml_init_module. *)
