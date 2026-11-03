(* TEST
 readonly_files = "param.mli arg.ml uses_param.ml";
 setup-ocamlc.byte-build-env;
 ocamlc_flags = "-w -misplaced-attribute -w -bad-module-name";
 flags = "-no-crc -as-parameter";
 module = "param.mli";
 ocamlc.byte;
 flags = "-no-crc -as-argument-for Param";
 module = "arg.ml";
 ocamlc.byte;
 flags = "-no-crc -parameter Param";
 module = "uses_param.ml";
 ocamlc.byte;
 module = "";
 {
   flags = "-no-crc -instantiate";
   program = "uses_param-Arg.cmo";
   all_modules = "uses_param.cmo arg.cmo";
   ocamlc.byte;
 }{
   (* Instantiating a [-no-crc] object requires [-no-crc]; this exercises
      the [Cmo_io] reader shared by the instantiator and the functorizer. *)
   flags = "-instantiate";
   program = "uses_param-Arg.cmo";
   all_modules = "uses_param.cmo arg.cmo";
   compiler_output = "${test_build_directory}/instantiate.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/instantiate.reference";
   check-ocamlc.byte-output;
 }
*)
