(* TEST
 readonly_files = "a.ml alias.ml b.mli c.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-no-crc";
   module = "c.ml";
   ocamlc.byte;
   module = "b.mli";
   ocamlc.byte;
   module = "a.ml";
   ocamlc.byte;
   module = "";
   {
     program = "${test_build_directory}/a.byte";
     all_modules = "c.cmo a.cmo";
     ocamlc.byte;
     output = "${test_build_directory}/a.output";
     run;
     reference = "${test_source_directory}/a.reference";
     check-program-output;
   }{
     flags = "-no-crc -a";
     program = "lib.cma";
     all_modules = "c.cmo a.cmo";
     ocamlc.byte;
     (* Linking a [-no-crc] archive requires [-no-crc]. *)
     flags = "-linkall";
     program = "${test_build_directory}/a.byte";
     all_modules = "lib.cma";
     compiler_output = "${test_build_directory}/link_lib.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
     compiler_reference = "${test_source_directory}/link_lib.ocamlc.reference";
     check-ocamlc.byte-output;
   }{
     (* Importing a [-no-crc] interface requires [-no-crc]. *)
     flags = "";
     module = "a.ml";
     compiler_output = "${test_build_directory}/import.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
     compiler_reference = "${test_source_directory}/import.compilers.reference";
     check-ocamlc.byte-output;
   }{
     (* Linking a [-no-crc] object requires [-no-crc]. *)
     flags = "";
     program = "${test_build_directory}/a.byte";
     all_modules = "c.cmo a.cmo";
     compiler_output = "${test_build_directory}/link.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
     compiler_reference = "${test_source_directory}/link.ocamlc.reference";
     check-ocamlc.byte-output;
   }{
     (* Archiving a [-no-crc] object requires [-no-crc]. *)
     flags = "-a";
     program = "lib2.cma";
     all_modules = "c.cmo a.cmo";
     compiler_output = "${test_build_directory}/archive.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
     compiler_reference = "${test_source_directory}/archive.ocamlc.reference";
     check-ocamlc.byte-output;
   }{
     (* An alias loads the interface, so it is rejected like any import. *)
     flags = "";
     module = "alias.ml";
     compiler_output = "${test_build_directory}/alias.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
     compiler_reference = "${test_source_directory}/alias.compilers.reference";
     check-ocamlc.byte-output;
   }{
     (* With [-no-alias-deps] the alias only reaches the delayed
        [~check:false] path, which reports warning 49 rather than an error. *)
     flags = "-no-alias-deps";
     module = "alias.ml";
     compiler_output = "${test_build_directory}/alias_no_alias_deps.output";
     ocamlc.byte;
     compiler_reference =
       "${test_source_directory}/alias_no_alias_deps.compilers.reference";
     check-ocamlc.byte-output;
   }
 }{
   native-compiler;
   setup-ocamlopt.byte-build-env;
   flags = "-no-crc";
   module = "c.ml";
   ocamlopt.byte;
   module = "b.mli";
   ocamlopt.byte;
   module = "a.ml";
   ocamlopt.byte;
   module = "";
   {
     program = "${test_build_directory}/a.opt";
     all_modules = "c.cmx a.cmx";
     ocamlopt.byte;
     output = "${test_build_directory}/a.output";
     run;
     reference = "${test_source_directory}/a.reference";
     check-program-output;
   }{
     flags = "-no-crc -a";
     program = "lib.cmxa";
     all_modules = "c.cmx a.cmx";
     ocamlopt.byte;
     flags = "";
     program = "${test_build_directory}/a.opt";
     all_modules = "lib.cmxa";
     compiler_output = "${test_build_directory}/link_lib.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;
     compiler_reference = "${test_source_directory}/link_lib.ocamlopt.reference";
     check-ocamlopt.byte-output;
   }{
     flags = "";
     program = "${test_build_directory}/a.opt";
     all_modules = "c.cmx a.cmx";
     compiler_output = "${test_build_directory}/link.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;
     compiler_reference = "${test_source_directory}/link.ocamlopt.reference";
     check-ocamlopt.byte-output;
   }{
     flags = "-a";
     program = "lib2.cmxa";
     all_modules = "c.cmx a.cmx";
     compiler_output = "${test_build_directory}/archive.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;
     compiler_reference = "${test_source_directory}/archive.ocamlopt.reference";
     check-ocamlopt.byte-output;
   }
 }
*)
