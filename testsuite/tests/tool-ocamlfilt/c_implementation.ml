(* TEST
 native-compiler;
 readonly_files = "e2e_ocaml.ml e2e_oxcaml.ml e2e_inline_lib.ml \
                   e2e_inline_mid.ml e2e_inline.ml";
 {
   compiler_directory_suffix = ".structured";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -extension comprehensions \
            -O3 -c";
   module = "e2e_ocaml.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_oxcaml.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline_lib.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline_mid.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.structured.output";
   script = "sh ${test_source_directory}/c_implementation.sh handwritten \
             ${test_build_directory}/e2e_ocaml.o \
             ${test_build_directory}/e2e_oxcaml.o \
             ${test_build_directory}/e2e_inline_lib.o \
             ${test_build_directory}/e2e_inline_mid.o \
             ${test_build_directory}/e2e_inline.o";
   script;
   reference = "${test_source_directory}/c_implementation.structured.reference";
   check-program-output;
 }{
   compiler_directory_suffix = ".flat";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme flat -extension comprehensions -O3 -c";
   module = "e2e_ocaml.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_oxcaml.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline_lib.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline_mid.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "e2e_inline.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.flat.output";
   script = "sh ${test_source_directory}/c_implementation.sh objects \
             ${test_build_directory}/e2e_ocaml.o \
             ${test_build_directory}/e2e_oxcaml.o \
             ${test_build_directory}/e2e_inline_lib.o \
             ${test_build_directory}/e2e_inline_mid.o \
             ${test_build_directory}/e2e_inline.o";
   script;
   reference = "${test_source_directory}/c_implementation.flat.reference";
   check-program-output;
 }{
   (* The [-for-pack] build embeds the pack prefix in the U payload. *)
   compiler_directory_suffix = ".packed";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -extension comprehensions \
            -for-pack Ox_pack -c";
   module = "e2e_oxcaml.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.packed.output";
   script = "sh ${test_source_directory}/c_implementation.sh objects \
             ${test_build_directory}/e2e_oxcaml.o";
   script;
   reference = "${test_source_directory}/c_implementation.packed.reference";
   check-program-output;
 }
*)

(* tools/demangle.c is a standalone C implementation of the default behavior
   of ocamlfilt, meant to be embedded in profilers and symbolizers. This test
   compiles the sources of the other end-to-end tests of this directory under
   both mangling schemes (and [e2e_oxcaml.ml] also with [-for-pack]) and
   checks, with c_implementation.sh, that [ocamlfilt --c-implementation]
   produces exactly the same output as [ocamlfilt] on every symbol of the
   resulting objects, as well as on a fixed corpus of hand-written symbols.
   Unlike the other end-to-end tests, the references do not list the symbols
   of the objects, so the test does not depend on inlining decisions. *)
