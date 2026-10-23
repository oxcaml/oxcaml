(* TEST
 native-compiler;
 readonly_files = "c_implementation_lib.ml c_implementation_input.ml";
 {
   compiler_directory_suffix = ".structured";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -O3 -c";
   module = "c_implementation_lib.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "c_implementation_input.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.structured.output";
   script = "sh ${test_source_directory}/c_implementation.sh handwritten \
             ${test_build_directory}/c_implementation_lib.o \
             ${test_build_directory}/c_implementation_input.o";
   script;
   reference = "${test_source_directory}/c_implementation.structured.reference";
   check-program-output;
 }{
   compiler_directory_suffix = ".flat";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme flat -O3 -c";
   module = "c_implementation_lib.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "c_implementation_input.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.flat.output";
   script = "sh ${test_source_directory}/c_implementation.sh objects \
             ${test_build_directory}/c_implementation_lib.o \
             ${test_build_directory}/c_implementation_input.o";
   script;
   reference = "${test_source_directory}/c_implementation.flat.reference";
   check-program-output;
 }{
   (* The [-for-pack] build embeds the pack prefix in the U payload. *)
   compiler_directory_suffix = ".packed";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -for-pack Ox_pack -c";
   module = "c_implementation_lib.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   module = "c_implementation_input.ml";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "c_implementation.packed.output";
   script = "sh ${test_source_directory}/c_implementation.sh objects \
             ${test_build_directory}/c_implementation_input.o";
   script;
   reference = "${test_source_directory}/c_implementation.packed.reference";
   check-program-output;
 }
*)

(* tools/demangle.c is a standalone C implementation of the default behavior
   of ocamlfilt, meant to be embedded in profilers and symbolizers. This test
   compiles c_implementation_input.ml, with c_implementation_lib.ml, under
   both mangling schemes and with [-for-pack], and checks, with
   c_implementation.sh, that [ocamlfilt --c-implementation] produces exactly
   the same output as [ocamlfilt] on every symbol of the resulting objects,
   as well as on a fixed corpus of hand-written symbols.

   The inputs are dedicated to this test rather than shared with the other
   end-to-end tests: some CI configurations (Oclassic, O3 metrics, reaper)
   delete those tests because their reference tables depend on inlining
   decisions, whereas this comparison holds whatever symbols a configuration
   produces. For the same reason the references do not list the symbols of
   the objects, only an agreement line per object. *)
