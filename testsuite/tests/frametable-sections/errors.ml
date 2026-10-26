(* TEST
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 module = "errors.ml";
 ocamlopt_byte_exit_status = "2";
 {
   setup-ocamlopt.byte-build-env;
   flags = "-frametable-sections";
   compiler_reference =
     "${test_source_directory}/errors.no_function_sections.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-function-sections -frametable-sections -internal-assembler";
   compiler_reference =
     "${test_source_directory}/errors.internal_assembler.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-function-sections -frametable-sections -verify-binary-emitter";
   compiler_reference =
     "${test_source_directory}/errors.verify_binary_emitter.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-function-sections -frametable-sections -llvm-backend";
   compiler_reference =
     "${test_source_directory}/errors.llvm_backend.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Combinations of options that -frametable-sections rejects. *)

let () = print_endline "not compiled"
