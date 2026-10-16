(* TEST
 flambda2;
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-extension layouts_beta";
 compile_only = "true";
 ocamlopt_byte_exit_status = "2";
 {
   arch_amd64;
   compiler_reference = "${test_source_directory}/unsupported_too_large_arg.amd64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
 {
   arch_arm64;
   compiler_reference = "${test_source_directory}/unsupported_too_large_arg.arm64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* A 24-byte struct argument is passed in memory on both ABIs. *)
external f : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "unsupported_too_large_arg_stub"

let g () = f #(#1L, #2L, #3L)
