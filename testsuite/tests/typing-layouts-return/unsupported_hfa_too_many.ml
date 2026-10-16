(* TEST
 flambda2;
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-extension layouts_beta";
 compile_only = "true";
 ocamlopt_byte_exit_status = "2";
 {
   arch_amd64;
   compiler_reference = "${test_source_directory}/unsupported_hfa_too_many.amd64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
 {
   arch_arm64;
   compiler_reference = "${test_source_directory}/unsupported_hfa_too_many.arm64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Five doubles: too many members for an arm64 homogeneous aggregate, and over
   16 bytes on x86-64. *)
external f : #(float# * float# * float# * float# * float#) -> float#
  = "" "unsupported_hfa_too_many_stub"

let g () = f #(#1.0, #2.0, #3.0, #4.0, #5.0)
