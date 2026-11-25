(* TEST
 flambda2;
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-extension layouts_beta";
 compile_only = "true";
 ocamlopt_byte_exit_status = "2";
 arch_arm64;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Five doubles: too many members for an arm64 homogeneous aggregate, so the
   struct is passed by reference. (On x86-64 it is passed by value on the
   stack; see unboxed_args_memory.ml.) *)
external f : #(float# * float# * float# * float# * float#) -> float#
  = "" "unsupported_hfa_too_many_stub"

let g () = f #(#1.0, #2.0, #3.0, #4.0, #5.0)
