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

(* A 24-byte struct argument is passed by reference on arm64. (On x86-64 it is
   passed by value on the stack; see unboxed_args_memory.ml.) *)
external f : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "unsupported_too_large_arg_stub"

let g () = f #(#1L, #2L, #3L)
