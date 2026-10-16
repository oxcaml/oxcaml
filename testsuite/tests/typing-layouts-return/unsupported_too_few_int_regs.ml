(* TEST
 flambda2;
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-extension layouts_beta";
 compile_only = "true";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Eight integer arguments use up the integer argument registers on both
   x86-64 (six, the rest going on the stack) and arm64 (eight); C would then
   pass the pair in memory. *)
external f :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * int64_u) -> int64_u
  = "" "unsupported_too_few_int_regs_stub"

let g () = f #1L #2L #3L #4L #5L #6L #7L #8L #(#9L, #10L)
