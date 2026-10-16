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

(* Eight floats use up the floating-point argument registers on both ABIs; C
   would then pass the pair in memory. *)
external f :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float# * float#) -> float#
  = "" "unsupported_too_few_float_regs_stub"

let g () = f #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0 #(#9.0, #10.0)
