(* TEST
 codegen_variant = "fp";
 variant_available;
 codegen_variant = "base";
 variant_available;
 readonly_files = "resolver_callee.ml resolver_caller.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-O3";
 codegen_variant = "fp";
 module = "resolver_callee.ml";
 ocamlopt.byte;
 module = "resolver_caller.ml";
 ocamlopt.byte;
 codegen_variant = "base";
 module = "resolver_callee.ml";
 ocamlopt.byte;
 codegen_variant = "fp";
 module = "resolver_mismatch.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The .cmx of a unit compiled for another codegen variant is refused with
   the usual error message also when it is first read while the optimizer
   looks up a symbol: this file does not name Resolver_callee, whose .cmx
   (rebuilt for another variant after Resolver_caller was compiled) is
   only reached through the body of the function inlined here. *)

let () = print_int (Resolver_caller.double_plus_one 1)
