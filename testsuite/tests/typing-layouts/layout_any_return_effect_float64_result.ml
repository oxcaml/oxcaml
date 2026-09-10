(* TEST
 flags = "-extension layouts_beta -keywords 5.3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_effect_float64_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The result of a match or try expression with effect-handler cases must have
   layout value.  A non-value result such as float# is a type error, reported
   by the typechecker rather than dying as an internal fatal error in
   translation. *)

open Effect
open Effect.Deep

type _ eff += E : unit eff

let f (g : unit -> float#) : float# =
  match g () with
  | x -> x
  | effect E, k -> continue k ()
