(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a match whose scrutinee is a statically-known
   constructor keeps no [switch].
   Rules: S.Rewrite.Switch.ArmPrune, S.Rewrite.Switch.Merge.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-02-known-switch.md
   Phenomenon: no [switch] survives; the body is [cont k (1)]. *)

type t = A | B | C

let f () = match A with A -> 1 | B -> 2 | C -> 3
