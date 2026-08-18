(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a match on a value already known (from an enclosing
   match) to be a specific constructor prunes the impossible inner arm; the
   residual two-arm switch is recognized as boolean negation.
   Rules: S.Rewrite.Switch.ArmPrune, S.Rewrite.Switch.Merge,
   S.Rewrite.Switch.BooleanNot.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-08-nested-switch.md
   Phenomenon: the body reduces to %boolean_not (x). *)

type t = A | B

let f (x : t) =
  match x with
  | A -> (match x with A -> 1 | B -> 2)
  | B -> 0
