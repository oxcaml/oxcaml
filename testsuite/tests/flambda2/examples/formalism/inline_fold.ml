(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a small function applied to a constant is inlined and
   the body constant-folds.
   Rules: S.Inline.Substitute, S.Inline.DeclDecision, S.Rewrite.Prim.ConstFold.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-05-inline-fold.md
   Phenomenon: g's body is [cont k (7)]; f survives unfolded. *)

let f x = x + 3

let g () = f 4
