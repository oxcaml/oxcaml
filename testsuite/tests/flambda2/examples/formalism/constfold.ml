(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a nested arithmetic expression on constants folds to a
   single constant.
   Rules: S.Rewrite.Prim.ConstFold, S.Rewrite.Alias.Canonicalize,
   S.Rewrite.Let.DeadBinding.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-01-constfold.md
   Phenomenon: the body collapses to [cont k (14)]. *)

let f () = (3 + 4) * 2
