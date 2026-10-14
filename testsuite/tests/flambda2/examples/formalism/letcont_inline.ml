(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a non-recursive continuation used exactly once in tail
   position is inlined and its binder disappears.
   Rules: S.Rewrite.LetCont.Inline, S.Rewrite.LetCont.DeadHandler,
   S.Rewrite.Alias.Canonicalize.
   Case study: middle_end/flambda2/docs/formalism/14-validation/new-03-letcont-inline.md
   Phenomenon: no residual [let_cont] for the join continuation. *)

let f x =
  let y = if true then x else x + 1 in
  y + 100
