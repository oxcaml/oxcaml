(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: two branches build mixed blocks of the same shape; at
   the join the block type keeps that shape (equal shapes => keep sigma), which
   lets the flat-suffix field be unboxed across the join.
   Rules: T.Meet.BlockShape.
   Case study: middle_end/flambda2/docs/formalism/14-validation/mixed-04-join.md
   Phenomenon: the join preserves the mixed shape [0 of val * float]; the
   flat-suffix field is threaded as an unboxed continuation argument. *)

type t = { a : string; b : float# }

external opaque : 'a -> 'a = "%opaque"

let choose cond (s1 : string) (f1 : float#) (s2 : string) (f2 : float#) =
  let r = if cond then { a = s1; b = f1 } else { a = s2; b = f2 } in
  let _ : t = opaque r in
  r.b
