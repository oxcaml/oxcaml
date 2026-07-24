(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: all-or-nothing exception-handler demotion.
   Rules: S.Rewrite.LetCont.DemoteExn (ch. 10), INV.Simplify.EffectfulDeletionInventory
   (ch. 13), S.Struct.Flow.ExnFirstParam (ch. 09).
   Case study: middle_end/flambda2/docs/formalism/14-validation/exn_demotion.md
   Phenomenon: fa's call-free try-body handler is demoted (no push/pop, raise
   becomes a jump; the try-region pair survives). fc's handler has an escaping
   Apply use (callee) so it is NOT demoted: push/pop survive and the raise's
   E-block allocation is pinned alive by the exn bucket param even though the
   handler ignores it. *)

external opaque : 'a -> 'a = "%opaque"
external raise : exn -> 'a = "%raise"
external ( + ) : int -> int -> int = "%addint"
external ( * ) : int -> int -> int = "%mulint"

exception E of int

let[@inline never] fa b x =
  try (if b then raise (E x) else x + 1) with E n -> n * 2

let[@inline never] callee x = x + 1

let[@inline never] fc b x =
  try (if b then raise (E (x * 41 + 1)) else callee x) with _ -> 0

let r1 = fa (opaque true) (opaque 5)
let r2 = fc (opaque true) (opaque 6)
