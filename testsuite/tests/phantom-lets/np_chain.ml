(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, under which no Simplify dump would be produced). *)
 set OCAMLPARAM = "_,";
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* Once [pair]'s projections have been simplified away, its binding becomes a
   phantom let referencing the non-user-visible temporary holding
   [(produce x * 2) + 1]. That temporary remains a normal let (its value is
   also used by the addition of the two components), so it is marked "np"
   (not user visible, but needed by a phantom let). Only variables directly
   referenced by the defining expressions of phantom lets are marked: the
   multiplication temporary and the continuation parameter receiving
   [produce x]'s result are not, since no phantom let refers to them. *)

let[@inline never] [@local never] produce x = x + 1

let test x =
  let pair = ((produce x * 2) + 1, x) in
  fst pair + snd pair
