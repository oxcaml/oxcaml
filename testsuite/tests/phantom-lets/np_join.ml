(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, under which no Simplify dump would be produced). *)
 set OCAMLPARAM = "_,";
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The join-point continuation's parameter [a] is referenced by the phantom
   let for [pair] and is marked (it is user visible, so the mark only records
   that it is needed by a phantom let). The non-user-visible temporaries
   computed in the two branches, whose values flow into [a], are not marked:
   only variables directly referenced by the defining expressions of phantom
   lets are. *)

let test b x =
  let a = if b then x + 1 else x * 2 in
  let pair = a, x in
  fst pair + snd pair
