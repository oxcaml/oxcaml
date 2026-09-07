(* TEST
 flambda2;
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The join-point continuation's parameter [a] is referenced by the phantom
   let for [pair] and is marked (it is user visible, so the mark only records
   that it is needed by a phantom let). The requirement must then follow the
   argument edges of the (non-recursive) continuation: the non-user-visible
   temporaries computed in the two branches, whose values flow into [a], must
   be marked "np" so that the debugger can recover [a] from whichever branch
   produced it. *)

let test b x =
  let a = if b then x + 1 else x * 2 in
  let pair = a, x in
  fst pair + snd pair
