(* TEST
 flambda2;
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* Once [pair]'s projections have been simplified away, its binding becomes a
   phantom let referencing the non-user-visible temporary holding
   [(produce x * 2) + 1]. That temporary remains a normal let (its value is
   also used by the addition of the two components), so it is marked "np";
   and the marking must propagate through the defining expressions of marked
   bindings: the multiplication temporary, and in turn the non-user-visible
   continuation parameter receiving [produce x]'s result, must also be marked
   "np", since the debugger may need them to recover the values of variables
   further down the chain. *)

let[@inline never] [@local never] produce x = x + 1

let test x =
  let pair = ((produce x * 2) + 1, x) in
  fst pair + snd pair
