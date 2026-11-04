(* TEST
 flambda2;
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The unboxed fields of [state] become non-user-visible parameters of the
   loopified recursive continuation. The phantom let for [pair] references
   only the first field's parameter, which must be marked "np". However the
   marking must extend through the loop-carried argument cycle: the first
   field's next value is the second field's parameter, so the debugger may
   need to locate the second field's parameter (and, similarly, the
   temporaries passed as loop arguments, such as the decremented fuel) in
   order to recover values that phantom lets refer to. These transitive
   requirements go through the argument edges of the recursive continuation
   and therefore need the fixed point computed by the flow analysis. *)

let[@inline never] [@local never] [@loop] rec iter fuel (state : int * int) =
  if fuel = 0
  then fst state + snd state
  else
    let pair = fst state, fst state in
    let t = fst pair + snd pair in
    iter (fuel - 1) (snd state, t)

let test fuel = iter fuel (1, 2)
