(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, under which no Simplify dump would be produced). *)
 set OCAMLPARAM = "_,";
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The unboxed fields of [state] become non-user-visible parameters of the
   loopified recursive continuation. The phantom let for [pair] references
   only the first field's parameter, which is therefore marked "np". The
   second field's parameter and the temporaries passed as loop arguments
   (such as the decremented fuel) are not marked, even though their values
   flow into the first field's parameter around the loop: the marking records
   only direct references from the defining expressions of phantom lets. *)

let[@inline never] [@local never] [@loop] rec iter fuel (state : int * int) =
  if fuel = 0
  then fst state + snd state
  else
    let pair = fst state, fst state in
    let t = fst pair + snd pair in
    iter (fuel - 1) (snd state, t)

let test fuel = iter fuel (1, 2)
