(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O3 -g -gno-upstream-dwarf";
   ocamlopt_flags += " -flambda2-expert-phantom-lets";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-simplify;
   check-fexpr-dump;
 *)

(* The unboxed fields of [state] become non-user-visible parameters of the
   loopified recursive continuation. The phantom let for [pair] references
   only the first field's parameter, which is therefore marked
   "needed_by_phantom_let". The second field's parameter and the temporaries
   passed as loop arguments (such as the decremented fuel) are not marked,
   even though their values flow into the first field's parameter around the
   loop: the marking records only direct references from the defining
   expressions of phantom lets. *)

(* CR mshinwell: The loopified handler has no phantom binding for [state],
   whose fields it receives unboxed, so the debugger sees the [state] passed
   to the function throughout the loop rather than the current iteration's.
   Removed parameters only receive a phantom binding when the handler still
   refers to them, which is not the case here: the projections from [state]
   have all been replaced by the unboxed fields. *)

let[@inline never] [@local never] [@loop] rec iter fuel (state : int * int) =
  if fuel = 0
  then fst state + snd state
  else
    let pair = fst state, fst state in
    let t = fst pair + snd pair in
    iter (fuel - 1) (snd state, t)

let test fuel = iter fuel (1, 2)
