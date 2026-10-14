(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Simplest case: a pair built in [f] and only read by a same-unit callee.
   In [f_tail] the call is a tail call, so making [p] local would require
   closing the region after the call, breaking the tail call: the pair stays
   on the heap (the type checker rejects the equivalent [local_] program).
   In [f] the call is marked [@nontail], so [p] can be local. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f_tail x y =
  let p = (x, y) in
  sum_pair p

let[@inline never] f x y =
  let p = (x, y) in
  (sum_pair p [@nontail])

let () = Printf.printf "%d %d\n" (f_tail 3 4) (f 3 4)
