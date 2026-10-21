(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Deep mutual tail recursion with a local allocation per activation: must not
   overflow the stack, so the tail calls must be preserved. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let rec ping n acc =
  if n = 0 then acc else let p = (n, 1) in pong (n - 1) (acc + sum_pair p)
and pong n acc =
  if n = 0 then acc else let p = (n, 2) in ping (n - 1) (acc + sum_pair p)
let () = (Printf.printf [@inlined never]) "%d\n" (ping 10_000_000 0)
