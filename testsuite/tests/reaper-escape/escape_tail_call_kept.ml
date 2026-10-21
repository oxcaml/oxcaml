(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A local allocation followed by an unrelated tail call: the region must be
   closed before the call, which must remain a tail call. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let[@inline never] g s = s * 2
let[@inline never] f x y =
  let p = (x, y) in
  let s = sum_pair p in
  g s
let () = (Printf.printf [@inlined never]) "%d\n" (f 3 4)
