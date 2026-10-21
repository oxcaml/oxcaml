(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Returning from inside a loop, and a loop-carried allocation. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let[@inline never] find n =
  let rec loop i prev =
    if i > n then -1
    else
      let p = (i, i * i) in
      if sum_pair p + sum_pair prev > 50 then i else loop (i + 1) p
  in
  loop 1 (0, 0)
let[@inline never] count n =
  let rec loop i acc =
    if i > n then acc
    else
      let p = (i, i) in
      if sum_pair p > 1000 then acc else loop (i + 1) (acc + sum_pair p)
  in
  loop 1 0
let () = (Printf.printf [@inlined never]) "%d %d\n" (find 100) (count 100)
