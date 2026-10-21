(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Control: the same as ex01 but with user annotations, to see the
   region shape the type checker produces. *)
let[@inline never] sum_pair (local_ p : int * int) = fst p + snd p

let[@inline never] f x y =
  let local_ p = (x, y) in
  let r = sum_pair p in
  r

let () = (Printf.printf [@inlined never]) "%d\n" (f 3 4)
