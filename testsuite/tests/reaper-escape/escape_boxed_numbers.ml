(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The pair and its two boxed floats can all be local. [sum] is exported, so
   they cannot be unboxed instead. Its returned float must remain on the heap.
   This exercises Box_number rebuilding as well as block containment. *)
let[@inline never] sum (p : float * float) = fst p +. snd p

let[@inline never] f x =
  let p = x +. 1., x +. 2. in
  (sum p [@nontail])

let () = (Printf.printf [@inlined never]) "%.1f\n" (f 2.)
