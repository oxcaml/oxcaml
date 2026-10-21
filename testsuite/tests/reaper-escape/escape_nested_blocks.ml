(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A block pointing at another block, neither escaping: both can be
   local, and the inner one must outlive (or share a region with) the
   outer one. *)
let[@inline never] sum_nested (p : (int * int) * int) =
  let (a, b), c = p in
  a + b + c

let[@inline never] f x y z =
  let inner = (x, y) in
  let outer = (inner, z) in
  (sum_nested outer [@nontail])

let () = (Printf.printf [@inlined never]) "%d\n" (f 1 2 3)
