(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Negative: the pair is stored into a global (static) mutable cell. *)
let cell = ref (0, 0)

let[@inline never] f x y =
  let p = (x, y) in
  cell := p

let () =
  f 5 6;
  let a, b = !cell in
  (Printf.printf [@inlined never]) "%d %d\n" a b
