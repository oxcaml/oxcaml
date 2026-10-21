(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A closure capturing [a] and [b], passed to a same-unit higher-order
   function that only calls it. *)
let[@inline never] apply_twice (f : int -> int) x = f (f x)

let[@inline never] g a b =
  let h y = (y * a) + b in
  (apply_twice h 1 [@nontail])

let () = (Printf.printf [@inlined never]) "%d\n" (g 2 3)
