(* TEST
 flambda2;
 native;
*)

(* [(compare x y) u< 0] should not be optimized to [x < y]. See PR#6497. *)

[@@@ocaml.flambda_o3]

external compare_int : int -> int -> int = "%int_compare"

external unsigned_lt : int -> int -> bool = "%int_unsigned_lessthan"
external unsigned_gt : int -> int -> bool = "%int_unsigned_greaterthan"

let[@inline never] f x y = unsigned_lt (compare_int x y) 0
let[@inline never] g x y = unsigned_gt (compare_int x y) 0

let () =
  Printf.printf "%b %b %b %b %b %b\n"
    (f (Sys.opaque_identity 1) (Sys.opaque_identity 2))
    (f (Sys.opaque_identity 2) (Sys.opaque_identity 1))
    (f (Sys.opaque_identity 3) (Sys.opaque_identity 3))
    (g (Sys.opaque_identity 1) (Sys.opaque_identity 2))
    (g (Sys.opaque_identity 2) (Sys.opaque_identity 1))
    (g (Sys.opaque_identity 3) (Sys.opaque_identity 3))
