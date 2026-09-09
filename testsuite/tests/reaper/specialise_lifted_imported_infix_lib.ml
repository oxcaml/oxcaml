[@@@ocaml.flambda_o3]

(* Both closures escape, so their shared runtime layout is exported. [first]
   occupies the infix slot, after the three-word [second] closure. *)
let[@inline] make use_second f =
  let[@inline never] rec first x y =
    if use_second then second x y else f (x + y)
  and[@inline never] second x y =
    if x <= 0 then f y else first (x - 1) y
  in
  first, second
