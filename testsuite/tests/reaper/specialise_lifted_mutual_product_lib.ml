(* Both callbacks become leading parameters of both mutually recursive
   functions.  Their synthetic slots must agree across the set of closures. *)
let[@inline] mutual f g n =
  let[@inline never] rec even n =
    if n = 0 then f 0 else f n + odd (n - 1)
  and[@inline never] odd n =
    if n = 0 then g 0 else g n + even (n - 1)
  in
  even n

(* The lifted callback is prepended to an already unboxed first parameter. *)
let[@inline] product f #(a, b) n =
  let[@inline never] rec loop #(a, b) n =
    if n = 0 then f #(a, b)
    else f #(a, b) + loop #(a, b) (n - 1)
  in
  loop #(a, b) n
