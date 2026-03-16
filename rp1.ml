external (+) : int -> int -> int = "%addint"

let[@inline never][@local never] g (a, b) c = a + b + c

let[@inline always] f x p =
  let partial_g = g p in
  let[@inline never][@local never] id2 u = u x in
  id2 partial_g

let[@inline never][@local never] pair x = (x, x)
let f1 x = f x (pair x)
(* let f2 x = f x (pair x) *)

let f = ()
let pair = ()
let g = ()