(* ocamlopt -nostdlib -nopervasives -flambda2-reaper -c unbox_unused.ml *)

external (+) : int -> int -> int = "%addint"

let[@inline never][@local never] g _ = ()

let f x p =
  let[@inline never][@local never] pair y = (x, y) in
  let u = pair x in
  let (a, b) = u in
  (a + b, g u)

let f2 x p =
  let[@inline never][@local never] pair y = (x, y) in
  let[@inline never][@local never] indg g = g in
  let u = pair x in
  let (a, b) = u in
  (a + b, (indg g) u)