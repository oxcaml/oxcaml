external (+) : int -> int -> int = "%addint"

let[@inline] p y =
  let[@inline] rec f x = y + f x in
  let g y = f y in
  let () = () in
  g
