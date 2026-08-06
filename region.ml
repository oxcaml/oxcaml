external (+) : int -> int -> int = "%addint"

type t = A | B | C of int

let[@inline] f x y =
  let r = match x with
  | A -> B
  | B -> C y
  | C a -> A
  in
  r, 1

let[@inline][@zero_alloc assume] p f x y = f x y

let[@inline] h x y =
  let z = match x with A -> B | B -> C y | C _ -> A in
  p f z y

let[@inline] h' x y =
  match h x y with A, _ -> y | B, _ -> 0 | C a, _ -> a

let[@inline] h'' x y = h' x y
