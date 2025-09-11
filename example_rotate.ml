
type t =
  | A | B | C

let rotate = function
  | A -> B
  | B -> C
  | C -> A

let rotate2 t = rotate (rotate t)

let rotate3 t = rotate (rotate (rotate t))

