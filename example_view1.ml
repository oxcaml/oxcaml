
type t =
  | A
  | B1 of int
  | B2 of int
  | C of float

type view =
  | A
  | B of bool * int
  | C of float

let[@inline] view (t: t) : view =
  match t with
  | A -> A
  | B1 x -> B (true, x)
  | B2 x -> B (false, x)
  | C f -> C f

let[@inline never] fprintf fmt = Format.fprintf fmt

let print fmt (t : t) =
  match view t with
  | A -> fprintf fmt "A"
  | B (b, x) -> fprintf fmt "B(%b, %d)" b x
  | C f -> fprintf fmt "C(%f)" f

