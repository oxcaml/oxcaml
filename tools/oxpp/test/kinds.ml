(* Kind annotations are erased; immediate ones become attributes. *)

type ('a : value) t = 'a list

type imm : immediate = int

let f (type a : value) (x : a) = x

type _ g = G : ('a : immediate). 'a -> 'a g

type u = int as (_ : value)

let i : ('a : value) 'b. 'a -> 'b -> 'a = fun x _ -> x
