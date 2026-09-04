(* Second implementation of [P], distinct identity from [P_int]. *)

type t = int

let create () = 10
let frob t = t + 10
let to_string = string_of_int
