(* TEST *)

let with_optional y ?z:_ x = y + x
let with_coercion y : int -> int = with_optional y
