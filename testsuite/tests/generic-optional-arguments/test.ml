(* Interface *)
val concat : ?'sep:string -> string list -> string

(* Implementation *)
let concat ?'(sep : string option <- " ") xs =
  String.concat sep xs

(* Usage *)
let default_concat ys = concat ys
let comma_concat zs = concat ~sep:" " zs
let chain_call ?'(sep : string option) arg = concat ?'sep arg
