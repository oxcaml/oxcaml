(* TEST *)

(* Interface *)
module type S = sig
val concat : ?sep:string -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat ?(sep : string = " ") xs =
  String.concat sep xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:" " zs
let chain_call ?(sep : string option) arg = M.concat ?sep arg
