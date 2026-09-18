(* Aliases share one type expression -- and its live mode variables -- with the original
   binding: applicative.ml's [let ( <*> ) = apply] and hashtbl.ml's re-export blocks
   ([let mem = mem]). *)

type t = { fixed : unit }

let apply (f : (t -> t) @ local) x = f x
let ( <*> ) = apply

(* The re-export block shape of hashtbl.ml. *)
module Export = struct
  let apply = apply
end
