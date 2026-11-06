(*
type t
type s = t
*)

module Even : sig
  type t
  val create : int -> t option
  val get : t -> int
end

(* val even_opt : Even.t option *)
