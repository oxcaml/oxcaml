(* Mode and modality annotations are erased. *)

val map : ('a -> 'b) @ local -> 'a list -> 'b list

val iter : ('a -> unit) @ local -> 'a list -> unit

val x : int @@ portable

val f : local_ (int -> int) -> int

type t = { global_ a : int; mutable b : string }

type ('a : value) u = A of global_ 'a | B of int * int

type imm : immediate

type imm64 : immediate64

type any : any

module M : sig @@ portable
  val y : int
end
