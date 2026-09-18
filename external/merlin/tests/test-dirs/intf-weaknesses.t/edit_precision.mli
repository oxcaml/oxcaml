type a (* trailing comment *)

type b
(** floating documentation after the declaration *)

type c   type d

type 'a e constraint 'a = int * int

val double : int -> int (* trailing comment *)

type f [@@deprecated "gone"]

val quad : int -> int [@@deprecated "gone"]

type g
and h

type t : value

val scale : int -> int @@ many
val id_portable : int -> int @@ portable
val id_writing : int -> int @@ writing
