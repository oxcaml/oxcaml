include (struct
type t = int

let of_int x = 10 * x
let to_string x = string_of_int x
end : sig
  type t

  val of_int : int -> t
  val to_string : t -> string
end)
