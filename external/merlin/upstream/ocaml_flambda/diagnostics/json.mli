exception Malformed of string

type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | Array of t list
  | Object of (string * t) list

val parse : string -> t

val to_string : t -> string

val malformed : ('a, unit, string, 'b) format4 -> 'a

val field : string -> t -> t

val optional_field : string -> t -> t option

val string : t -> string

val int : t -> int

val array : t -> t list
