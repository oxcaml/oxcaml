(** Stable long identifiers used by ppxlib's normalized AST. *)
type t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten : t -> string list
val parse : string -> t
