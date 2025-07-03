module Expr : sig
  val bool : bool -> <[bool]> expr
  val int : int -> <[int]> expr
  val int32 : int32 -> <[int32]> expr
  val int64 : int64 -> <[int64]> expr
  val char : char -> <[char]> expr
  val string : string -> <[string]> expr
end

val print : Format.formatter -> 'a expr -> unit
val as_string : 'a expr -> string
