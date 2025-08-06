module Expr : sig
  val bool : bool -> <[bool]> expr
  val int : int -> <[int]> expr
  val int32 : int32 -> <[int32]> expr
  val int64 : int64 -> <[int64]> expr
  val nativeint : nativeint -> <[nativeint]> expr
  val float : float -> <[float]> expr
  val char : char -> <[char]> expr
  val string : string -> <[string]> expr
end

val print : Format.formatter -> 'a expr -> unit
val string_of_expr : 'a expr -> string
