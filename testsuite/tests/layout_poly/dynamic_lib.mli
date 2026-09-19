(* No file-level [@@ static], so this unit is dynamic: none of its values has a
   compile-time half, including the layout-polymorphic [Builtin.f]. *)

module Builtin : sig
  val poly_ f : 'a -> 'a
end

val h : int -> int
