type 'a wrapped = ('a @@ global)

val wrap : 'a -> 'a wrapped
val unwrap : 'a wrapped -> 'a
val id : 'a wrapped -> 'a wrapped

module Nested : sig
  val id : 'a wrapped -> 'a wrapped
end
