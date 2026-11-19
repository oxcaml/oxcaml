@@ static

val id : layout_ l. ('a : l). 'a -> 'a

val pair : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('a * 'b)

val calls : int ref

val counted_id : layout_ l. ('a : l). 'a -> 'a

module Stamper (X : sig
  val offset : int @@ dynamic
  val counter : int ref @@ dynamic
end @ static) : sig
  val initial : int @@ dynamic
  val with_stamp : layout_ l. ('a : l). 'a -> #(int * 'a)
end @ static
