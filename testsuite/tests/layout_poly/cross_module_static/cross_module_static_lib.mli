@@ static

val id : layout_ l. ('a : l). 'a -> 'a

val pair : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('a * 'b)

(* [calls] is bound to a function application in the implementation, so it is
   dynamic and must say so in this [@@ static] interface. *)
val calls : int ref @@ dynamic

val counted_id : layout_ l. ('a : l). 'a -> 'a
