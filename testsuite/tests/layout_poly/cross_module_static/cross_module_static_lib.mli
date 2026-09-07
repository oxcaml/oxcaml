@@ static

val id : layout_ l. ('a : l). 'a -> 'a

val pair : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('a * 'b)

val calls : int ref

val counted_id : layout_ l. ('a : l). 'a -> 'a
