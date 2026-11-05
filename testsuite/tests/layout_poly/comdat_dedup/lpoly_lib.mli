@@ static

val lpoly_pair : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('b * 'a)

(* Has a frame descriptor, for the call to [Gc.minor]. *)
val lpoly_call_between :
  layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('b * 'a)
