@@ static

val lpoly_pair : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('b * 'a)

(* Has a frame descriptor, for the call to [Gc.minor]. *)
val lpoly_call_between :
  layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('b * 'a)

(* Has a jump table, for the [match]. *)
val lpoly_select :
  layout_ l1 l2. ('a : l1) ('b : l2). int -> int -> 'a -> 'b -> #(int * 'b * 'a)

(* Has an exception handler, which is listed in a trap note, and a C call. *)
val lpoly_try :
  layout_ l1 l2. ('a : l1) ('b : l2). string -> 'a -> 'b -> #(int * 'b * 'a)

(* Has a local allocation and a float constant. *)
val lpoly_local :
  layout_ l1 l2. ('a : l1) ('b : l2). int -> 'a -> 'b -> #(int * 'b * 'a)
