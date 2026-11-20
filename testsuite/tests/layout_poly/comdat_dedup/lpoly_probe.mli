@@ static

(* Has a probe site, which is listed in a probe note. *)
val lpoly_probe :
  layout_ l1 l2. ('a : l1) ('b : l2). int -> 'a -> 'b -> #(int * 'b * 'a)
