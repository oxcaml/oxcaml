(* A second static module, so that reading static data can chain across two
   units. *)

@@ static

val relay_id : layout_ l. ('a : l). 'a -> 'a
