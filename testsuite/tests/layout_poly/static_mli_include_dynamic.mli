@@ static

(* This interface claims the whole unit is static, but the implementation
   [include]s a dynamic unit, so the items it gets that way are dynamic and
   the implementation must be rejected. *)

val poly_ f : 'a -> 'a

val h : int -> int
