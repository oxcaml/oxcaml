@@ static

(* This interface claims the whole unit is static, but the implementation
   defines [x] by a function application, which is dynamic, so the
   implementation must be rejected. *)

val x : int
