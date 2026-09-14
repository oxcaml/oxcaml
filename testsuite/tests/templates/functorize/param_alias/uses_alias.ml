(* Parameters: P_alias *)

(* [-as-parameter] scrapes [module B = A] to a concrete declaration, so
   [B] has its own runtime field and this reads it (not [A]'s). *)
let x = P_alias.B.x
