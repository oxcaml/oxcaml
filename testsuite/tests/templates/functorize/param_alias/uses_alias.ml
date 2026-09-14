(* Parameters: P_alias *)

(* Reads through the alias: [P_alias.B.x] compiles to a read of [A]'s
   runtime field, since [module B = A] in the parameter interface is
   [Mp_absent] (no field of its own). *)
let x = P_alias.B.x
