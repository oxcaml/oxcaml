module SL := Slambda

(** Extracts the lambda from the slambda if it's a quote with no splices,
    otherwise returns None. *)
val trivial_slambda : Lambda.slambda -> Lambda.lambda option

val eval : SL.program -> Lambda.program
