module SL := Slambda

val is_slambda_trivial : Lambda.slambda -> bool

val eval : SL.program -> Lambda.program
