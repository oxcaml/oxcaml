module SL := Slambda0

open Format

val slambda0 : (formatter -> 'lam -> unit) -> formatter -> 'lam SL.t0 -> unit
