module SL := Slambda
open Format

val slambda : formatter -> SL.t -> unit

val program : formatter -> SL.program -> unit
