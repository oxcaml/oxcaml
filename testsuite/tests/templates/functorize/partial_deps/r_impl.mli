(* Parameters: P.  Argument for R.

   [filler] is a non-function value exported before [greeting], so it
   sits in [R_impl]'s primary block at offset 0.  The arg block
   coerces to [R]'s single-field sig ([greeting] only).  If a consumer
   accidentally receives the whole main block instead of the arg
   block, reading offset 0 as [R.greeting] (a function) and applying
   it dereferences an integer as a code pointer — segfault. *)

val filler : int
val greeting : unit -> string
