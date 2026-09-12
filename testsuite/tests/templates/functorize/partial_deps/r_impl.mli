(* Parameters: P.  Argument for R.

   [filler] is an unboxed value exported before [greeting]: it makes
   [R_impl]'s main block a mixed block, and it sits before [greeting]
   in source order.  The arg block coerces to [R]'s single-field sig
   ([greeting] only).  Projecting the arg block out of the main block
   must therefore use the block's mixed representation as well as the
   right index. *)

val filler : float#
val greeting : unit -> string
