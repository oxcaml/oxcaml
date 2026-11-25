(* [x]'s declaration in [S] isn't representable, but its type is representable
   here, so [include] records its layout in this unit's inferred signature. *)
module type S = sig
  type t : any
  val x : t
end

include S with type t = int

val y : int
