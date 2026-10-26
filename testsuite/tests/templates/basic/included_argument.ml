(* [x]'s type is representable here, but its declaration in [S] isn't, so its
   layout isn't recorded in this unit's inferred signature. *)
module type S = sig
  type t : any
  val x : t
end

include (struct type t = int let x = 1 end : S with type t = int)

let y = 2
