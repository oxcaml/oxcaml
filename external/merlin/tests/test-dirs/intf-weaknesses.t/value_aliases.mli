type t

val apply : (t -> t) @ local -> t -> t
val ( <*> ) : (t -> t) @ local -> t -> t

module Export : sig
  val apply : (t -> t) @ local -> t -> t
end
