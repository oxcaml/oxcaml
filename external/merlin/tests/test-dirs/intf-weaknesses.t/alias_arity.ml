(* Arity is the declared type's spelled arrow spine: [spelled] and [aliased] have
   identical implementations, but [aliased]'s row hides the second arrow behind [step],
   capping the spine at one. *)

type t = { fixed : unit }
type step = t -> unit

let spelled (_ : t) (_ : t) = ()
let aliased (_ : t) (_ : t) = ()
