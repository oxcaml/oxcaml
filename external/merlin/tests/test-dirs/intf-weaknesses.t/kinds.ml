type id = int

type point =
  { x : float
  ; y : float
  }

type counter = { mutable count : int }

(* This type has an explicit kind annotation in the signature, but it is weaker than it
   can be. *)
type can_be_strengthened = Foo [@@warning "-37"]
