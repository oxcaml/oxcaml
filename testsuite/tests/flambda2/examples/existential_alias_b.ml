(* Comes with existential_alias_a.ml and existential_alias_c.ml.

   Applying [Existential_alias_a.Make] introduces an existential for field 0
   of [X]. [D] is a second variable for the same value, aliased to that
   existential, and [h] captures [D] in a value slot. *)

module Make (X : Existential_alias_a.S) = struct
  module E = (Existential_alias_a.Make [@inlined never]) (X)
  module D = X.D

  let h y = D.f y
end
