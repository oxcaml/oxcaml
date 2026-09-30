(* Comes with existential_alias_b.ml and existential_alias_c.ml.

   The body of [Make] loads field 0 of [X], so the result type of [Make]
   describes that field with an existential variable that is also the type of
   the result's [P] field. *)

module type S = sig
  module D : sig
    val f : int -> int
  end
end

module Make (X : S) = struct
  module P = X.D
end
