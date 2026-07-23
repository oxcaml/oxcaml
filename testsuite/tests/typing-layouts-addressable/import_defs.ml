(* Compiled to a .cmi/.cmo imported by import.ml: the addressability of
   kinds must survive being saved to a cmi ([Subst.Prepare_for_saving]). *)

type t8_plain : bits8
type t8_marked : bits8 addressable

external[@layout_poly] magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"

(* [f]'s parameter has sort [bits8] (layout-poly externals share one sort
   variable, resolved here by the result annotation) but unconstrained
   addressability: its bound is the join of [bits8] and
   [bits8 addressable], accepting both [t8_plain] and [t8_marked]... *)
let f x = (magic x : t8_plain)

(* ... as checked here, in the defining unit. Importers must agree. *)
let in_unit_plain () = f (assert false : t8_plain)
let in_unit_marked () = f (assert false : t8_marked)

(* Module types with a rigid layout variable, for cross-unit inclusion
   checks: [x] and [x addressable] are incomparable, so neither [S_plain]
   nor [S_marked] includes the other - in the defining unit and after a
   cmi round trip alike. *)
module type S_plain = sig
  val g : layout_ x. ('a : x). 'a -> 'a
end

module type S_marked = sig
  val g : layout_ x. ('a : x addressable). 'a -> 'a
end
