(* A unit whose expectations are all loaded from [mtf_aux.cmi]: the module
   type of an ascription and the module type expected of the parameter of a
   functor of another unit. *)

module A = struct type t = int end

module FA = Mtf_aux.F (A)

module M : Mtf_aux.S = struct type t = char end

(* The members of [Mtf_aux.Container] are declared in [mtf_aux.mli], so the
   member checks of this ascription cross the unit boundary; they must not be
   mistaken for pairs between this unit and an interface of its own, which it
   does not have. *)
module Container : Mtf_aux.Container = struct
  module type Local = Mtf_aux.S

  module Member : Mtf_aux.S = struct type t = int end
end
