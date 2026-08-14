(* A unit whose facts mention the module types and the functor parameter of
   [Mtf_aux], all loaded from [mtf_aux.cmi]. *)

module A = struct type t = int end

module FA = Mtf_aux.F (A)

module M : Mtf_aux.S = struct type t = char end

module N : Mtf_aux.Wrapper.Inner = struct type t = int end

module type Local = Mtf_aux.S with type t = int

module P : Local = struct type t = int end

module Q : Mtf_aux.T = struct type t = int end

module R : Mtf_aux.S with Mtf_aux.M = Mtf_aux.M

(* The members of [Mtf_aux.Container] are declared in [mtf_aux.mli], so the
   member checks of this ascription cross the unit boundary; they must not be
   mistaken for pairs between this unit and an interface of its own, which it
   does not have. *)
module Container : Mtf_aux.Container = struct
  module type Local = Mtf_aux.S

  module Member : Mtf_aux.S = struct type t = int end
end
