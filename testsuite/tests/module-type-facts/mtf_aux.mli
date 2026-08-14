(* An auxiliary unit, compiled separately, whose facts are read back from its
   artifacts by [mtf_artifacts.ml]. *)

module type S = sig type t end

module type T = S with type t = int

module M : S

module F (X : S) : sig type u end

module Wrapper : sig
  module type Inner = S

  module N : Inner
end

(* A signature with a module member and a module type member, so that an
   ascription against it from another unit pairs members across the unit
   boundary. *)
module type Container = sig
  module type Local = S

  module Member : S
end
