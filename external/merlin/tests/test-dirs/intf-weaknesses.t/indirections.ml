(* Module-type indirections from core/map.ml: a seal by a named signature, a module alias,
   and an include; the pairing walk expands each through the environment. *)

module type SMALL = sig
  type t : immutable_data

  val id : t -> t @@ stateless
end

module Base0 = struct
  type t = { leaf : unit }

  let id (x : t) = x
end

(* Sealed by name: claims are capped at [SMALL], though the interface may still be weaker
   than the seal. *)
module Sealed : SMALL = Base0

(* An alias keeps [Base0]'s unsealed signature. *)
module Alias = Base0

(* The include-then-extend shape of core/map.ml's [Tree0]. *)
module Combined = struct
  include Base0

  let use (_ : t) = ()
end
