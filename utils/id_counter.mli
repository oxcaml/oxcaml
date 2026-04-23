(** Fresh integer-backed ID namespaces.

    Each application of [Make] produces a new namespace with its own hidden
    counter and a distinct abstract type [t], so IDs from different
    namespaces cannot be confused. *)

module type S = sig
  type t = private int

  val create : unit -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end

module Make () : S
