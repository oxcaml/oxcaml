(* A valid but narrower interface for the bundle: only [Make], with its
   result signature spelled out instead of [Intf(P).S], and exposing
   only [Basic] of the two bundled modules. *)

module Make (P : sig
  type t
  val create : unit -> t
  val frob : t -> t
  val to_string : t -> string
end) () : sig
  module Basic : sig
    type t
    val create : P.t -> t
    val to_string : t -> string
  end
end
