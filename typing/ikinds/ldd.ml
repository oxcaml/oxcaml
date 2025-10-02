(* Aggregator for LDD backends. Default points to the memoized backend. *)

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

(* Switch here to choose the backend. *)
module Make (V : ORDERED) = Ldd_no_fast_path.Make (V)
