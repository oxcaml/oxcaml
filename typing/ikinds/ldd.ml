(* Aggregator for LDD backends. Default points to the memoized backend. *)

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

(* Switch here to choose the backend. *)
(* module Make (V : ORDERED) = Ldd_cached_down0.Make (V) *)

(* To test the no-id rigid ordering experiment, switch to: *)

(* module Make (V : ORDERED) = Ldd_no_id.Make (V) *)

(* To test the hybrid hash-id experiment, switch to: *)
(* module Make (V : ORDERED) = Ldd_hybrid.Make (V) *)

(* Optimized hybrid backend (no memoization / no hash-consing). *)
module Make (V : ORDERED) = Ldd_hybrid_opt.Make (V)
