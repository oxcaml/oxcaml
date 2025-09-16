(* Aggregator for LDD backends. Default points to the memoized backend. *)

module type LATTICE = sig
  type t

  val bot : t

  val top : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val co_sub : t -> t -> t

  val to_string : t -> string

  val equal : t -> t -> bool

  val hash : t -> int

  val find_non_bot_axis : t -> int option
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

(* Switch here to choose the backend. *)
module Make (C : LATTICE) (V : ORDERED) = Ldd_memo.Make (C) (V)
