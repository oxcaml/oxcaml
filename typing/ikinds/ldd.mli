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

  val non_bot_axes : t -> int list
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) : sig
  type node

  type var

  val bot : node

  val top : node

  val const : C.t -> node

  val rigid : V.t -> var

  val new_var : unit -> var

  val var : var -> node

  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  val solve_lfp : var -> node -> unit

  val enqueue_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  val decompose_linear : universe:var list -> node -> node * node list

  val leq : node -> node -> bool

  val leq_with_reason : node -> node -> int list option

  val round_up : node -> C.t

  val map_rigid : (V.t -> node) -> node -> node

  val clear_memos : unit -> unit

  val pp : node -> string

  val pp_debug : node -> string
end
