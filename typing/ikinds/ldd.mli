module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (V : ORDERED) : sig
  type node

  type var

  (* Constructors *)
  val bot : node

  val top : node

  val const : Axis_lattice.t -> node

  val rigid : V.t -> var

  val new_var : unit -> var

  val var : var -> node

  (* Boolean algebra over nodes *)
  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  (* Solving interface *)
  val solve_lfp : var -> node -> unit

  val enqueue_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  (* Linear decomposition/composition helpers *)
  val decompose_linear : universe:var list -> node -> node * node list

  val leq : node -> node -> bool

  (* If [a ⊑ b] fails, return witness axis indices where they differ. *)
  val leq_with_reason : node -> node -> int list option

  val round_up : node -> Axis_lattice.t

  val map_rigid : (V.t -> node) -> node -> node

  (* Clear all memo tables *)
  val clear_memos : unit -> unit

  (* Pretty printers and checks *)
  val pp : node -> string

  val pp_debug : node -> string
end
