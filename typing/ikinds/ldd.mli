module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (V : ORDERED) : sig
  type node

  type var

  (** Constructors. *)
  val bot : node

  val top : node

  val const : Axis_lattice.t -> node

  val rigid : V.t -> var

  val new_var : unit -> var

  val node_of_var : var -> node

  (** Boolean algebra over nodes. *)
  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  (** Solving interface. *)
  val solve_lfp : var -> node -> unit

  val enqueue_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  (** Linear decomposition/composition helpers.
      [decompose_into_linear_terms ~universe n] returns a base term and a list
      of linear coefficients, one per variable in [universe]. *)
  val decompose_into_linear_terms :
    universe:var list -> node -> node * node list

  (** If [a ⊑ b] fails, return witness axis indices where they differ.
      Empty list means [a ⊑ b] succeeds. Non-empty list is the witness axes
      where it fails. *)
  val leq_with_reason : node -> node -> int list

  val round_up : node -> Axis_lattice.t

  val is_const : node -> bool

  val map_rigid : (V.t -> node) -> node -> node

  (** Pretty printers and checks. *)
  val pp : node -> string

  val pp_debug : node -> string
end
