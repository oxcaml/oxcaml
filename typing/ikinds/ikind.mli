(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

(** Rigid names are the variables that may occur in ikind formulas. *)
module Rigid_name : sig

  (* CR jujacobs: see if we can make this type abstract. *)
  type t =
    | Atom of
        { constr : Path.t;
          arg_index : int
          (** [arg_index] = 0 refers to the base contribution, and subsequent
              indices refer to the coefficients of the i-th argument. *)
        }
    | Param of int
        (** [Param id] only occurs in formulas for type constructors. Refers to
            a type-parameter of the constructor, where [id] is the id of the
            type variable representing the parameter. *)
    | Unknown of int
        (** An unknown quantity with a given id. Used to model not-best in
            ikinds. This is used when we couldn't compute a precise ikind,
            e.g. for a polymorphic variant with conjunctive type --
            `Constr of (a & b & ...) *)

  (** Ordering on rigid names used in the LDD to order the nodes. *)
  val compare : t -> t -> int

  val to_string : t -> string

  val atomic : Path.t -> int -> t

  val param : int -> t

  val fresh_unknown : unit -> t
end

module Ldd : sig
  type lat = Axis_lattice.t

  type constr = Path.t

  type node

  type var

  module Name : sig
    include module type of Rigid_name
  end

  val bot : node

  val const : lat -> node

  val rigid : Name.t -> var

  val new_var : unit -> var

  val mk_var : var -> node

  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  val solve_lfp : var -> node -> unit

  val enqueue_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  val decompose_into_linear_terms :
    universe:var list -> node -> node * node list

  val leq : node -> node -> bool

  (** Empty list means [a ⊑ b] succeeds. Non-empty list is the witness axes
      where it fails. *)
  val leq_with_reason : node -> node -> int list

  val round_up : node -> lat

  val is_const : node -> bool

  val map_rigid : (Name.t -> node) -> node -> node

  val pp : node -> string

  val pp_debug : node -> string
end
