(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

module RigidName : sig
  (* Rigid names are the variables that may occur in ikind formulas. *)

  (* CR jujacobs: see if we can make this type abstract. *)
  type t =
    | Atom of
        { constr : constr;
          (* arg_index = 0 refers to the base contribution, 
            subsequent refer to the coefficients of the i-th argument. *)
          arg_index : int 
        }
    (* Param only occurs in the formula for a type constructor, 
       refers to the i-th parameter. *)
    | Param of int 
    (* An unknown quantity with a given id. Used to model not-best in ikinds. 
       This is used when we couldn't compute a precise ikind, e.g. for a 
       polymorphic variant with conjunctive type -- `Constr of (a & b & ...) *)
    | Unknown of int
  and constr = Path.t

  (* Ordering on rigid names used in the LDD to order the nodes. *)
  val compare : t -> t -> int

  val to_string : t -> string

  val atomic : constr -> int -> t

  val param : int -> t

  val fresh_unknown : unit -> t
end

module Ldd : sig
  type lat = Axis_lattice.t

  type constr = RigidName.constr

  type node

  type var

  module Name : sig
    include module type of RigidName
  end

  val bot : node

  val const : lat -> node

  val rigid : Name.t -> var

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

  val round_up : node -> lat

  val map_rigid : (Name.t -> node) -> node -> node

  val clear_memos : unit -> unit

  val pp : node -> string

  val pp_debug : node -> string
end
