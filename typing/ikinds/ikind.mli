module RigidName : sig
  type constr = Path.t

  type t =
    | Atom of { constr : constr; arg_index : int }
    | Param of int
    | Unknown of int

  val compare : t -> t -> int

  val to_string : t -> string

  val atomic : constr -> int -> t

  val param : int -> t

  val unknown : int -> t

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
