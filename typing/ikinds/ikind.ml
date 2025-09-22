(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

type sub_or_intersect = Jkind.sub_or_intersect =
  | Sub
  | Disjoint of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t
  | Has_intersection of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t

module RigidName = struct
  type constr = Path.t

  type t =
    | Atom of
        { constr : constr;
          arg_index : int
        }
    | Param of int

  let compare a b =
    match a, b with
    | Atom a1, Atom a2 -> (
      match Path.compare a1.constr a2.constr with
      | 0 -> Int.compare a1.arg_index a2.arg_index
      | c -> c)
    | Param x, Param y -> Int.compare x y
    | Atom _, Param _ -> -1
    | Param _, Atom _ -> 1

  let to_string = function
    | Atom { constr; arg_index } ->
      Printf.sprintf "%s.%d" (Format.asprintf "%a" Path.print constr) arg_index
    | Param i -> Printf.sprintf "param%d" i

  let atomic constr arg_index = Atom { constr; arg_index }

  let param i = Param i

  let classify = function
    | Atom { constr; arg_index } -> `Atom (constr, arg_index)
    | Param i -> `Param i
end

module Ldd = struct
  module type BASE = sig
    type node

    type var

    val bot : node

    val const : Axis_lattice_bits.t -> node

    val rigid : RigidName.t -> var

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

    val round_up : node -> Axis_lattice_bits.t

    val map_rigid : (RigidName.t -> node) -> node -> node

    val clear_memos : unit -> unit

    val pp : node -> string

    val pp_debug : node -> string
  end

  module Base : BASE = Ldd.Make (Axis_lattice_bits) (RigidName)

  type lat = Axis_lattice_bits.t

  type constr = RigidName.constr

  type node = Base.node

  type var = Base.var

  module Name = RigidName

  let bot = Base.bot

  let const = Base.const

  let rigid = Base.rigid

  let new_var = Base.new_var

  let var = Base.var

  let join = Base.join

  let meet = Base.meet

  let sub_subsets = Base.sub_subsets

  let solve_lfp = Base.solve_lfp

  let enqueue_lfp = Base.enqueue_lfp

  let enqueue_gfp = Base.enqueue_gfp

  let solve_pending = Base.solve_pending

  let decompose_linear = Base.decompose_linear

  let leq = Base.leq

  let leq_with_reason = Base.leq_with_reason

  let round_up = Base.round_up

  let map_rigid = Base.map_rigid

  let clear_memos = Base.clear_memos

  let pp = Base.pp

  let pp_debug = Base.pp_debug
end
