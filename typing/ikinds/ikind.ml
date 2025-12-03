(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

module RigidName = struct
  type constr = Path.t

  type t =
    | Atom of
        { constr : constr;
          arg_index : int
        }
    | Param of int
    | Unknown of int

  let compare a b =
    match a, b with
    | Unknown x, Unknown y -> Int.compare x y
    | Unknown _, _ -> 1
    | _, Unknown _ -> -1
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
    | Unknown id -> Printf.sprintf "unknown%d" id

  let atomic constr arg_index = Atom { constr; arg_index }

  let param i = Param i

  let unknown id = Unknown id

  let fresh_unknown =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      Unknown id
end

module Ldd = struct
  module Base = Ldd.Make (RigidName)

  type lat = Axis_lattice.t

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
