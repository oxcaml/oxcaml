(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

(* Rigid names are the variables that may occur in ikind formulas. *)
module Rigid_name = struct
  type t =
    | Atom of
        { constr : Path.t;
          (* arg_index = 0 refers to the base contribution,
             subsequent refer to the coefficients of the i-th argument. *)
          arg_index : int
        }
    | Param of int
        (** Only occurs in formulas for type constructors. Refers to the i-th
            parameter, where i is the id of that type variable. *)
    | Unknown of int
        (** An unknown quantity with a given id. Used to model not-best in
            ikinds. This is used when we couldn't compute a precise ikind,
            e.g. for a polymorphic variant with conjunctive type --
            `Constr of (a & b & ...) *)

  (** Ordering on rigid names used in the LDD to order the nodes. *)
  let compare a b =
    if a == b
    then 0
    else
      match a, b with
      | Atom a1, Atom a2 ->
        let h = Path.compare a1.constr a2.constr in
        if h != 0 then h else Int.compare a1.arg_index a2.arg_index
      | Param x, Param y -> Int.compare x y
      | Atom _, Param _ -> -1
      | Param _, Atom _ -> 1
      | Unknown x, Unknown y -> Int.compare x y
      | Unknown _, _ -> 1
      | _, Unknown _ -> -1

  let to_string = function
    | Atom { constr; arg_index } ->
      Printf.sprintf "%s.%d" (Format.asprintf "%a" Path.print constr) arg_index
    | Param i -> Printf.sprintf "param[%d]" i
    | Unknown id -> Printf.sprintf "unknown[%d]" id

  let atomic constr arg_index = Atom { constr; arg_index }

  let param i = Param i

  let fresh_unknown =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      Unknown id
end

module Ldd = struct
  module Name = Rigid_name

  type lat = Axis_lattice.t

  type constr = Path.t

  include Ldd.Make (Rigid_name)
end
