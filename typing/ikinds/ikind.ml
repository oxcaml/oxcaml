(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

type sub_or_intersect = Jkind.sub_or_intersect =
  | Sub
  | Disjoint of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t
  | Has_intersection of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t

module TyM = struct
  type t = Types.type_expr

  let compare (t1 : t) (t2 : t) =
    Int.compare (Types.get_id t1) (Types.get_id t2)

  let unique_id (t : t) : int = Types.get_id t
end

module ConstrM = struct
  type t = Path.t

  let compare = Path.compare

  let to_string (p : t) : string = Format.asprintf "%a" Path.print p
end

module JK = Ldd_jkind_solver.Make (Axis_lattice_bits) (TyM) (ConstrM)
