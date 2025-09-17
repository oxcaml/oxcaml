type sub_or_intersect = Jkind.sub_or_intersect =
  | Sub
  | Disjoint of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t
  | Has_intersection of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t

module TyM : sig
  type t = Types.type_expr

  val compare : t -> t -> int

  val unique_id : t -> int
end

module ConstrM : sig
  type t = Path.t

  val compare : t -> t -> int

  val to_string : t -> string
end

module JK : module type of Ldd_jkind_solver.Make (Axis_lattice_bits) (TyM) (ConstrM)
