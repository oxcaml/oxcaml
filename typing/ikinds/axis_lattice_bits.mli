type t

(* Lattice_intf.LATTICE operations expected by the solver. *)
val bot : t
val top : t
val join : t -> t -> t
val meet : t -> t -> t
val leq : t -> t -> bool
val co_sub : t -> t -> t
val equal : t -> t -> bool
val hash : t -> int
val to_string : t -> string
val non_bot_axes : t -> int list

(* Build a mask from a set of relevant axes. *)
val of_axis_set : Jkind_axis.Axis_set.t -> t

(* Relevant axes of a constant modality and the corresponding mask. *)
val mask_of_modality
  :  relevant_for_shallow:[ `Relevant | `Irrelevant ]
  -> Mode.Modality.Const.t
  -> t

(* Convert to/from mod bounds. *)
val of_mod_bounds : Types.Jkind_mod_bounds.t -> t
val to_mod_bounds : t -> Types.Jkind_mod_bounds.t

(* Canonical lattice constants used by ikinds. *)
val nonfloat_value : t
val immutable_data : t
val mutable_data : t
val value : t
val arrow : t
val immediate : t
val object_legacy : t

(* Map from internal axis number (used in diagnostics) to an axis descriptor. *)
val axis_number_to_axis_packed : int -> Jkind_axis.Axis.packed
