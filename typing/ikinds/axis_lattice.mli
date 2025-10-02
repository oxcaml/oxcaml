type t

(* Lattice operations expected by the solver. *)
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

(* Individual axis constants: each axis at a specific non-bot level *)

(* Areality axis *)
val areality_global : t
val areality_regional : t
val areality_local : t

(* Linearity axis *)
val linearity_many : t
val linearity_once : t

(* Uniqueness axis (monadic) *)
val uniqueness_aliased : t
val uniqueness_unique : t

(* Portability axis *)
val portability_portable : t
val portability_nonportable : t

(* Contention axis (monadic) *)
val contention_contended : t
val contention_shared : t
val contention_uncontended : t

(* Yielding axis *)
val yielding_unyielding : t
val yielding_yielding : t

(* Statefulness axis *)
val statefulness_stateless : t
val statefulness_observing : t
val statefulness_stateful : t

(* Visibility axis (monadic) *)
val visibility_immutable : t
val visibility_read : t
val visibility_read_write : t

(* Externality axis *)
val externality_external : t
val externality_external64 : t
val externality_internal : t

(* Nullability axis *)
val nullability_non_null : t
val nullability_maybe_null : t

(* Separability axis *)
val separability_non_float : t
val separability_separable : t
val separability_maybe_separable : t

(* Mask that excludes the shallow axes (nullability and separability). *)
val mask_shallow : t

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
