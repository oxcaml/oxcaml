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

(* Direct access to packed levels. Length must match the number of axes. *)
val of_levels : levels:int array -> t
val to_levels : t -> int array

(* Build a mask from a set of relevant axes. *)
val of_axis_set : Jkind_axis.Axis_set.t -> t

(* Relevant axes of a constant modality and the corresponding mask. *)
val mask_of_modality
  :  relevant_for_shallow:[ `Relevant | `Irrelevant ]
  -> Mode.Modality.Const.t
  -> t

module Levels : sig
  val level_of_areality : Mode.Regionality.Const.t -> int
  val level_of_linearity : Mode.Linearity.Const.t -> int
  val level_of_uniqueness_monadic : Mode.Uniqueness.Const.t -> int
  val level_of_portability : Mode.Portability.Const.t -> int
  val level_of_contention_monadic : Mode.Contention.Const.t -> int
  val level_of_forkable : Mode.Forkable.Const.t -> int
  val level_of_yielding : Mode.Yielding.Const.t -> int
  val level_of_statefulness : Mode.Statefulness.Const.t -> int
  val level_of_visibility_monadic : Mode.Visibility.Const.t -> int
  val level_of_externality : Jkind_axis.Externality.t -> int
  val level_of_nullability : Jkind_axis.Nullability.t -> int
  val level_of_separability : Jkind_axis.Separability.t -> int

  val areality_of_level : int -> Mode.Regionality.Const.t
  val linearity_of_level : int -> Mode.Linearity.Const.t
  val uniqueness_of_level_monadic : int -> Mode.Uniqueness.Const.t
  val portability_of_level : int -> Mode.Portability.Const.t
  val contention_of_level_monadic : int -> Mode.Contention.Const.t
  val forkable_of_level : int -> Mode.Forkable.Const.t
  val yielding_of_level : int -> Mode.Yielding.Const.t
  val statefulness_of_level : int -> Mode.Statefulness.Const.t
  val visibility_of_level_monadic : int -> Mode.Visibility.Const.t
  val externality_of_level : int -> Jkind_axis.Externality.t
  val nullability_of_level : int -> Jkind_axis.Nullability.t
  val separability_of_level : int -> Jkind_axis.Separability.t
end

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
