(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = private int

(** Lattice operations expected by the solver. *)
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

(** Build a mask from a set of relevant axes. *)
val of_axis_set : Jkind_axis.Axis_set.t -> t

(** Mask that excludes the shallow axes (nullability and separability). *)
val mask_shallow : t

(** Relevant axes of a constant modality and the corresponding mask. *)
val mask_of_modality :
  relevant_for_shallow:[`Relevant | `Irrelevant] -> Mode.Modality.Const.t -> t

(** [Jkind_mod_bounds.t] is morally the same type as this.
   However, its encoding of Crossing.t is different and more complex
   than record below. *)
type boxed = {
  areality : Mode.Regionality.Const.t;
  linearity : Mode.Linearity.Const.t;
  uniqueness : Mode.Uniqueness.Const.t;
  portability : Mode.Portability.Const.t;
  contention : Mode.Contention.Const.t;
  forkable : Mode.Forkable.Const.t;
  yielding : Mode.Yielding.Const.t;
  statefulness : Mode.Statefulness.Const.t;
  visibility : Mode.Visibility.Const.t;
  staticity : Mode.Staticity.const;
  externality : Jkind_axis.Externality.t;
  nullability : Jkind_axis.Nullability.t;
  separability : Jkind_axis.Separability.t;
}

val of_boxed : boxed -> t

val to_boxed : t -> boxed

(** Canonical lattice constants used by ikinds. *)
val nonfloat_value : t

val immutable_data : t

val mutable_data : t

val value : t

val arrow : t

val immediate : t

val object_legacy : t

(** Map from internal axis number (used in diagnostics) to an axis
    descriptor. *)
val axis_number_to_axis_packed : int -> Jkind_axis.Axis.packed
