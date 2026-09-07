(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of conditional control flow: the [Switch] expression.

    The scrutinee of a [Switch] is an integer of one of the kinds described by
    [Flambda_kind.Standard_int.t]: a tagged immediate, or a naked integer of any
    width (but not a vector or a mask). The discriminant of each arm is given as
    a [Targetint_32_64.t] holding the value of the corresponding constant of the
    scrutinee's kind, sign-extended. The discriminants of a switch on naked
    64-bit integers always use the 64-bit representation of [Targetint_32_64.t];
    those of switches on all other kinds use the representation corresponding to
    the target's machine width (see [discriminant_width]).

    There are no default cases. Switches always have at least two arms. *)

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create :
  condition_dbg:Debuginfo.t ->
  scrutinee_kind:Flambda_kind.Standard_int.t ->
  scrutinee:Simple.t ->
  arms:Apply_cont_expr.t Targetint_32_64.Map.t ->
  t

(** Create a [Switch] on a scrutinee of kind [Naked_immediate] corresponding to
    a traditional if-then-else. *)
val if_then_else :
  machine_width:Target_system.Machine_width.t ->
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  if_true:Apply_cont_expr.t ->
  if_false:Apply_cont_expr.t ->
  t

(** The scrutinee of the switch. *)
val scrutinee : t -> Simple.t

(** The kind of the scrutinee of the switch. *)
val scrutinee_kind : t -> Flambda_kind.Standard_int.t

(** The debuginfo to be used for the condition. *)
val condition_dbg : t -> Debuginfo.t

(** Call the given function [f] on each (discriminant, action) pair in the
    switch. *)
val iter : t -> f:(Targetint_32_64.t -> Apply_cont_expr.t -> unit) -> unit

(** What the switch will do for each possible value of the discriminant. *)
val arms : t -> Apply_cont_expr.t Targetint_32_64.Map.t

(** The width of [Targetint_32_64.t] used for the discriminants of a switch on a
    scrutinee of the given kind, when compiling for the given machine width (see
    the description of discriminants above). *)
val discriminant_width :
  Flambda_kind.Standard_int.t ->
  machine_width:Target_system.Machine_width.t ->
  Target_system.Machine_width.t

(** The constant, of the given scrutinee kind, denoted by a discriminant (see
    the description of discriminants above). *)
val const_of_discriminant :
  machine_width:Target_system.Machine_width.t ->
  Flambda_kind.Standard_int.t ->
  Targetint_32_64.t ->
  Reg_width_const.t

(** How many cases the switch has. (Note that this is not the number of
    destinations reached by the switch, which may be a smaller number.) *)
val num_arms : t -> int
