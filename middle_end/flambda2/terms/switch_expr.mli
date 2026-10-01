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

    Scrutinees of [Switch]es are of kind [Naked_immediate]. There are no default
    cases. Switches always have at least two cases.

    Switch arms are annotated with a (static) frequency information, encoded as
    a [Likelihood.t]. An arm is expected to be selected at a frequency
    proportional to its likelihood. *)

type arm

val create_arm_with_likelihood :
  likelihood:Likelihood.t -> Apply_cont_expr.t -> arm

val create_arm : ?weight:float -> Apply_cont_expr.t -> arm

(** A cold arm is an arm with zero weight (expected to be taken a minuscule
    amount of time compared to the other arms). *)
val create_arm_cold : Apply_cont_expr.t -> arm

val arm_action : arm -> Apply_cont_expr.t

val arm_likelihood : arm -> Likelihood.t

val arm_is_cold : arm -> bool

val map_arm_action : (Apply_cont_expr.t -> Apply_cont_expr.t) -> arm -> arm

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create :
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  arms:arm Target_ocaml_int.Map.t ->
  t

(** Create a [Switch] corresponding to a traditional if-then-else. *)
val if_then_else :
  machine_width:Target_system.Machine_width.t ->
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  if_true:arm ->
  if_false:arm ->
  t

(** The scrutinee of the switch. *)
val scrutinee : t -> Simple.t

(** The debuginfo to be used for the condition. *)
val condition_dbg : t -> Debuginfo.t

(** Call the given function [f] on each (discriminant, action) pair in the
    switch. *)
val iter : t -> f:(Target_ocaml_int.t -> arm -> unit) -> unit

(** What the switch will do for each possible value of the discriminant. *)
val arms : t -> arm Target_ocaml_int.Map.t

(** How many cases the switch has. (Note that this is not the number of
    destinations reached by the switch, which may be a smaller number.) *)
val num_arms : t -> int
