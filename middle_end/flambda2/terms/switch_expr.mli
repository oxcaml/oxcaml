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
    cases. Switches always have at least two cases. *)

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create :
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  arms:Apply_cont_expr.t Target_ocaml_int.Map.t ->
  t

(** Create a [Switch] corresponding to a traditional if-then-else. *)
val if_then_else :
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  if_true:Apply_cont_expr.t ->
  if_false:Apply_cont_expr.t ->
  t

(** The scrutinee of the switch. *)
val scrutinee : t -> Simple.t

(** The debuginfo to be used for the condition. *)
val condition_dbg : t -> Debuginfo.t

(** Call the given function [f] on each (discriminant, action) pair in the
    switch. *)
val iter : t -> f:(Target_ocaml_int.t -> Apply_cont_expr.t -> unit) -> unit

(** What the switch will do for each possible value of the discriminant. *)
val arms : t -> Apply_cont_expr.t Target_ocaml_int.Map.t

(** How many cases the switch has. (Note that this is not the number of
    destinations reached by the switch, which may be a smaller number.) *)
val num_arms : t -> int
