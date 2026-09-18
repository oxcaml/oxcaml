(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Upwards environments used during simplification. *)

type t

(** Create an upwards environment.

    The [are_rebuilding_terms] provided is only used for printing. *)
val create :
  Are_rebuilding_terms.t -> machine_width:Target_system.Machine_width.t -> t

val machine_width : t -> Target_system.Machine_width.t

val print : Format.formatter -> t -> unit

val add_non_inlinable_continuation :
  Are_rebuilding_terms.t ->
  t ->
  Continuation.t ->
  params:Bound_parameters.t ->
  handler:
    (Rebuilt_expr.t
    * is_exn_handler:bool
    * free_names_without_params:Name_occurrences.t)
    Or_unknown.t ->
  t

val add_invalid_continuation :
  t -> Continuation.t -> [`Unarized] Flambda_arity.t -> t

val add_continuation_shortcut :
  t ->
  Continuation.t ->
  params:Bound_parameters.t ->
  shortcut_to:Continuation.t ->
  args:Simple.t list ->
  t

val add_linearly_used_inlinable_continuation :
  t ->
  Continuation.t ->
  params:Bound_parameters.t ->
  handler:Rebuilt_expr.t ->
  free_names_of_handler:Name_occurrences.t ->
  cost_metrics_of_handler:Cost_metrics.t ->
  t

val add_function_return_or_exn_continuation :
  t -> Continuation.t -> [`Unarized] Flambda_arity.t -> t

val find_continuation : t -> Continuation.t -> Continuation_in_env.t

val mem_continuation : t -> Continuation.t -> bool

val find_continuation_shortcut :
  t -> Continuation.t -> Continuation_shortcut.t option

(* [find_unique_continuation_handler] searches for an existing continuation with
   the same handler contents (up to permutation of the continuation parameters)
   as [handler].

   If it finds any, it returns a continuation [k] and arguments [args] such that
   [cont params -> handler] and [cont params -> k (args)] are equivalent as
   continuation handlers.

   [k] is allowed to be an exception handler iff the provided [is_exn_handler]
   is [true], and if it is, the first argument of both handlers must match. *)
val find_unique_continuation_handler :
  Are_rebuilding_terms.t ->
  t ->
  params:Bound_parameters.t ->
  handler:Rebuilt_expr.t ->
  is_exn_handler:bool ->
  free_names_without_params:Name_occurrences.t ->
  (Continuation.t * Simple.t list) option

val add_apply_cont_rewrite : t -> Continuation.t -> Apply_cont_rewrite.t -> t

val replace_apply_cont_rewrite :
  t -> Continuation.t -> Apply_cont_rewrite.t -> t

val find_apply_cont_rewrite : t -> Continuation.t -> Apply_cont_rewrite.t option
