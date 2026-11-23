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

open! Flambda.Import

(** Unlike [Named.t], this type does not include [Static_consts] because such
    constants are propagated separately after simplification. *)
type simplified_named = private
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t * Alloc_mode.For_allocations.t
  | Rec_info of Rec_info_expr.t

val to_named : simplified_named -> Named.t

type t = private
  { named : simplified_named;
    cost_metrics : Cost_metrics.t;
    free_names : Name_occurrences.t
  }

(** It is an error to pass [Set_of_closures] or [Static_consts] to this
    function. (Sets of closures are disallowed because computation of their free
    names might be expensive; use [reachable_with_known_free_names] instead.) *)
val create : machine_width:Target_system.Machine_width.t -> Named.t -> t

(** It is an error to pass [Static_consts] to this function. *)
val create_with_known_free_names :
  machine_width:Target_system.Machine_width.t ->
  find_code_characteristics:(Code_id.t -> Cost_metrics.code_characteristics) ->
  Named.t ->
  free_names:Name_occurrences.t ->
  t

val print : Format.formatter -> t -> unit

val cost_metrics : t -> Cost_metrics.t

val update_cost_metrics : Cost_metrics.t -> t -> t

(** The result of simplifying the defining expression of a [Let].

    [Rewritten] asks for the binding [let x = e in body] that is being
    simplified to be replaced by
    [let t1 = e1 in ... let tn = en in let x = defining_expr in body], where
    [temporaries] is [(t1, e1); ...; (tn, en)], and for that expression to be
    simplified instead.

    The temporaries must be fresh variables. They are defined as extra variables
    (see [Downwards_env.begin_extra_definitions]), like the extra parameters of
    continuations. Whether a primitive is rewritten, and into what, depends on
    the types of its arguments, so two downwards passes on the same handler may
    disagree on the temporaries: they cannot be part of the [Replay_history],
    nor become parameters of the continuations lifted out of the handler, as a
    pass that does not bind them would have no argument for them. *)
type 'a or_rewritten =
  | Simplified of 'a
  | Rewritten of
      { temporaries : (Bound_var.t * Named.t) list;
        defining_expr : Named.t
      }

type binding_to_place =
  | Keep_binding of
      { let_bound : Bound_pattern.t;
        simplified_defining_expr : t;
        original_defining_expr : Named.t option
      }
  | Delete_binding of { original_defining_expr : Named.t option }
