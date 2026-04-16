(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type t

val create : Downwards_acc.t -> Expr_builder.binding_to_place -> t

val create_empty : Downwards_acc.t -> t

val create_have_lifted_set_of_closures :
  Downwards_acc.t ->
  find_code_characteristics:(Code_id.t -> Cost_metrics.code_characteristics) ->
  (Bound_var.t * Symbol.t) list ->
  original_defining_expr:Set_of_closures.t ->
  t

val dacc : t -> Downwards_acc.t

val bindings_to_place : t -> Expr_builder.binding_to_place list

val no_bindings : t -> bool

val was_lifted_set_of_closures : t -> Cost_metrics.t option

val with_dacc : dacc:Downwards_acc.t -> t -> t
