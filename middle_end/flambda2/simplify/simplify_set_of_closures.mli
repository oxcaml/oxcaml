(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Simplification of recursive groups of sets of closures. This process makes
    new, simplified versions of [Code] bindings based on the contextual
    information available from the corresponding set of closures definition.
    ([Code] bindings are not simplified earlier, except in the special case of
    stub functions, because much more information is available at the set of
    closures definitions. Stub functions are simplified once since it is
    unlikely more information will be gained at the set of closures definitions;
    this also avoids potential performance problems in pathological cases. *)

open! Simplify_import

(** Simplify a single, non-lifted set of closures, as may occur on the
    right-hand side of a [Let] binding. *)
val simplify_non_lifted_set_of_closures :
  Downwards_acc.t ->
  Bound_pattern.t ->
  Set_of_closures.t ->
  Alloc_mode.For_allocations.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Simplify_named_result.t

(** Simplify a group of possibly-recursive sets of closures, as may occur on the
    right-hand side of a [Let_symbol] binding. *)
val simplify_lifted_sets_of_closures :
  Downwards_acc.t ->
  all_sets_of_closures_and_symbols:
    (Symbol.t Function_slot.Lmap.t * Set_of_closures.t) list ->
  closure_bound_names_all_sets:Bound_name.t Function_slot.Map.t list ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Bound_static.t * Rebuilt_static_const.Group.t * Downwards_acc.t

(** Simplify code where it is bound, keeping its code ID, rather than at the
    definition of a set of closures (see
    [Simplify_set_of_closures_context.create_for_code_where_bound]). *)
val simplify_code_where_bound :
  Downwards_acc.t ->
  Code.t ->
  all_code:Code.t Code_id.Map.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  template_env_type:T.Typing_env_extension.With_extra_variables.t option ->
  Rebuilt_static_const.t * Downwards_acc.t
