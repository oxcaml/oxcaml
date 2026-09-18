(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type rewrite_context

(** [rewrite_kind_with_subkind context var kind_with_subkind] For
    [kind_with_subkind] the kind associated to variable [var], removes the
    subkinds on the parts that are not used. *)
val rewrite_kind_with_subkind :
  rewrite_context ->
  Name.t ->
  Flambda_kind.With_subkind.t ->
  Flambda_kind.With_subkind.t
(* CR pchambart: rename to remove_unused_part_of_subkind or something like
   that *)

val prepare_rewrite_context :
  Unboxing_analysis.result ->
  (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list ->
  rewrite_context

val rewrite_typing_env :
  rewrite_context -> unit_symbol:Symbol.t -> typing_env -> typing_env

val rewrite_result_types :
  rewrite_context ->
  old_typing_env:typing_env ->
  my_closure:Variable.t ->
  params:(Variable.t * Points_to_analysis.keep_or_delete) list ->
  results:(Variable.t * Points_to_analysis.keep_or_delete) list ->
  Result_types.t ->
  Result_types.t
