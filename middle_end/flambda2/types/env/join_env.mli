(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type env_id

type 'a join_arg = env_id * 'a

type t

type n_way_join_type =
  t -> Type_grammar.t join_arg list -> Type_grammar.t Or_unknown.t * t

val joined_env : t -> env_id -> Typing_env.t

val machine_width : t -> Target_system.Machine_width.t

val code_age_relation : t -> Code_age_relation.t

val code_age_relation_resolver :
  t -> Compilation_unit.t -> Code_age_relation.t option

val n_way_join_simples :
  t -> Flambda_kind.t -> Simple.t join_arg list -> Simple.t Or_bottom.t * t

val n_way_join_env_extension :
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  t ->
  Typing_env_extension.t join_arg list ->
  (Typing_env_extension.t * t) Or_bottom.t

val cut_and_n_way_join :
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  cut_after:Scope.t ->
  Meet_env.t ->
  Typing_env.t list ->
  Meet_env.t

module Analysis : sig
  type 'a t

  val print : Format.formatter -> 'a t -> unit

  module Variable_refined_at_join : sig
    type 'a t

    val fold_values_at_uses :
      ('a -> Reg_width_const.t Or_unknown.t -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

  type 'a simple_refined_at_join =
    | Not_refined_at_join
    | Invariant_in_all_uses of Simple.t
    | Variable_refined_at_these_uses of 'a Variable_refined_at_join.t

  val simple_refined_at_join :
    'a t -> Typing_env.t -> Simple.t -> 'a simple_refined_at_join
end

val cut_and_n_way_join_with_analysis :
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  cut_after:Scope.t ->
  Typing_env.t ->
  ('a * Typing_env.t) list ->
  Typing_env.t * 'a Analysis.t
