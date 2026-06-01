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

val n_way_join_simples :
  t -> Flambda_kind.t -> Simple.t join_arg list -> Simple.t Or_bottom.t * t

val n_way_join_env_extension :
  n_way_join_type:n_way_join_type ->
  meet_expanded_head:Meet_env.meet_expanded_head ->
  t ->
  Typing_env_extension.t join_arg list ->
  (Typing_env_extension.t * t) Or_bottom.t

val cut_and_n_way_join :
  n_way_join_type:n_way_join_type ->
  meet_expanded_head:Meet_env.meet_expanded_head ->
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

  module Simples_at_join : sig
    type 'a t

    type definition_at_use = At_normal_mode of Simple.t [@@unboxed]

    val fold_definitions_at_uses :
      ('a -> definition_at_use -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

  val fold_variables_created_at_join :
    f:(Name.t -> 'a Simples_at_join.t -> Flambda_kind.t -> 'b -> 'b) ->
    'a t ->
    init:'b ->
    'b
end

val cut_and_n_way_join_with_analysis :
  n_way_join_type:n_way_join_type ->
  meet_expanded_head:Meet_env.meet_expanded_head ->
  cut_after:Scope.t ->
  Typing_env.t ->
  ('a * Typing_env.t) list ->
  Typing_env.t * 'a Analysis.t
