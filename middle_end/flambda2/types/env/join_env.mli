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

module Join_info : sig
  (* At any other use, the value is not defined (i.e. [Poison]). *)
  type known_values_at_uses =
    { known_at_uses : Reg_width_const.t Apply_cont_rewrite_id.Map.t;
      unknown_at_uses : Simple.t Apply_cont_rewrite_id.Map.t
    }

  type t

  val empty : t

  val reduce : t -> Typing_env.t -> t

  val print : Format.formatter -> t -> unit

  val known_values_at_uses : Name.t -> t -> known_values_at_uses Or_unknown.t
end

val cut_and_n_way_join :
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  cut_after:Scope.t ->
  Meet_env.t ->
  Typing_env.t list ->
  Meet_env.t

val cut_and_n_way_join_with_use_ids :
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  cut_after:Scope.t ->
  Meet_env.t ->
  (Apply_cont_rewrite_id.t * Typing_env.t) list ->
  Meet_env.t * Join_info.t
