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
  type 'a known_values_at_uses =
    { known_at_uses : ('a * Reg_width_const.t) list;
      unknown_at_uses : 'a list
    }

  type 'a t

  val empty : 'a t

  val reduce : 'a t -> Typing_env.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val known_values_at_uses :
    Name.t -> 'a t -> 'a known_values_at_uses Or_unknown.t
end

val cut_and_n_way_join :
  ?join_id:Join_id.t ->
  n_way_join_type:n_way_join_type ->
  meet_type:Meet_env.meet_type ->
  cut_after:Scope.t ->
  Meet_env.t ->
  ('a * Typing_env.t) list ->
  Meet_env.t * 'a Join_info.t
