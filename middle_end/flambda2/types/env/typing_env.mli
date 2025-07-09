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

type typing_env

type t = typing_env

module Pre_serializable : sig
  type t

  val create :
    typing_env ->
    used_value_slots:Value_slot.Set.t ->
    t * (Simple.t -> Simple.t)

  val find_or_missing : t -> Name.t -> Type_grammar.t option
end

module Serializable : sig
  type t

  val create : Pre_serializable.t -> reachable_names:Name_occurrences.t -> t

  val create_from_closure_conversion_approx :
    'a Value_approximation.t Symbol.Map.t -> t

  val predefined_exceptions : Symbol.Set.t -> t

  val free_function_slots_and_value_slots : t -> Name_occurrences.t

  val print : Format.formatter -> t -> unit

  val name_domain : t -> Name.Set.t

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  val merge : t -> t -> t

  val extract_symbol_approx :
    t -> Symbol.t -> (Code_id.t -> 'code) -> 'code Value_approximation.t
end

module Join_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> left_env:typing_env -> right_env:typing_env -> t

  val target_join_env : t -> typing_env

  val left_join_env : t -> typing_env

  val right_join_env : t -> typing_env

  type now_joining_result = private
    | Continue of t
    | Stop

  val now_joining : t -> Simple.t -> Simple.t -> now_joining_result

  val already_joining : t -> Simple.t -> Simple.t -> bool
end

val print : Format.formatter -> t -> unit

val create :
  resolver:(Compilation_unit.t -> Serializable.t option) ->
  get_imported_names:(unit -> Name.Set.t) ->
  t

val is_bottom : t -> bool

val make_bottom : t -> t

val closure_env : t -> t

val resolver : t -> Compilation_unit.t -> Serializable.t option

val code_age_relation_resolver :
  t -> Compilation_unit.t -> Code_age_relation.t option

val current_scope : t -> Scope.t

val increment_scope : t -> t

val add_variable_definition :
  t -> Variable.t -> Flambda_kind.t -> Name_mode.t -> t

val add_definition : t -> Bound_name.t -> Flambda_kind.t -> t

val replace_equation : t -> Name.t -> Type_grammar.t -> t

type add_alias_result =
  { canonical_element : Simple.t;
    demoted_name : Name.t;
    t : t
  }

val add_alias :
  t ->
  canonical_element1:Simple.t ->
  canonical_element2:Simple.t ->
  add_alias_result Or_unknown_or_bottom.t

val add_definitions_of_params : t -> params:Bound_parameters.t -> t

val add_symbol_definition : t -> Symbol.t -> t

val add_symbol_definitions : t -> Symbol.Set.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

(** If the kind of the name is known, it should be specified, otherwise it can
    be omitted. Such omission will cause an error if the name satisfies
    [variable_is_from_missing_cmx_file]. *)
val find : t -> Name.t -> Flambda_kind.t option -> Type_grammar.t

val find_or_missing : t -> Name.t -> Type_grammar.t option

val find_params : t -> Bound_parameters.t -> Type_grammar.t list

val variable_is_from_missing_cmx_file : t -> Name.t -> bool

val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

val alias_is_bound_strictly_earlier :
  t -> bound_name:Name.t -> alias:Simple.t -> bool

(** [stable_compare_simples t simple1 simple2] is a total extension of the
    binding time order that does not depend on [Int_ids] hashing. *)
val stable_compare_simples : t -> Simple.t -> Simple.t -> int

val type_simple_in_term_exn :
  t -> ?min_name_mode:Name_mode.t -> Simple.t -> Type_grammar.t * Simple.t

val get_canonical_simple_ignoring_name_mode : t -> Simple.t -> Simple.t

(** [name_mode_of_existing_simple] can be provided to improve performance of
    this function. *)
val get_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Simple.t ->
  Simple.t

val get_alias_then_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Type_grammar.t ->
  Simple.t

val aliases_of_simple :
  t -> min_name_mode:Name_mode.t -> Simple.t -> Aliases.Alias_set.t

val aliases_of_simple_allowable_in_types : t -> Simple.t -> Aliases.Alias_set.t

val add_to_code_age_relation :
  t -> new_code_id:Code_id.t -> old_code_id:Code_id.t option -> t

val code_age_relation : t -> Code_age_relation.t

val with_code_age_relation : t -> Code_age_relation.t -> t

val cut : t -> cut_after:Scope.t -> Typing_env_level.t

val cut_as_extension : t -> cut_after:Scope.t -> Typing_env_extension.t

val free_names_transitive : t -> Type_grammar.t -> Name_occurrences.t

val bump_current_level_scope : t -> t

val compute_joined_aliases : t -> Name.Set.t -> t list -> t
