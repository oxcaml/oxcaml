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

type t

val print_name_modes :
  restrict_to:Name.Set.t ->
  min_binding_time:Binding_time.t ->
  Format.formatter ->
  t ->
  unit

val empty : t

val names_to_types :
  t -> (Type_grammar.t * Binding_time.With_name_mode.t) Name.Map.t

(** [find_opt t name] is equivalent to
    [Name.Map.find_opt name (names_to_types t)] but avoids applying any pending
    renaming to the types of names other than [name]. *)
val find_opt :
  t -> Name.t -> (Type_grammar.t * Binding_time.With_name_mode.t) option

(** [name_domain t] returns the set of names in the map returned by
    [names_to_types t]. Unlike calling [Name.Map.keys (names_to_types t)] it
    does not need to apply any pending renaming to the types themselves. *)
val name_domain : t -> Name.Set.t

(** [mem t name] is [true] iff [name] has an entry in [names_to_types t]. It
    avoids applying any pending renaming to the types themselves. *)
val mem : t -> Name.t -> bool

val aliases : t -> Aliases.t

val add_or_replace_binding :
  t -> Name.t -> Type_grammar.t -> Binding_time.t -> Name_mode.t -> t

val replace_variable_binding : t -> Variable.t -> Type_grammar.t -> t

val with_aliases : t -> aliases:Aliases.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val clean_for_export : t -> reachable_names:Name_occurrences.t -> t

val apply_renaming : t -> Renaming.t -> t

include Contains_ids.S with type t := t

val merge : t -> t -> t

val remove_unused_value_slots_and_shortcut_aliases :
  t -> used_value_slots:Value_slot.Set.t -> t

val canonicalise : t -> Simple.t -> Simple.t

val free_function_slots_and_value_slots : t -> Name_occurrences.t
