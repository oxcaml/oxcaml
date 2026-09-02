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

type result = Unboxing_analysis.result

val fixpoint : Global_flow_graph.graph -> result

val get_unboxed_fields :
  result -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  result -> Code_id_or_name.t -> Unboxing_analysis.changed_representation option

val has_use : result -> Code_id_or_name.t -> bool

(** The code ids and symbols defined in [compilation_unit] that the solution
    considers used at runtime. Under a whole-program solution, a rebuilt unit's
    statics can lose all references from within their own unit (e.g. when a
    module block field is poisoned) while still being referenced by other units,
    so they must be treated as roots when computing which names are reachable
    for emission. *)
val used_code_ids_and_symbols_in_unit :
  result -> compilation_unit:Compilation_unit.t -> Name_occurrences.t

(** The value and function slots owned by [compilation_unit] whose contents the
    solution considers read somewhere in the program. Under a whole-program
    solution, a unit's rebuilt code can lose all of its own projections from a
    slot (e.g. when the only reads live in copies inlined into other units)
    while the slot is still read elsewhere; slot offset finalisation must not
    mark such slots as dead. *)
val slots_used_in_unit :
  result ->
  compilation_unit:Compilation_unit.t ->
  Value_slot.Set.t * Function_slot.Set.t

val any_usage : result -> Code_id_or_name.t -> bool

val has_source : result -> Code_id_or_name.t -> bool

val any_source : result -> Code_id_or_name.t -> bool

val field_used : result -> Code_id_or_name.t -> Field.t -> bool

val not_local_field_has_source : result -> Code_id_or_name.t -> Field.t -> bool

val cannot_change_calling_convention : result -> Code_id.t -> bool

val code_id_actually_directly_called :
  result -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  result ->
  Code_id_or_name.t ->
  'a list ->
  ('a * Points_to_analysis.keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  result ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * Points_to_analysis.keep_or_delete) list list
