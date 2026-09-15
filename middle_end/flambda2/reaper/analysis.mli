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

(** What the rebuild needs to know about the solved analysis. *)
type 'f solution

val solve :
  'f Traverse.Problem.t -> analysis_scope:Analysis_scope.t -> 'f solution

(** What the rebuild consumes from a solution, in a form that can be stored: the
    answers to its queries, the code changes and the solved slot offsets. A
    whole-program solution is stored in one part per compilation unit. *)
module Rebuild_data : sig
  type t

  (** The data about units that did not take part in the solve. *)
  val empty : t

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  (** Partition by the compilation unit owning each identifier and slot. *)
  val partition_by_compilation_unit : t -> t Compilation_unit.Map.t
end

(** Combine the problems of the units of a whole program and solve them
    together. [analysis_scope] is the set of these units. Mutates the graphs of
    the problems. *)
val solve_whole_program :
  [`Without_types] Traverse.Problem.t list ->
  analysis_scope:Analysis_scope.t ->
  Rebuild_data.t

(** A whole-program solution read back from storage: the data about the
    identifiers and slots of a compilation unit comes from [data_for_unit],
    called on demand. *)
val sharded :
  analysis_scope:Analysis_scope.t ->
  (Compilation_unit.t -> Rebuild_data.t) ->
  [`Without_types] solution

val get_unboxed_fields :
  'f solution -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  'f solution ->
  Code_id_or_name.t ->
  Unboxing_analysis.changed_representation option

val has_use : 'f solution -> Code_id_or_name.t -> bool

val has_source : 'f solution -> Code_id_or_name.t -> bool

val field_used : 'f solution -> Code_id_or_name.t -> Field.t -> bool

(* The call queries below expect the corresponding application to have been
   recorded during traversal. *)
val code_id_actually_directly_called :
  'f solution -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  'f solution ->
  Code_id_or_name.t ->
  'a list ->
  ('a * Points_to_analysis.keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  'f solution ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * Points_to_analysis.keep_or_delete) list list

val get_calling_convention_change :
  'f solution -> Code_id.t -> Unboxing_analysis.calling_convention_change

val is_changing_calling_convention : 'f solution -> Code_id.t -> bool

(* Returns [None] for code ids of units that did not participate in the
   solve. *)
val find_code_metadata : 'f solution -> Code_id.t -> Code_metadata.t option

(** The solved offsets and used value slots, provided by the solve of a unit
    compiled with its typing information. The offsets of a whole-program
    solution are obtained with [offsets_for_free_names]. *)
val slot_offsets :
  'f solution -> ('f, Slot_offsets.result) Traverse.With_types.t

(** The solved offsets of the slots occurring in the given free names. *)
val offsets_for_free_names :
  'f solution -> Name_occurrences.t -> Exported_offsets.t

val rewrite_kind_with_subkind :
  'f solution ->
  Name.t ->
  Flambda_kind.With_subkind.t ->
  Flambda_kind.With_subkind.t

val final_typing_env : 'f solution -> ('f, typing_env) Traverse.With_types.t
