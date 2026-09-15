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

(** Contents of middle-end-specific portion of .cmx files when using Flambda. *)

type t

type raw

val from_raw : sections:File_sections.t -> raw -> t

(** [extra_ids_for_lto] are the identifiers of the unit's LTO sections (see
    [Flambda2_reaper.Lto_sections]), which share this unit's table. *)
val create_raw :
  final_typing_env:Flambda2_types.Typing_env.Serializable.t option ->
  all_code:Exported_code.t ->
  exported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  extra_ids_for_lto:Ids_for_export.t ->
  sections:File_sections.Builder.t ->
  raw

val import_typing_env_and_code :
  t -> Flambda2_types.Typing_env.Serializable.t option * Exported_code.t

val exported_offsets : t -> Exported_offsets.t

val with_exported_offsets : t -> Exported_offsets.t -> t

(** The renaming that imports the unit's export information, for importing the
    unit's LTO sections, which share its identifier table. Only for units that
    are not packs. *)
val import_renaming_of_unit : t -> Renaming.t

(** Create the Flambda data for a pack *)
val pack : sections:File_sections.Builder.t -> t option list -> raw option

(** For ocamlobjinfo *)
val print :
  print_typing_env:bool ->
  print_code:bool ->
  print_offsets:bool ->
  Format.formatter ->
  t ->
  unit

(* [create_table_data] and [import_renaming] are exposed for the sections of
   .ltosol files, which carry their own tables. *)

(** The exported forms of hashconsed identifiers in the marshalled data. *)
type table_data

val create_table_data : Ids_for_export.t -> table_data

val import_renaming :
  table_data:table_data ->
  used_value_slots:Value_slot.Set.t ->
  original_compilation_unit:Compilation_unit.t ->
  Renaming.t * Code_id.importer
