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

(** [cmx_path] is where the .cmx file was read from, relative to the manifest
    files root, when that location is to be recorded on the identifiers of its
    units for the benefit of the users of the current unit's .cmx file (see
    [Compilation_unit.Found]). *)
val from_raw :
  sections:File_sections.t -> cmx_path:Misc.filepath option -> raw -> t

val create_raw :
  final_typing_env:Flambda2_types.Typing_env.Serializable.t ->
  all_code:Exported_code.t ->
  exported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  sections:File_sections.Builder.t ->
  raw

val import_typing_env_and_code :
  t -> Flambda2_types.Typing_env.Serializable.t * Exported_code.t

val exported_offsets : t -> Exported_offsets.t

val with_exported_offsets : t -> Exported_offsets.t -> t

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
