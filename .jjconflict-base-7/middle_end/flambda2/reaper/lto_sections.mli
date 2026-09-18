(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** The Reaper's link-time optimisation data. A unit compiled with -support-lto
    stores it in its .cmx file as three sections: a header, whose index is
    recorded in [Cmx_format.unit_infos.ui_lto_info]; the unit's inputs to
    -reaper-solve; and the data -reaper-rebuild needs to regenerate the unit.

    The sections contain hashconsed identifiers, which are renamed on import
    using the table of the unit's Flambda export information, so
    [ids_for_export] must be added to that table (see
    [Flambda_cmx.prepare_cmx_file_contents]). *)

(** A unit's LTO data, with the solve inputs pruned for the whole-program solve
    (see [Reaper.Staged.Solve_inputs.prune_for_lto]). *)
type t

val create :
  unit_metadata:Flambda_unit.Metadata.t ->
  imported_offsets:Exported_offsets.t ->
  solve_inputs:Reaper.Staged.Solve_inputs.t ->
  rebuild_inputs:Reaper.Staged.Rebuild_inputs.t ->
  t

val ids_for_export : t -> Ids_for_export.t

(** Add the three sections and return the index of the header. Also records the
    current ID stamp counters, so this must be called after the last identifier
    of the compilation has been created. *)
val to_sections : sections:File_sections.Builder.t -> t -> File_sections.Idx.t

module Header : sig
  type t

  val id_stamp_counters : t -> Id_stamp_counters.t
end

type error =
  | No_lto_info of string
  | Corrupted of string

exception Error of error

(** [filename] is the .cmx file the sections were read from, for error messages.
    Raises [Error (No_lto_info _)] if the index is [None]. *)
val read_header :
  filename:string ->
  sections:File_sections.t ->
  File_sections.Idx.t option ->
  Header.t

(** Import the offsets imported when the unit was compiled and the unit's inputs
    to the solve. [renaming] imports the identifiers of the unit's export
    information, whose table the sections share. *)
val read_for_solve :
  filename:string ->
  sections:File_sections.t ->
  renaming:Renaming.t ->
  Header.t ->
  Exported_offsets.t * Reaper.Staged.Solve_inputs.t

(** Import the unit metadata and the data needed to rebuild the unit, with
    [renaming] as for [read_for_solve]. *)
val read_for_rebuild :
  filename:string ->
  sections:File_sections.t ->
  renaming:Renaming.t ->
  Header.t ->
  Flambda_unit.Metadata.t * Reaper.Staged.Rebuild_inputs.t
