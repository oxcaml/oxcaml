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

    The header holds the tables of the hashconsed identifiers of the other
    sections, which are renamed on import. *)

(** A unit's LTO data, with the solve inputs pruned for the whole-program solve
    (see [Reaper.For_lto.Solve_inputs.prune_for_lto]). *)
type t

(** [used_value_slots] are as for the unit's Flambda export information. *)
val create :
  imported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  problem:[`Without_types] Traverse.Problem.t ->
  skeleton:Traverse.Skeleton.t ->
  t

(** Add the three sections and return the index of the header. Also records the
    current ID stamp counters, so this must be called after the last identifier
    of the compilation has been created. *)
val to_sections : sections:File_sections.Builder.t -> t -> File_sections.Idx.t

(** Snapshots of the stamp counters used to create identifiers. Identifiers are
    compared by compilation unit and stamp only, so a resuming process must not
    mint stamps that collide with the imported ones. *)
module Id_stamp_counters : sig
  type t

  (** Capture the current values of all stamp counters for serialisation. *)
  val save : unit -> t

  (** Restore all stamp counters to the values they had when [save] was called
      in the process that serialised this value. This can only be called once,
      before any stamps have been created, and will error otherwise. *)
  val restore_for_resume : t -> unit

  (** Restore the stamp counters in preparation for merging the data of several
      units, setting each counter to its maximum value across all of the units.
      Like [restore_for_resume], can only be called once. *)
  val restore_for_merge : t list -> unit

  (** True if any counter in the first set is greater than its corresponding
      counter in the second. Used to guard against monotonicity breaking. *)
  val any_greater_than : t -> t -> bool
end

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
    to the solve. *)
val read_for_solve :
  filename:string ->
  sections:File_sections.t ->
  Header.t ->
  Exported_offsets.t * [`Without_types] Traverse.Problem.t

(** Import the data needed to rebuild the unit. *)
val read_for_rebuild :
  filename:string -> sections:File_sections.t -> Header.t -> Traverse.Skeleton.t
