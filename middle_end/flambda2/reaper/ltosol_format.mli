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

(** The header of a solution file, from which the solution of each participant
    can be read. *)
type t

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string
  | Not_a_participant of
      { filename : string;
        compilation_unit : Compilation_unit.t
      }

exception Error of error

(** Store the solutions of the participants of a whole-program solve, each in a
    section of its own with the identifier tables needed to import it. *)
val save :
  filename:string ->
  solutions:(Compilation_unit.t * [`Without_types] Analysis.Solution.t) list ->
  unit

val load : string -> t

val stamp_state : t -> Ids_for_export.Stamp_state.t

val participants : t -> Compilation_unit.Set.t

(** Read and import the solution of a participant, which only holds what its
    rebuild needs. The identifiers it contains are only valid until the
    identifier tables are reset. Raises [Error] if the unit did not take part in
    the solve. *)
val solution_for_unit :
  t -> Compilation_unit.t -> [`Without_types] Analysis.Solution.t
