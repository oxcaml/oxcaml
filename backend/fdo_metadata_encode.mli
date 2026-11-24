(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

(** The FDO metadata section of a compilation unit: for each function, the
    addresses of the instructions that carry FDO counters, with what executing
    them means, so that a profile decoder can attribute the branches it samples
    to counters.

    Each entry has a complete counter, with its inlining context within the
    function. Sharing inlining contexts with the previous entry is purely an
    encoding: decoding a sample does not depend on earlier branch outcomes. *)

(** What Emit reports about the emitted code, at a label: an instrumented
    instruction or an exception handler. *)
module Event : sig
  type t =
    | Conditional_branch of
        { taken : Fdo_annotation.t;
          fallthrough : Fdo_annotation.t
        }
    | Call of
        { return_address : Asm_targets.Asm_label.t;
          counter : Fdo_counter.t
        }
    | Tail_call of Fdo_counter.t
    | Indirect_jump of
        { target : Asm_targets.Asm_label.t;
          taken : Fdo_annotation.t
        }
    | Exception_handler
end

(** The metadata of one compilation unit, accumulated function by function. *)
type t

(** [record_names] also records the canonical names behind the hashes, for
    readable decoder output. *)
val create : record_names:bool -> t

val record_body :
  t ->
  function_:Fdo_counter.Function.t ->
  body_hash:Fdo_counter.Body_hash.t ->
  unit

(** The metadata of one function, from [begin_function] to [end_function]. *)
type function_metadata

val begin_function :
  t ->
  start:Asm_targets.Asm_label.t ->
  entry_annotation:Fdo_annotation.t ->
  function_metadata

val record_event :
  function_metadata -> Asm_targets.Asm_label.t -> Event.t -> unit

val end_function : function_metadata -> finish:Asm_targets.Asm_label.t -> unit

(** Record the counters dropped in the unit, with the reason. They have no
    address: this is for assessing what dropping them loses. *)
val set_dropped : t -> Fdo_dropped_counters.t -> unit

(** Emit the unit's ".debug_fdo_metadata" section. Nothing is emitted when
    nothing was recorded. *)
val emit_section : t -> unit
