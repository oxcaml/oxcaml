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

open Asm_targets

(** Emission of SFrame (Simple Frame format) stack trace information.

    SFrame is a simple stack unwinding format that tracks the Canonical Frame
    Address (CFA), Frame Pointer (FP), and Return Address (RA) for each
    function. See
    {{:https://sourceware.org/binutils/docs-2.45/sframe-spec.html} the SFrame
     specification}.

    This module collects frame state changes during code emission and emits a
    [.sframe] section at the end of the compilation unit. *)

(** The base register used to compute the Canonical Frame Address (CFA). *)
type cfa_base_reg =
  | SP
  | FP

(** A single Frame Row Entry, recording the frame state at a specific PC. *)
type fre =
  { fre_label : Asm_label.t;
        (** Label in the .text section at the PC where this FRE applies. *)
    cfa_base : cfa_base_reg;  (** Base register for CFA computation. *)
    cfa_offset : int;  (** CFA = cfa_base + cfa_offset *)
    ra_offset : int option;
        (** RA save location relative to CFA. [None] if RA is at the fixed
            offset given in the header (e.g., AMD64 where RA = CFA - 8). *)
    fp_offset : int option
        (** FP save location relative to CFA. [None] if FP is not saved or is at
            the fixed offset given in the header. *)
  }

(** Opaque environment for collecting SFrame data. *)
type t

(** Create a fresh SFrame environment. *)
val create : unit -> t

(** Begin recording a new function.
    @param fun_symbol The function's global symbol.
    @param fun_start_label A label at the function entry point (in .text).
    @param contains_calls Whether the function makes non-tail calls. *)
val new_function :
  t ->
  fun_symbol:Asm_symbol.t ->
  fun_start_label:Asm_label.t ->
  contains_calls:bool ->
  unit

(** Record a Frame Row Entry (frame state change). *)
val add_fre : t -> fre -> unit

(** End recording the current function.
    @param fun_end_label A label at the end of the function (in .text). *)
val end_function : t -> fun_end_label:Asm_label.t -> unit

(** Emit the [.sframe] section. Should be called at the end of the compilation
    unit (from [end_assembly]). *)
val emit : t -> unit
