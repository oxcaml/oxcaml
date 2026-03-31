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

(** A colour directive. Can be passed as an argument to [Format.printf] and
    frients using the "%t" specifier. Each directive (besides [pop]) acts by
    pushing a new state onto a stack, allowing the previous state to be restored
    using [pop]. *)
type directive = Format.formatter -> unit

(** Undo the most recent colour directive, restoring the previous state. Raises
    a fatal error if the stack is empty. *)
val pop : directive

(** Push a copy of the current state onto the stack. Useful when setting a
    colour conditionally so that a following [pop] will always be matched. *)
val none : directive

val terminator : directive

val block_label : directive

val block_label_exn : directive

val instr_id : directive

val pred_succ : directive

val liveness : directive

val function_name : directive

val basic : directive

val without_colours : f:(unit -> 'a) -> 'a
