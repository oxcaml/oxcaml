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

(** Instrumentation for [%%expect_ssa] tests (see
    oxcaml/testsuite/tools/expectcommon.ml), the SSA counterpart of
    [Emit.register_expect_asm_callback]. While a callback is registered,
    [Asmgen] records the SSA graph of every function as it leaves the SSA
    pipeline; at the end of the compilation unit the recorded text is handed to
    the callbacks, which are then cleared. *)

(** Register a callback to receive the recorded SSA of the next compilation
    unit. Registering the first callback discards anything recorded earlier. *)
val register_callback : (string -> unit) -> unit

(** Record the final SSA of a function, if a callback is registered. [summary]
    is one line per SSA optimization pass saying what it did to the function; it
    is printed before the graph. Module entry functions and generic functions
    (caml_apply, caml_curry, ...) are not recorded. *)
val record : summary:string list -> Ssa.finished Ssa.graph -> unit

(** Hand the recorded SSA of the compilation unit to the callbacks and clear
    them. *)
val invoke_callbacks : unit -> unit
