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

(** Capture of the emitted assembly for [%%expect_asm] tests. Each emitter
    formats the code of its functions; this module collects the results and
    hands them to the tests. *)

(** Register a callback to receive the recorded code, for expect tests. The
    callback is automatically cleared after being invoked once. *)
val register_callback : (string -> unit) -> unit

(** When set, the emitters record each function past its hot body to the end
    of the function, including the trailing out-of-line code (GC jump pads,
    safety-error calls, the stack-realloc handler). Used by the
    [%%expect_asm_full] variant. *)
val whole_function : bool ref

(** Whether a test is waiting for the assembly, i.e. whether the emitter
    should capture it. *)
val enabled : unit -> bool

(** Record the code of the function [fun_name], as formatted by [format]
    under the name given to it, unless no test is waiting for it or the
    function is not written by the user (such as currying helpers). Tabs in
    the formatted code are expanded. *)
val record :
  fun_name:string ->
  fun_dbg:Debuginfo.t ->
  format:(name:string -> string) ->
  unit

(** Pass the code recorded since the last call to the registered callbacks,
    then clear them. *)
val invoke_callbacks : unit -> unit
