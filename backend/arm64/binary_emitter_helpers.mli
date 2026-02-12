(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(** Hooks for connecting the ARM64 binary emitter to the emit process. This
    module handles JIT compilation and binary section saving for verification.

    CR mshinwell: Adapt the binary sections saving to x86 in due course. *)

(** Returns true if the binary emitter should be enabled for this compilation
    (either for JIT or for saving binary sections for verification). *)
val should_use_binary_emitter : unit -> bool

(** Initialize the binary emitter if needed. Call this at the start of assembly.
    Returns a directive emission callback that should be passed to the
    Asm_directives initialization. *)
val begin_emission : unit -> Asm_targets.Asm_directives.Directive.t -> unit

(** Finalize the binary emitter if it was enabled. This handles section
    aggregation, file saving for verification, and JIT hook invocation. Call
    this at the end of assembly. *)
val end_emission : unit -> unit
