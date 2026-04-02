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

(** Edge coverage instrumentation in the style of clang's
    [-fsanitize-coverage=trace-pc-guard] with gated callbacks (enabled by
    [-trace-pc-guard]).

    Critical edges are split, and then probes are added at the start of blocks.
    Each probe tests the global [__sancov_should_track] and, if it is non-zero,
    calls [__sanitizer_cov_trace_pc_guard(&guard)], where [guard] is a distinct
    32-bit slot of a per-unit array that the emitter places in the
    [ocaml_trace_pc_guards] section. The linker concatenates the arrays of all
    units and provides [__start_ocaml_trace_pc_guards] and
    [__stop_ocaml_trace_pc_guards], which the runtime uses to call
    [__sanitizer_cov_trace_pc_guard_init].

    As with clang, every instrumented unit defines [__sancov_should_track] as a
    weak, hidden, zero-initialised 64-bit global in the [__sancov_gate] section,
    so that probes can address it PC-relatively. A strong definition (e.g. in
    the coverage runtime) takes precedence.

    A probe is the target-specific operation [Arch.trace_pc_guard_operation].
    The emitter places the call out of line, and it preserves every register.

    See https://clang.llvm.org/docs/SanitizerCoverage.html *)

(** Instrument a function. Must run before register allocation. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t

(** The name of the gate, [__sancov_should_track]. *)
val gate : string

(** The name of the callback, [__sanitizer_cov_trace_pc_guard]. *)
val callback : string
