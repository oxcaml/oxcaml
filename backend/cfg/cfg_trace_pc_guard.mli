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
    [-fsanitize-coverage=trace-pc-guard] with gated callbacks, i.e. with
    [-mllvm -sanitizer-coverage-gated-trace-callbacks] (enabled by
    [-gated-trace-pc-guard]). Clang's default form, where probes call the
    callback unconditionally, is not supported.

    Critical edges are split, and then probes are added at the start of blocks.
    Each probe tests the global [__sancov_should_track] and, if it is non-zero,
    calls [__sanitizer_cov_trace_pc_guard(&guard)], where [guard] is a distinct
    32-bit slot of a per-unit array that the emitter places in the
    [__sancov_guards] section. The linker concatenates the arrays of all units,
    including those of C code instrumented by clang, and provides
    [__start___sancov_guards] and [__stop___sancov_guards]. As with clang, every
    instrumented unit also has a constructor that passes these bounds to
    [__sanitizer_cov_trace_pc_guard_init]; it is in a COMDAT group, so that it
    runs once per executable or shared object. Only ELF targets are supported.

    As with clang, every instrumented unit defines [__sancov_should_track] as a
    weak, hidden, zero-initialised 64-bit global in the [__sancov_gate] section,
    so that probes can address it PC-relatively. A strong definition (e.g. in
    the coverage runtime) takes precedence.

    A probe is made of two operations, implemented for amd64 and arm64:
    [Trace_pc_guard_gate], a load of the gate, which is then tested, and
    [Trace_pc_guard], a call to the callback in a block of its own, marked as
    cold. The call is an ordinary instruction that carries the debuginfo of the
    probe, and it preserves every register (see [Emit_sancov]). The callback is
    called like an external function that does not allocate: it must not call
    into the OCaml runtime.

    With [-g], the flag also enables the DWARF description of inlined frames (as
    [-gdwarf-inlined-frames] does, unless [-gno-dwarf-inlined-frames] is given),
    so that coverage tools can attribute a probe in inlined code to the function
    that the code comes from.

    See https://clang.llvm.org/docs/SanitizerCoverage.html *)

(** Instrument a function. Must run before register allocation. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t

(** The name of the gate, [__sancov_should_track]. *)
val gate : string

(** The name of the callback, [__sanitizer_cov_trace_pc_guard]. *)
val callback : string

(** The name of the function that the constructor calls,
    [__sanitizer_cov_trace_pc_guard_init]. *)
val init : string

(** The bounds of the guards, as defined by the linker. *)
val guards_start : string

val guards_stop : string

(** The name of the constructor and of its COMDAT group, as chosen by clang. *)
val constructor : string
