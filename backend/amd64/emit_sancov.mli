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

(** Emission of the coverage probes of [-trace-pc-guard] (see
    [Cfg_trace_pc_guard]). This is the part of the emitter that deals with
    probes; it is only called from [Emit]. *)

(** To be called at the start of each compilation unit. *)
val reset_unit : unit -> unit

(** To be called at the start of each function. *)
val reset_function : unit -> unit

(** Emit the inline part of a probe: a test of the gate and a branch to an
    out-of-line slow path, which is recorded for [emit_slow_paths]. [live] is
    the set of registers live across the probe. No register is clobbered. *)
val emit_probe : live:Reg.Set.t -> dbg:Debuginfo.t -> unit

(** Emit the slow paths of the probes of the current function. A slow path calls
    the callback with the address of the probe's guard, preserving every
    register.

    [move] emits a move between a hardware register and an [Outgoing] stack
    slot. [emit_call] emits a call to a C function. *)
val emit_slow_paths :
  move:(Reg.t -> Reg.t -> unit) -> emit_call:(Cmm.symbol -> unit) -> unit

(** Emit the guards of the probes of the current unit and the definition of the
    gate, if the unit has any probe. [add_def_symbol] is called on the name of
    the gate. *)
val emit_data : add_def_symbol:(string -> unit) -> unit
