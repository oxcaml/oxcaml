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

(** Emission of the coverage probes of [-gated-trace-pc-guard] (see
    [Cfg_trace_pc_guard]). This is the part of the emitter that deals with
    probes; it is only called from [Emit]. *)

(** To be called at the start of each compilation unit. *)
val reset_unit : unit -> unit

(** To be called at the start of each function. *)
val reset_function : unit -> unit

(** Emit the load of the gate into [res] ([Trace_pc_guard_gate]). *)
val emit_gate : res:Reg.t -> unit

(** Emit a call to the callback, with the address of the guard of the probe
    numbered [index] in the current function ([Trace_pc_guard]). Every register
    is preserved; [live] is the set of registers live across the call. [move]
    emits a move between a hardware register and an [Outgoing] stack slot.

    [Emit] emits each instruction several times, the first ones to measure it;
    all share one guard. *)
val emit_call :
  live:Reg.Set.t -> move:(Reg.t -> Reg.t -> unit) -> index:int -> unit

(** Emit the guards of the probes of the current unit, the definition of the
    gate and the constructor that registers the guards, if the unit has any
    probe. *)
val emit_data : unit -> unit
