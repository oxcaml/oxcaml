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
    probes; it is only called from [Emit].

    [Emit] emits each function twice, the first time to measure it, so this
    module keeps no state for the current function: [Emit] records the slow
    paths returned by [emit_probe] and passes them to [emit_slow_path]. *)

(** The out-of-line part of a probe. *)
type slow_path

(** To be called at the start of each compilation unit. *)
val reset_unit : unit -> unit

(** Emit the inline part of a probe: a test of the gate and a branch to the
    returned slow path. [live] is the set of registers live across the probe. No
    register is clobbered, except the scratch registers of the emitter. *)
val emit_probe : live:Reg.Set.t -> dbg:Debuginfo.t -> slow_path

(** A guard for [emit_slow_path]. If [register] holds, the guard is added to
    those of the current unit, for [emit_data]. It must not hold when measuring
    code. *)
val new_guard : register:bool -> Asm_targets.Asm_label.t

(** Emit a slow path, which calls the callback with the address of [guard],
    preserving every register. [move] emits a move between a hardware register
    and an [Outgoing] stack slot. *)
val emit_slow_path :
  move:(Reg.t -> Reg.t -> unit) ->
  guard:Asm_targets.Asm_label.t ->
  slow_path ->
  unit

(** Emit the guards of the probes of the current unit, the definition of the
    gate and the constructor that registers the guards, if the unit has any
    probe. *)
val emit_data : unit -> unit
