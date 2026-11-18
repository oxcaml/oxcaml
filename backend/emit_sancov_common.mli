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

(** The parts of the emission of the coverage probes of [-gated-trace-pc-guard]
    (see [Cfg_trace_pc_guard]) that do not depend on the architecture. This
    module is used by [Emit_sancov]. *)

(** To be called at the start of each compilation unit. *)
val reset_unit : unit -> unit

(** To be called at the start of each function. *)
val reset_function : unit -> unit

(** The guard of the probe numbered [index] in the current function. It is
    created on the first call, so that an instruction emitted several times
    keeps its guard. *)
val guard_of_index : int -> Asm_targets.Asm_label.t

(** Whether the current unit has a guard. *)
val unit_has_guards : unit -> bool

(** The registers of [live] that a call to the callback must save, each with
    the [Outgoing] stack slot to save it in, and the size in bytes of the frame
    holding these slots. A slot has the size of its register and is aligned to
    that size, or to 16 bytes if it is larger. The size of the frame is a
    multiple of 16. *)
val registers_to_save : live:Reg.Set.t -> (Reg.t * Reg.t) list * int

(** Emit a zero-initialised definition of the gate in its section, either weak
    and hidden, as in instrumented units, or global. *)
val emit_gate_definition : weak:bool -> unit

(** Emit the guards of the probes of the current unit, the weak definition of
    the gate and the constructor that registers the guards, if the unit has any
    guard.

    The code of the constructor is aligned to [code_alignment] bytes. Its
    instructions are emitted by [emit_constructor_body], which must call
    [init] with the addresses of [start] and [stop] as arguments, and
    return. *)
val emit_data :
  code_alignment:int ->
  emit_constructor_body:
    (start:Asm_targets.Asm_symbol.t ->
    stop:Asm_targets.Asm_symbol.t ->
    init:Asm_targets.Asm_symbol.t ->
    unit) ->
  unit
