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

(** Reducer framework for SSA-level optimizations.

    The framework walks the input SSA graph and, for each instruction and
    terminator, gives the reducer two hooks to override:

    - [visit_*] intercepts the walk: when an input instruction is reached, the
      reducer can handle it itself (emit something else or nothing at all) or
      return [For_next_reducer] to let another reducer have a go (when using
      Combine) or let the framework apply the default translation, which will
      map the inputs to the output graph and continue with the output graph
      hooks described in the next bullet.

    - [emit_*] and [finish_block] intercept every emission into the output
      graph: as the framework (or the reducer itself via the context) is about
      to emit an operation or finish a block, the reducer can keep it as-is or
      swap in a replacement.

    The hooks are interleaved for each instruction: if [visit] returns
    [For_next_reducer], the framework's default translation maps args and calls
    the output-side hook ([emit_*] or [finish_block]) before moving on. For both
    visit and emit functions, [Emitted_replacement values] means that the
    reducer has already emitted a replacement (or not, if the intention was to
    delete the instruction) and the values [values] are what the input
    instruction's results map to. An input operation of arity [n] must map to an
    array of exactly [n] values. Note that returning
    [Emitted_replacement values] does NOT add anything to the output graph by
    itself; that must be done beforehand through the context.

    Input and output graphs are distinguished by the construction-state phantom:
    the input is [Ssa.finished], the output [Context.out]. Their [Block.t] /
    [instruction] / [value] / [terminator] types are therefore distinct, so the
    type checker prevents accidentally mixing references between them. *)

open Ssa.Export

(** Framework-provided context: the input and output graphs and the
    input->output value maps. The reducer's analysis result is passed directly
    to the reducer hooks as opposed to being available from the context to
    simplify the functor construction. *)
module type Context = sig
  type t

  (** A marker for the output graph's construction state, kept abstract so a
      reducer can only extend the output through this module's operations (which
      route through the [emit_*] hooks), not via the raw [Ssa] builder. It is
      really [Ssa.under_construction], but only [Make_run] knows that. *)
  type out

  (** Cursors into the output graph. Mirrors [Ssa.Cursor], with the context
      taking the graph's place. Kept as an abstract type for the same reason as
      [out]: a reducer can only drive the output through these operations, not
      via the raw [Ssa.Cursor] functions. *)
  module Cursor : sig
    type context := t

    type t

    val start : out Block.t -> t

    val move : t -> new_pos:out Block.t -> unit

    val is_finished : t -> bool

    (** Emit an [Op] into the cursor, going through the reducer's [emit_op]
        hook. Returns the value(s) it produces, which may differ from a plain
        [Ssa.Cursor.emit_op] if the reducer replaced the emission.

        ATTENTION: Calling this from the reducer's [emit_op] without first
        reducing the size of the arguments will lead to infinite recursion! Use
        [For_next_reducer] if the intention is to emit the operation without
        change. *)
    val emit_op :
      context ->
      t ->
      Ssa.op ->
      Debuginfo.t ->
      Cmm.machtype ->
      out Value.t array ->
      out Value.t array

    val emit_push_trap : t -> handler:out Block.t -> unit

    val emit_pop_trap : t -> handler:out Block.t -> unit

    (** Finish the cursor's block, going through the reducer's [finish_block]
        hook.

        ATTENTION: Calling this from the reducer's [finish_block] without first
        reducing the size of the arguments will lead to infinite recursion! Use
        [For_next_reducer] if the intention is to emit the terminator without
        change. *)
    val finish_block :
      context -> t -> dbg:Debuginfo.t -> out Terminator.t -> unit
  end

  val in_graph : t -> finished Ssa.graph

  val out_graph : t -> out Ssa.graph

  (** Resolve an input-graph value to its output-graph counterpart. *)
  val map_value : t -> finished Value.t -> out Value.t

  (** Resolve an input-graph block reference to its output-graph counterpart. *)
  val map_block : t -> finished Block.t -> out Block.t
end

type 'a did_emit =
  | For_next_reducer
  | Emitted_replacement of 'a

(** The signature of a reducer applied to a [Context]: the analysis pass plus
    the hooks. [context] / [cursor] / [out] are the corresponding types of that
    context; {!Reducer} substitutes them with [C.t] / [C.Cursor.t] / [C.out]. *)
module type S = sig
  type analysis_result

  type context

  type cursor

  type out

  (** Called once at the start of each run, before any [visit_*] / [emit_*].
      Scans the input graph and produces the analysis result consulted by the
      per-instruction hooks. *)
  val analyze : finished Ssa.graph -> analysis_result

  (** Called for each instruction in each input block. [For_next_reducer]: defer
      to the framework's default translation. [Emitted_replacement values]: the
      reducer has handled the instruction itself (by emitting replacements via
      the context, or by doing nothing, which means it is dropped). [values] is
      what the input instruction's results map to (empty for a trap
      instruction), and is remembered as the output-graph mapping. *)
  val visit_instruction :
    analysis_result ->
    context ->
    finished Block.t ->
    instr_index:int ->
    cursor ->
    out Value.t array did_emit

  (** Called once per input block, after its body. *)
  val visit_terminator :
    analysis_result -> context -> finished Block.t -> cursor -> unit did_emit

  (** Called each time the framework is about to emit an [Op] into the cursor.
      [For_next_reducer]: emit it as-is. [Emitted_replacement values]: the
      reducer has already emitted a replacement producing the values [values].
  *)
  val emit_op :
    analysis_result ->
    context ->
    cursor ->
    op:Ssa.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:out Value.t array ->
    out Value.t array did_emit

  (** Called each time the framework is about to finish a block.
      [For_next_reducer]: the framework will finish the block with the given
      terminator. [Emitted_replacement ()]: the reducer indicates it has already
      finalised the block using the context. *)
  val finish_block :
    analysis_result ->
    context ->
    cursor ->
    dbg:Debuginfo.t ->
    out Terminator.t ->
    unit did_emit
end

(** A reducer is a functor over a [Context]. It is instantiated exactly once (by
    {!Make_run}, not per graph). Its [analyze] runs once per graph and produces
    an [analysis_result] that is then passed to every hook. *)
module type Reducer = functor (C : Context) ->
  S with type context := C.t and type cursor := C.Cursor.t and type out := C.out

(** A trivial reducer: every hook returns [For_next_reducer], so the framework
    always takes the default path. Typically used via [include Default (C)] so a
    reducer only writes the hooks it actually overrides. *)
module Default : functor (C : Context) ->
  S
    with type analysis_result = unit
     and type context := C.t
     and type cursor := C.Cursor.t
     and type out := C.out

(** Combine two reducers into one. For each hook the first reducer is tried, and
    if it returns [For_next_reducer] the second one is tried. *)
module Combine (_ : Reducer) (_ : Reducer) : Reducer

(** Instantiate a reducer (once) and obtain its driver.

    [run]'s [keep_unused_ops] (default [false]) disables structural pruning:
    dead [Op]s and dropped block params are preserved in the output, useful when
    the output must remain faithful to the input shape (e.g. before
    [Cfg_compare]). *)
module Make_run (_ : Reducer) : sig
  val run : ?keep_unused_ops:bool -> finished Ssa.graph -> finished Ssa.graph
end
