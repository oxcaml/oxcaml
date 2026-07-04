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
      return [Unchanged] to let another reducer have a go (when using Combine)
      or let the framework apply the default translation, which will map the
      inputs to the output graph and continue with the output graph hooks
      described in the next bullet.

    - [emit_op] and [finish_block] intercept every emission into the output
      graph: as an operation is about to be emitted (by the framework, or by the
      reducer itself via [Cursor.emit_op]) or a block is about to be finished,
      the reducer can keep it as-is or swap in a replacement.

    The hooks are interleaved for each instruction: if [visit] returns
    [Unchanged], the framework's default translation maps args and calls the
    output-side hook ([emit_*] or [finish_block]) before moving on. For both
    visit and emit functions, [Reduce (fun c -> ...)] means that the reducer
    wants to emit a replacement. It is handed the cursor [c] to do so and is
    expected to return the replacement values or the new terminator, depending
    on the hook. An input operation of arity [n] must map to an array of exactly
    [n] values.

    Input and output graphs are distinguished by the construction-state phantom:
    the input is [Ssa.finished], the output [Context.out]. Their [Block.t] /
    [instruction] / [value] / [terminator] types are therefore distinct, so the
    type checker prevents accidentally mixing references between them. *)

open Ssa.Export

(** Framework-provided context: the input and output graphs and the
    input->output value maps. A reducer is handed the context once, by
    {!Reducer.create}, and keeps in its own state whatever it needs from it
    (including, if any, its analysis result). *)
module Context : sig
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

    (** Emit an [Op] into the cursor, going through the reducer's [emit_op]
        hook. Returns the value(s) it produces, which may differ from a plain
        [Ssa.Cursor.emit_op] if the reducer replaced the emission.

        ATTENTION: Calling this from the reducer's [emit_op] without first
        reducing the size of the arguments will lead to infinite recursion! Use
        [Unchanged] if the intention is to emit the operation without change. *)
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
  end

  val in_graph : t -> finished Ssa.graph

  val out_graph : t -> out Ssa.graph

  (** Resolve an input-graph value to its output-graph counterpart. *)
  val map_value : t -> finished Value.t -> out Value.t

  (** Resolve an input-graph block reference to its output-graph counterpart. *)
  val map_block : t -> finished Block.t -> out Block.t

  (** Translate an input-graph terminator into the output graph: map its values
      and blocks, and drop the [Continue (Goto _)] args that feed parameters the
      framework removed from the target block. *)
  val map_terminator : t -> finished Terminator.t -> out Terminator.t
end

(** A hook's decision for one instruction or terminator. [Unchanged]: defer to
    the next reducer (under [Combine]) or the framework's default translation.
    [Reduce f]: the reducer takes over; [f] is run with the output cursor (to
    emit through the [Context]) and returns what the input maps to: the result
    value(s) for [visit_instruction] / [emit_op], or the replacement
    [(dbg, terminator)] for [visit_terminator] / [finish_block] (which the
    framework then finishes and re-reduces). *)
type 'a reduction =
  | Unchanged
  | Reduce of (Context.Cursor.t -> 'a)

(** A reducer is turned into an optimization pass using the {!Make_run} functor.
    The {!Combine} functor can be used to compose multiple reducers into one. *)
module type Reducer = sig
  type t

  val create : Context.t -> t

  (** Called for each instruction in each input block. [Unchanged]: defer to the
      framework's default translation. [Reduce (fun c -> ...)]: the reducer
      handles the instruction itself (by emitting replacements via the given
      cursor [c], or by doing nothing, which means it is dropped). The closure
      returns what the input instruction's results map to (empty for a trap
      instruction), and is remembered as the output-graph mapping. *)
  val visit_instruction :
    t ->
    finished Block.t ->
    instr_index:int ->
    Context.out Value.t array reduction

  (** Called once per input block, after its body. [Unchanged]: the framework
      finishes the block with its translated terminator.
      [Reduce (fun c -> ...)]: the reducer returns the replacement
      [(dbg, terminator)] for the framework to finish (and re-reduce through
      [finish_block]). *)
  val visit_terminator :
    t -> finished Block.t -> (Debuginfo.t * Context.out Terminator.t) reduction

  (** Called each time the framework is about to emit an [Op] into the cursor.
      [Unchanged]: emit it as-is. [Reduce (fun c -> ...)]: the reducer emits a
      replacement using the given cursor [c]. *)
  val emit_op :
    t ->
    op:Ssa.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Context.out Value.t array ->
    Context.out Value.t array reduction

  (** Called each time the framework is about to finish a block. [Unchanged]:
      the framework finishes the block with the given terminator. Otherwise the
      reducer returns a replacement [(dbg, terminator)] and the framework
      finishes the block with it — the reducer does not finish the block itself.

      ATTENTION: The returned terminator will be reduced recursively, including
      calling this function again, so it needs to be smaller than the original
      one. *)
  val finish_block :
    t ->
    dbg:Debuginfo.t ->
    Context.out Terminator.t ->
    (Debuginfo.t * Context.out Terminator.t) reduction
end

(** Trivial hooks that all return [Unchanged], so the framework always takes the
    default path. [include Default_reducer] lets a reducer mention only the
    hooks it actually overrides; it still supplies its own [type t] and
    [create]. This is not using the [Reducer] signature because the more generic
    hook types keep working even when [t] has been overwritten. *)
module Default_reducer : sig
  type t = Context.t

  val create : Context.t -> t

  val visit_instruction :
    't ->
    finished Block.t ->
    instr_index:int ->
    Context.out Value.t array reduction

  val visit_terminator :
    't -> finished Block.t -> (Debuginfo.t * Context.out Terminator.t) reduction

  val emit_op :
    't ->
    op:Ssa.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Context.out Value.t array ->
    Context.out Value.t array reduction

  val finish_block :
    't ->
    dbg:Debuginfo.t ->
    Context.out Terminator.t ->
    (Debuginfo.t * Context.out Terminator.t) reduction
end

(** Combine two reducers into one. For each hook the first reducer is tried, and
    if it returns [Unchanged] the second one is tried. *)
module Combine (_ : Reducer) (_ : Reducer) : Reducer

(** Instantiate a reducer (once) and obtain its driver.

    [run]'s [keep_unused_ops] (default [false]) disables structural pruning:
    dead [Op]s and dropped block params are preserved in the output, useful when
    the output must remain faithful to the input shape (e.g. before
    [Cfg_compare]). *)
module Make_run (_ : Reducer) : sig
  val run : ?keep_unused_ops:bool -> finished Ssa.graph -> finished Ssa.graph
end
