(** Reducer framework for SSA-level optimizations.

    The framework walks the input SSA graph and, for each instruction and
    terminator, gives the reducer two hooks to override:

    - [visit_*] intercepts the walk: when an input instruction is reached, the
      reducer can handle it itself (emit something else or nothing at all) or
      return [Unchanged] to let the framework apply the default translation.

    - [rewrite_*] intercepts every emission into the output graph: as the
      framework (or the reducer itself via the [Context]) is about to emit an
      instruction, the reducer can keep it as-is or swap in a replacement.

    The hooks are interleaved for each instruction: if [visit] returns
    [Unchanged], the framework's default translation rewrites args and calls the
    output-side [rewrite] hook before moving on. Both for visit and rewrite
    functions, [Replaced instr] means that the reducer has already emitted a
    replacement (or not, if the intention was to delete the instruction) and
    [instr] will be used as the replacement. [instr] could be an instruction
    emitted via [Context], or it could be a [Tuple] if the replacement are zero
    or multiple values. Note that returning an instruction with [Replaced instr]
    does NOT add the instruction to the output graph, this has to be done
    beforehand using the [Context].

    The new interface keeps input and output graphs in separate modules ([C.In]
    for the read-only input, [C] itself for the in-progress output), so the type
    checker prevents accidentally mixing references between them. *)

(** Framework-provided context: bindings to the input and output graphs, builder
    ops on the output, and lookups from input- to output-graph references. *)
module type Context = sig
  module In : Ssa.Finished_graph

  include Ssa.Graph_builder

  (** Emit an instruction into the cursor. Returns the instruction that was
      actually emitted, which might be different as this goes through the
      reducer recursively. Note that the return type is different from
      [Graph_builder.emit_instruction]. *)
  val emit_instruction : Cursor.t -> Instruction.t -> Instruction.t

  (** Shorthand for [emit_instruction cursor (Instruction.make_op ...)]*)
  val emit_op :
    Cursor.t ->
    op:op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Instruction.t array ->
    Instruction.t

  (** Resolve an input-graph instruction reference to its output-graph
      counterpart. *)
  val map_arg : In.Instruction.t -> Instruction.t

  (** Resolve an input-graph block reference to its output-graph counterpart. *)
  val map_block : In.Block.t -> Block.t
end

type 'a result =
  | Unchanged
  | Replaced of 'a

(** A reducer functor is constructed for a specific [Context], which provides
    both the input and the output graph. *)
module type Reducer = functor (C : Context) -> sig
  (** Called once at the start, before any [visit_*] / [rewrite_*]. The reducer
      can scan the input graph (via [C.In]) and stash results in module-level
      state for the per-instruction hooks to consult. Since the reducer module
      is specific to one optimization phase, module-level state is safe to use.
  *)
  val analyze : unit -> unit

  (** Called once per input block, before its body is visited. Returns
      [Unchanged] to defer to the framework default behavior (which visits body
      and terminator in turn), or [Replaced] to indicate the reducer has handled
      this block already. *)
  val visit_block : C.In.Block.t -> C.Cursor.t -> unit result

  (** Called for each instruction in each input block. [Unchanged]: defer to the
      framework's default translation. [Replaced instr]: the reducer has handled
      the instruction itself (by emitting a replacement via the [Context] or by
      doing nothing, which means the instruction will be dropped completely).
      [instr] will be remembered as the output graph mapping for the input
      instruction. *)
  val visit_instruction :
    C.In.Block.t -> instr_index:int -> C.Cursor.t -> C.Instruction.t result

  (** Called once per input block, after its body. *)
  val visit_terminator : C.In.Block.t -> C.Cursor.t -> unit result

  (** Called each time the framework is about to emit an instruction into the
      cursor. [Unchanged]: keep as-is. [Replaced instr]: the reducer has already
      emitted a replacement [instr]. *)
  val rewrite_instruction :
    C.Cursor.t -> C.Instruction.t -> C.Instruction.t result

  (** Called each time the framework is about to finish a block. [Unchanged]:
      the framework will finish the block with the given terminator.
      [Replaced ()]: the reducer indicates it has already finalised the block
      using [Context]. *)
  val rewrite_terminator :
    C.Cursor.t -> dbg:Debuginfo.t -> C.Terminator.t -> unit result
end

(** A trivial reducer: every hook returns [Unchanged], so the framework always
    takes the default path. Typically used via [include] so a reducer only
    writes the hooks it actually overrides. *)
module Default : Reducer

(** Combine several reducers into one. For each hook, children are tried in
    order; the first one to return a non-[Unchanged] result wins and its result
    is propagated. If every child returns [Unchanged], the combined reducer also
    returns [Unchanged]. *)
val combine : (module Reducer) list -> (module Reducer)

(** Run the given reducer over the input graph, producing a new
    [Finished_graph]. Each call instantiates a fresh reducer, so module level
    state is not shared between runs.

    [keep_unused_ops] (default to [false]) disables structural pruning: dead
    [Op]s and dropped block params are preserved in the output, useful when the
    output must remain faithful to the input shape (e.g. before [Cfg_compare]).
*)
val run :
  ?keep_unused_ops:bool ->
  (module Reducer) ->
  (module Ssa.Finished_graph) ->
  (module Ssa.Finished_graph)
