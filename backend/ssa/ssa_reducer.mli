(** Reducer framework for SSA-level optimizations.

    The framework walks the input SSA graph and, for each instruction and
    terminator, gives the reducer two hooks to override:

    - [visit_*] intercepts the walk: when an input instruction is reached, the
      reducer can handle it itself (skip, duplicate, inline from other blocks,
      etc.) or return [`Unchanged] to let the framework apply the default
      translation.

    - [rewrite_*] intercepts every emission into the output graph: as the
      framework (or the reducer itself) is about to emit an instruction, the
      reducer can keep it as-is or swap in a replacement.

    The hooks are interleaved for each instruction: if [visit] returns
    [`Unchanged], the framework's default translation rewrites args and calls
    the output-side [rewrite] hook before moving on.

    The new interface keeps input and output graphs in separate modules ([C.In]
    for the read-only input, [C.Out] for the in-progress output): the type
    checker prevents accidentally mixing references between them. *)

(** Framework-provided context: bindings to the input and output modules,
    builder ops on the output, lookups from input- to output-graph references,
    and inlining helpers the reducer can use to splice another input block's
    content into the current output position. *)
module type Context = sig
  module In : Ssa.Finished_graph

  module Out : Ssa.Graph_builder

  (** Emit an instruction into the cursor (subject to the reducer's
      [rewrite_instruction] hook). Returns the new cursor and the instruction
      that was actually appended (for the canonical builder this is just the
      input; chained reducers may rewrite). *)
  val emit_instruction :
    Out.unfinished_block ->
    Out.Instruction.t ->
    Out.unfinished_block * Out.Instruction.t

  val emit_op :
    Out.unfinished_block ->
    op:Out.op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Out.Instruction.t array ->
    Out.unfinished_block * Out.Instruction.t

  val finish_block :
    Out.unfinished_block -> dbg:Debuginfo.t -> Out.Terminator.t -> unit

  val new_block : params:Cmm.machtype -> Out.new_block_result

  (** Resolve an input-graph instruction reference to its output-graph
      counterpart. *)
  val map_arg : In.Instruction.t -> Out.Instruction.t

  (** Resolve an input-graph block reference to its output-graph counterpart. *)
  val map_block : In.Block.t -> Out.Block.t

  (** Inline [blk]'s body and terminator into the current cursor, treating
      [Block_param { block = blk; index; _ }] as aliasing [block_args.(index)]
      in all subsequent [map_arg] lookups. The reducer's hooks fire with
      [~is_inlining:true] for [blk]'s instructions and terminator. *)
  val inline_block :
    In.Block.t ->
    block_args:Out.Instruction.t array ->
    Out.unfinished_block ->
    unit

  (** Inline a single instruction: consults the reducer's [visit_instruction]
      with [~is_inlining:true] and falls back to the default translation
      (rewrite args, then [rewrite_instruction]). Returns the advanced cursor.
  *)
  val inline_instruction :
    In.Block.t ->
    instr_index:int ->
    Out.unfinished_block ->
    Out.unfinished_block

  (** Inline an input block's terminator: same layering as [inline_instruction].
  *)
  val inline_terminator : In.Block.t -> Out.unfinished_block -> unit
end

(** A reducer functor. Given a [Context], returns the per-run hooks. *)
module type Reducer = functor (C : Context) -> sig
  (** Called once at the start, before any [visit_*] / [rewrite_*]. The reducer
      can scan the input graph (via [C.In]) and stash results in module-level
      state for the per-instruction hooks to consult. *)
  val analyze : unit -> unit

  (** Called once per input block, before its body is visited. [`Unchanged]:
      defer to the framework (which visits body and terminator in turn).
      [`Replaced]: the reducer has handled this block already. *)
  val visit_block :
    C.In.Block.t -> C.Out.unfinished_block -> [> `Unchanged | `Replaced]

  (** Called for each instruction in each input block. [`Unchanged]: defer to
      the framework's default translation. [`Replaced]: the reducer has handled
      the instruction itself (via direct emission on the cursor or through one
      of the [Context] helpers). *)
  val visit_instruction :
    C.In.Block.t ->
    instr_index:int ->
    C.Out.unfinished_block ->
    [> `Unchanged | `Replaced]

  (** Called once per input block, after its body. *)
  val visit_terminator :
    C.In.Block.t -> C.Out.unfinished_block -> [> `Unchanged | `Replaced]

  (** Called each time the framework is about to emit an instruction into the
      cursor. [`Unchanged]: keep as-is. [`Replaced (b', i)]: the reducer has
      emitted a replacement; [b'] is the continuation cursor and [i] is the
      representative for op-id remapping (use [Tuple [||]] to remap to nothing).
  *)
  val rewrite_instruction :
    C.Out.unfinished_block ->
    C.Out.Instruction.t ->
    [> `Unchanged | `Replaced of C.Out.unfinished_block * C.Out.Instruction.t]

  (** Called each time the framework is about to finish a block. [`Unchanged]:
      finish with this terminator. [`Replaced]: the reducer has already
      finalised the block. *)
  val rewrite_terminator :
    C.Out.unfinished_block ->
    dbg:Debuginfo.t ->
    C.Out.Terminator.t ->
    [> `Unchanged | `Replaced]
end

(** A trivial reducer: every hook returns [`Unchanged], so the framework always
    takes the default path. Typically used via [include] so a reducer only
    writes the hooks it actually overrides. *)
module Default : Reducer

(** Combine several reducers into one. For each hook, children are tried in
    order; the first one to return a non-[`Unchanged] result wins and its result
    is propagated. If every child returns [`Unchanged], the combined reducer
    also returns [`Unchanged]. *)
val combine : (module Reducer) list -> (module Reducer)

(** Run the given reducer over the input graph, producing a new
    [Finished_graph]. Each call instantiates a fresh reducer, so module level
    state is not shared between runs.

    [keep_unused_ops] (default [false]) disables structural pruning: dead [Op]s
    and dropped block params are preserved in the output. Use it when the output
    must remain faithful to the input shape (e.g. before [Cfg_compare], which
    expects the baseline's instructions and params verbatim). *)
val run :
  ?keep_unused_ops:bool ->
  (module Reducer) ->
  (module Ssa.Finished_graph) ->
  (module Ssa.Finished_graph)
