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
    the output-side [rewrite] hook before moving on. *)

(** Framework-provided context: the output builder, lookups from input- to
    output-graph references, and inlining helpers the reducer can use to splice
    another input block's content into the current output position. *)
module type Context = sig
  include Ssa.BuilderS

  (** Resolve an input-graph instruction reference to its output-graph
      counterpart. *)
  val map_arg : Ssa.instruction -> Ssa.instruction

  (** Resolve an input-graph block reference to its output-graph counterpart. *)
  val map_block : Ssa.block -> Ssa.block

  (** Inline [blk]'s body and terminator into the current output builder [t],
      treating [Block_param { block = blk; index; _ }] as aliasing
      [block_args.(index)] in all subsequent [map_arg] lookups. The reducer's
      hooks fire with [~is_inlining:true] for [blk]'s instructions and
      terminator. *)
  val inline_block : Ssa.block -> block_args:Ssa.instruction array -> t -> unit

  (** Inline a single instruction: consults the reducer's [visit_instruction]
      with [~is_inlining:true] and falls back to the default translation
      (rewrite args, then [rewrite_instruction]). *)
  val inline_instruction : Ssa.block -> instr_index:int -> t -> unit

  (** Inline an input block's terminator: same layering as [inline_instruction].
  *)
  val inline_terminator : Ssa.block -> t -> unit
end

(** The rules of a single reducer. *)
module type S = sig
  type t

  (** Called once at the start, before any [visit_*] / [rewrite_*]. The reducer
      can scan the input graph and stash results in module-level state for the
      per-instruction hooks to consult. *)
  val analyze : Ssa.t -> unit

  (** Called once per input block, before its body is visited. [`Unchanged]:
      defer to the framework (which visits body and terminator in turn).
      [`Replaced]: the reducer has handled this block alrady. *)
  val visit_block : Ssa.block -> t -> [> `Unchanged | `Replaced]

  (** Called for each instruction in each input block. [`Unchanged]: defer to
      the framework's default translation. [`Replaced]: the reducer has handled
      the instruction itself (via direct emission on [t] or through one of the
      [Context] helpers). *)
  val visit_instruction :
    Ssa.block -> instr_index:int -> t -> [> `Unchanged | `Replaced]

  (** Called once per input block, after its body. *)
  val visit_terminator : Ssa.block -> t -> [> `Unchanged | `Replaced]

  (** Called each time the framework is about to emit an instruction into [t].
      [`Unchanged]: keep as-is. [`Replaced (t', i)]: the reducer has emitted a
      replacement; [t'] is the continuation cursor and [i] is the representative
      for op-id remapping (use [Tuple [||]] to remap to nothing). *)
  val rewrite_instruction :
    t -> Ssa.instruction -> [> `Unchanged | `Replaced of t * Ssa.instruction]

  (** Called each time the framework is about to finish a block. [`Unchanged]:
      finish with this terminator. [`Replaced]: the reducer has already
      finalised the block. *)
  val rewrite_terminator :
    t -> dbg:Debuginfo.t -> Ssa.terminator -> [> `Unchanged | `Replaced]
end

module type Reducer = functor (C : Context) -> S with type t = C.t

(** A trivial reducer: every hook returns [`Unchanged], so the framework always
    takes the default path. Typically used via [include] so a reducer only
    writes the hooks it actually overrides. *)
module Default : Reducer

(** Combine several reducers into one. For each hook, children are tried in
    order; the first one to return a non-[`Unchanged] result wins and its result
    is propagated. If every child returns [`Unchanged], the combined reducer
    also returns [`Unchanged]. *)
val combine : (module Reducer) list -> (module Reducer)

(** Instantiate the given reducer functor. This will be a fresh instance per
    run, so module level state is not shared between runs.*)
val run : (module Reducer) -> Ssa.t -> Ssa.t
