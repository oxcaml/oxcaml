(** Reducer framework for SSA-level optimizations.

    A reducer inspects each instruction (and each terminator) in turn and either
    leaves it alone or replaces it. [Copy.run] takes a list of reducers and
    applies them in order for every instruction: the first one to return a
    non-[`Unchanged] result owns the emission. Reducers emit replacement
    instructions through an [Assembler], which re-enters the same chain so later
    reducers see rewrites made by earlier ones. *)

module type S = sig
  (** [`Unchanged]: try the next reducer. [`Replaced]: this reducer already
      emitted the replacement via the [Assembler]; stop the chain.
      [`ReplacedWith i]: replace the current instruction with [i] and re-run the
      chain from the top on [i]. *)
  val emit_instruction :
    Ssa.instruction ->
    [> `Unchanged | `Replaced | `ReplacedWith of Ssa.instruction]

  (** [`Unchanged] / [`Replaced] semantics as for [emit_instruction]. There is
      no [`ReplacedWith] case since a terminator also finalises the block:
      reducers must use [Assembler.emit_terminator] to drive replacement. *)
  val emit_terminator :
    dbg:Debuginfo.t -> Ssa.terminator -> [> `Unchanged | `Replaced]
end

(** The imperative handle reducers use to emit into the current block or branch
    off into fresh blocks. Emissions go back through the full reducer chain. *)
module type Assembler = sig
  val emit_instruction : Ssa.instruction -> unit

  val emit_terminator : dbg:Debuginfo.t -> Ssa.terminator -> unit

  val new_block : ?params:Cmm.machtype -> Ssa.block_desc -> Ssa.block

  val change_block : Ssa.block -> unit

  val current_block : unit -> Ssa.block
end

(** A reducer is expressed as a functor, instantiated once per [run] with the
    assembler for that run. Instantiating per run gives the reducer a fresh copy
    of any mutable state it holds in its body. *)
module type Reducer = functor (_ : Assembler) -> S

(** Combine a list of reducers into a single reducer. Children are tried in
    order; the first one to return a non-[`Unchanged] result wins, and its
    result is propagated to the caller. *)
module Combine (_ : sig
  val reducers : (module Reducer) list
end) : Reducer

val run : (module Reducer) -> Ssa.t -> Ssa.t
