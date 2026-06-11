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

open! Int_replace_polymorphic_compare

(** SSA graph interface.

    An SSA graph is a control flow graph where def-use edges are expressed as
    direct inputs of instructions. Instead of phi functions, we use continuation
    passing style block parameters. Instructions, instruction inputs (values)
    and blocks are mutually recursive and refer to each other directly.

    An SSA graph is a value of type ['g graph]. The phantom parameter ['g]
    records the graph's construction state and is one of:

    - under_construction: the graph is being built. Blocks are created with
      [Block.create] and filled in through the imperative [Cursor] interface
      ([Cursor.emit_op] / [Cursor.finish_block]); the predecessor / dominator /
      use-count metadata is not yet populated.

    - finished: [finish_graph] has run. The metadata is populated and the
      graph-level queries ([blocks], the dominator/predecessor queries on
      [Block], [body] / [terminator]) become available.

    The phantom flows through every SSA type: ['g Block.t], ['g instruction],
    ['g terminator] all carry it, so a finished block/instruction and an
    under_construction one are distinct types. The type checker therefore
    refuses to mix references between the read-only finished input of a
    transformation and the under_construction output it is producing in
    {!Ssa_reducer}.

    The phantom only distinguishes construction states, not graphs: two graphs
    in the same state share all types, and block/instruction IDs restart at zero
    for each graph, so they are only unique within their graph. Code that keys
    containers by ID (e.g. [Block.Tbl]) must not mix blocks or instructions of
    different graphs.

    Compared to CFG, control flow has been unified. Where control goes next is
    factored into a shared [continuation] ([Goto] a block, [Return] from the
    function, or [Raise] to the enclosing handler), so that [Continue] is the
    single unconditional transfer (an ordinary jump, a return, or a raise), and
    [Call] is the single call form — a [Call] whose [continuation] is [Return]
    is a tail call. [Switch] is the only conditional; a two-target [Switch] is
    the boolean branch, with [targets.(0)] the false edge and [targets.(1)] the
    true edge. A self-recursive tail call is just a [Continue (Goto entry)]
    back-edge. *)

(** The callee of a [Call] terminator. [Direct] / [Indirect] are calls to OCaml
    functions; [External] / [Probe] are primitive calls, which cannot be tail
    called. *)
type call_op =
  | Direct of Cmm.symbol
  | Indirect of Cmm.symbol list option
  | External of Cfg_intf.S.external_call_operation
  | Probe of
      { name : string;
        handler_code_sym : string;
        enabled_at_init : bool
      }

module Function_info : sig
  type t =
    { sym_name : string;
      parameters : (Backend_var.With_provenance.t * Cmm.machtype) list;
      codegen_options : Cmm.codegen_option list;
      dbg : Debuginfo.t;
      poll : Lambda.poll_attribute;
      ret_type : Cmm.machtype
    }

  val flattened_parameters : t -> Cmm.machtype
end

(** Phantom tags for a graph's construction state. They are abstract, so a
    [finished] graph and an [under_construction] one are distinct types to every
    caller — but inside [Ssa] itself they are defined as the same type, which is
    what lets [finish_graph] re-tag a graph in place without copying or casting.
*)
type under_construction

type finished

(** An SSA graph. ['g] is [under_construction] or [finished]; see the file
    header. *)
type 'g graph

(** [usage_count] is [int], but kept abstract to prevent access during
    construction. *)
type usage_count

type op = Operation.t

type 'g block

module Instruction : sig
  (** ID namespace for [Op] instructions. IDs are only unique within one graph,
      so ID-keyed containers ([Id.Tbl] etc.) must not mix instructions of
      different graphs. *)
  module Id : Oxcaml_utils.Id_counter.S

  type 'g op_data = private
    { id : Id.t;
      op : op;
      typ : Cmm.machtype;
      args : 'g value array;
      dbg : Debuginfo.t;
      mutable usage_count : usage_count;
      mutable name : string option
          (** Optional human-readable name, irrelevant for the semantics. *)
    }

  and 'g t = private
    | Op of 'g op_data
    | Push_trap of { handler : 'g block }
    | Pop_trap of { handler : 'g block }

  and 'g value = private
    | Res of 'g op_data * int
    | Block_param of 'g block * int
    | Undefined
        (** An arbitrary value that is not observed. Currently used as a [Goto]
            arg when the target block parameter is unused. The existence of an
            undefined value does *NOT* imply undefined behavior or unreachable
            control flow. It only means that the optimizer is free to chose any
            replacement value without changing semantics. For example, writing
            undefined to memory can be compiled as a no-op. *)

  (** The number of results produced by an instruction. *)
  val result_arity : 'g t -> int

  (** Whether the instruction can be deleted when none of its results are used.
      Implies [not (has_side_effect t)]; additionally [false] for instructions
      that are kept for non-semantic reasons (debug info, emit-time
      constraints). *)
  val removable_when_unused : 'g t -> bool

  (** Whether the instruction has an effect that is observable without using its
      results (e.g. writing to memory). Unlike [removable_when_unused], this is
      [false] for debug-info markers. *)
  val has_side_effect : 'g t -> bool

  val usage_count : finished op_data -> int

  val print : Format.formatter -> 'g t -> unit
end

module Value : sig
  type 'g t = 'g Instruction.value

  (** The [Undefined] value, see the constructor above for mor info. *)
  val undefined : 'g t

  val equal : 'g t -> 'g t -> bool

  (** The machtype of a value when used as an argument. [Undefined] has no type
      and raises. *)
  val typ : 'g t -> Cmm.machtype_component

  (** Set the [name] hint on the value's defining [Op] (for a [Res]) or block
      parameter (for a [Block_param]); a no-op for [Undefined]. Construction
      only. *)
  val set_name : under_construction t -> string -> unit

  val name : 'g t -> string option

  (** Usage count of the underlying block parameter or operation. Note that this
      does not distinguish between the different results of an operation,
      instead adding them all together in a single count. *)
  val usage_count : finished t -> int

  val print : Format.formatter -> 'g t -> unit
end

(** Where control transfers after a terminator. *)
type 'g continuation =
  | Goto of 'g block  (** jump to this block, passing the args *)
  | Return  (** return the args from the function *)
  | Raise of Lambda.raise_kind
      (** raise the args to the topmost handler in the enclosing block's
          [block_end_trap_stack]; if that stack is empty, the exception escapes
          the function *)

module Terminator : sig
  type 'g t =
    | Continue of
        { continuation : 'g continuation;
          args : 'g Value.t array
              (** For [Goto b], positional args for [b]'s params (a dead param's
                  arg can be [Undefined]); for [Return] / [Raise] the returned /
                  raised values. *)
        }
    | Switch of
        { index : 'g Value.t;
          targets : 'g block array
              (** Control goes to [targets.(index)]. [index] being out of range
                  is undefined behavior. A two-target switch is the boolean
                  branch: [targets.(0)] is the false edge, [targets.(1)] the
                  true edge. *)
        }
    | Call of
        { op : call_op;
          args : 'g Value.t array;
          continuation : 'g continuation;
              (** [Goto b]: a normal call returning its results as [b]'s block
                  params. [Return]: a tail call. [Raise] is not allowed
                  ([Cursor.finish_block] checks this). *)
          may_raise : bool;
          nontail : bool
              (** If [true], this call must not be tail-call optimized.
                  Incompatible with a [Return] continuation ([finish_block]
                  checks this). *)
        }
    | Invalid of
        { message : string;
          args : 'g Value.t array;
          continuation : 'g block option
        }

  val print : Format.formatter -> 'g t -> unit
end

module Block : sig
  (** A block. ['g] is the construction state of the owning graph: it keeps
      finished and under_construction blocks apart, but blocks of two graphs in
      the same state (unfortunately) share a type. *)
  type 'g t = 'g block

  (** ID namespace for blocks. IDs are only unique within one graph. *)
  module Id : Oxcaml_utils.Id_counter.S

  (** Create a new block with parameters of the given types. *)
  val create :
    under_construction graph -> params:Cmm.machtype -> under_construction t

  val create_with_names :
    under_construction graph ->
    params:(Cmm.machtype_component * string option) array ->
    under_construction t

  val equal : 'g t -> 'g t -> bool

  val compare : 'g t -> 'g t -> int

  val hash : 'g t -> int

  (** Block-keyed containers. Keys are compared by ID, so a container must only
      hold blocks of a single graph. *)
  module Map : Map.S with type key = finished t

  module Set : Set.S with type elt = finished t

  module Tbl : Hashtbl.S with type key = finished t

  val print_id : Format.formatter -> 'g t -> unit

  val id : 'g t -> Id.t

  val is_function_start : 'g t -> bool

  (** An array of [Block_param] values for this block's parameters. *)
  val params : 'g t -> 'g Value.t array

  (** The [Block_param] value for the block's [index]-th parameter. *)
  val param : 'g t -> int -> 'g Value.t

  (** The machine types of the block parameters. *)
  val params_machtype : 'g t -> Cmm.machtype

  (* The following queries are only meaningful once [finish_graph] has populated
     the graph metadata. *)

  val predecessors : finished t -> finished t list

  (** Structural successors of a terminator, not including the implicit
      exception successor derived from [block_end_trap_stack]. *)
  val non_exn_successors : finished t -> finished t list

  (** All successors of a block, including [exn_successor]. *)
  val successors : finished t -> finished t list

  (** The implicit exception successor of the block: the topmost handler in
      [block_end_trap_stack], if the terminator can raise. *)
  val exn_successor : finished t -> finished t option

  (** Trap stack at the end of the block, innermost handler first. *)
  val block_end_trap_stack : finished t -> finished t list

  val dominates : finished t -> finished t -> bool

  val common_dominator : finished t -> finished t -> finished t

  (** The immediate dominator of a block (the entry dominates itself). *)
  val immediate_dominator : finished t -> finished t

  (** The depth of a block in the dominator tree (the entry has depth 0). *)
  val dominator_depth : finished t -> int

  (** A finished block's body, in program order. *)
  val body : finished t -> finished Instruction.t array

  val terminator : finished t -> finished Terminator.t

  val terminator_dbg : finished t -> Debuginfo.t
end

module Cursor : sig
  type t

  (** A new cursor pointing to the given block. *)
  val start : under_construction Block.t -> t

  (** Move the given cursor to the given block. Asserts that the cursor used to
      point to a finished block. *)
  val move : t -> new_pos:under_construction Block.t -> unit

  (** [true] if the cursor's current block has had its terminator sealed by
      [finish_block]. *)
  val is_finished : t -> bool

  (** Emit an [Op] into the cursor's block, allocating a fresh
      [Instruction.Id.t]. Returns one [Res] value per result in the
      [Cmm.machtype]. This is the only way to produce [Res] values, which
      guarantees they always refer to an emitted instruction with an in-range
      output index. *)
  val emit_op :
    under_construction graph ->
    t ->
    op ->
    Debuginfo.t ->
    Cmm.machtype ->
    under_construction Value.t array ->
    under_construction Value.t array

  (** Emit a [Push_trap] / [Pop_trap] into the cursor's block. *)
  val emit_push_trap : t -> handler:under_construction Block.t -> unit

  val emit_pop_trap : t -> handler:under_construction Block.t -> unit

  (** Seal the cursor's block with the given terminator. *)
  val finish_block :
    under_construction graph ->
    t ->
    dbg:Debuginfo.t ->
    under_construction Terminator.t ->
    unit
end

(** Create a fresh graph in the under_construction state, with its [entry] block
    already created from [function_info.parameters]. *)
val create_graph :
  Function_info.t -> keep_unused_ops:bool -> under_construction graph

val function_info : 'g graph -> Function_info.t

(** The function-start block, created at [Ssa.create_graph] time using
    [function_info.parameters] as its parameters. *)
val entry : 'g graph -> 'g Block.t

(** Populate metadata (predecessors, trap stacks, dominators, use counts), prune
    unreachable blocks and instructions, and transition the graph to finished. A
    graph must be finished exactly once. *)
val finish_graph : under_construction graph -> finished graph

(** The graph's blocks in an order based on the order of calling
    [Cursor.finish_block], but where every block's dominators precede it. *)
val blocks : finished graph -> finished Block.t list

module Export : sig
  module Instruction = Instruction
  module Value = Value
  module Terminator = Terminator
  module Block = Block
  module Cursor = Cursor

  type nonrec finished = finished

  type nonrec under_construction = under_construction
end
