[@@@ocaml.warning "-30"]

module InstructionId : Oxcaml_utils.Id_counter.S

module BlockId : Oxcaml_utils.Id_counter.S

type op_data = private
  { id : InstructionId.t;
    op : Operation.t;
    typ : Cmm.machtype;
    args : instruction array;
    dbg : Debuginfo.t;
    mutable usage_count : int
  }

and instruction =
  | Op of op_data
  | Block_param of
      { block : block;
        index : int;
        typ : Cmm.machtype_component
      }
  | Proj of
      { index : int;
        src : instruction
      }
  | Tuple of instruction array
      (** A multi-value bundle. Only appears transiently, e.g. as the
          representative an [Ssa_reducer] reducer nominates for a multi-output
          instruction. Projections out of a [Tuple] short-circuit via
          {!make_proj}, so a well-formed graph never contains a [Tuple] in an
          arg or block body. *)
  | Push_trap of { handler : block option }
      (** [handler = None] means "no handler block exists"; CFG lowering
          provides a shared dummy invalid block. *)
  | Pop_trap of { handler : block option }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

and call_op =
  | Func of Cfg_intf.S.func_call_operation
  | Prim of Cfg_intf.S.prim_call_operation

and terminator =
  | Pending_construction
  | Goto of
      { goto : block;
        args : instruction array
      }
  | Branch of
      { cond : instruction;
        ifso : block;
        ifnot : block
      }
  | Switch of
      { index : instruction;
        targets : block array
      }
  | Return of { args : instruction array }
  | Raise of
      { raise_kind : Lambda.raise_kind;
        args : instruction array;
        handler : block option
      }
  | Tailcall_self of
      { destination : block;
        args : instruction array
      }
  | Tailcall_func of
      { op : Cfg_intf.S.func_call_operation;
        args : instruction array
      }
  | Call of
      { op : call_op;
        args : instruction array;
        continuation : block;
        exn_continuation : block option
      }
  | Invalid of
      { message : string;
        args : instruction array;
        continuation : block option
      }

and dominator_info =
  | Unreachable
  | Reachable of dominator_of_reachable

and dominator_of_reachable = private
  { dominator : block;  (** Immediate dominator, or self for the entry. *)
    depth : int  (** Depth in the dominator tree, 0 for the entry. *)
  }

and block = private
  { id : BlockId.t;
    is_function_start : bool;
    params : Cmm.machtype;
    mutable predecessors : block list;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable dominator_info : dominator_info;
    mutable label_hint : Label.t option;
        (** Cached CFG label. Set by [Cfg_of_ssa.convert] so a second conversion
            pass can reuse it, and updated after [Cfg_compare] to align labels
            with the old pipeline. *)
    param_usage_counts : int array
        (** Per-parameter usage counts, symmetric with [Op]'s [usage_count]. A
            param with count 0 is "dead": no arg passed via an unconditional
            jump ([Goto]/[Raise]/[Tailcall_self]) to this param need be kept
            alive. *)
  }

val block_equal : block -> block -> bool

val set_label_hint : block -> Label.t option -> unit

(** Smart constructor for [Op] instructions. Allocates a fresh [InstructionId.t]
    and sets [usage_count] to 0. Does not emit into a builder. *)
val make_op :
  op:Operation.t ->
  typ:Cmm.machtype ->
  args:instruction array ->
  dbg:Debuginfo.t ->
  instruction

(** Smart constructor for [Proj] that short-circuits projections out of a
    [Tuple]: [make_proj ~index (Tuple elems)] returns [elems.(index)] directly.
    This is the only supported way to consume a [Tuple]. *)
val make_proj : index:int -> instruction -> instruction

val reachable : dominator_info -> bool

(** A block is "finished" once its terminator has been set (i.e. is no longer
    [Pending_construction]). *)
val block_finished : block -> bool

module Block : sig
  type t = block

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end

type t =
  { blocks : block list;
    fun_name : string;
    fun_args : Cmm.machtype;
    fun_args_names : (Backend_var.With_provenance.t * Cmm.machtype) list;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
    entry : block;
    fun_poll : Lambda.poll_attribute;
    fun_ret_type : Cmm.machtype
  }

val predecessors : block -> block list

val successors : block -> block list

val dominates : block -> block -> bool

val common_dominator : dominator_info -> dominator_info -> dominator_info

(** Iterate over all reachable blocks of [ssa] in a priority order over the
    unvisited set [(priority, input_position)] (smaller is better):
    - priority 0: all predecessors are already visited (canonical topological);
    - priority 1: block transitively reaches an already-visited block (closes an
      existing "open" structure before starting a new one);
    - priority 2: neither.

    [input_position] (the index of the block in [ssa.blocks]) breaks ties.

    Reachability is computed dynamically, initially only the start block is
    considered reachable. The callback returns the blocks that should be
    considered reachable after processing the given block. *)
val iter_reachable_topological : t -> (block -> block list) -> unit

(** The subset of the builder interface that reducers may use: operations that
    act on an already-existing cursor. Lifecycle operations ([make] and
    [finish]) live on [Builder] itself. *)
module type BuilderS = sig
  type t

  (** Emit [i] and return the instruction that was actually added. For the
      canonical [Builder] this is just [i]; for chained builders used in
      [Ssa_reducer], the chain may rewrite the instruction and return the
      rewritten version, so callers that intend to reference the emission (e.g.
      via [emit_op]) must use the returned value. *)
  val emit_instruction : t -> instruction -> instruction

  val emit_op :
    t ->
    op:Operation.t ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:instruction array ->
    instruction

  val finish_block : t -> dbg:Debuginfo.t -> terminator -> unit

  val current_block : t -> block

  val new_block : t -> params:Cmm.machtype -> t

  val block_params : t -> instruction array
end

module Builder : sig
  include BuilderS

  val make : Cmm.machtype -> t

  val finish : t -> block list
end

val increment_use : instruction -> unit

val decrement_use : instruction -> unit
