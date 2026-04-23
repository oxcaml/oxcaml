[@@@ocaml.warning "-30"]

module InstructionId : Oxcaml_utils.Id_counter.S

module BlockId : Oxcaml_utils.Id_counter.S

type block_desc =
  | Merge
  | Branch_target
  | Function_start
  | Call_continuation
  | Trap_handler

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
  | Switch of block array * instruction array
  | Return of instruction array
  | Raise of Lambda.raise_kind * instruction array * block option
  | Tailcall_self of
      { destination : block;
        args : instruction array
      }
  | Tailcall_func of Cfg_intf.S.func_call_operation * instruction array
  | Call of
      { op : Cfg_intf.S.func_call_operation;
        args : instruction array;
        continuation : block;
        exn_continuation : block option
      }
  | Prim of
      { op : Cfg_intf.S.prim_call_operation;
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
    desc : block_desc;
    params : Cmm.machtype;
    mutable predecessors : block list;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable dominator_info : dominator_info;
    mutable label_hint : Label.t option
        (** Cached CFG label. Set by [Cfg_of_ssa.convert] so a second conversion
            pass can reuse it, and updated after [Cfg_compare] to align labels
            with the old pipeline. *)
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

val reachable : dominator_info -> bool

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

module Builder : sig
  type t

  val make : Cmm.machtype -> t

  val emit_instruction : t -> instruction -> unit

  val emit_op :
    t ->
    op:Operation.t ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:instruction array ->
    instruction

  val finish_block : t -> dbg:Debuginfo.t -> terminator -> unit

  val current_block : t -> block

  val new_block : t -> ?params:Cmm.machtype -> block_desc -> t

  val block_params : t -> instruction array

  val finish : t -> block list
end

val increment_use : instruction -> unit

val decrement_use : instruction -> unit
