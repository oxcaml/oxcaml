[@@@ocaml.warning "-30"]

module InstructionId : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end

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
  | Push_trap of { handler : block }
  | Pop_trap of { handler : block }
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

and block_desc =
  | Merge of { mutable predecessors : block list }
  | Loop of
      { mutable predecessors : block list;
        mutable backedges : block list
      }
  | Branch_target of { predecessor : block }
  | Function_start
  | Call_continuation of { predecessor : block }
  | Trap_handler of { mutable predecessors : block list }

and block =
  { id : int;
    desc : block_desc;
    params : Cmm.machtype;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable reachable : bool;
    mutable dominator : block;
    mutable dominator_depth : int
  }

val block_equal : block -> block -> bool

val block_id : block -> int

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

val dominates : block -> block -> bool

val common_dominator : block -> block -> block

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

  val create_merge : t -> Cmm.machtype -> t

  val create_loop : t -> Cmm.machtype -> t

  val create_trap_handler : t -> Cmm.machtype -> t

  val create_branch_target : t -> t

  val create_call_continuation : t -> Cmm.machtype -> t

  val block_params : t -> instruction array

  val finish : t -> block list
end

val increment_use : instruction -> unit

val decrement_use : instruction -> unit
