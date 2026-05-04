[@@@ocaml.warning "-30"]

(** SSA graph interface.

    An SSA graph is a first-class module instance, not a record: each call to
    {!Ssa.make_builder} creates a fresh module that owns its own [Block_id] /
    [Instruction_id] generators and its own concrete
    [Block]/[Instruction]/[Terminator] modules. Two distinct graphs therefore
    have *distinct* [Block.t] and [Instruction.t] types — the type checker
    refuses to mix references between them. This is what gives us a clean
    separation between the read-only input of a transformation and the
    in-progress output (e.g., the [In] / [Out] distinction in {!Ssa_reducer}).

    A graph has two views, captured by two module types:

    - {!Graph_builder}: the view during construction. [Block.t] is opaque (only
      [id], [is_function_start] and [params] accessors are exposed), the
      predecessor / dominator / use-count fields are not yet populated, and the
      operations are emit/finish_block/new_block, plus the smart constructors
      {!Instruction.make_op} and {!Instruction.make_proj}. Construction is
      mostly functional: each emit returns a new [unfinished_block] cursor, and
      only [finish_block] mutates the underlying [Block.t] to seal its body and
      terminator.

    - {!Finished_graph}: the view after [finish ()] has run. The full [Block.t]
      record is exposed, [predecessors] / [dominator_info] /
      [param_usage_counts] are populated, and graph-level queries
      ([predecessors], [successors], [dominates], [common_dominator]) become
      available.

    {!Standalone_graph_builder} is [Graph_builder] plus a
    [finish : unit -> (module Finished_graph)] operation; it is what
    {!Ssa.make_builder} returns. The [Graph_builder] view alone (without
    [finish]) is what [Ssa_reducer]'s [Out] sees, so a reducer can build into
    the output graph but can't itself finalise it.

    Within one graph instance, [Block.t] is the same record at runtime under
    both views; the views just expose different subsets of its fields and
    different graph-level helpers. *)

type call_op =
  | Func of Cfg_intf.S.func_call_operation
  | Prim of Cfg_intf.S.prim_call_operation

type function_info =
  { fun_name : string;
    fun_args : Cmm.machtype;
    fun_args_names : (Backend_var.With_provenance.t * Cmm.machtype) list;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
    fun_poll : Lambda.poll_attribute;
    fun_ret_type : Cmm.machtype
  }

module type Graph_builder = sig
  module Instruction_id : Oxcaml_utils.Id_counter.S

  module Block_id : Oxcaml_utils.Id_counter.S

  type op = Operation.t

  (** [usage_count] is kept abstract during construction to prevent premature
      access; the counts are populated by [Builder.finish]. *)
  type usage_count

  module rec Block : sig
    (** [Block.t] is opaque during construction: callers can hash, compare and
        consult the visible accessors, but the fields are deliberately
        inaccessible because they are not yet meaningful. *)
    type t

    val id : t -> Block_id.t

    val is_function_start : t -> bool

    val params : t -> Cmm.machtype

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val hash : t -> int

    module Map : Map.S with type key = t

    module Set : Set.S with type elt = Block.t

    module Tbl : Hashtbl.S with type key = t
  end

  and Block_set : sig
    include Set.S with type elt = Block.t
  end

  and Instruction : sig
    type t =
      | Op of op_data
      | Block_param of block_param_data
      | Proj of proj_data
      | Tuple of Instruction.t array
          (** A multi-value bundle. Only appears transiently, e.g. as the
              representative an [Ssa_reducer] reducer nominates for a
              multi-output instruction. Projections out of a [Tuple]
              short-circuit via {!proj}, so a well-formed graph never contains a
              [Tuple] in an arg or block body. *)
      | Push_trap of { handler : Block.t option }
          (** [handler = None] means "no handler block exists"; CFG lowering
              provides a shared dummy invalid block. *)
      | Pop_trap of { handler : Block.t option }
      | Stack_check of { max_frame_size_bytes : int }
      | Name_for_debugger of
          { ident : Ident.t;
            provenance : Backend_var.Provenance.t option;
            which_parameter : int option;
            regs : Instruction.t array
          }

    and op_data = private
      { id : Instruction_id.t;
        op : op;
        typ : Cmm.machtype;
        args : Instruction.t array;
        dbg : Debuginfo.t;
        mutable usage_count : usage_count
      }

    and block_param_data = private
      { block : Block.t;
        index : int
      }

    and proj_data = private
      { index : int;
        src : Instruction.t
      }

    val equal : t -> t -> bool

    (** Smart constructor for [Op] instructions. Allocates a fresh
        [Instruction_id.t]. Use this rather than constructing [Op] directly,
        since [op_data] is private. *)
    val make_op :
      op:op -> typ:Cmm.machtype -> args:t array -> dbg:Debuginfo.t -> t

    (** Smart constructor for [Block_param]. [index] is bounds-checked against
        [block]'s param array. The component type is not stored; reach for it
        via [block.params.(index)] when needed. *)
    val make_block_param : Block.t -> int -> t

    (** Smart constructor for [Proj] that short-circuits projections out of a
        [Tuple]: [make_proj ~index (Tuple elems)] returns [elems.(index)]
        directly. This is the only supported way to consume a [Tuple]. *)
    val make_proj : index:int -> t -> t

    (** The type of an instruction when used as an argument: a single
        [Cmm.machtype_component]. Fatals on [Op] with multi-component result
        (must be projected first), [Tuple], or non-value instructions
        ([Push_trap]/[Pop_trap]/[Stack_check]/[Name_for_debugger]). *)
    val arg_type : t -> Cmm.machtype_component
  end

  module Terminator : sig
    type t =
      | Goto of
          { goto : Block.t;
            args : Instruction.t array
          }
      | Branch of
          { cond : Instruction.t;
            ifso : Block.t;
            ifnot : Block.t
          }
      | Switch of
          { index : Instruction.t;
            targets : Block.t array
          }
      | Return of { args : Instruction.t array }
      | Raise of
          { raise_kind : Lambda.raise_kind;
            args : Instruction.t array
          }
          (** Raise to the topmost handler in the enclosing block's
              [block_end_trap_stack]; if that stack is empty, the exception
              escapes the function. *)
      | Tailcall_self of
          { destination : Block.t;
            args : Instruction.t array
          }
      | Tailcall_func of
          { op : Cfg_intf.S.func_call_operation;
            args : Instruction.t array
          }
      | Call of
          { op : call_op;
            args : Instruction.t array;
            continuation : Block.t;
            may_raise : bool;
            nontail : bool
                (** If [true], this call must not be tail-call optimized even if
                    its continuation is a trivial [Return]. *)
          }
      | Invalid of
          { message : string;
            args : Instruction.t array;
            continuation : Block.t option
          }
  end

  type unfinished_block

  (** Start emitting into [blk]: the resulting cursor is empty (no instructions
      yet appended). *)
  val start_block : Block.t -> unfinished_block

  (** Emit [i] and return the instruction that was actually added. For the
      canonical builder this is just [i]; for chained builders used in
      [Ssa_reducer], the chain may rewrite the instruction and return the
      rewritten version, so callers that intend to reference the emission must
      use the returned value. *)
  val emit_instruction :
    unfinished_block -> Instruction.t -> unfinished_block * Instruction.t

  val emit_op :
    unfinished_block ->
    op:op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Instruction.t array ->
    unfinished_block * Instruction.t

  val finish_block : unfinished_block -> dbg:Debuginfo.t -> Terminator.t -> unit

  type new_block_result =
    { block : Block.t;
      params : Instruction.t array
    }

  val new_block : params:Cmm.machtype -> new_block_result

  (** The function-start block, created at [make_builder] time using
      [function_info.fun_args] as its parameters. *)
  val entry : Block.t

  (** [Block_param] instructions referring to [entry]'s parameters, in order. *)
  val entry_params : Instruction.t array
end

module type Finished_graph = sig
  module Instruction_id : Oxcaml_utils.Id_counter.S

  module Block_id : Oxcaml_utils.Id_counter.S

  type op = Operation.t

  type usage_count = int

  module rec Block : sig
    type predecessors = Block_set.t

    type terminator = Terminator.t

    type dominator_info = private
      { depth : int;
        dominator : t
      }

    and t = private
      { id : Block_id.t;
        is_function_start : bool;
        params : Cmm.machtype;
        mutable predecessors : predecessors;
        mutable body : Instruction.t array;
        mutable terminator : Terminator.t;
        mutable terminator_dbg : Debuginfo.t;
        mutable dominator_info : dominator_info;
        mutable param_usage_counts : usage_count array;
            (** Per-parameter usage counts, symmetric with [Op]'s [usage_count].
                A param with count 0 is "dead": no arg passed via an
                unconditional jump ([Goto]/[Raise]/[Tailcall_self]) to this
                param need be kept alive. *)
        mutable block_end_trap_stack : t list
            (** Trap stack at the end of the block (after applying the body's
                [Push_trap]/[Pop_trap] effects to the stack inherited from
                predecessors), innermost handler first. Computed by
                {!Ssa.finish} and used to resolve trap successors of the
                terminator: a [Call] with [may_raise = true] (or any [Raise])
                branches to [List.hd block_end_trap_stack] when the stack is
                non-empty. *)
      }

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val hash : t -> int

    module Map : Map.S with type key = t

    module Set : Set.S with type elt = Block.t

    module Tbl : Hashtbl.S with type key = t
  end

  and Block_set : sig
    include Set.S with type elt = Block.t
  end

  and Instruction : sig
    type t =
      | Op of op_data
      | Block_param of block_param_data
      | Proj of proj_data
      | Tuple of Instruction.t array
          (** A multi-value bundle. Only appears transiently, e.g. as the
              representative an [Ssa_reducer] reducer nominates for a
              multi-output instruction. Projections out of a [Tuple]
              short-circuit via {!proj}, so a well-formed graph never contains a
              [Tuple] in an arg or block body. *)
      | Push_trap of { handler : Block.t option }
          (** [handler = None] means "no handler block exists"; CFG lowering
              provides a shared dummy invalid block. *)
      | Pop_trap of { handler : Block.t option }
      | Stack_check of { max_frame_size_bytes : int }
      | Name_for_debugger of
          { ident : Ident.t;
            provenance : Backend_var.Provenance.t option;
            which_parameter : int option;
            regs : Instruction.t array
          }

    and op_data = private
      { id : Instruction_id.t;
        op : op;
        typ : Cmm.machtype;
        args : Instruction.t array;
        dbg : Debuginfo.t;
        mutable usage_count : usage_count
      }

    and block_param_data = private
      { block : Block.t;
        index : int
      }

    and proj_data = private
      { index : int;
        src : Instruction.t
      }

    val equal : t -> t -> bool

    (** The type of an instruction when used as an argument; see
        {!Graph_builder} for details. *)
    val arg_type : t -> Cmm.machtype_component
  end

  and Terminator : sig
    type t =
      | Goto of
          { goto : Block.t;
            args : Instruction.t array
          }
      | Branch of
          { cond : Instruction.t;
            ifso : Block.t;
            ifnot : Block.t
          }
      | Switch of
          { index : Instruction.t;
            targets : Block.t array
          }
      | Return of { args : Instruction.t array }
      | Raise of
          { raise_kind : Lambda.raise_kind;
            args : Instruction.t array
          }
      | Tailcall_self of
          { destination : Block.t;
            args : Instruction.t array
          }
      | Tailcall_func of
          { op : Cfg_intf.S.func_call_operation;
            args : Instruction.t array
          }
      | Call of
          { op : call_op;
            args : Instruction.t array;
            continuation : Block.t;
            may_raise : bool;
            nontail : bool
          }
      | Invalid of
          { message : string;
            args : Instruction.t array;
            continuation : Block.t option
          }
  end

  val function_info : function_info

  val entry : Block.t

  val blocks : Block.t list

  val predecessors : Block.t -> Block.t list

  (** All successors of a block, including [trap_successor]. *)
  val successors : Block.t -> Block.t list

  (** The implicit trap successor of [blk]: the topmost handler in
      [block_end_trap_stack], if the terminator can raise. *)
  val trap_successor : Block.t -> Block.t option

  val dominates : Block.t -> Block.t -> bool

  val common_dominator : Block.t -> Block.t -> Block.t
end

module type Standalone_graph_builder = sig
  include Graph_builder

  val finish : unit -> (module Finished_graph)
end

type call_op_alias = call_op

type function_info_alias = function_info

module type Intf = sig
  type call_op = call_op_alias

  type function_info = function_info_alias

  module type Graph_builder = Graph_builder

  module type Finished_graph = Finished_graph

  module type Standalone_graph_builder = Standalone_graph_builder

  val make_builder : function_info -> (module Standalone_graph_builder)
end
