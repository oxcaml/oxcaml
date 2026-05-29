open! Int_replace_polymorphic_compare

(** SSA graph interface.

    An SSA graph is a first-class module instance: each call to
    {!Ssa.make_builder} creates a fresh module that owns its own [Block.Id] /
    [Instruction.Id] generators and its own concrete
    [Block]/[Instruction]/[Terminator] modules. Two distinct graphs therefore
    have *distinct* [Block.t] and [Instruction.t] types — the type checker
    refuses to mix references between them. This is what gives us a clean
    separation between the read-only input of a transformation and the
    in-progress output in {!Ssa_reducer}.

    A graph has two views, captured by two module types:

    - [Graph_builder]: the view during construction. [Block.t] is opaque (only
      [id], [is_function_start] and [params] accessors are exposed), the
      predecessor / dominator / use-count fields are not yet populated, and the
      operations are [emit_instruction] / [emit_op] / [finish_block] /
      [new_block], plus the smart constructors [make_op], [make_block_param] and
      [make_proj]. Construction uses the imperative [Cursor] interface, to avoid
      accidentally dropping already emitted parts of the graph.

    - [Finished_graph]: the view after [finish_graph] has run. The full
      [Block.t] record is exposed, [predecessors] / [dominator_info] /
      [usage_count] are populated, and graph-level queries ([predecessors],
      [successors], [dominates], [common_dominator]) become available.

    Within one graph instance, the actual data structures are the same under
    both views and [finish_graph] does not create a copy; the views just expose
    different subsets of its fields and different graph-level helpers. *)

type call_op =
  | Func of Cfg_intf.S.func_call_operation
  | Prim of Cfg_intf.S.prim_call_operation

module Function_info = struct
  type t =
    { sym_name : string;
      parameters : (Backend_var.With_provenance.t * Cmm.machtype) list;
      codegen_options : Cmm.codegen_option list;
      dbg : Debuginfo.t;
      poll : Lambda.poll_attribute;
      ret_type : Cmm.machtype
    }
end

module type Graph_shared_sig = sig
  type usage_count

  module Instruction_id : Oxcaml_utils.Id_counter.S

  module Block_id : Oxcaml_utils.Id_counter.S

  type op = Operation.t

  (** A param with [usage_count = 0] is "dead": args passed from predecessor
      blocks do not need to be kept alive and we can remove the parameter
      completely in Ssa_reducer. *)
  type block_param = private
    { typ : Cmm.machtype_component;
      mutable name : string option;
      mutable usage_count : usage_count
    }

  type 'block instruction =
    | Op of 'block op_data
    | Block_param of 'block block_param_data
    | Proj of 'block proj_data
    | Tuple of 'block instruction array
        (** A multi-value bundle. Only appears transiently, e.g. as the
            representative an [Ssa_reducer] reducer nominates to replace a
            multi-output instruction. Projections out of a [Tuple] short-circuit
            via [make_proj], so a well-formed graph never contains a [Tuple],
            neither in an arg nor block body. *)
    | Push_trap of { handler : 'block }
    | Pop_trap of { handler : 'block }
    | Stack_check of { max_frame_size_bytes : int }
    | Name_for_debugger of
        { ident : Ident.t;
          provenance : Backend_var.Provenance.t option;
          which_parameter : int option;
          args : 'block instruction array
        }

  and 'block op_data = private
    { id : Instruction_id.t;
      op : Operation.t;
      typ : Cmm.machtype;
      args : 'block instruction array;
      dbg : Debuginfo.t;
      mutable usage_count : usage_count;
      mutable name : string option
          (** Optional human-readable name, irrelevant for the semantics. *)
    }

  and 'block block_param_data = private
    { block : 'block;
      param_index : int
    }

  and 'block proj_data = private
    { src : 'block instruction;
      output_index : int
    }

  type 'block terminator =
    | Goto of
        { goto : 'block;
          args : 'block instruction option array
        }
    | Branch of
        { cond : 'block instruction;
          ifso : 'block;
          ifnot : 'block
        }
    | Switch of
        { index : 'block instruction;
          targets : 'block array
        }
    | Return of { args : 'block instruction array }
    | Raise of
        { raise_kind : Lambda.raise_kind;
          args : 'block instruction array
        }
        (** Raise to the topmost handler in the enclosing block's
            [block_end_trap_stack]; if that stack is empty, the exception
            escapes the function. *)
    | Tailcall_self of
        { destination : 'block;
          args : 'block instruction array
        }
    | Tailcall_func of
        { op : Cfg_intf.S.func_call_operation;
          args : 'block instruction array
        }
    | Call of
        { op : call_op;
          args : 'block instruction array;
          continuation : 'block;
          may_raise : bool;
          nontail : bool
              (** If [true], this call must not be tail-call optimized even if
                  its continuation is a trivial [Return]. *)
        }
    | Invalid of
        { message : string;
          args : 'block instruction array;
          continuation : 'block option
        }

  val function_info : Function_info.t
end

module type Block_shared_sig = sig
  type param

  module Id : Oxcaml_utils.Id_counter.S

  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t

  module Tbl : Hashtbl.S with type key = t

  val print_param : Format.formatter -> t -> int -> unit

  val print_id : Format.formatter -> t -> unit

  val id : t -> Id.t

  val is_function_start : t -> bool

  val params : t -> param array
end

module type Instruction_shared_sig = sig
  type t

  module Id : Oxcaml_utils.Id_counter.S

  val equal : t -> t -> bool

  (** The number of results produced by an instruction. *)
  val result_arity : t -> int

  (** The type of an instruction when used as an argument. Must only be called
      on instructions with [result_arity] of 1. *)
  val arg_type : t -> Cmm.machtype_component

  val print : Format.formatter -> t -> unit

  val print_as_ref : Format.formatter -> t -> unit
end

module type Terminator_shared_sig = sig
  type t

  type block

  (** Structural successors of a terminator, not including the implicit trap
      successor derived from [block_end_trap_stack]. *)
  val non_trap_successors : t -> block list

  val print : Format.formatter -> t -> unit
end

module type Finished_graph = sig
  include Graph_shared_sig with type usage_count = int

  module Block : sig
    (** Only used during graph building. *)
    type pending_body

    type dominator_info = private
      { depth : int;
        dominator : block
      }

    and block = private
      { id : Block_id.t;
        is_function_start : bool;
        params : block_param array;
        mutable predecessors : block list;
        mutable body : block instruction array;
        mutable pending_body : pending_body;
        mutable terminator : block terminator;
        mutable terminator_dbg : Debuginfo.t;
        mutable dominator_info : dominator_info;
        mutable block_end_trap_stack : block list
            (** Trap stack at the end of the block (after applying the body's
                [Push_trap]/[Pop_trap] effects to the stack inherited from
                predecessors), innermost handler first. Computed by
                [finish_graph] and used to resolve trap successors of the
                terminator: a [Call] with [may_raise = true] (or any [Raise])
                branches to [List.hd block_end_trap_stack] when the stack is
                non-empty. *)
      }

    include
      Block_shared_sig
        with type param = block_param
         and type t = block
         and module Id = Block_id

    val predecessors : t -> t list

    (** Project just the machtype out of a block's [params] array. *)
    val params_machtype : t -> Cmm.machtype

    (** All successors of a block, including [trap_successor]. *)
    val successors : t -> t list

    (** The implicit trap successor of the block: the topmost handler in
        [block_end_trap_stack], if the terminator can raise. *)
    val trap_successor : t -> t option

    val dominates : t -> t -> bool

    val common_dominator : t -> t -> t

    (** Replace a block's terminator. For in-place control-flow rewrites on a
        finished graph (e.g. bypassing a loop). The caller is responsible for
        keeping the graph well-formed (argument arities, predecessors, trap
        stacks); metadata computed by [finish_graph] is not refreshed. *)
    val set_terminator : t -> block terminator -> unit
  end

  module Instruction : sig
    include
      Instruction_shared_sig
        with type t = Block.t instruction
         and module Id = Instruction_id
  end

  module Terminator : sig
    include
      Terminator_shared_sig
        with type t = Block.t terminator
         and type block = Block.t
  end

  val entry : Block.t

  val blocks : Block.t list
end

module type Graph_builder = sig
  include Graph_shared_sig

  type block

  type cursor

  module Block : sig
    (** [Block.t] is opaque during construction to prevent access to fields that
        are only meaningful after [finish_graph]. *)
    include
      Block_shared_sig
        with type param = block_param
         and type t = block
         and module Id = Block_id

    val set_param_name : param -> string -> unit
  end

  module Instruction : sig
    include
      Instruction_shared_sig
        with type t = Block.t instruction
         and module Id = Instruction_id

    (** Smart constructor for [Op] instructions. Allocates a fresh
        [Instruction.Id.t]. Use this rather than constructing [Op] directly,
        since [op_data] is private. *)
    val make_op :
      op:op -> typ:Cmm.machtype -> args:t array -> dbg:Debuginfo.t -> t

    (** Set the [name] hint on an [Op]. No-op for non-[Op] instructions. *)
    val set_name : t -> string -> unit

    (** Smart constructor for [Block_param] that bounds-checks [index] against
        the block's param array. *)
    val make_block_param : Block.t -> index:int -> t

    (** Smart constructor for [Proj] that short-circuits projections out of a
        [Tuple]: [make_proj (Tuple elems) ~index] returns [elems.(index)]
        directly. This is the only supported way to consume a [Tuple]. *)
    val make_proj : t -> index:int -> t
  end

  module Terminator : sig
    include
      Terminator_shared_sig
        with type t = Block.t terminator
         and type block = Block.t
  end

  module Cursor : sig
    type t = cursor

    (** A new cursor pointing to the given block. *)
    val start : Block.t -> t

    (** Move the given cursor to the given position. As they now point to the
        same block, only one of the two cursors can be finished with
        [finish_block]. Asserts that the modified cursor used to point to a
        finished block. *)
    val move : t -> new_pos:t -> unit

    (** [true] if the cursor's current block has had its terminator sealed by
        [finish_block]. *)
    val is_finished : t -> bool
  end

  val emit_instruction : Cursor.t -> Instruction.t -> unit

  val emit_op :
    Cursor.t ->
    op:op ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:Instruction.t array ->
    Instruction.t

  val finish_block : Cursor.t -> dbg:Debuginfo.t -> Terminator.t -> unit

  type new_block_result =
    { block : Block.t;
      params : Instruction.t array
    }

  val new_block : params:Cmm.machtype -> new_block_result

  val new_block_with_names :
    params:(Cmm.machtype_component * string option) array -> Block.t

  (** The function-start block, created at [make_builder] time using
      [function_info.parameters] as its parameters. *)
  val entry : Block.t

  (** [Block_param] instructions referring to [entry]'s parameters, in order. *)
  val entry_params : Instruction.t array
end

module type Make_builder_result = sig
  include Graph_builder

  val finish_graph : unit -> (module Finished_graph)
end

module type Intf = sig
  type nonrec call_op = call_op

  module Function_info : sig
    type t = Function_info.t

    val flattened_parameters : t -> Cmm.machtype
  end

  module type Graph_builder = Graph_builder

  module type Finished_graph = Finished_graph

  module type Make_builder_result = Make_builder_result

  val make_builder :
    Function_info.t -> keep_unused_ops:bool -> (module Make_builder_result)
end
