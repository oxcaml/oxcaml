[@@@ocaml.warning "+a-40-41-42"]

module InstructionId : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end = struct
  type t = int

  let next_id = ref 0

  let create () =
    let id = !next_id in
    incr next_id;
    id

  let equal = Int.equal

  let compare = Int.compare

  let hash = Fun.id

  module Self = struct
    type nonrec t = t

    let equal = equal

    let compare = compare

    let hash = hash
  end

  module Set = Set.Make (Self)
  module Map = Map.Make (Self)
  module Tbl = Hashtbl.Make (Self)
end

(* All types are mutually recursive because block contains terminator which
   references block. *)

type instruction =
  | Op of
      { id : InstructionId.t;
        op : Operation.t;
        typ : Cmm.machtype;
        args : instruction array;
        dbg : Debuginfo.t
      }
  | Block_param of
      { block : block;
        index : int;
        typ : Cmm.machtype_component
      }
  | Proj of
      { index : int;
        src : instruction
      }
  | Pushtrap of { handler : block }
  | Poptrap of { handler : block }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

and terminator =
  | Never
  | Goto of
      { goto : block;
        args : instruction array
      }
  | Branch of
      { conditions : (instruction * block) array;
        else_goto : block
      }
  | Switch of block array * instruction array
  | Return of instruction array
  | Raise of
      Lambda.raise_kind * instruction array * block option (* exn handler *)
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
  | BranchTarget of { predecessor : block }
  | FunctionStart
  | CallContinuation of { predecessor : block }
  | TrapHandler of { mutable predecessors : block list }

and block =
  { id : int;
    desc : block_desc;
    params : Cmm.machtype;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t
  }

(* Block identity: based on the unique [id] field *)
let block_equal a b = Int.equal a.id b.id

let block_id b = b.id

let next_block_id = ref 0

let create_block ~desc ~params =
  let id = !next_block_id in
  incr next_block_id;
  { id;
    desc;
    params;
    body = [||];
    terminator = Never;
    terminator_dbg = Debuginfo.none
  }

module Block = struct
  type t = block

  let equal = block_equal

  let compare a b = Int.compare a.id b.id

  let hash b = b.id

  module Self = struct
    type nonrec t = t

    let equal = equal

    let compare = compare

    let hash = hash
  end

  module Set = Set.Make (Self)
  module Map = Map.Make (Self)
  module Tbl = Hashtbl.Make (Self)
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

let add_pred ~src ~(dst : block) =
  match dst.desc with
  | Merge r -> r.predecessors <- src :: r.predecessors
  | TrapHandler r -> r.predecessors <- src :: r.predecessors
  | BranchTarget _ | FunctionStart | CallContinuation _ -> ()

let add_terminator_preds src (term : terminator) =
  let add dst = add_pred ~src ~dst in
  match term with
  | Never | Return _ | Raise _ | Tailcall_func _ -> ()
  | Goto { goto; _ } -> add goto
  | Branch { conditions; else_goto } ->
    Array.iter (fun (_, dst) -> add dst) conditions;
    add else_goto
  | Switch (targets, _) -> Array.iter add targets
  | Tailcall_self { destination; _ } -> add destination
  | Call { continuation; exn_continuation; _ }
  | Prim { continuation; exn_continuation; _ } ->
    add continuation;
    Option.iter add exn_continuation
  | Invalid { continuation; _ } -> Option.iter add continuation

(* === Builder: assembler-like interface for constructing SSA graphs ===
   Maintains a current open block. [finish_block] seals the current block with a
   terminator. *)

module Builder : sig
  type t

  (** Create a builder that binds the start block. *)
  val make : Cmm.machtype -> t

  (** Add an instruction to the given incomplete block. *)
  val emit_instruction : t -> instruction -> unit

  (** Create an Op instruction and add it to the current block. *)
  val emit_op :
    t ->
    op:Operation.t ->
    dbg:Debuginfo.t ->
    typ:Cmm.machtype ->
    args:instruction array ->
    instruction

  (** Seal the current block with a terminator. *)
  val finish_block : t -> dbg:Debuginfo.t -> terminator -> unit

  val current_block : t -> block

  (** Creates a new builder that binds a new merge block. *)
  val create_merge : t -> Cmm.machtype -> t

  val create_trap_handler : t -> Cmm.machtype -> t

  (** Creates a new builder for a branch target, implicitly sets the current
      block as predecessor. *)
  val create_branch_target : t -> t

  val create_call_continuation : t -> Cmm.machtype -> t

  (** Create block_param instructions for a given block. *)
  val block_params : t -> instruction array

  (** Finalize: collect all created blocks. Returns blocks in emission order. *)
  val finish : t -> block list
end = struct
  (* Each [t] represents an incomplete block being built. All [t] values created
     from the same [make] call share the same [blocks] ref, which accumulates
     all created blocks in reverse order. *)
  type t =
    { blocks : block list ref;
      current_block : block;
      mutable body : instruction list;
      mutable block_finished : bool
    }

  let create_block_from t desc params =
    let blk = create_block ~desc ~params in
    let new_t =
      { blocks = t.blocks;
        current_block = blk;
        body = [];
        block_finished = false
      }
    in
    new_t

  let make params =
    let start_block = create_block ~desc:FunctionStart ~params in
    { blocks = ref [];
      current_block = start_block;
      body = [];
      block_finished = false
    }

  let current_block t = t.current_block

  let emit_instruction t (i : instruction) =
    assert (not t.block_finished);
    t.body <- i :: t.body

  let emit_op t ~op ~dbg ~typ ~args =
    let i : instruction =
      Op { id = InstructionId.create (); op; typ; args; dbg }
    in
    emit_instruction t i;
    i

  let finish_block t ~dbg term =
    assert (not t.block_finished);
    t.current_block.body <- Array.of_list (List.rev t.body);
    t.current_block.terminator <- term;
    t.current_block.terminator_dbg <- dbg;
    add_terminator_preds t.current_block term;
    t.block_finished <- true;
    t.blocks := t.current_block :: !(t.blocks)

  let create_merge t params =
    create_block_from t (Merge { predecessors = [] }) params

  let create_trap_handler t params =
    create_block_from t (TrapHandler { predecessors = [] }) params

  let create_branch_target t =
    create_block_from t (BranchTarget { predecessor = t.current_block }) [||]

  let create_call_continuation t params =
    create_block_from t
      (CallContinuation { predecessor = t.current_block })
      params

  let block_params t =
    Array.mapi
      (fun i typ ->
        (Block_param { block = t.current_block; index = i; typ } : instruction))
      t.current_block.params

  let finish t = List.rev !(t.blocks)
end
