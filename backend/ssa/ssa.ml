[@@@ocaml.warning "+a-30-40-41-42"]

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

type op_data =
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

(* Block identity: based on the unique [id] field *)
let block_equal (a : block) (b : block) = Int.equal a.id b.id

let block_id (b : block) = b.id

let next_block_id = ref 0

let create_block ~desc ~params =
  let id = !next_block_id in
  incr next_block_id;
  let rec blk =
    { id;
      desc;
      params;
      body = [||];
      terminator = Pending_construction;
      terminator_dbg = Debuginfo.none;
      reachable = true;
      dominator = blk;
      dominator_depth = 0
    }
  in
  blk

module Block = struct
  type t = block

  let equal = block_equal

  let compare (a : t) (b : t) = Int.compare a.id b.id

  let hash (b : t) = b.id

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

let rec common_dominator (a : block) (b : block) =
  if a.dominator_depth > b.dominator_depth
  then common_dominator a.dominator b
  else if block_equal a b
  then a
  else (
    assert (b.dominator_depth > 0);
    common_dominator a b.dominator)

let rec dominates (a : block) (b : block) =
  block_equal a b || (not b.reachable)
  || (b.dominator_depth > a.dominator_depth && dominates a b.dominator)

let block_finished (b : block) =
  match[@warning "-fragile-match"] b.terminator with
  | Pending_construction -> false
  | _ -> true

let add_pred ~src ~(dst : block) =
  match dst.desc with
  | Merge r ->
    assert (not (block_finished dst));
    r.predecessors <- src :: r.predecessors
  | Loop r ->
    if block_finished dst
    then (
      assert (dominates dst src);
      r.backedges <- src :: r.backedges)
    else r.predecessors <- src :: r.predecessors
  | Trap_handler r ->
    assert (not (block_finished dst));
    r.predecessors <- src :: r.predecessors
  | Function_start -> ()
  | Branch_target { predecessor } | Call_continuation { predecessor } ->
    assert (block_equal src predecessor);
    assert (not (block_finished dst))

(* Use counts are recursively accurate: an Op with usage_count = 0 does NOT
   contribute to its args' use counts. Transitions (0 → 1 on increment, 1 → 0 on
   decrement) propagate to args. *)
let rec increment_use (i : instruction) =
  match i with
  | Op r ->
    r.usage_count <- r.usage_count + 1;
    if r.usage_count = 1 then Array.iter increment_use r.args
  | Proj { src; _ } -> increment_use src
  | Block_param _ | Push_trap _ | Pop_trap _ | Stack_check _
  | Name_for_debugger _ ->
    ()

let rec decrement_use (i : instruction) =
  match i with
  | Op r ->
    assert (r.usage_count > 0);
    r.usage_count <- r.usage_count - 1;
    if r.usage_count = 0 then Array.iter decrement_use r.args
  | Proj { src; _ } -> decrement_use src
  | Block_param _ | Push_trap _ | Pop_trap _ | Stack_check _
  | Name_for_debugger _ ->
    ()

let increment_uses_in_terminator (term : terminator) =
  let incr_all = Array.iter increment_use in
  match term with
  | Pending_construction -> ()
  | Goto { args; _ } -> incr_all args
  | Branch { cond; _ } -> increment_use cond
  | Switch (_, args) -> incr_all args
  | Return args -> incr_all args
  | Raise (_, args, _) -> incr_all args
  | Tailcall_self { args; _ } -> incr_all args
  | Tailcall_func (_, args) -> incr_all args
  | Call { args; _ } -> incr_all args
  | Prim { args; _ } -> incr_all args
  | Invalid { args; _ } -> incr_all args

let add_terminator_preds src (term : terminator) =
  let add dst = add_pred ~src ~dst in
  match term with
  | Pending_construction | Return _ | Raise _ | Tailcall_func _ -> ()
  | Goto { goto; _ } -> add goto
  | Branch { ifso; ifnot; _ } ->
    add ifso;
    add ifnot
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

  val create_loop : t -> Cmm.machtype -> t

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
      mutable body : instruction list
    }

  let create_block_from t desc params =
    let blk = create_block ~desc ~params in
    { blocks = t.blocks; current_block = blk; body = [] }

  let make params =
    let start_block = create_block ~desc:Function_start ~params in
    { blocks = ref []; current_block = start_block; body = [] }

  let current_block t = t.current_block

  let emit_instruction t (i : instruction) =
    assert (not (block_finished t.current_block));
    (match i with
    | Name_for_debugger { regs; _ } -> Array.iter increment_use regs
    | Op _ | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
      ->
      ());
    t.body <- i :: t.body

  let emit_op t ~op ~dbg ~typ ~args =
    let i : instruction =
      Op { id = InstructionId.create (); op; typ; args; dbg; usage_count = 0 }
    in
    (* Impure ops are "pre-incremented" because their side effect counts as a
       use. [increment_use] handles the recursive propagation to args. *)
    if not (Operation.is_pure op) then increment_use i;
    emit_instruction t i;
    i

  let non_backedge_predecessors (blk : block) : block list =
    match blk.desc with
    | Function_start -> []
    | Branch_target { predecessor } | Call_continuation { predecessor } ->
      [predecessor]
    | Loop { predecessors; _ }
    | Merge { predecessors }
    | Trap_handler { predecessors } ->
      predecessors

  let compute_dominator (blk : block) =
    match
      List.filter (fun pred -> pred.reachable) (non_backedge_predecessors blk)
    with
    | [] ->
      blk.reachable
        <- (match[@warning "-fragile-match"] blk.desc with
           | Function_start -> true
           | _ -> false);
      blk.dominator <- blk;
      blk.dominator_depth <- 0
    | first :: rest ->
      let dom = List.fold_left common_dominator first rest in
      blk.dominator <- dom;
      blk.dominator_depth <- dom.dominator_depth + 1

  let finish_block t ~dbg term =
    assert (not (block_finished t.current_block));
    (match[@warning "-fragile-match"] term with
    | Pending_construction ->
      Misc.fatal_error
        "Ssa.Builder.finish_block: cannot finish with Pending_construction"
    | _ -> ());
    (* Ensures that blocks are finished in an order where predecessors come
       before successors (except loop back-edges) and dominators before
       dominated. *)
    non_backedge_predecessors t.current_block
    |> List.iter (fun p ->
        if not (block_finished p)
        then
          Misc.fatal_errorf
            "Ssa.Builder.finish_block: predecessor %d of block %d is not \
             finished"
            (p : block).id (t.current_block : block).id);
    compute_dominator t.current_block;
    t.current_block.body <- Array.of_list (List.rev t.body);
    t.current_block.terminator <- term;
    t.current_block.terminator_dbg <- dbg;
    increment_uses_in_terminator term;
    add_terminator_preds t.current_block term;
    t.blocks := t.current_block :: !(t.blocks)

  let create_merge t params =
    create_block_from t (Merge { predecessors = [] }) params

  let create_loop t params =
    create_block_from t (Loop { predecessors = []; backedges = [] }) params

  let create_trap_handler t params =
    create_block_from t (Trap_handler { predecessors = [] }) params

  let create_branch_target t =
    create_block_from t (Branch_target { predecessor = t.current_block }) [||]

  let create_call_continuation t params =
    create_block_from t
      (Call_continuation { predecessor = t.current_block })
      params

  let block_params t =
    Array.mapi
      (fun i typ ->
        (Block_param { block = t.current_block; index = i; typ } : instruction))
      t.current_block.params

  let finish t = List.rev !(t.blocks)
end
