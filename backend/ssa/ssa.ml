[@@@ocaml.warning "+a-30-40-41-42"]

module InstructionId = Oxcaml_utils.Id_counter.Make ()
module BlockId = Oxcaml_utils.Id_counter.Make ()

type instruction =
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
  | Push_trap of { handler : block option }
      (** [handler = None] is resolved to a shared dummy "invalid handler" block
          at CFG conversion time (used for trap handlers that are statically
          known to never fire). *)
  | Pop_trap of { handler : block option }
  | Stack_check of { max_frame_size_bytes : int }
  | Name_for_debugger of
      { ident : Ident.t;
        provenance : Backend_var.Provenance.t option;
        which_parameter : int option;
        regs : instruction array
      }

and op_data =
  { id : InstructionId.t;
    op : Operation.t;
    typ : Cmm.machtype;
    args : instruction array;
    dbg : Debuginfo.t;
    mutable usage_count : int
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

and call_op =
  | Func of Cfg_intf.S.func_call_operation
  | Prim of Cfg_intf.S.prim_call_operation

and dominator_info =
  | Unreachable
  | Reachable of dominator_of_reachable

and dominator_of_reachable =
  { dominator : block;  (** Immediate dominator, or self for the entry. *)
    depth : int  (** Depth in the dominator tree, 0 for the entry. *)
  }

and block =
  { id : BlockId.t;
    is_function_start : bool;
    params : Cmm.machtype;
    mutable predecessors : block list;
    mutable body : instruction array;
    mutable terminator : terminator;
    mutable terminator_dbg : Debuginfo.t;
    mutable dominator_info : dominator_info;
    mutable label_hint : Label.t option;
        (** Cached CFG label. Set by [cfg_of_ssa] during conversion so that a
            second conversion pass can reuse the same label. May be updated
            after a [cfg_compare] round to align with the old pipeline's labels.
        *)
    param_usage_counts : int array
        (** Per-parameter usage counts, tracked symmetrically with [Op]'s
            [usage_count]. A [Block_param] use propagates through this count to
            each predecessor's terminator arg at the same index, so that
            arguments passed to a dead parameter are themselves treated as dead.
        *)
  }

(* Block identity: based on the unique [id] field *)
let block_equal (a : block) (b : block) = BlockId.equal a.id b.id

let set_label_hint (b : block) (hint : Label.t option) = b.label_hint <- hint

(* Smart constructor for [Op] instructions. Allocates a fresh [InstructionId.t]
   and sets [usage_count] to 0. Does not emit into any builder: the caller (e.g.
   a reducer) controls emission. *)
let make_op ~op ~typ ~args ~dbg : instruction =
  Op { id = InstructionId.create (); op; typ; args; dbg; usage_count = 0 }

(* Smart constructor for [Proj] that short-circuits projections out of a
   [Tuple]. This is the only supported way to consume a [Tuple]. *)
let make_proj ~index src : instruction =
  match src with
  | Tuple elems -> elems.(index)
  | Op _ -> Proj { index; src }
  | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
  | Name_for_debugger _ ->
    Misc.fatal_error
      "Ssa.make_proj: cannot project from a non-value-producing instruction"

let create_block ~is_function_start ~params =
  { id = BlockId.create ();
    is_function_start;
    params;
    predecessors = [];
    body = [||];
    terminator = Pending_construction;
    terminator_dbg = Debuginfo.none;
    dominator_info = Unreachable;
    label_hint = None;
    param_usage_counts = Array.make (Array.length params) 0
  }

module Block = struct
  type t = block

  let equal = block_equal

  let compare (a : t) (b : t) = BlockId.compare a.id b.id

  let hash (b : t) = BlockId.hash b.id

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

let predecessors (blk : block) : block list = blk.predecessors

let successors (blk : block) : block list =
  match blk.terminator with
  | Pending_construction | Return _ | Tailcall_func _ -> []
  | Raise { handler; _ } -> ( match handler with Some h -> [h] | None -> [])
  | Goto { goto; _ } -> [goto]
  | Branch { ifso; ifnot; _ } -> [ifso; ifnot]
  | Switch { targets; _ } -> Array.to_list targets
  | Tailcall_self { destination; _ } -> [destination]
  | Call { continuation; exn_continuation; _ } -> (
    match exn_continuation with
    | Some l -> [continuation; l]
    | None -> [continuation])
  | Invalid { continuation; _ } -> (
    match continuation with Some l -> [l] | None -> [])

let reachable (dominator_info : dominator_info) =
  match dominator_info with Unreachable -> false | Reachable _ -> true

(* [Unreachable] is the neutral element of the dominator lattice meet: an
   unreachable block imposes no constraint on the common dominator. The result
   may itself be unreachable if both inputs are unreachable. *)
let rec common_dominator (a : dominator_info) (b : dominator_info) :
    dominator_info =
  match a, b with
  | Unreachable, other | other, Unreachable -> other
  | ( Reachable { depth = depth_a; dominator = dominator_a },
      Reachable { depth = depth_b; dominator = dominator_b } ) ->
    if depth_a > depth_b
    then common_dominator dominator_a.dominator_info b
    else if block_equal dominator_a dominator_b
    then (
      assert (depth_a = depth_b);
      Reachable { depth = depth_a; dominator = dominator_a })
    else (
      assert (depth_b > 0);
      common_dominator a dominator_b.dominator_info)

let rec dominates (a : block) (b : block) =
  match a.dominator_info, b.dominator_info with
  | _, Unreachable -> true
  | Unreachable, _ -> false
  | Reachable { depth = depth_a; _ }, Reachable { depth = depth_b; dominator }
    ->
    block_equal a b || (depth_b > depth_a && dominates a dominator)

let iter_reachable_topological (ssa : t) (visit : block -> block list) : unit =
  let position : int Block.Tbl.t = Block.Tbl.create 64 in
  List.iteri (fun pos blk -> Block.Tbl.replace position blk pos) ssa.blocks;
  let unvisited_pred_count : int Block.Tbl.t = Block.Tbl.create 64 in
  List.iter
    (fun (blk : block) ->
      let n =
        if block_equal blk ssa.entry then 0 else List.length (predecessors blk)
      in
      Block.Tbl.replace unvisited_pred_count blk n)
    ssa.blocks;
  let reaches_visited : unit Block.Tbl.t = Block.Tbl.create 64 in
  let priority_of (blk : block) =
    if Block.Tbl.find unvisited_pred_count blk = 0
    then 0
    else if Block.Tbl.mem reaches_visited blk
    then 1
    else 2
  in
  let module Q = Set.Make (struct
    type t = int * int * block

    let compare (p1, i1, b1) (p2, i2, b2) =
      match Int.compare p1 p2 with
      | 0 -> ( match Int.compare i1 i2 with 0 -> Block.compare b1 b2 | n -> n)
      | n -> n
  end) in
  let queue = ref Q.empty in
  let in_queue : unit Block.Tbl.t = Block.Tbl.create 64 in
  let popped : unit Block.Tbl.t = Block.Tbl.create 64 in
  let push (blk : block) =
    if
      (not (Block.Tbl.mem popped blk))
      && (not (Block.Tbl.mem in_queue blk))
      && Block.Tbl.mem position blk
    then begin
      Block.Tbl.add in_queue blk ();
      queue := Q.add (priority_of blk, Block.Tbl.find position blk, blk) !queue
    end
  in
  let bump_priority (blk : block) ~old_priority =
    let pos = Block.Tbl.find position blk in
    let old_key = old_priority, pos, blk in
    if Q.mem old_key !queue
    then begin
      let new_prio = priority_of blk in
      if new_prio < old_priority
      then begin
        queue := Q.remove old_key !queue;
        queue := Q.add (new_prio, pos, blk) !queue
      end
    end
  in
  push ssa.entry;
  while not (Q.is_empty !queue) do
    let ((_, _, blk) as item) = Q.min_elt !queue in
    queue := Q.remove item !queue;
    Block.Tbl.remove in_queue blk;
    Block.Tbl.add popped blk ();
    let had_unvisited_preds = Block.Tbl.find unvisited_pred_count blk > 0 in
    let newly_reachable = visit blk in
    (* Pred-count maintenance: every input successor of [blk] sees one fewer
       unvisited predecessor now that [blk] has been popped, regardless of
       whether the callback considered [blk]'s output edge to it reachable. *)
    List.iter
      (fun (succ : block) ->
        match Block.Tbl.find_opt unvisited_pred_count succ with
        | None -> ()
        | Some cnt ->
          let old_prio = priority_of succ in
          Block.Tbl.replace unvisited_pred_count succ (cnt - 1);
          bump_priority succ ~old_priority:old_prio)
      (successors blk);
    (* Push the blocks the callback declared newly reachable. *)
    List.iter push newly_reachable;
    if had_unvisited_preds
    then begin
      (* Propagate [reaches_visited] backward from [blk] through transitive
         predecessors. We descend through both already-popped and unpopped
         blocks so we can mark unpopped ancestors that are only reachable via a
         popped-but-not-yet-marked path. The [walked] set guards against loops
         in the predecessor graph. *)
      let walked : unit Block.Tbl.t = Block.Tbl.create 16 in
      let rec walk (b : block) =
        if not (Block.Tbl.mem walked b)
        then begin
          Block.Tbl.add walked b ();
          List.iter
            (fun (pred : block) ->
              if not (Block.Tbl.mem reaches_visited pred)
              then begin
                if Block.Tbl.mem in_queue pred
                then begin
                  let old_prio = priority_of pred in
                  Block.Tbl.add reaches_visited pred ();
                  bump_priority pred ~old_priority:old_prio
                end;
                walk pred
              end)
            (predecessors b)
        end
      in
      walk blk
    end
  done

let block_finished (b : block) =
  match[@warning "-fragile-match"] b.terminator with
  | Pending_construction -> false
  | _ -> true

let dominator_info_equal (a : dominator_info) (b : dominator_info) =
  match a, b with
  | Unreachable, Unreachable -> true
  | ( Reachable { dominator = d1; depth = dp1 },
      Reachable { dominator = d2; depth = dp2 } ) ->
    block_equal d1 d2 && dp1 = dp2
  | Unreachable, Reachable _ | Reachable _, Unreachable -> false

(* For a predecessor's terminator, return the argument at [index] that is passed
   through an unconditional jump to a successor whose params may be dropped.
   Only [Goto] targets such successors ([Merge] blocks); [Tailcall_self] and
   [Raise] target blocks whose params are never dropped (function re-entry /
   trap handlers fed by the runtime), so their args are counted unconditionally
   by [increment_uses_in_terminator] rather than gated here. *)
let block_param_arg (term : terminator) (index : int) : instruction option =
  match term with
  | Goto { args; _ } -> Some args.(index)
  | Pending_construction | Branch _ | Switch _ | Return _ | Raise _
  | Tailcall_self _ | Tailcall_func _ | Call _ | Invalid _ ->
    None

(* Use counts behave like reference counting. An [Op] with [usage_count = 0]
   does NOT contribute to its args' use counts; a [Block_param] whose
   [param_usage_counts] slot is zero does NOT contribute to its predecessors'
   terminator-arg use counts either. Transitions (0 → 1 on increment, 1 → 0 on
   decrement) propagate outward through the respective edges. *)
let rec increment_use (i : instruction) =
  match i with
  | Op r ->
    r.usage_count <- r.usage_count + 1;
    if r.usage_count = 1 then Array.iter increment_use r.args
  | Proj { src; _ } -> increment_use src
  | Block_param { block; index; _ } ->
    let old = block.param_usage_counts.(index) in
    block.param_usage_counts.(index) <- old + 1;
    if old = 0
    then
      List.iter
        (fun pred ->
          match block_param_arg pred.terminator index with
          | Some arg -> increment_use arg
          | None -> ())
        block.predecessors
  | Tuple _ ->
    Misc.fatal_error
      "Ssa.increment_use: Tuple should have been short-circuited by make_proj"
  | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()

let rec decrement_use (i : instruction) =
  match i with
  | Op r ->
    assert (r.usage_count > 0);
    r.usage_count <- r.usage_count - 1;
    if r.usage_count = 0 then Array.iter decrement_use r.args
  | Proj { src; _ } -> decrement_use src
  | Block_param { block; index; _ } ->
    let old = block.param_usage_counts.(index) in
    assert (old > 0);
    block.param_usage_counts.(index) <- old - 1;
    if old = 1
    then
      List.iter
        (fun pred ->
          match block_param_arg pred.terminator index with
          | Some arg -> decrement_use arg
          | None -> ())
        block.predecessors
  | Tuple _ ->
    Misc.fatal_error
      "Ssa.decrement_use: Tuple should have been short-circuited by make_proj"
  | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()

let rec update_dominator (blk : block) =
  if block_finished blk
  then (
    let old_info = blk.dominator_info in
    let new_info =
      if blk.is_function_start
      then Reachable { dominator = blk; depth = 0 }
      else
        let info blk =
          match blk.dominator_info with
          | Unreachable -> Unreachable
          | Reachable { depth; _ } ->
            Reachable { depth = depth + 1; dominator = blk }
        in
        match predecessors blk with
        | [] -> Unreachable
        | first :: rest ->
          List.fold_left
            (fun a b -> common_dominator a (info b))
            (info first) rest
    in
    blk.dominator_info <- new_info;
    if not (dominator_info_equal new_info old_info)
    then
      if (not (reachable old_info)) && reachable new_info
      then
        (* Block became reachable; register it as a predecessor of its
           successors. *)
        List.iter (fun succ -> add_pred ~src:blk ~dst:succ) (successors blk)
      else List.iter update_dominator (successors blk))

and add_pred ~src ~(dst : block) =
  (* Skip unreachable predecessors: they will be added later if this block
     becomes reachable. *)
  if reachable src.dominator_info
  then begin
    dst.predecessors <- src :: dst.predecessors;
    (* Retroactively propagate already-live params of [dst] through the
       newly-wired edge to the matching args in [src]'s terminator. *)
    Array.iteri
      (fun i count ->
        if count > 0
        then
          match block_param_arg src.terminator i with
          | Some arg -> increment_use arg
          | None -> ())
      dst.param_usage_counts;
    if block_finished dst then update_dominator dst
  end

let increment_uses_in_terminator (term : terminator) =
  let incr_all = Array.iter increment_use in
  match term with
  | Pending_construction -> ()
  (* Only [Goto] gates its args on target param usage; the target ([Merge]) has
     params that may be dropped. All other terminators either don't pass args to
     block params, or target blocks whose params are always kept ([Trap_handler]
     for [Raise], the tail-rec entry for [Tailcall_self]). *)
  | Goto _ -> ()
  | Branch { cond; _ } -> increment_use cond
  | Switch { index; _ } -> increment_use index
  | Return { args }
  | Raise { args; _ }
  | Tailcall_self { args; _ }
  | Tailcall_func { args; _ }
  | Call { args; _ }
  | Invalid { args; _ } ->
    incr_all args

(* === Builder: assembler-like interface for constructing SSA graphs ===
   Maintains a current open block. [finish_block] seals the current block with a
   terminator. *)

module type BuilderS = sig
  type t

  (** Add an instruction to the given incomplete block and return the
      instruction that was actually added. For [Builder] this is always the
      input; chained builders in [Ssa_reducer] may substitute a rewritten
      instruction. *)
  val emit_instruction : t -> instruction -> instruction

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

  (** Create a new builder that binds a fresh block with the given [desc] and
      [params] (defaulting to no parameters). Predecessor wiring happens when
      the current block's terminator references the new block (via
      [finish_block]). *)
  val new_block : t -> params:Cmm.machtype -> t

  (** Create block_param instructions for the current block. *)
  val block_params : t -> instruction array
end

module Builder : sig
  include BuilderS

  (** Create a builder that binds the start block. *)
  val make : Cmm.machtype -> t

  (** Finalize: collect reachable blocks in emission order. *)
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

  let new_block t ~params =
    let blk = create_block ~is_function_start:false ~params in
    { blocks = t.blocks; current_block = blk; body = [] }

  let make params =
    let start_block = create_block ~is_function_start:true ~params in
    { blocks = ref []; current_block = start_block; body = [] }

  let current_block t = t.current_block

  let emit_instruction t (i : instruction) =
    assert (not (block_finished t.current_block));
    (match i with
    | Name_for_debugger { regs; _ } -> Array.iter increment_use regs
    | Block_param _ | Proj _ | Tuple _ ->
      Misc.fatal_errorf
        "Ssa.Builder.emit_instruction: %s is a virtual value and cannot be \
         emitted into a block body"
        (match[@warning "-fragile-match"] i with
        | Block_param _ -> "Block_param"
        | Proj _ -> "Proj"
        | Tuple _ -> "Tuple"
        | _ -> assert false)
    | Op _ | Push_trap _ | Pop_trap _ | Stack_check _ -> ());
    t.body <- i :: t.body;
    i

  let emit_op t ~op ~dbg ~typ ~args =
    let i : instruction =
      Op { id = InstructionId.create (); op; typ; args; dbg; usage_count = 0 }
    in
    (* Non-removable impure ops are "pre-incremented" because their side effect
       counts as a use. Ops like [Opaque]/[Alloc] are impure in [is_pure] terms
       but may still be dropped when their result is unused, so we skip the
       pre-increment for them and let the normal reference counting decide. *)
    if not (Operation.is_removable_when_unused op) then increment_use i;
    emit_instruction t i

  let finish_block t ~dbg term =
    assert (not (block_finished t.current_block));
    (match[@warning "-fragile-match"] term with
    | Pending_construction ->
      Misc.fatal_error
        "Ssa.Builder.finish_block: cannot finish with Pending_construction"
    | _ -> ());
    t.current_block.body <- Array.of_list (List.rev t.body);
    t.current_block.terminator <- term;
    t.current_block.terminator_dbg <- dbg;
    increment_uses_in_terminator term;
    t.blocks := t.current_block :: !(t.blocks);
    update_dominator t.current_block

  let block_params t =
    Array.mapi
      (fun i typ ->
        (Block_param { block = t.current_block; index = i; typ } : instruction))
      t.current_block.params

  let finish t =
    List.filter
      (fun (b : block) -> reachable b.dominator_info)
      (List.rev !(t.blocks))
end
