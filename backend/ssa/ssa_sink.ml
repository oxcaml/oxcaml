open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export
open Ssa_reducer

(** An operation is moved from its block [D] to a block [T] that [D] strictly
    dominates when:

    - it is pure, or a load from immutable memory, as classified by CSE: it can
      then be computed later, on a subset of the paths that computed it;
    - none of its arguments is an [Addr]: sinking extends the live ranges of the
      arguments, and a derived pointer must not be live across a call, an
      allocation or a poll (including the ones [Cfg_polling] inserts later);
    - if it is a load, or if it takes an OCaml value as an argument, no
      [End_region] can execute between its original position and the start of
      [T]: the load's memory may be freed by the end of the region, and an OCaml
      value argument may point to a local allocation whose region is ending.
      Keeping it live as a GC root can corrupt reused storage. Any [End_region]
      counts, even of a region begun after the operation, which is conservative;
    - [T] dominates all its uses, a use as a [Goto] argument being a use in the
      block of the terminator (in SSA, this is all that legality requires);
    - every loop containing [T] contains [D], so that the operation is never
      moved into a loop;
    - [T] does not post-dominate [D], ignoring exception edges, i.e. the
      operation becomes partially dead; or [T] is cold while [D] is not.

    [T] is chosen as deep as possible on the dominator tree path from [D] to the
    common dominator of the uses. The uses of an operation are placed before the
    operation itself, so that chains of operations sink together.

    In particular, sinking into the continuation of a call, which post-dominates
    the block of the call, is not done: it only pays off if the arguments of the
    operation are live across the call anyway. Functions with irreducible
    control flow are left untouched.

    Recognized heap allocations move with their complete initialization. The
    group is placed before its inputs, ignoring uses internal to the group. All
    members retain their original order. An allocation may not acquire an
    external [Addr] input or move to a block with a live [Addr] at entry. It
    also may not cross region operations, pointer reinterpretations, or an
    unrecognized allocation: the latter could still be under initialization at
    the new GC point. Removing allocations must also leave safe positions for
    any prologue or back-edge polls inserted later. Local allocations are not
    moved.

    Debugger markers ([Name_for_debugger]) are not considered uses: they follow
    the values they name, and are dropped in the rare case where these values
    end up in unrelated blocks. *)

module Cse = Cfg_cse.Cse_generic (CSE)

(* The additional restrictions on a move, beyond dominance and profitability.
   Heap allocations also need a safe GC point at their new position. *)
type sinkable =
  | Can_cross_end_region
  | Blocked_by_end_region
  | Heap_allocation

let has_gc_root_arg (op : finished Instruction.op_data) : bool =
  Array.exists
    (fun arg ->
      match Value.typ arg with
      | Val | Valx2 -> true
      | Addr | Int | Float | Vec128 | Vec256 | Vec512 | Mask | Float32 -> false)
    op.args

let sinkable (instr : finished Instruction.op_data) : sinkable option =
  let op = instr.op in
  match op with
  | Move | Spill | Reload | Opaque | Poll -> None
  | Alloc _ ->
    (* CR-someday xclerc: sink local allocations as well, accounting for their
       region lifetimes. Recognized heap allocations are handled as groups
       below. *)
    None
  | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
  | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Const_mask _
  | Stackoffset _ | Load _ | Store _ | Intop _ | Int128op _ | Intop_imm _
  | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
  | Probe_is_enabled _ | Begin_region | End_region | Specific _
  | Name_for_debugger _ | Dls_get | Tls_get | Domain_index | Pause -> (
    match Cse.class_of_operation op with
    | (Op_pure | Op_load Immutable) when not (Operation.is_pure op) ->
      (* [Operation.is_pure] also rules out the [Value_of_int] and
         [Int_of_value] casts, which CSE deems pure. *)
      None
    | Op_pure ->
      Some
        (if has_gc_root_arg instr
         then Blocked_by_end_region
         else Can_cross_end_region)
    | Op_load Immutable -> Some Blocked_by_end_region
    | Op_load Mutable ->
      (* CR-someday xclerc: sink mutable loads as well, when no store, call,
         allocation or poll can happen between their original and new
         positions. *)
      None
    | Op_store _ | Op_other -> None)

let has_addr_arg (op : finished Instruction.op_data) : bool =
  Array.exists
    (fun arg -> Cmm.equal_machtype_component (Value.typ arg) Addr)
    op.args

let iter_terminator_values (f : finished Value.t -> unit)
    (terminator : finished Terminator.t) : unit =
  match terminator with
  | Continue { continuation = _; args } ->
    Array.iter
      (function
        | Terminator.Arg value -> f value
        | Terminator.Omitted_since_unused -> ())
      args
  | Switch { index; targets = _ } -> f index
  | Call { op = _; args; continuation = _; may_raise = _; nontail = _ }
  | Invalid { message = _; args; continuation = _ } ->
    Array.iter f args

(* GC safety must include the operand lifetimes introduced by branch fusion.
   Placement decisions still use the SSA operands, including the comparison
   result, so that the comparison itself is placed before its uses. *)
let iter_lowered_terminator_values f (terminator : finished Terminator.t) =
  match terminator with
  | Switch { index; targets } when Array.length targets = 2 -> (
    match Cfg_of_ssa.fused_branch_args index with
    | Some args -> Array.iter f args
    | None -> f index)
  | Continue _ | Switch _ | Call _ | Invalid _ ->
    iter_terminator_values f terminator

(* Where a value is used: by an operation, whose block may change as it is sunk,
   or by the terminator of a block. *)
type use =
  | Op_use of finished Instruction.op_data
  | Terminator_use of finished Block.t

type decision =
  | Sink of finished Block.t
  | Drop

type allocation_group =
  { construction : Ssa_allocation.t;
    last : finished Instruction.Id.t
  }

let group_members group =
  group.construction.allocation :: group.construction.initialization

let collect_allocations (graph : finished Ssa.graph) =
  let allocations = Instruction.Id.Tbl.create 16 in
  List.iter
    (fun block ->
      Ssa_allocation.find_in_block block
      |> List.iter (fun (construction : Ssa_allocation.t) ->
          let last =
            List.fold_left
              (fun _ (op : finished Instruction.op_data) -> op.id)
              construction.allocation.id construction.initialization
          in
          let group = { construction; last } in
          List.iter
            (fun (op : finished Instruction.op_data) ->
              Instruction.Id.Tbl.replace allocations op.id group)
            (group_members group)))
    (Ssa.blocks graph);
  allocations

let is_allocation_barrier allocations (op : finished Instruction.op_data) =
  match op.op with
  | Begin_region | End_region | Reinterpret_cast (Int_of_value | Value_of_int)
    ->
    true
  | Alloc _ -> Option.is_none (Instruction.Id.Tbl.find_opt allocations op.id)
  | Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
  | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
  | Const_mask _ | Stackoffset _ | Load _ | Store _ | Intop _ | Int128op _
  | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _
  | Reinterpret_cast
      ( V128_of_vec _ | V256_of_vec _ | V512_of_vec _ | Float32_of_float
      | Float32_of_int32 | Float_of_float32 | Float_of_int64 | Int64_of_float
      | Int32_of_float32 | Mask_of_int64 | Int64_of_mask )
  | Static_cast _ | Probe_is_enabled _ | Opaque | Specific _
  | Name_for_debugger _ | Dls_get | Tls_get | Domain_index | Poll | Pause ->
    false

(* Inserting a heap allocation must not make an existing derived pointer live
   across a GC point. Walk backwards from the original Addr uses to their
   definitions. Definitions in one block share the same stopping point; block
   parameters, unlike instruction results, are also live at that block's entry.
   This is conservative when a definition will itself move later in the pass. *)
let live_addr_at_entry ~homes (graph : finished Ssa.graph) =
  let live = Block.Tbl.create 16 in
  let users = Block.Tbl.create 16 in
  let add_use block (value : finished Value.t) =
    if Cmm.equal_machtype_component (Value.typ value) Addr
    then begin
      let def_block =
        match value with
        | Res (op, _) -> Instruction.Id.Tbl.find homes op.id
        | Block_param (block, _) ->
          Block.Tbl.replace live block ();
          block
      in
      let other_users =
        Option.value
          (Block.Tbl.find_opt users def_block)
          ~default:Block.Set.empty
      in
      Block.Tbl.replace users def_block (Block.Set.add block other_users)
    end
  in
  List.iter
    (fun block ->
      Array.iter
        (fun (instr : finished Instruction.t) ->
          match instr with
          | Op op -> Array.iter (add_use block) op.args
          | Push_trap _ | Pop_trap _ -> ())
        (Block.body block);
      iter_lowered_terminator_values (add_use block) (Block.terminator block))
    (Ssa.blocks graph);
  Block.Tbl.iter
    (fun def_block users ->
      let visited = Block.Tbl.create 16 in
      let rec mark block =
        if
          (not (Block.equal block def_block))
          && not (Block.Tbl.mem visited block)
        then begin
          Block.Tbl.replace visited block ();
          Block.Tbl.replace live block ();
          List.iter mark (Block.predecessors block)
        end
      in
      Block.Set.iter mark users)
    users;
  live

(* Removing an allocation may reinstate a prologue poll or introduce a poll on a
   loop back edge. Without tracking the naked Ints of pointer casts, do not move
   allocations in functions containing them. For loop motion, also require
   complete constructions throughout the function, no region barriers, no
   exceptional back edges, and no Addr at a possible back-edge poll (including
   terminator arguments). These checks deliberately use the input graph and are
   conservative. *)
let allocation_polling_safety ~allocation_barriers ~live_addr_at_entry graph =
  let pointer_casts = ref false in
  let loop_safe = ref (Block.Tbl.length allocation_barriers = 0) in
  List.iter
    (fun block ->
      Array.iter
        (fun (instr : finished Instruction.t) ->
          match[@warning "-fragile-match"] instr with
          | Op { op = Reinterpret_cast (Int_of_value | Value_of_int); _ } ->
            pointer_casts := true
          | _ -> ())
        (Block.body block);
      (* [Cfg_polling] can only split normal edges to insert a poll. *)
      (match Block.exn_successor block with
      | Some handler when Block.dominates handler block -> loop_safe := false
      | Some _ | None -> ());
      let has_back_edge = ref false in
      List.iter
        (fun successor ->
          if Block.dominates successor block
          then begin
            has_back_edge := true;
            if Block.Tbl.mem live_addr_at_entry successor
            then loop_safe := false
          end)
        (Block.all_successors block);
      if !has_back_edge
      then
        iter_lowered_terminator_values
          (fun value ->
            if Cmm.equal_machtype_component (Value.typ value) Addr
            then loop_safe := false)
          (Block.terminator block))
    (Ssa.blocks graph);
  ( (not !pointer_casts)
    && not (Block.Tbl.mem live_addr_at_entry (Ssa.entry graph)),
    !loop_safe )

(* The state shared by the phases of the analysis. *)
type t =
  { loops : Ssa_loops.t;
    post_dominators : Ssa_post_dominators.t;
    homes : (finished, finished Block.t) Instruction.Id.Tbl.t;
        (* The block of each operation, markers excepted. *)
    uses : (finished, use list) Instruction.Id.Tbl.t;
        (* The uses of the results of each operation; markers are not uses. *)
    ends_region : unit Block.Tbl.t;
        (* The blocks whose body contains an [End_region]. *)
    allocations : (finished, allocation_group) Instruction.Id.Tbl.t;
        (* Every member of a construction maps to the same group. *)
    allocation_barriers : unit Block.Tbl.t;
    live_addr_at_entry : unit Block.Tbl.t;
    can_move_allocations : bool;
    can_move_loop_allocations : bool;
    markers : (finished Block.t * finished Instruction.op_data) list;
        (* The [Name_for_debugger] markers, with their blocks. *)
    decisions : (finished, decision) Instruction.Id.Tbl.t
        (* The decisions for the instructions that are not left in place, by
           id. *)
  }

(* Collect instruction uses, construction groups, and movement barriers. *)
let create ~loops (graph : finished Ssa.graph) : t =
  let homes = Instruction.Id.Tbl.create 256 in
  let uses = Instruction.Id.Tbl.create 256 in
  let ends_region = Block.Tbl.create 16 in
  let allocations = collect_allocations graph in
  let allocation_barriers = Block.Tbl.create 16 in
  let markers = ref [] in
  let add_use (value : finished Value.t) use =
    match value with
    | Res (op, _) ->
      let other_uses =
        Option.value (Instruction.Id.Tbl.find_opt uses op.id) ~default:[]
      in
      Instruction.Id.Tbl.replace uses op.id (use :: other_uses)
    | Block_param _ -> ()
  in
  let record_op block (op : finished Instruction.op_data) =
    if is_allocation_barrier allocations op
    then Block.Tbl.replace allocation_barriers block ();
    Instruction.Id.Tbl.replace homes op.id block;
    Array.iter (fun arg -> add_use arg (Op_use op)) op.args
  in
  List.iter
    (fun block ->
      Array.iter
        (fun (instr : finished Instruction.t) ->
          match[@warning "-fragile-match"] instr with
          | Op
              ({ id = _;
                 op = Name_for_debugger _;
                 typ = _;
                 args = _;
                 dbg = _;
                 usage_count = _;
                 name = _
               } as marker) ->
            markers := (block, marker) :: !markers
          | Op
              ({ id = _;
                 op = End_region;
                 typ = _;
                 args = _;
                 dbg = _;
                 usage_count = _;
                 name = _
               } as op) ->
            Block.Tbl.replace ends_region block ();
            record_op block op
          | Op op -> record_op block op
          | Push_trap _ | Pop_trap _ -> ())
        (Block.body block);
      iter_terminator_values
        (fun value -> add_use value (Terminator_use block))
        (Block.terminator block))
    (Ssa.blocks graph);
  let live_addr_at_entry = live_addr_at_entry ~homes graph in
  let can_move_allocations, can_move_loop_allocations =
    allocation_polling_safety ~allocation_barriers ~live_addr_at_entry graph
  in
  { loops;
    post_dominators = Ssa_post_dominators.compute graph;
    homes;
    uses;
    ends_region;
    allocations;
    allocation_barriers;
    live_addr_at_entry;
    can_move_allocations;
    can_move_loop_allocations;
    markers = !markers;
    decisions = Instruction.Id.Tbl.create 16
  }

(* The block where an operation will be, according to the decisions made so far.
   The sinking walk decides about the uses of an operation before the operation
   itself, so the answer is final by the time it is needed for a use. *)
let placement t (op : finished Instruction.op_data) : finished Block.t =
  match Instruction.Id.Tbl.find_opt t.decisions op.id with
  | Some (Sink block) -> block
  | Some Drop | None -> Instruction.Id.Tbl.find t.homes op.id

let use_block t (use : use) : finished Block.t =
  match use with Op_use op -> placement t op | Terminator_use block -> block

let value_block t (value : finished Value.t) : finished Block.t =
  match value with
  | Res (op, _) -> placement t op
  | Block_param (block, _) -> block

(* Whether a barrier may execute between the end of [def_block] and the start of
   [target]. Every path to [target] goes through [def_block], so the search
   stops at [def_block] and at blocks it does not dominate. *)
let crosses_barrier barriers ~def_block ~target : bool =
  let visited = Block.Tbl.create 16 in
  let rec from block =
    (not (Block.equal block def_block))
    && Block.dominates def_block block
    && (not (Block.Tbl.mem visited block))
    && begin
      Block.Tbl.replace visited block ();
      Block.Tbl.mem barriers block
      || List.exists from (Block.predecessors block)
    end
  in
  List.exists from (Block.predecessors target)

(* Whether an operation of [def_block] may be sunk to [candidate], a block that
   [def_block] strictly dominates and that dominates all the uses. *)
let is_allowed t ~def_block ~sinkable candidate : bool =
  (match Ssa_loops.innermost_loop t.loops candidate with
    | None -> true
    | Some loop -> Ssa_loops.Loop.contains loop def_block)
  && ((Block.cold candidate && not (Block.cold def_block))
     || not
          (Ssa_post_dominators.post_dominates t.post_dominators candidate
             def_block))
  &&
  match sinkable with
  | Can_cross_end_region -> true
  | Blocked_by_end_region ->
    not (crosses_barrier t.ends_region ~def_block ~target:candidate)
  | Heap_allocation ->
    (not (Block.Tbl.mem t.live_addr_at_entry candidate))
    && not (crosses_barrier t.allocation_barriers ~def_block ~target:candidate)

(* The deepest allowed block on the dominator tree path from [candidate] up to,
   and excluding, [def_block]. *)
let rec choose_target t ~def_block ~sinkable candidate : finished Block.t option
    =
  if Block.dominator_depth candidate <= Block.dominator_depth def_block
  then None
  else if is_allowed t ~def_block ~sinkable candidate
  then Some candidate
  else
    choose_target t ~def_block ~sinkable (Block.immediate_dominator candidate)

(* Find a destination dominating the uses at their already-decided positions.
   An empty use list is left to dead-code elimination, not sinking. *)
(* CR-someday xclerc: an instruction is never duplicated, so an operation whose
   uses are in sibling branches only sinks down to their common dominator. A
   copy in each of these branches would remove the operation from the paths
   that do not use it, at the cost of code size, and the heuristics deciding
   when this pays off are not clear yet. The same question arises for spill
   moves, which can likewise be placed once at a common dominator or duplicated
   into the branches that need them. *)
let target_for_uses t ~block ~sinkable uses =
  match uses with
  | [] -> None
  | use :: other_uses ->
    let common_dominator =
      List.fold_left
        (fun acc use -> Block.common_dominator acc (use_block t use))
        (use_block t use) other_uses
    in
    choose_target t ~def_block:block ~sinkable common_dominator

let uses_of t (op : finished Instruction.op_data) =
  Option.value (Instruction.Id.Tbl.find_opt t.uses op.id) ~default:[]

let sink_operation t ~block ~sinkable (op : finished Instruction.op_data) =
  target_for_uses t ~block ~sinkable (uses_of t op)
  |> Option.iter (fun target ->
      Instruction.Id.Tbl.replace t.decisions op.id (Sink target))

let sink_allocation t ~block group =
  let is_member (op : finished Instruction.op_data) =
    match Instruction.Id.Tbl.find_opt t.allocations op.id with
    | None -> false
    | Some other ->
      Instruction.Id.equal group.construction.allocation.id
        other.construction.allocation.id
  in
  let is_external_addr (value : finished Value.t) =
    Cmm.equal_machtype_component (Value.typ value) Addr
    &&
    match value with
    | Res (op, _) -> not (is_member op)
    | Block_param _ -> true
  in
  let members = group_members group in
  if
    t.can_move_allocations
    && (match Ssa_loops.innermost_loop t.loops block with
      | None -> true
      | Some _ -> t.can_move_loop_allocations)
    && not
         (List.exists
            (fun (op : finished Instruction.op_data) ->
              Array.exists is_external_addr op.args)
            members)
  then begin
    let uses =
      List.concat_map (uses_of t) members
      |> List.filter (function
        | Op_use op -> not (is_member op)
        | Terminator_use _ -> true)
    in
    target_for_uses t ~block ~sinkable:Heap_allocation uses
    |> Option.iter (fun target ->
        List.iter
          (fun (op : finished Instruction.op_data) ->
            Instruction.Id.Tbl.replace t.decisions op.id (Sink target))
          members)
  end

(* Every block comes after its dominators in [blocks], so in the reverse order,
   the uses of an operation are visited before the operation. *)
(* CR xclerc for ttebbi: this relies on the order documented for [Ssa.blocks],
   as does the dominators-first walk of [Ssa_reducer]. If that order is not
   meant to be a guarantee that passes can rely on, this should instead walk the
   dominator tree explicitly. *)
let sink_operations t (blocks : finished Block.t list) : unit =
  List.iter
    (fun block ->
      let body = Block.body block in
      (* Whether an [End_region] follows the current instruction in [block]: an
         operation blocked by [End_region] cannot then leave the block. *)
      let end_region_below = ref false in
      let allocation_barrier_below = ref false in
      for i = Array.length body - 1 downto 0 do
        match body.(i) with
        | Op op -> (
          (match[@warning "-fragile-match"] op.op with
          | End_region -> end_region_below := true
          | _ -> ());
          if is_allocation_barrier t.allocations op
          then allocation_barrier_below := true;
          match Instruction.Id.Tbl.find_opt t.allocations op.id with
          | Some group ->
            (* Decide at the last initializer, before visiting any field
               computations interleaved with the group's members. *)
            if
              Instruction.Id.equal op.id group.last
              && not !allocation_barrier_below
            then sink_allocation t ~block group
          | None -> (
            if not (has_addr_arg op)
            then
              match sinkable op with
              | None -> ()
              | Some Blocked_by_end_region when !end_region_below -> ()
              | Some
                  (( Can_cross_end_region | Blocked_by_end_region
                   | Heap_allocation ) as sinkable) ->
                sink_operation t ~block ~sinkable op))
        | Push_trap _ | Pop_trap _ -> ()
      done)
    (List.rev blocks)

(* Where a marker of [block] goes: the deepest block where the values it names
   are, if that block is dominated by [block] and by the blocks of all these
   values; [None] if the values are all still available in [block]. *)
(* CR xclerc: a marker is dropped when no such block exists, e.g. when the
   values it names sink into different branches, and the variable then becomes
   invisible to the debugger. Alternatives: duplicate the marker in each of
   these blocks, or treat markers as uses (which would prevent most sinking
   under [-g]). *)
let marker_decision t block (marker : finished Instruction.op_data) :
    decision option =
  let value_blocks = Array.map (value_block t) marker.args in
  if
    Array.for_all
      (fun value_block -> Block.dominates value_block block)
      value_blocks
  then None
  else
    let deepest =
      Array.fold_left
        (fun deepest value_block ->
          if Block.dominator_depth value_block > Block.dominator_depth deepest
          then value_block
          else deepest)
        value_blocks.(0) value_blocks
    in
    if
      Block.dominates block deepest
      && Array.for_all
           (fun value_block -> Block.dominates value_block deepest)
           value_blocks
    then Some (Sink deepest)
    else Some Drop

let place_markers t : unit =
  List.iter
    (fun (block, (marker : finished Instruction.op_data)) ->
      marker_decision t block marker
      |> Option.iter (fun decision ->
          Instruction.Id.Tbl.replace t.decisions marker.id decision))
    t.markers

(* The decisions for the instructions that are not left in place, by id. There
   are none for a function with irreducible control flow. *)
let analyze (graph : finished Ssa.graph) :
    (finished, decision) Instruction.Id.Tbl.t =
  match Ssa_loops.compute graph with
  | None -> Instruction.Id.Tbl.create 0
  | Some loops ->
    let t = create ~loops graph in
    sink_operations t (Ssa.blocks graph);
    place_markers t;
    t.decisions

module Sink_reducer : Reducer = struct
  include Default_reducer

  type t = (finished, decision) Instruction.Id.Tbl.t

  let create ctx = analyze (Context.in_graph ctx)

  let visit_instruction t block ~instr_index =
    match Array.get (Block.body block) instr_index with
    | Push_trap _ | Pop_trap _ -> Unchanged
    | Op { id; op = _; typ = _; args = _; dbg = _; usage_count = _; name = _ }
      -> (
      match Instruction.Id.Tbl.find_opt t id with
      | None -> Unchanged
      | Some (Sink target) -> Move_to target
      | Some Drop -> Reduce (fun _c -> [||]))
end

module Runner = Make_run (Sink_reducer)

let run ~ppf_dump ssa = Runner.run ~ppf_dump ssa
