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

    Debugger markers ([Name_for_debugger]) are not considered uses: they follow
    the values they name, and are dropped in the rare case where these values
    end up in unrelated blocks. *)

module Cse = Cfg_cse.Cse_generic (CSE)

(* How an operation may be sunk, if at all: anywhere its arguments are
   available, or additionally not past an [End_region], which may free the
   memory a load reads, or invalidate a local allocation that is an argument. *)
type sinkable =
  | Can_cross_end_region
  | Blocked_by_end_region

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
    (* CR-someday xclerc: sink allocations as well. Their initializing stores
       would have to move along, local allocations are tied to their region, and
       the zero-alloc checker, which runs later, would see the change. *)
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

(* Where a value is used: by an operation, whose block may change as it is sunk,
   or by the terminator of a block. *)
type use =
  | Op_use of finished Instruction.op_data
  | Terminator_use of finished Block.t

type decision =
  | Sink of finished Block.t
  | Drop

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
    markers : (finished Block.t * finished Instruction.op_data) list;
        (* The [Name_for_debugger] markers, with their blocks. *)
    decisions : (finished, decision) Instruction.Id.Tbl.t
        (* The decisions for the instructions that are not left in place, by
           id. *)
  }

(* The pass over the graph that collects [homes], [uses], [ends_region] and
   [markers]. *)
let create ~loops (graph : finished Ssa.graph) : t =
  let homes = Instruction.Id.Tbl.create 256 in
  let uses = Instruction.Id.Tbl.create 256 in
  let ends_region = Block.Tbl.create 16 in
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
  { loops;
    post_dominators = Ssa_post_dominators.compute graph;
    homes;
    uses;
    ends_region;
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

(* Whether an [End_region] may execute on a path from the end of [def_block] to
   the start of [target], which [def_block] strictly dominates: whether a block
   other than [def_block] on such a path contains one. Every path to [target]
   goes through [def_block], so these blocks are dominated by [def_block]: the
   search over the predecessors of [target] stops at [def_block] and at the
   blocks it does not dominate. *)
let crosses_end_region t ~def_block ~target : bool =
  let visited = Block.Tbl.create 16 in
  let rec from block =
    (not (Block.equal block def_block))
    && Block.dominates def_block block
    && (not (Block.Tbl.mem visited block))
    && begin
      Block.Tbl.replace visited block ();
      Block.Tbl.mem t.ends_region block
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
    not (crosses_end_region t ~def_block ~target:candidate)

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

(* Decide where the operation [op] of [block] goes, if it is not left in
   place. *)
(* CR-someday xclerc: an instruction is never duplicated, so an operation whose
   uses are in sibling branches only sinks down to their common dominator. A
   copy in each of these branches would remove the operation from the paths
   that do not use it, at the cost of code size, and the heuristics deciding
   when this pays off are not clear yet. The same question arises for spill
   moves, which can likewise be placed once at a common dominator or duplicated
   into the branches that need them. *)
let sink_operation t ~block ~sinkable (op : finished Instruction.op_data) : unit
    =
  match Instruction.Id.Tbl.find_opt t.uses op.id with
  | None | Some [] -> ()
  | Some (use :: other_uses) ->
    let common_dominator =
      List.fold_left
        (fun acc use -> Block.common_dominator acc (use_block t use))
        (use_block t use) other_uses
    in
    choose_target t ~def_block:block ~sinkable common_dominator
    |> Option.iter (fun target ->
        Instruction.Id.Tbl.replace t.decisions op.id (Sink target))

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
      for i = Array.length body - 1 downto 0 do
        match[@warning "-fragile-match"] body.(i) with
        | Op
            { id = _;
              op = End_region;
              typ = _;
              args = _;
              dbg = _;
              usage_count = _;
              name = _
            } ->
          end_region_below := true
        | Op op when not (has_addr_arg op) -> (
          match sinkable op with
          | None -> ()
          | Some Blocked_by_end_region when !end_region_below -> ()
          | Some sinkable -> sink_operation t ~block ~sinkable op)
        | Op _ | Push_trap _ | Pop_trap _ -> ()
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
