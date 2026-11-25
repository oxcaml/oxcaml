(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Doubly_linked_list

let gate = "__sancov_should_track"

let callback = "__sanitizer_cov_trace_pc_guard"

let init = "__sanitizer_cov_trace_pc_guard_init"

let guards_start = "__start___sancov_guards"

let guards_stop = "__stop___sancov_guards"

let constructor = Asm_targets.Asm_section.trace_pc_guard_constructor

(* A block containing nothing but an unreachable terminator is never executed:
   instrumenting it would only add a guard that is never hit. *)
let is_unreachable (block : Cfg.basic_block) =
  DLL.is_empty block.body
  &&
  match block.terminator.desc with
  | Invalid { label_after = None; _ } -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | External_call_no_return _
  | Invalid { label_after = Some _; _ }
  | Call _ | Prim _ ->
    false

(* A probe can only record a critical edge if a block is inserted on it.
   Exceptional edges cannot be split, but they still count towards the number of
   successors. Edges to unreachable blocks are left alone. *)
let split_critical_edges cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let critical_edges =
    Cfg_edge.Set.filter
      (fun { Cfg_edge.src = _; dst } ->
        not (is_unreachable (Cfg.get_block_exn cfg dst)))
      (Regalloc_rewrite.compute_critical_edges ~all:true cfg)
  in
  Cfg_edge.Set.iter
    (fun { Cfg_edge.src; dst } ->
      let (_ : Cfg.basic_block list) =
        Cfg_with_layout.insert_block cfg_with_layout (DLL.make_empty ())
          ~after:(Cfg.get_block_exn cfg src)
          ~before:(Some (Cfg.get_block_exn cfg dst))
      in
      ())
    critical_edges;
  not (Cfg_edge.Set.is_empty critical_edges)

(* CR-soon xclerc: use the upcoming generic framework computing
   (post-)dominators on Cfg/SSA instead of this module. *)
module Post_dominators : sig
  type t

  val build : Cfg.t -> t

  (** [is_post_dominating t x y] is [true] iff all paths from [y] to the exit of
      the function go through [x]. *)
  val is_post_dominating : t -> Label.t -> Label.t -> bool
end = struct
  (* The algorithm of Cooper, Harvey and Kennedy ("A Simple, Fast Dominance
     Algorithm") applied to the reversed graph, whose root is a virtual exit
     node succeeding every block without successors. Nodes are numbered in
     postorder of the reversed graph, so the root has the highest number. Blocks
     from which no exit is reachable (infinite loops) are not numbered: they
     neither post-dominate nor are post-dominated by any other block. *)
  type t =
    { number : int Label.Tbl.t;
      ipdom : int array
    }

  let successors cfg label =
    Cfg.successor_labels ~normal:true ~exn:true (Cfg.get_block_exn cfg label)

  let build cfg =
    let exits =
      Cfg.fold_blocks cfg ~init:[] ~f:(fun label block acc ->
          if
            Label.Set.is_empty
              (Cfg.successor_labels ~normal:true ~exn:true block)
          then label :: acc
          else acc)
    in
    let number = Label.Tbl.create 32 in
    let postorder = ref [] in
    let counter = ref 0 in
    (* Iterative depth-first search over predecessors, to avoid deep recursion
       on large functions. *)
    let visited = Label.Tbl.create 32 in
    let rec visit stack =
      match stack with
      | [] -> ()
      | (label, children) :: rest -> (
        match children with
        | [] ->
          Label.Tbl.replace number label !counter;
          incr counter;
          postorder := label :: !postorder;
          visit rest
        | child :: children ->
          let stack = (label, children) :: rest in
          if Label.Tbl.mem visited child
          then visit stack
          else (
            Label.Tbl.replace visited child ();
            let block = Cfg.get_block_exn cfg child in
            visit ((child, Label.Set.elements block.predecessors) :: stack)))
    in
    List.iter
      (fun exit ->
        if not (Label.Tbl.mem visited exit)
        then (
          Label.Tbl.replace visited exit ();
          let block = Cfg.get_block_exn cfg exit in
          visit [exit, Label.Set.elements block.predecessors]))
      exits;
    let root = !counter in
    let ipdom = Array.make (root + 1) (-1) in
    ipdom.(root) <- root;
    let is_exit = Label.Tbl.create 32 in
    List.iter (fun exit -> Label.Tbl.replace is_exit exit ()) exits;
    let rec intersect b1 b2 =
      if b1 = b2
      then b1
      else if b1 < b2
      then intersect ipdom.(b1) b2
      else intersect b1 ipdom.(b2)
    in
    (* [!postorder] lists the blocks in reverse postorder, i.e. from the one
       closest to the root. *)
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun label ->
          let b = Label.Tbl.find number label in
          let candidates =
            Label.Set.fold
              (fun successor acc ->
                match Label.Tbl.find_opt number successor with
                | Some n when ipdom.(n) >= 0 -> n :: acc
                | Some _ | None -> acc)
              (successors cfg label)
              (if Label.Tbl.mem is_exit label then [root] else [])
          in
          match candidates with
          | [] -> ()
          | first :: rest ->
            let new_ipdom = List.fold_left intersect first rest in
            if ipdom.(b) <> new_ipdom
            then (
              ipdom.(b) <- new_ipdom;
              changed := true))
        !postorder
    done;
    { number; ipdom }

  let is_post_dominating t x y =
    match Label.Tbl.find_opt t.number x, Label.Tbl.find_opt t.number y with
    | None, _ | _, None -> Label.equal x y
    | Some x, Some y ->
      let root = Array.length t.ipdom - 1 in
      let rec walk n =
        if n = x then true else if n = root then false else walk t.ipdom.(n)
      in
      walk y
end

(* The pruning of LLVM's [shouldInstrumentBlock]: a block that dominates all of
   its successors is covered by them, and so is a block with several
   predecessors that post-dominates all of them. The block executed on function
   entry is always instrumented. *)
let should_instrument ~dominators ~post_dominators ~is_entry
    (block : Cfg.basic_block) =
  is_entry
  ||
  let successors = Cfg.successor_labels ~normal:true ~exn:true block in
  let is_full_dominator =
    (not (Label.Set.is_empty successors))
    && Label.Set.for_all
         (fun successor ->
           Cfg_dominators.is_dominating dominators block.start successor)
         successors
  in
  let is_full_post_dominator =
    (not (Label.Set.is_empty block.predecessors))
    && Label.Set.for_all
         (fun predecessor ->
           Post_dominators.is_post_dominating post_dominators block.start
             predecessor)
         block.predecessors
  in
  let has_single_predecessor = Label.Set.cardinal block.predecessors = 1 in
  (not is_full_dominator)
  && not (is_full_post_dominator && not has_single_predecessor)

(* The block treated as the function entry by [should_instrument] and
   [probe_dbg]. The entry block of a Cfg is often an empty block that jumps to
   the first real block; that block is then executed exactly when the function
   is, so it is used instead and the empty block is pruned like any other. *)
let effective_entry (cfg : Cfg.t) =
  let entry = Cfg.get_block_exn cfg cfg.entry_label in
  match entry.terminator.desc with
  | Always target ->
    if
      DLL.is_empty entry.body
      && Label.Set.equal (Cfg.get_block_exn cfg target).predecessors
           (Label.Set.singleton cfg.entry_label)
    then target
    else cfg.entry_label
  | Never | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ | Switch _
  | Return | Raise _ | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | External_call_no_return _ | Invalid _ | Call _ | Prim _ ->
    cfg.entry_label

(* The debuginfo of the probe of [block]. The callback records its return
   address, which coverage tools map to a source line, so this determines the
   line that the probe covers. The first instructions of a block (moves,
   constants, jumps) usually have no location, so this is the first location in
   the block; failing that, the function's location for the entry probe, or the
   location of the branch leading to the block if all predecessors that have one
   agree. Otherwise it is the function's location: reaching the block implies
   that the function was entered, so this never reports a line that was not
   executed as covered, whereas a probe without any location could be attributed
   by the assembler to the line of unrelated code. *)
let probe_dbg (cfg : Cfg.t) (block : Cfg.basic_block) ~is_entry =
  let has_line (dbg : Debuginfo.t) = Emitaux.has_source_line dbg in
  let in_block =
    match
      DLL.find_opt block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
          has_line instr.dbg)
    with
    | Some instr -> Some instr.dbg
    | None ->
      if has_line block.terminator.dbg then Some block.terminator.dbg else None
  in
  match in_block with
  | Some dbg -> dbg
  | None when is_entry && has_line cfg.fun_dbg -> cfg.fun_dbg
  | None -> (
    let branch_dbgs =
      Label.Set.fold
        (fun predecessor acc ->
          let dbg = (Cfg.get_block_exn cfg predecessor).terminator.dbg in
          if
            has_line dbg
            && not (List.exists (fun d -> Debuginfo.compare d dbg = 0) acc)
          then dbg :: acc
          else acc)
        block.predecessors []
    in
    match branch_dbgs with [dbg] -> dbg | [] | _ :: _ :: _ -> cfg.fun_dbg)

(* A probe to be added at the start of a block. *)
type probe =
  { label : Label.t;
    dbg : Debuginfo.t
  }

(* A probe splits its block in three:

   - the block itself, which keeps its label, loads the gate and tests it;

   - a cold block calling the callback, which is a single operation preserving
   all registers;

   - a block with the original body and terminator, where both paths join.

   The emitter gives each call its own guard. [index] makes the calls distinct,
   so that otherwise identical instrumented blocks are not merged; it is unique
   within the function. All instructions of a probe have its debuginfo, which
   gives the call its line and its inlined frames. *)
let instrument_block cfg_with_layout { label; dbg } ~index =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let block = Cfg.get_block_exn cfg label in
  let stack_offset = Cfg.first_instruction_stack_offset block in
  let make_instruction desc ~arg ~res =
    Cfg.make_instruction ~desc ~arg ~res ~dbg ~stack_offset
      ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
      ()
  in
  let rest_label = Label.new_label () in
  let call_label = Label.new_label () in
  let rest : Cfg.basic_block =
    { start = rest_label;
      body = DLL.make_empty ();
      terminator = block.terminator;
      predecessors = Label.Set.of_list [label; call_label];
      stack_offset;
      exn = block.exn;
      can_raise = block.can_raise;
      is_trap_handler = false;
      cold = block.cold
    }
  in
  DLL.transfer ~to_:rest.body ~from:block.body ();
  Label.Set.iter
    (fun successor ->
      let successor = Cfg.get_block_exn cfg successor in
      successor.predecessors
        <- Label.Set.add rest_label
             (Label.Set.remove label successor.predecessors))
    (Cfg.successor_labels ~normal:true ~exn:true rest);
  let call : Cfg.basic_block =
    { start = call_label;
      body = DLL.make_empty ();
      terminator =
        make_instruction (Always rest_label : Cfg.terminator) ~arg:[||] ~res:[||];
      predecessors = Label.Set.singleton label;
      stack_offset;
      exn = None;
      can_raise = false;
      is_trap_handler = false;
      cold = true
    }
  in
  DLL.add_end call.body
    (make_instruction
       (Op (Trace_pc_guard index) : Cfg.basic)
       ~arg:[||] ~res:[||]);
  let gate = Reg.create Int in
  DLL.add_end block.body
    (make_instruction
       (Op Trace_pc_guard_gate : Cfg.basic)
       ~arg:[||] ~res:[| gate |]);
  block.terminator
    <- make_instruction
         (Truth_test { ifso = call_label; ifnot = rest_label } : Cfg.terminator)
         ~arg:[| gate |] ~res:[||];
  block.exn <- None;
  block.can_raise <- false;
  Cfg_with_layout.add_block cfg_with_layout rest ~after:label;
  let last =
    match DLL.last (Cfg_with_layout.layout cfg_with_layout) with
    | Some last -> last
    | None -> Misc.fatal_error "Cfg_trace_pc_guard: empty layout"
  in
  Cfg_with_layout.add_block cfg_with_layout call ~after:last

let run cfg_with_infos =
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  if split_critical_edges cfg_with_layout
  then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
  let dominators = Cfg_with_infos.dominators cfg_with_infos in
  let post_dominators = Post_dominators.build cfg in
  (* Probes are chosen before any is added, since adding one changes the graph.
     Blocks are visited in layout order so that probes are numbered
     deterministically. *)
  let entry_label = effective_entry cfg in
  let probes =
    DLL.fold_left (Cfg_with_layout.layout cfg_with_layout) ~init:[]
      ~f:(fun probes label ->
        let block = Cfg.get_block_exn cfg label in
        let is_entry = Label.equal label entry_label in
        if
          (not (is_unreachable block))
          && should_instrument ~dominators ~post_dominators ~is_entry block
        then { label; dbg = probe_dbg cfg block ~is_entry } :: probes
        else probes)
  in
  List.iteri
    (fun index probe -> instrument_block cfg_with_layout probe ~index)
    (List.rev probes);
  if not (Misc.Stdlib.List.is_empty probes)
  then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
  Cfg_with_infos.invalidate_liveness cfg_with_infos;
  cfg_with_infos
