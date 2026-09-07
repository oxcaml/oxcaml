[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

(* Static block layout: permute the layout (and only the layout) to (1) sink
   cold blocks to the end of the function and (2) place a likely successor of
   each block immediately after it, so that [Cfg_to_linear] can turn the edge
   into a fallthrough instead of a branch. The CFG itself is not modified;
   [Cfg_to_linear] repairs stack offsets at layout boundaries with
   [Ladjust_stack_offset], making the permutation transparent to the emitted
   code.

   The only constraint is that the entry block remains first (the first block of
   the layout is emitted without a label). Any other permutation is legal:
   [Cfg_to_linear] emits a starting label whenever the previous block cannot
   fall through.

   With shrink-wrapping, the DWARF CFI of blocks executed before the prologue
   but laid out after post-prologue blocks is imprecise for asynchronous
   unwinding; this is a pre-existing property of the emitter (which resets the
   CFA at [Lprologue]) that the permutation performed here does not change. *)

(* A block is considered hot if it is reachable from the entry block through
   non-cold blocks only. In particular, blocks reachable only through cold
   blocks are effectively cold themselves. *)
let compute_hot_labels (cfg : Cfg.t) =
  let hot = ref Label.Set.empty in
  let queue = Queue.create () in
  let entry = Cfg.entry_label cfg in
  hot := Label.Set.add entry !hot;
  Queue.add entry queue;
  while not (Queue.is_empty queue) do
    let block = Cfg.get_block_exn cfg (Queue.pop queue) in
    Label.Set.iter
      (fun succ ->
        if
          (not (Label.Set.mem succ !hot))
          && not (Cfg.get_block_exn cfg succ).cold
        then (
          hot := Label.Set.add succ !hot;
          Queue.add succ queue))
      (* CR-soon xclerc: revisit the [~exn:true] part. This is tricky because
         exceptions could be used for "legitimate" control flow and thus not be
         a trustworthy indicator that the code following an exceptional edge is
         cold. *)
      (Cfg.successor_labels ~normal:true ~exn:true block)
  done;
  !hot

(* Successors of [block] that are candidates to be placed immediately after it,
   in decreasing order of preference. Ties between the outcomes of a test are
   broken using the original layout order, thereby preserving the branch
   structure chosen at selection. *)
(* CR-someday xclerc: the position-based tie-break sorts back-edge targets
   first, and only the [placed] filter in [grow_chain] prevents chaining them.
   That such targets are always placed by then relies on the target dominating
   the source (natural loops) and on dominators preceding the blocks they
   dominate in the original layout; neither is enforced, and the former does
   not apply to irreducible flow. If the assumption ever broke, the layout
   would remain legal, merely rotating the loop (back edge as fallthrough). *)
let preferred_successors (block : Cfg.basic_block) ~position =
  let[@inline] find_position label =
    match Label.Tbl.find_opt position label with
    | Some index -> index
    | None ->
      Misc.fatal_errorf
        "Cfg_block_layout.preferred_successors: block %a is not in the layout"
        Label.print label
  in
  let sorted_distinct labels =
    List.sort_uniq
      (fun left right -> compare (find_position left) (find_position right))
      labels
  in
  match block.terminator.desc with
  | Always label -> [label]
  | Call { op = _; label_after } | Prim { op = _; label_after } -> [label_after]
  | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
    sorted_distinct [ifso; ifnot]
  | Int_test { lt; eq; gt; is_signed = _; imm = _ } ->
    sorted_distinct [lt; eq; gt]
  | Float_test { width = _; lt; eq; gt; uo } ->
    (* The unordered outcome is assumed unlikely, and is used as a fallthrough
       only if no other successor is available. *)
    sorted_distinct [lt; eq; gt] @ [uo]
  | Never | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Invalid _ ->
    (* Either no successor, or no fallthrough at linearization ([Switch],
       [Tailcall_self]), so adjacency brings no benefit. *)
    []

(* Computes the new layout: chains of hot blocks, followed by the cold blocks in
   their original relative order. [layout] is not modified. *)
let compute_new_layout (cfg : Cfg.t) (layout : Label.t DLL.t) =
  let position = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  DLL.iteri layout ~f:(fun i label -> Label.Tbl.replace position label i);
  let hot_labels = compute_hot_labels cfg in
  let placed = ref Label.Set.empty in
  let hot_layout = DLL.make_empty () in
  let is_candidate label =
    Label.Set.mem label hot_labels && not (Label.Set.mem label !placed)
  in
  let rec grow_chain label =
    placed := Label.Set.add label !placed;
    DLL.add_end hot_layout label;
    let block = Cfg.get_block_exn cfg label in
    match List.find_opt is_candidate (preferred_successors block ~position) with
    | Some next -> grow_chain next
    | None -> ()
  in
  (* Grow a chain from each not-yet-placed hot block, in original layout order.
     The first chain necessarily starts at the entry block, which therefore
     remains first. Cold blocks are appended afterwards, keeping their original
     relative order. *)
  DLL.iter layout ~f:(fun seed -> if is_candidate seed then grow_chain seed);
  let cold_layout = DLL.make_empty () in
  DLL.iter layout ~f:(fun label ->
      if not (Label.Set.mem label hot_labels) then DLL.add_end cold_layout label);
  DLL.transfer ~to_:hot_layout ~from:cold_layout ();
  hot_layout

let run (cfg_with_layout : Cfg_with_layout.t) =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let layout = Cfg_with_layout.layout cfg_with_layout in
  if DLL.compare_length_with layout 2 > 0
  then (
    let new_layout = compute_new_layout cfg layout in
    (match DLL.hd new_layout with
    | Some first when Label.equal first (Cfg.entry_label cfg) -> ()
    | Some _ | None ->
      Misc.fatal_errorf "Cfg_block_layout: entry block is not first (%s)"
        (Cfg.fun_name cfg));
    if DLL.compare_lengths new_layout layout <> 0
    then
      Misc.fatal_errorf "Cfg_block_layout: layout size changed (%s)"
        (Cfg.fun_name cfg);
    Cfg_with_layout.set_layout cfg_with_layout new_layout)
