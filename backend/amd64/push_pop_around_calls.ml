[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list

let is_gp_register (reg : Reg.t) =
  match Regs.Reg_class.of_machtype reg.typ with
  | Regs.GPR -> true
  | Regs.SIMD -> false

let unsupported_after_push (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  | Op (Spill | Reload) -> false
  | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
  | Op (Stackoffset _)
  | Reloadretaddr
  (* GC or other hidden calls are not supported in the middle of using push to
     spill. *)
  | Op (Alloc _ | Poll | Specific _) ->
    true
  | Op
      ( Move | Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
      | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Load _ | Store _
      | Intop _ | Int128op _ | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _
      | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _ | Opaque
      | Begin_region | End_region | Name_for_debugger _ | Dls_get | Tls_get
      | Domain_index | Pause ) ->
    Array.exists Reg.is_stack instr.arg || Array.exists Reg.is_stack instr.res

(* Collect consecutive reload instructions from the start of a block body, as
   long as no destination register is repeated. Returns a map from spill slot to
   reload instruction. *)
let collect_leading_reloads cfg_with_infos (body : Cfg.basic_instruction_list) =
  let reloads = ref Reg.Map.empty in
  let seen_regs = ref Reg.Set.empty in
  let rec loop (cell : Cfg.basic Cfg.instruction DLL.cell) =
    let instr = DLL.value cell in
    if Cfg.is_reload instr
    then begin
      let spill_slot = instr.arg.(0) in
      let r = instr.res.(0) in
      let live_across =
        (Cfg_with_infos.liveness_find cfg_with_infos instr.id).across
      in
      if
        (not (Reg.Set.mem spill_slot live_across))
        && not (Reg.Set.mem r !seen_regs)
      then reloads := Reg.Map.add spill_slot instr !reloads;
      seen_regs := Reg.Set.add r !seen_regs;
      Option.iter loop (DLL.next cell)
    end
  in
  Option.iter loop (DLL.hd_cell body);
  !reloads

(* Collect spills from the end of a block body, walking backwards. Stops at any
   incompatible instruction. Filters out spills whose slot is read by a later
   instruction in the block (between the spill and the call) or by the exception
   handler. Reads in the after-block (via the matching reloads) are
   intentionally not considered: we are about to replace those reloads with
   pop. *)
let collect_spills_backwards cfg_with_infos (block : Cfg.basic_block) =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let exn_entry_live =
    match block.exn with
    | None -> Reg.Set.empty
    | Some exn_label ->
      let exn_first_id =
        Cfg.first_instruction_id (Cfg.get_block_exn cfg exn_label)
      in
      (Cfg_with_infos.liveness_find cfg_with_infos exn_first_id).before
  in
  let body = block.body in
  let spills = ref [] in
  let used_after = ref exn_entry_live in
  let add_args args = used_after := Reg.add_set_array !used_after args in
  let rec loop cell =
    let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
    if unsupported_after_push instr
    then ()
    else begin
      if
        Cfg.is_spill instr
        && (not (Reg.Set.mem instr.res.(0) !used_after))
        && is_gp_register instr.arg.(0)
      then begin
        spills := instr :: !spills;
        (* Also mark the spill slot as used, to avoid collecting two spills to
           the same slot. *)
        used_after := Reg.Set.add instr.res.(0) !used_after
      end
      else add_args instr.arg;
      Option.iter loop (DLL.prev cell)
    end
  in
  (match DLL.last_cell body with Some cell -> loop cell | None -> ());
  !spills

let label_after_call (block : Cfg.basic_block) =
  match block.terminator.desc with
  | Call { label_after; _ } -> Some label_after
  | Prim _ | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Invalid _ ->
    None

(* Follow a chain of empty [Always] blocks from [label] to find the block
   holding the post-call reloads. The regalloc rewrite pass may insert
   intermediate blocks for call result spills; their stack_offsets will be
   updated alongside the reloads block. *)
let rec find_reload_block (cfg : Cfg.t) label =
  let blk = Cfg.get_block_exn cfg label in
  if Label.Set.cardinal blk.predecessors <> 1
  then None
  else if DLL.is_empty blk.body
  then
    match[@ocaml.warning "-4"] blk.terminator.desc with
    | Always next -> find_reload_block cfg next
    | _ -> Some blk
  else Some blk

(* From the candidate spills and the per-slot reload map, keep only spills with
   a matching reload, trim to a multiple of the call stack alignment, and pair
   each remaining spill with its reload. Returned lists have the same length and
   are aligned by index. *)
let match_spills_to_reloads (spills : Cfg.basic Cfg.instruction list)
    (reloads_map : Cfg.basic Cfg.instruction Reg.Map.t) =
  let spills =
    List.filter
      (fun (instr : Cfg.basic Cfg.instruction) ->
        Reg.Map.mem instr.res.(0) reloads_map)
      spills
  in
  let n = List.length spills in
  let n = n - (n mod (Arch.call_stack_alignment / Arch.size_addr)) in
  let spills = List.filteri (fun i _ -> i < n) spills in
  let reloads =
    List.map
      (fun (spill : Cfg.basic Cfg.instruction) ->
        Reg.Map.find spill.res.(0) reloads_map)
      spills
  in
  spills, reloads

(* Replace each selected spill with a [push] instruction. Stack offsets of all
   instructions and blocks between (and including) the first push and the first
   post-call reload block are bumped to reflect the additional pushed slots. On
   entry [stack_offset] starts at [base_stack_offset + n * size_addr] and ends
   at [base_stack_offset]. *)
let replace_spills_with_pushes (cfg : Cfg.t)
    ~(spills : Cfg.basic Cfg.instruction list) ~base_stack_offset
    (after_block : Cfg.basic_block) =
  let now_unused_spill_slots =
    List.fold_left
      (fun acc (spill : Cfg.basic Cfg.instruction) ->
        Reg.Set.add spill.res.(0) acc)
      Reg.Set.empty spills
  in
  let selected_spill_ids =
    List.fold_left
      (fun acc (spill : Cfg.basic Cfg.instruction) ->
        InstructionId.Set.add spill.id acc)
      InstructionId.Set.empty spills
  in
  let unprocessed_stack_regs = ref now_unused_spill_slots in
  let stack_offset =
    ref (base_stack_offset + (List.length spills * Arch.size_addr))
  in
  let rewrite_instr (type a) (instr : a Cfg.instruction) cell =
    assert (instr.stack_offset = base_stack_offset);
    instr.stack_offset <- !stack_offset;
    if InstructionId.Set.mem instr.id selected_spill_ids
    then begin
      unprocessed_stack_regs
        := Reg.Set.remove instr.res.(0) !unprocessed_stack_regs;
      stack_offset := !stack_offset - Arch.size_addr;
      let push =
        Cfg.make_instruction ~desc:(Cfg.Op (Specific Arch.Ipush_to_stack))
          ~arg:[| instr.arg.(0) |]
          ~res:[||] ~dbg:instr.dbg ~fdo:instr.fdo ~stack_offset:!stack_offset
          ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
          ()
      in
      DLL.set_value (Option.get cell) push
    end
  in
  let rec rewrite_block (block : Cfg.basic_block) =
    let rec rewrite_cell cell =
      rewrite_instr (DLL.value cell) (Some cell);
      if not (Reg.Set.is_empty !unprocessed_stack_regs)
      then rewrite_cell (DLL.prev cell |> Option.get)
    in
    if not (Reg.Set.is_empty !unprocessed_stack_regs)
    then begin
      assert (block.stack_offset = base_stack_offset);
      block.stack_offset <- !stack_offset;
      assert (Label.Set.cardinal block.predecessors = 1);
      let pred = Cfg.get_block_exn cfg (Label.Set.choose block.predecessors) in
      rewrite_instr pred.terminator None;
      Option.iter rewrite_cell (DLL.last_cell pred.body);
      rewrite_block pred
    end
  in
  rewrite_block after_block;
  assert (Reg.Set.is_empty !unprocessed_stack_regs);
  assert (!stack_offset = base_stack_offset)

(* Delete the [reloads] from the start of [after_block.body] and prepend the
   matching [pop] instructions in LIFO order (so the topmost pushed value pops
   first). Stack offsets on the new pops match the depth at which the pop
   executes. *)
let replace_reloads_with_pops (cfg : Cfg.t)
    ~(reloads : Cfg.basic Cfg.instruction list) ~base_stack_offset
    (after_block : Cfg.basic_block) =
  let selected_reload_ids =
    List.fold_left
      (fun acc (reload : Cfg.basic Cfg.instruction) ->
        InstructionId.Set.add reload.id acc)
      InstructionId.Set.empty reloads
  in
  let n_to_remove = List.length reloads in
  let rec remove_reloads remaining cell =
    let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
    assert (Cfg.is_reload instr);
    let next = DLL.next cell in
    let remaining =
      if InstructionId.Set.mem instr.id selected_reload_ids
      then (
        DLL.delete_curr cell;
        remaining - 1)
      else remaining
    in
    if remaining > 0 then remove_reloads remaining (Option.get next)
  in
  remove_reloads n_to_remove (DLL.hd_cell after_block.body |> Option.get);
  let stack_offset = ref base_stack_offset in
  List.iter
    (fun (reload : Cfg.basic Cfg.instruction) ->
      stack_offset := !stack_offset + Arch.size_addr;
      let pop =
        Cfg.make_instruction ~desc:(Cfg.Op (Specific Arch.Ipop_from_stack))
          ~arg:[||]
          ~res:[| reload.res.(0) |]
          ~dbg:reload.dbg ~fdo:reload.fdo ~stack_offset:!stack_offset
          ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
          ()
      in
      DLL.add_begin after_block.body pop)
    reloads

let process_call cfg_with_infos (block : Cfg.basic_block) =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  match label_after_call block with
  | None -> ()
  | Some label_after -> (
    match find_reload_block cfg label_after with
    | None -> ()
    | Some after_block -> (
      let reloads_map =
        collect_leading_reloads cfg_with_infos after_block.body
      in
      let spills, reloads =
        match_spills_to_reloads
          (collect_spills_backwards cfg_with_infos block)
          reloads_map
      in
      match spills with
      | [] -> ()
      | _ :: _ ->
        let base_stack_offset = block.terminator.stack_offset in
        replace_spills_with_pushes cfg ~spills ~base_stack_offset after_block;
        replace_reloads_with_pops cfg ~reloads ~base_stack_offset after_block))

let run (cfg_with_infos : Cfg_with_infos.t) =
  if !Oxcaml_flags.cfg_push_pop_around_calls
  then begin
    let cfg = Cfg_with_infos.cfg cfg_with_infos in
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        process_call cfg_with_infos block);
    Cfg_with_infos.invalidate_liveness cfg_with_infos
  end
