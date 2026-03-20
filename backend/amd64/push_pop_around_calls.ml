[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list
module Int_set = Stdlib.Set.Make (Stdlib.Int)
module Int_map = Stdlib.Map.Make (Stdlib.Int)

let is_gp_register (reg : Reg.t) =
  match reg.typ with
  | Int | Val | Addr -> true
  | Float | Float32 | Vec128 | Vec256 | Vec512 | Valx2 -> false

let reg_on_stack (reg : Reg.t) =
  match reg.loc with Stack _ -> true | Reg _ | Unknown -> false

let is_spill (instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] instr.desc with Op Spill -> true | _ -> false

let is_reload (instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] instr.desc with Op Reload -> true | _ -> false

let unsupported_after_push (instr : Cfg.basic Cfg.instruction) =
  if is_spill instr || is_reload instr
  then false
  else
    Array.exists reg_on_stack instr.arg
    || Array.exists reg_on_stack instr.res
    ||
    match instr.desc with
    | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
    | Op (Stackoffset _)
    | Reloadretaddr
    (* GC or other hidden calls are not supported in the middle of using push to
       spill. *)
    | Op (Alloc _ | Poll | Specific _) ->
      true
    | Op
        ( Move | Spill | Reload | Dummy_use | Const_int _ | Const_float _
        | Const_float32 _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
        | Const_vec512 _ | Load _ | Store _ | Intop _ | Int128op _ | Intop_imm _
        | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
        | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Name_for_debugger _ | Dls_get | Tls_get | Domain_index
        | Pause ) ->
      false

(* Collect consecutive reload instructions from the start of a block body, as
   long as no destination register is repeated. Returns a map from spill slot to
   reload instruction. *)
let collect_leading_reloads (body : Cfg.basic_instruction_list) =
  let reloads = ref Reg.Map.empty in
  let seen_regs = ref Reg.Set.empty in
  let rec loop (cell : Cfg.basic Cfg.instruction DLL.cell) =
    let instr = DLL.value cell in
    if is_reload instr
    then begin
      let spill_slot = instr.arg.(0) in
      let r = instr.res.(0) in
      if
        (not (Reg.Set.mem spill_slot instr.live))
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
   instruction (between the spill and the call). *)
let collect_spills_backwards (cfg : Cfg.t) (block : Cfg.basic_block) =
  let exn_entry_live =
    match block.exn with
    | None -> Reg.Set.empty
    | Some exn_label -> (
      let exn_block = Cfg.get_block_exn cfg exn_label in
      match DLL.hd_cell exn_block.body with
      | Some cell -> (DLL.value cell).live
      | None -> exn_block.terminator.live)
  in
  let body = block.body in
  let spills = ref [] in
  let used_after = ref exn_entry_live in
  let add_args args =
    Array.iter (fun r -> used_after := Reg.Set.add r !used_after) args
  in
  let rec loop cell =
    let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
    if unsupported_after_push instr
    then ()
    else begin
      if
        is_spill instr
        && (not (Reg.Set.mem instr.res.(0) !used_after))
        && is_gp_register instr.arg.(0)
      then begin
        spills := instr :: !spills;
        (* Also mark the spill slot as used, to avoid collecting two spills to
           the same slot. *)
        used_after := Reg.Set.add instr.res.(0) !used_after
      end
      else add_args instr.arg;
      match DLL.prev cell with Some prev -> loop prev | None -> ()
    end
  in
  (match DLL.last_cell body with Some cell -> loop cell | None -> ());
  !spills

let process_call (cfg : Cfg.t) (block : Cfg.basic_block) =
  let label_after =
    match block.terminator.desc with
    | Call { label_after; _ } -> Some label_after
    | Prim _ | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Invalid _ ->
      None
  in
  match label_after with
  | None -> ()
  | Some label_after -> (
    (* Follow chain of empty Always blocks to find the block with the reloads
       (the rewrite pass may insert intermediate blocks for call result spills).
       Returns the reload block and intermediate blocks whose stack_offsets need
       updating. *)
    let rec find_reload_block label =
      let blk = Cfg.get_block_exn cfg label in
      if Label.Set.cardinal blk.predecessors <> 1
      then None
      else if DLL.is_empty blk.body
      then
        match[@ocaml.warning "-4"] blk.terminator.desc with
        | Always next -> find_reload_block next
        | _ -> Some blk
      else Some blk
    in
    match find_reload_block label_after with
    | None -> ()
    | Some after_block ->
      let reloads = collect_leading_reloads after_block.body in
      let spills =
        collect_spills_backwards cfg block
        |> List.filter (fun (instr : Cfg.basic Cfg.instruction) ->
            Reg.Map.mem instr.res.(0) reloads)
      in
      let n_to_push = List.length spills in
      let n_to_push =
        n_to_push - (n_to_push mod (Arch.call_stack_alignment / Arch.size_addr))
      in
      let spills = List.filteri (fun i _ -> i < n_to_push) spills in
      let reloads =
        spills
        |> List.map (fun (spill : Cfg.basic Cfg.instruction) ->
            Reg.Map.find spill.res.(0) reloads)
      in
      if n_to_push > 0
      then begin
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
        let selected_reload_ids =
          List.fold_left
            (fun acc (reload : Cfg.basic Cfg.instruction) ->
              InstructionId.Set.add reload.id acc)
            InstructionId.Set.empty reloads
        in
        let base_stack_offset = block.terminator.stack_offset in
        (* Replace spills with pushes in-place, adjusting stack offsets of
           intervening instructions. *)
        let unprocessed_stack_regs = ref now_unused_spill_slots in
        let stack_offset =
          ref (base_stack_offset + (List.length spills * Arch.size_addr))
        in
        let rewrite_spills_instr (type a) (instr : a Cfg.instruction) cell =
          assert (instr.stack_offset = base_stack_offset);
          instr.stack_offset <- !stack_offset;
          instr.live <- Reg.Set.diff instr.live !unprocessed_stack_regs;
          if InstructionId.Set.mem instr.id selected_spill_ids
          then begin
            unprocessed_stack_regs
              := Reg.Set.remove instr.res.(0) !unprocessed_stack_regs;
            stack_offset := !stack_offset - Arch.size_addr;
            let push =
              Cfg.make_instruction ~desc:(Cfg.Op (Specific Arch.Ipush_to_stack))
                ~arg:[| instr.arg.(0) |]
                ~res:[||] ~dbg:instr.dbg ~fdo:instr.fdo ~live:instr.live
                ~stack_offset:!stack_offset
                ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
                ()
            in
            DLL.set_value (Option.get cell) push
          end
        in
        let rec rewrite_spills_block (after_block : Cfg.basic_block) =
          let rec rewrite_spills_cell cell =
            rewrite_spills_instr (DLL.value cell) (Some cell);
            if not (Reg.Set.is_empty !unprocessed_stack_regs)
            then rewrite_spills_cell (DLL.prev cell |> Option.get)
          in
          if not (Reg.Set.is_empty !unprocessed_stack_regs)
          then begin
            assert (after_block.stack_offset = base_stack_offset);
            after_block.stack_offset <- !stack_offset;
            let predecessors = after_block.predecessors in
            assert (Label.Set.cardinal after_block.predecessors = 1);
            let block = Cfg.get_block_exn cfg (Label.Set.choose predecessors) in
            rewrite_spills_instr block.terminator None;
            Option.iter rewrite_spills_cell (DLL.last_cell block.body);
            rewrite_spills_block block
          end
        in
        rewrite_spills_block after_block;
        assert (Reg.Set.is_empty !unprocessed_stack_regs);
        (* Delete selected reloads from their original positions *)
        let rec remove_unspills remaining_reloads cell =
          let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
          assert (is_reload instr);
          let next = DLL.next cell in
          let remaining_reloads =
            if InstructionId.Set.mem instr.id selected_reload_ids
            then (
              DLL.delete_curr cell;
              remaining_reloads - 1)
            else remaining_reloads
          in
          instr.live <- Reg.Set.diff instr.live now_unused_spill_slots;
          if remaining_reloads > 0
          then remove_unspills remaining_reloads (Option.get next)
        in
        remove_unspills n_to_push (DLL.hd_cell after_block.body |> Option.get);
        (* Insert pops at the beginning in LIFO order. *)
        assert (!stack_offset = base_stack_offset);
        List.iter
          (fun (reload : Cfg.basic Cfg.instruction) ->
            stack_offset := !stack_offset + Arch.size_addr;
            (* CR ttebbi: This live set would have been accurate for the old
               reload position, but not for the permutation. Since live sets are
               not used later except for call positions, this is likely fine.
               Ideally, we should recompute live sets properly or clear them
               completely. *)
            let pop =
              Cfg.make_instruction
                ~desc:(Cfg.Op (Specific Arch.Ipop_from_stack)) ~arg:[||]
                ~res:[| reload.res.(0) |]
                ~dbg:reload.dbg ~fdo:reload.fdo ~live:reload.live
                ~stack_offset:!stack_offset
                ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
                ()
            in
            DLL.add_begin after_block.body pop)
          reloads
      end)

(* Compact stack slots: remove gaps left by converted spills/reloads and
   renumber remaining slots. *)
let compact_stack_slots (cfg : Cfg.t) =
  let used = Stack_class.Tbl.make Int_set.empty in
  let registers = ref Reg.Set.empty in
  let record_reg (r : Reg.t) =
    registers := Reg.Set.add r !registers;
    match r.loc with
    | Stack (Local slot) ->
      let sc = Stack_class.of_machtype r.typ in
      Stack_class.Tbl.replace used sc
        (Int_set.add slot (Stack_class.Tbl.find used sc))
    | Stack (Incoming _ | Outgoing _ | Domainstate _) | Reg _ | Unknown -> ()
  in
  let record_arr a = Array.iter record_reg a in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      DLL.iter block.body ~f:(fun (i : Cfg.basic Cfg.instruction) ->
          record_arr i.arg;
          record_arr i.res);
      record_arr block.terminator.arg;
      record_arr block.terminator.res);
  let renamings = Stack_class.Tbl.make Int_map.empty in
  Stack_class.Tbl.iter used ~f:(fun sc slots ->
      let num_slots, renaming =
        Int_set.fold
          (fun old (next, m) -> next + 1, Int_map.add old next m)
          slots (0, Int_map.empty)
      in
      Stack_class.Tbl.replace renamings sc renaming;
      Stack_class.Tbl.replace cfg.fun_num_stack_slots sc num_slots);
  let rename_reg (r : Reg.t) : unit =
    match r.loc with
    | Stack (Local slot) ->
      let new_slot =
        Stack_class.of_machtype r.typ
        |> Stack_class.Tbl.find renamings
        |> Int_map.find slot
      in
      Reg.set_loc r (Stack (Local new_slot))
    | Reg _ | Unknown | Stack (Domainstate _ | Incoming _ | Outgoing _) -> ()
  in
  Reg.Set.iter rename_reg !registers

let run (cfg_with_infos : Cfg_with_infos.t) =
  if !Oxcaml_flags.cfg_push_pop_around_calls
  then begin
    let cfg = Cfg_with_infos.cfg cfg_with_infos in
    Cfg.iter_blocks cfg ~f:(fun _label block -> process_call cfg block);
    compact_stack_slots cfg;
    cfg_with_infos
  end
  else cfg_with_infos
