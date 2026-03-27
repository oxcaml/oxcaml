[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list

type entry =
  | Output of
      { label : Label.t;
        instr : Cfg.basic Cfg.instruction;
        res_index : int
      }
  | OverwrittenOutput of
      { label : Label.t;
        instr : Cfg.basic Cfg.instruction;
        res_index : int;
        overwritten_input : Reg.t
      }
  | Phi of
      { merge_label : Label.t;
        regs : Reg.t array
      }
  | NotSSA

type t = entry Reg.Tbl.t

let build (cfg : Cfg.t) : t =
  let table : t = Reg.Tbl.create 64 in
  let predecessor_index preds label =
    let i =
      Label.Set.fold
        (fun l acc -> if Label.equal label l then 0 else acc + 1)
        preds 0
    in
    let n = Label.Set.cardinal preds in
    if i < n then Some (n - 1 - i) else None
  in
  let find_common_successor label1 label2 =
    let block1 = Cfg.get_block_exn cfg label1 in
    let block2 = Cfg.get_block_exn cfg label2 in
    let succs1 = Cfg.successor_labels ~normal:true ~exn:true block1 in
    let succs2 = Cfg.successor_labels ~normal:true ~exn:true block2 in
    Label.Set.min_elt_opt (Label.Set.inter succs1 succs2)
  in
  let process_body_def label cell res_index reg =
    if reg.Reg.preassigned
    then ()
    else
      let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
      let success =
        match[@ocaml.warning "-fragile-match"] Reg.Tbl.find_opt table reg with
        | None ->
          Reg.Tbl.replace table reg (Output { label; instr; res_index });
          true
        | Some NotSSA -> true
        | Some
            (Output
               { label = old_label;
                 instr = { desc = Op Move; arg = [| old_input |]; _ };
                 _
               })
          when not (Label.equal label old_label) -> begin
          match find_common_successor old_label label with
          | None -> false
          | Some merge_label ->
            let succ_block = Cfg.get_block_exn cfg merge_label in
            let preds = succ_block.predecessors in
            let n = Label.Set.cardinal preds in
            let regs = Array.make n Reg.dummy in
            regs.(predecessor_index preds old_label |> Option.get) <- old_input;
            regs.(predecessor_index preds label |> Option.get) <- reg;
            Reg.Tbl.replace table reg (Phi { merge_label; regs });
            true
        end
        | Some (Output { label = old_label; instr = old_instr; _ }) ->
          if Label.equal label old_label
          then begin
            match DLL.prev cell with
            | Some prev_cell
              when InstructionId.equal
                     (DLL.value prev_cell : Cfg.basic Cfg.instruction).id
                     old_instr.id -> (
              match[@ocaml.warning "-4"] old_instr.desc, old_instr.arg with
              | Op Move, [| overwritten_input |] ->
                Reg.Tbl.replace table reg
                  (OverwrittenOutput
                     { label; instr; res_index; overwritten_input });
                true
              | _ -> false)
            | _ -> false
          end
          else false
        | Some (OverwrittenOutput _) -> false
        | Some (Phi { merge_label; regs }) ->
          let succ_block = Cfg.get_block_exn cfg merge_label in
          if not (Label.Set.mem label succ_block.predecessors)
          then false
          else begin
            let i =
              predecessor_index succ_block.predecessors label |> Option.get
            in
            if not (Reg.same regs.(i) Reg.dummy)
            then false
            else begin
              regs.(i) <- reg;
              true
            end
          end
      in
      if not success then Reg.Tbl.replace table reg NotSSA
  in
  Cfg.iter_blocks_dfs cfg ~f:(fun label block ->
      DLL.iter_cell block.body ~f:(fun cell ->
          let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
          Array.iteri (fun i reg -> process_body_def label cell i reg) instr.res));
  (* Demote incomplete phis (those with remaining Reg.dummy entries) to NotSSA.
     This happens when not all predecessors of the merge block define the
     register. *)
  Reg.Tbl.filter_map_inplace
    (fun _reg entry ->
      match[@ocaml.warning "-4"] entry with
      | Phi { regs; _ } when Array.exists (Reg.same Reg.dummy) regs ->
        Some NotSSA
      | _ -> Some entry)
    table;
  table

let find table reg = Reg.Tbl.find_opt table reg

let iter table ~f = Reg.Tbl.iter f table

let has_ssa_semantics_at table cfg ~at_block reg =
  match[@ocaml.warning "-4"] Reg.Tbl.find_opt table reg with
  | Some (Output _ | OverwrittenOutput _) -> true
  | Some (Phi { merge_label; _ }) ->
    (* Between the assignment in a predecessor block and the actual control flow
       edge to the merge block, the observable value of a register and the value
       according to SSA semantics differ, so we exclude the whole predecessor
       block. This is an over-approximation, only the part of the block from the
       assignment to the end is critical. *)
    not
      (Label.Set.mem at_block (Cfg.get_block_exn cfg merge_label).predecessors)
  | Some NotSSA | None -> false
