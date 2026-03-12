[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let add_phys_reg value phys_reg =
  let cls = Regs.Phys_reg.reg_class phys_reg in
  let cls_idx = Regs.Reg_class.hash cls in
  let bit = Regs.index_in_class phys_reg in
  value.(cls_idx) <- value.(cls_idx) lor (1 lsl bit)

let add_reg value (reg : Reg.t) =
  match reg.loc with
  | Reg.Reg phys_reg -> add_phys_reg value phys_reg
  | Reg.Unknown | Reg.Stack _ -> ()

let add_regs value regs = Array.iter (add_reg value) regs

let cfg (cfg_with_infos : Cfg_with_infos.t) =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  (* CR xclerc: we use [fun_contains_calls] as a proxy for "leaf function",
     but we should perhaps reconsider this choice, e.g. by directly checking
     for the absence of [Call]/[Tailcall_func] terminators in the CFG. *)
  if not cfg.Cfg.fun_contains_calls
  then begin
    let num_classes = List.length Regs.Reg_class.all in
    let value = Array.make num_classes 0 in
    Cfg.iter_blocks cfg ~f:(fun _label block ->
      DLL.iter block.Cfg.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
        add_regs value instr.res;
        add_regs value (Proc.destroyed_at_basic instr.desc));
      let term = block.Cfg.terminator in
      add_regs value term.res;
      add_regs value (Proc.destroyed_at_terminator term.desc));
    let info = (Compilenv.current_unit_infos ()).ui_callee_regs_info in
    Callee_regs_info.set_value info cfg.Cfg.fun_name value
  end
