[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let value_to_phys_regs (value : Callee_regs_info.value) : Regs.Phys_reg.t list =
  List.concat_map
    (fun cls ->
      let cls_idx = Regs.Reg_class.hash cls in
      let bitmask = value.(cls_idx) in
      Array.fold_left
        (fun acc phys_reg ->
          let bit = Regs.index_in_class phys_reg in
          if bitmask land (1 lsl bit) <> 0 then phys_reg :: acc else acc)
        [] (Regs.available_registers cls))
    Regs.Reg_class.all

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

exception Has_ocaml_call

(* A "leaf function" for our purposes is one that contains no [Call] or
   [Tailcall_func] terminators — i.e. it never transfers control to another
   OCaml function. Allocations, polls, external C calls, and probes are all
   fine: their register effects are captured by [Proc.destroyed_at_basic] /
   [Proc.destroyed_at_terminator]. *)
let is_leaf_function (cfg : Cfg.t) =
  match
    Cfg.iter_blocks cfg ~f:(fun _label block ->
      (match[@ocaml.warning "-4"] block.Cfg.terminator.desc with
      | Cfg.Call _ | Cfg.Tailcall_func _ -> raise Has_ocaml_call
      | _ -> ()))
  with
  | () -> true
  | exception Has_ocaml_call -> false

let cfg (cfg_with_layout : Cfg_with_layout.t) =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  if is_leaf_function cfg
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
