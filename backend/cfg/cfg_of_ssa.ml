[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let concat_map_regs f arr = Array.concat (Array.to_list (Array.map f arr))

let instr_seq = Sub_cfg.instr_id

type reg_env =
  { op_regs : Reg.t array Ssa.InstructionId.Tbl.t;
    block_params_regs : Reg.t array Label.Tbl.t;
    call_result_locs : Reg.t array Label.Tbl.t;
    call_stack_ofs : int Label.Tbl.t
  }

let create_reg_env () =
  { op_regs = Ssa.InstructionId.Tbl.create 256;
    block_params_regs = Label.Tbl.create 64;
    call_result_locs = Label.Tbl.create 16;
    call_stack_ofs = Label.Tbl.create 16
  }

let rec get_regs env (i : Ssa.instruction) : Reg.t array =
  match i with
  | Op { id; _ } -> (
    try Ssa.InstructionId.Tbl.find env.op_regs id
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa: no regs for Op %d"
        (Ssa.InstructionId.hash id))
  | Block_param { block; index; _ } -> (
    try
      let regs = Label.Tbl.find env.block_params_regs block in
      [| regs.(index) |]
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa: no regs for Block_param %a.%d" Label.format
        block index)
  | Proj { index; src } ->
    let src_regs = get_regs env src in
    [| src_regs.(index) |]
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
    Misc.fatal_error "Cfg_of_ssa.get_regs: unexpected instruction"

let rec allocate_regs env (i : Ssa.instruction) =
  match i with
  | Op { id; typ; args; _ } ->
    if not (Ssa.InstructionId.Tbl.mem env.op_regs id)
    then (
      Array.iter (allocate_regs env) args;
      Ssa.InstructionId.Tbl.replace env.op_regs id (Reg.createv typ))
  | Block_param _ -> ()
  | Proj { src; _ } -> allocate_regs env src
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> ()

let allocate_block_params_regs env (block : Ssa.basic_block) =
  if not (Label.Tbl.mem env.block_params_regs block.label)
  then
    Label.Tbl.replace env.block_params_regs block.label
      (Reg.createv block.params)

let get_block_params_regs env (block : Ssa.basic_block) =
  try Label.Tbl.find env.block_params_regs block.label
  with Not_found ->
    Misc.fatal_errorf "Cfg_of_ssa: no regs for block params of %a" Label.format
      block.label

let allocate_body_regs env (i : Ssa.instruction) =
  match i with
  | Op _ | Block_param _ | Proj _ -> allocate_regs env i
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> ()

let make_cfg_instr desc arg res dbg =
  { Cfg.desc;
    id = InstructionId.get_and_incr instr_seq;
    arg;
    res;
    dbg;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    available_before = Reg_availability_set.Unreachable;
    available_across = Reg_availability_set.Unreachable
  }

let emit_moves body ~src ~dst =
  Array.iter2
    (fun s d ->
      if not (Reg.same s d)
      then
        DLL.add_end body
          (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
    src dst

let reconstruct_test renv (cond : Ssa.instruction) ~true_label ~false_label :
    Cfg.terminator * Reg.t array =
  match cond with
  (* Truth test wrapper: Cne 0 folds to Truth_test *)
  | Op { op = Intop_imm (Icomp Cne, 0); args; _ } ->
    let arg = concat_map_regs (get_regs renv) args in
    Cfg.Truth_test { ifso = true_label; ifnot = false_label }, arg
  | Op { op = Intop (Icomp cmp); args; _ } ->
    let arg = concat_map_regs (get_regs renv) args in
    ( Select_utils.terminator_of_test (Iinttest cmp) ~label_true:true_label
        ~label_false:false_label,
      arg )
  | Op { op = Intop_imm (Icomp cmp, n); args; _ } ->
    let arg = concat_map_regs (get_regs renv) args in
    ( Select_utils.terminator_of_test
        (Iinttest_imm (cmp, n))
        ~label_true:true_label ~label_false:false_label,
      arg )
  | Op { op = Floatop (w, Icompf cmp); args; _ } ->
    let arg = concat_map_regs (get_regs renv) args in
    ( Select_utils.terminator_of_test
        (Ifloattest (w, cmp))
        ~label_true:true_label ~label_false:false_label,
      arg )
  | Op { op = Intop_imm (Iand, 1); args; _ } ->
    let arg = concat_map_regs (get_regs renv) args in
    Cfg.Parity_test { ifso = false_label; ifnot = true_label }, arg
  | _ ->
    let arg = get_regs renv cond in
    Cfg.Truth_test { ifso = true_label; ifnot = false_label }, arg

(* Always fold a comparison that flows directly into the Branch condition. For
   Itruetest/Ifalsetest, the condition is a raw value (not a comparison Op), so
   it won't match and falls to Truth_test. For cases where Cmm-to-SSA bound a
   comparison in a Clet, ssa_of_cmm should add an extra truth-test layer so the
   folding produces the right result. *)
let branch_cond_id (block : Ssa.basic_block) : Ssa.InstructionId.t option =
  match block.terminator with
  | Branch
      { conditions =
          [| ( Op
                 { id;
                   op =
                     ( Intop (Icomp _)
                     | Intop_imm (Icomp _, _)
                     | Floatop (_, Icompf _)
                     | Intop_imm (Iand, 1) );
                   _
                 },
               _ )
          |];
        _
      } ->
    Some id
  | _ -> None

module Make (Target : Cfg_selectgen_target_intf.S) = struct
  let pad_to a target_len filler =
    let n = Array.length a in
    if n >= target_len
    then a
    else Array.init target_len (fun i -> if i < n then a.(i) else filler.(i))

  let emit_op body op dbg rs rd =
    match Target.pseudoregs_for_operation op rs rd with
    | Constrained (rsrc, rdst) ->
      let rs = pad_to rs (Array.length rsrc) rsrc in
      let rd = pad_to rd (Array.length rdst) rdst in
      emit_moves body ~src:rs ~dst:rsrc;
      DLL.add_end body (make_cfg_instr (Cfg.Op op) rsrc rdst dbg);
      emit_moves body ~src:rdst ~dst:rd
    | Use_default_regs ->
      DLL.add_end body (make_cfg_instr (Cfg.Op op) rs rd dbg)

  let convert_block (renv : reg_env) (ssa : Ssa.t) (block : Ssa.basic_block) :
      Cfg.basic_block =
    let skip_id = branch_cond_id block in
    let body = DLL.make_empty () in
    (* For CallContinuation blocks, emit moves from physical result locations to
       virtual result registers *)
    (match block.desc with
    | CallContinuation _ -> (
      let virt_res = get_block_params_regs renv block in
      match Label.Tbl.find_opt renv.call_result_locs block.label with
      | Some loc_res ->
        emit_moves body ~src:loc_res ~dst:virt_res;
        let stack_ofs =
          match Label.Tbl.find_opt renv.call_stack_ofs block.label with
          | Some ofs -> ofs
          | None -> 0
        in
        if stack_ofs <> 0
        then
          DLL.add_end body
            (make_cfg_instr (Cfg.Op (Stackoffset (-stack_ofs))) [||] [||]
               Debuginfo.none)
      | None -> ())
    | TrapHandler _ ->
      (* Only move the exn bucket into the first param, matching cfg_selectgen
         setup_catch_handler. Extra handler params (e.g. from reraise with extra
         state) are kept alive across the try body by the register allocator. *)
      let virt_res = get_block_params_regs renv block in
      let exn_bucket = [| Proc.loc_exn_bucket |] in
      let first_param = [| virt_res.(0) |] in
      emit_moves body ~src:exn_bucket ~dst:first_param
    | Merge _ | BranchTarget _ | FunctionStart -> ());
    Array.iter
      (fun (i : Ssa.instruction) ->
        match i with
        | Op { id; op; args; dbg; _ } ->
          let is_branch_cond =
            match skip_id with
            | Some sid -> Ssa.InstructionId.equal id sid
            | None -> false
          in
          if not is_branch_cond
          then
            let arg = concat_map_regs (get_regs renv) args in
            let res = get_regs renv i in
            emit_op body op dbg arg res
        | Block_param _ | Proj _ -> ()
        | Pushtrap { lbl_handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Pushtrap { lbl_handler })
               [||] [||] Debuginfo.none)
        | Poptrap { lbl_handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Poptrap { lbl_handler })
               [||] [||] Debuginfo.none)
        | Stack_check { max_frame_size_bytes } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Stack_check { max_frame_size_bytes })
               [||] [||] Debuginfo.none)
        | Name_for_debugger { ident; provenance; which_parameter; regs } -> (
          let regs = concat_map_regs (get_regs renv) regs in
          let naming_instr =
            make_cfg_instr
              (Cfg.Op
                 (Name_for_debugger { ident; provenance; which_parameter; regs }))
              [||] [||] Debuginfo.none
          in
          (* Insert naming right after the instruction that defines the
             referenced registers, matching bind_let in cfg_selectgen. *)
          let rec find_def cell =
            let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
            if Array.exists (fun r -> Array.exists (Reg.same r) instr.res) regs
            then DLL.insert_after cell naming_instr
            else
              match DLL.prev cell with
              | Some prev -> find_def prev
              | None -> DLL.add_end body naming_instr
          in
          match DLL.last_cell body with
          | Some cell -> find_def cell
          | None -> DLL.add_end body naming_instr))
      block.body;
    let dbg = block.terminator_dbg in
    let terminator, can_raise =
      match block.terminator with
      | Never ->
        ( make_cfg_instr
            (Cfg.Invalid
               { message = "unreachable";
                 stack_ofs = 0;
                 stack_align = Align_16;
                 label_after = None
               })
            [||] [||] dbg,
          false )
      | Goto { goto; args } ->
        let target_block = Label.Tbl.find ssa.blocks goto in
        let dst_regs = get_block_params_regs renv target_block in
        let src_regs = concat_map_regs (get_regs renv) args in
        (* Use intermediate registers when src and dst overlap (e.g.
           loop-carried phi nodes), matching cfg_selectgen. *)
        let has_overlap =
          Array.exists
            (fun s -> Array.exists (fun d -> Reg.same s d) dst_regs)
            src_regs
        in
        if has_overlap
        then (
          let tmp_regs = Reg.createv_with_typs src_regs in
          emit_moves body ~src:src_regs ~dst:tmp_regs;
          emit_moves body ~src:tmp_regs ~dst:dst_regs)
        else emit_moves body ~src:src_regs ~dst:dst_regs;
        make_cfg_instr (Cfg.Always goto) [||] [||] dbg, false
      | Branch { conditions; else_goto } -> (
        match conditions with
        | [| (cond_instr, target_label) |] ->
          let term, arg =
            reconstruct_test renv cond_instr ~true_label:target_label
              ~false_label:else_goto
          in
          make_cfg_instr term arg [||] dbg, false
        | _ ->
          Misc.fatal_error
            "Cfg_of_ssa: multi-condition Branch not yet supported")
      | Switch (labels, args) ->
        let arg = concat_map_regs (get_regs renv) args in
        make_cfg_instr (Cfg.Switch labels) arg [||] dbg, false
      | Return args ->
        let arg = concat_map_regs (get_regs renv) args in
        let loc_res = Proc.loc_results_return (Reg.typv arg) in
        emit_moves body ~src:arg ~dst:loc_res;
        make_cfg_instr Cfg.Return loc_res [||] dbg, false
      | Raise (k, args, handler_label) ->
        let exn_val = concat_map_regs (get_regs renv) args in
        let exn_bucket = [| Proc.loc_exn_bucket |] in
        (* Move exn value to exn bucket, and extra args (from reraise) to the
           handler's extra param registers, matching cfg_selectgen
           emit_expr_raise. *)
        let extra_dst =
          match handler_label with
          | Some hl -> (
            match Label.Tbl.find_opt ssa.blocks hl with
            | Some handler_block ->
              let handler_regs = get_block_params_regs renv handler_block in
              (* Skip the first param (exn bucket) *)
              if Array.length handler_regs > 1
              then Array.sub handler_regs 1 (Array.length handler_regs - 1)
              else [||]
            | None -> [||])
          | None -> [||]
        in
        let dst = Array.append exn_bucket extra_dst in
        let src =
          if Array.length exn_val > Array.length dst
          then Array.sub exn_val 0 (Array.length dst)
          else exn_val
        in
        emit_moves body ~src ~dst;
        make_cfg_instr (Cfg.Raise k) exn_bucket [||] dbg, true
      | Tailcall_self { destination; args } ->
        let virt_args = concat_map_regs (get_regs renv) args in
        let loc_arg = Proc.loc_parameters (Reg.typv virt_args) in
        emit_moves body ~src:virt_args ~dst:loc_arg;
        ( make_cfg_instr (Cfg.Tailcall_self { destination }) loc_arg [||] dbg,
          false )
      | Tailcall_func (call_op, args) ->
        let virt_args = concat_map_regs (get_regs renv) args in
        let rarg, loc_arg =
          match call_op with
          | Indirect _ ->
            let rarg = Array.sub virt_args 1 (Array.length virt_args - 1) in
            let loc, _ofs = Proc.loc_arguments (Reg.typv rarg) in
            rarg, loc
          | Direct _ ->
            let loc, _ofs = Proc.loc_arguments (Reg.typv virt_args) in
            virt_args, loc
        in
        emit_moves body ~src:rarg ~dst:loc_arg;
        let call_arg =
          match call_op with
          | Indirect _ -> Array.append [| virt_args.(0) |] loc_arg
          | Direct _ -> loc_arg
        in
        make_cfg_instr (Cfg.Tailcall_func call_op) call_arg [||] dbg, false
      | Call { op = call_op; args; continuation; exn_continuation = _ } ->
        let virt_args = concat_map_regs (get_regs renv) args in
        let cont_block = Label.Tbl.find ssa.blocks continuation in
        let virt_res = get_block_params_regs renv cont_block in
        let rarg, loc_arg, stack_ofs_args =
          match call_op with
          | Indirect _ ->
            let rarg = Array.sub virt_args 1 (Array.length virt_args - 1) in
            let loc, ofs = Proc.loc_arguments (Reg.typv rarg) in
            rarg, loc, ofs
          | Direct _ ->
            let loc, ofs = Proc.loc_arguments (Reg.typv virt_args) in
            virt_args, loc, ofs
        in
        let loc_res, stack_ofs_res =
          Proc.loc_results_call (Reg.typv virt_res)
        in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if stack_ofs <> 0
        then
          DLL.add_end body
            (make_cfg_instr (Cfg.Op (Stackoffset stack_ofs)) [||] [||]
               Debuginfo.none);
        emit_moves body ~src:rarg ~dst:loc_arg;
        let call_arg =
          match call_op with
          | Indirect _ -> Array.append [| virt_args.(0) |] loc_arg
          | Direct _ -> loc_arg
        in
        Label.Tbl.replace renv.call_result_locs continuation loc_res;
        Label.Tbl.replace renv.call_stack_ofs continuation stack_ofs;
        ( make_cfg_instr
            (Cfg.Call { op = call_op; label_after = continuation })
            call_arg loc_res dbg,
          true )
      | Prim { op = prim_op; args; continuation; exn_continuation = _ } -> (
        let virt_args = concat_map_regs (get_regs renv) args in
        let cont_block = Label.Tbl.find ssa.blocks continuation in
        let virt_res = get_block_params_regs renv cont_block in
        match prim_op with
        | External ({ ty_args; _ } as ext) ->
          let ty_args =
            if ty_args = []
            then Array.to_list (Array.map (fun _ -> Cmm.XInt) virt_args)
            else ty_args
          in
          let locs, stack_ofs, stack_align =
            Proc.loc_external_arguments ty_args
          in
          let loc_arg = Array.concat (Array.to_list locs) in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          if stack_ofs <> 0
          then
            DLL.add_end body
              (make_cfg_instr (Cfg.Op (Stackoffset stack_ofs)) [||] [||]
                 Debuginfo.none);
          Array.iteri
            (fun i arg -> emit_moves body ~src:[| arg |] ~dst:locs.(i))
            virt_args;
          Label.Tbl.replace renv.call_result_locs continuation loc_res;
          Label.Tbl.replace renv.call_stack_ofs continuation stack_ofs;
          ( make_cfg_instr
              (Cfg.Prim
                 { op = External { ext with stack_ofs; stack_align };
                   label_after = continuation
                 })
              loc_arg loc_res dbg,
            true )
        | Probe _ ->
          ( make_cfg_instr
              (Cfg.Prim { op = prim_op; label_after = continuation })
              virt_args virt_res dbg,
            true ))
    in
    let is_trap_handler =
      match block.desc with TrapHandler _ -> true | _ -> false
    in
    let exn = None in
    { Cfg.start = block.label;
      body;
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = Cfg.invalid_stack_offset;
      exn;
      can_raise;
      is_trap_handler;
      cold = false
    }

  let convert ~future_funcnames (ssa : Ssa.t) : Cfg_with_layout.t =
    let renv = create_reg_env () in
    Label.Tbl.iter
      (fun _label (block : Ssa.basic_block) ->
        allocate_block_params_regs renv block;
        Array.iter (allocate_body_regs renv) block.body)
      ssa.blocks;
    (* Pre-compute calling convention locations for all calls *)
    Label.Tbl.iter
      (fun _label (block : Ssa.basic_block) ->
        match block.terminator with
        | Call { op = call_op; args; continuation; _ } ->
          let virt_args = concat_map_regs (get_regs renv) args in
          let cont_block = Label.Tbl.find ssa.blocks continuation in
          let virt_res = get_block_params_regs renv cont_block in
          let _rarg, _loc_arg, stack_ofs_args =
            match call_op with
            | Indirect _ ->
              let rarg = Array.sub virt_args 1 (Array.length virt_args - 1) in
              let loc, ofs = Proc.loc_arguments (Reg.typv rarg) in
              rarg, loc, ofs
            | Direct _ ->
              let loc, ofs = Proc.loc_arguments (Reg.typv virt_args) in
              virt_args, loc, ofs
          in
          let loc_res, stack_ofs_res =
            Proc.loc_results_call (Reg.typv virt_res)
          in
          let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
          Label.Tbl.replace renv.call_result_locs continuation loc_res;
          Label.Tbl.replace renv.call_stack_ofs continuation stack_ofs
        | Prim { op = External { ty_args; _ }; args; continuation; _ } ->
          let virt_args = concat_map_regs (get_regs renv) args in
          let cont_block = Label.Tbl.find ssa.blocks continuation in
          let virt_res = get_block_params_regs renv cont_block in
          let ty_args =
            if ty_args = []
            then Array.to_list (Array.map (fun _ -> Cmm.XInt) virt_args)
            else ty_args
          in
          let _locs, stack_ofs, _align = Proc.loc_external_arguments ty_args in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          Label.Tbl.replace renv.call_result_locs continuation loc_res;
          Label.Tbl.replace renv.call_stack_ofs continuation stack_ofs
        | _ -> ())
      ssa.blocks;
    let entry_block_ssa = Label.Tbl.find ssa.blocks ssa.entry_label in
    let fun_arg_regs = get_block_params_regs renv entry_block_ssa in
    let loc_arg = Proc.loc_parameters (Reg.typv fun_arg_regs) in
    let prologue_poll_instr_id = ref None in
    let cfg =
      Cfg.create ~fun_name:ssa.fun_name ~fun_args:loc_arg
        ~fun_codegen_options:(Cfg.of_cmm_codegen_option ssa.fun_codegen_options)
        ~fun_dbg:ssa.fun_dbg ~fun_contains_calls:true
        ~fun_num_stack_slots:(Stack_class.Tbl.make 0) ~fun_poll:ssa.fun_poll
        ~next_instruction_id:instr_seq ~fun_ret_type:ssa.fun_ret_type
        ~allowed_to_be_irreducible:false
    in
    let layout = DLL.make_empty () in
    let entry_body = DLL.make_empty () in
    (* Emit Name_for_debugger for function params, matching
       insert_param_name_for_debugger in cfg_selectgen. *)
    let loc_arg_idx = ref 0 in
    List.iteri
      (fun param_index (var, ty) ->
        let provenance = Backend_var.With_provenance.provenance var in
        let ident = Backend_var.With_provenance.var var in
        let n = Array.length ty in
        let hard_regs = Array.init n (fun i -> loc_arg.(!loc_arg_idx + i)) in
        loc_arg_idx := !loc_arg_idx + n;
        if Option.is_some provenance
        then
          DLL.add_end entry_body
            (make_cfg_instr
               (Cfg.Op
                  (Name_for_debugger
                     { ident;
                       provenance;
                       which_parameter = Some param_index;
                       regs = hard_regs
                     }))
               hard_regs [||] Debuginfo.none))
      ssa.fun_args_names;
    let entry_block =
      { Cfg.start = Cfg.entry_label cfg;
        body = entry_body;
        terminator =
          make_cfg_instr (Cfg.Always ssa.entry_label) [||] [||] Debuginfo.none;
        predecessors = Label.Set.empty;
        stack_offset = Cfg.invalid_stack_offset;
        exn = None;
        can_raise = false;
        is_trap_handler = false;
        cold = false
      }
    in
    Cfg.add_block_exn cfg entry_block;
    DLL.add_end layout entry_block.start;
    List.iter
      (fun label ->
        let ssa_block = Label.Tbl.find ssa.blocks label in
        let cfg_block = convert_block renv ssa ssa_block in
        cfg_block.can_raise
          <- Cfg.can_raise_terminator cfg_block.terminator.desc;
        (* Insert arg moves and prologue poll at start of entry block *)
        if Label.equal label ssa.entry_label
        then (
          (* Poll instruction (may be deleted later by Cfg_polling if not
             needed) *)
          let poll_instr =
            make_cfg_instr (Cfg.Op Poll) [||] [||] Debuginfo.none
          in
          prologue_poll_instr_id := Some poll_instr.id;
          DLL.add_begin cfg_block.body poll_instr;
          (* Arg moves (added at beginning, so they end up before poll) *)
          Array.iter2
            (fun s d ->
              if not (Reg.same s d)
              then
                DLL.add_begin cfg_block.body
                  (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
            (Array.of_list (List.rev (Array.to_list loc_arg)))
            (Array.of_list (List.rev (Array.to_list fun_arg_regs)));
          (* Param naming (added at beginning AFTER arg moves, so ends up before
             them, matching cfg_selectgen) *)
          let loc_idx = ref 0 in
          List.iteri
            (fun pi (var, ty) ->
              let prov = Backend_var.With_provenance.provenance var in
              let ident = Backend_var.With_provenance.var var in
              let n = Array.length ty in
              let regs = Array.init n (fun i -> loc_arg.(!loc_idx + i)) in
              loc_idx := !loc_idx + n;
              if Option.is_some prov
              then
                DLL.add_begin cfg_block.body
                  (make_cfg_instr
                     (Cfg.Op
                        (Name_for_debugger
                           { ident;
                             provenance = prov;
                             which_parameter = Some pi;
                             regs
                           }))
                     regs [||] Debuginfo.none))
            ssa.fun_args_names);
        if Cfg.is_return_terminator cfg_block.terminator.desc
        then
          DLL.add_end cfg_block.body
            (make_cfg_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
        Cfg.add_block_exn cfg cfg_block;
        DLL.add_end layout label)
      ssa.block_order;
    Select_utils.Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
    (* Delete prologue poll if not needed, matching cfg_selectgen *)
    (match !prologue_poll_instr_id with
    | None -> ()
    | Some poll_id ->
      let delete =
        not
          (Cfg_polling.requires_prologue_poll ~future_funcnames
             ~fun_name:ssa.fun_name ~optimistic_prologue_poll_instr_id:poll_id
             cfg)
      in
      if delete
      then
        let found = ref false in
        Cfg.iter_blocks cfg ~f:(fun _label (block : Cfg.basic_block) ->
            if not !found
            then
              DLL.filter_left block.body
                ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
                  let is_poll = InstructionId.equal instr.id poll_id in
                  if is_poll then found := true;
                  not is_poll)));
    let cfg_with_layout = Cfg_with_layout.create cfg ~layout in
    Cfg_simplify.run cfg_with_layout
end
