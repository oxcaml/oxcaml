[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let instr_seq = Sub_cfg.instr_id

type reg_env =
  { op_regs : Reg.t array Ssa.InstructionId.Tbl.t;
    block_params_regs : Reg.t array Ssa.Block.Tbl.t;
    call_result_locs : Reg.t array Ssa.Block.Tbl.t;
    call_stack_ofs : int Ssa.Block.Tbl.t
  }

let create_reg_env () =
  { op_regs = Ssa.InstructionId.Tbl.create 256;
    block_params_regs = Ssa.Block.Tbl.create 64;
    call_result_locs = Ssa.Block.Tbl.create 16;
    call_stack_ofs = Ssa.Block.Tbl.create 16
  }

let get_reg env (i : Ssa.instruction) : Reg.t =
  match i with
  | Op { id; _ } ->
    let regs = Ssa.InstructionId.Tbl.find env.op_regs id in
    assert (Array.length regs = 1);
    regs.(0)
  | Block_param { block; index; _ } -> (
    try
      let regs = Ssa.Block.Tbl.find env.block_params_regs block in
      regs.(index)
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa: no regs for Block_param %d.%d"
        (Ssa.block_id block) index)
  | Proj { index; src = Op { id; _ } } ->
    let src_regs = Ssa.InstructionId.Tbl.find env.op_regs id in
    src_regs.(index)
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ | Proj _ ->
    Misc.fatal_error "Cfg_of_ssa.get_reg: unexpected instruction"

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

let allocate_block_params_regs env (block : Ssa.block) =
  if not (Ssa.Block.Tbl.mem env.block_params_regs block)
  then
    Ssa.Block.Tbl.replace env.block_params_regs block (Reg.createv block.params)

let get_block_params_regs env (block : Ssa.block) =
  try Ssa.Block.Tbl.find env.block_params_regs block
  with Not_found ->
    Misc.fatal_errorf "Cfg_of_ssa: no regs for block params of %d"
      (Ssa.block_id block)

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
  | Op { op = Intop_imm (Icomp Cne, 0); args; _ } ->
    let arg = Array.map (get_reg renv) args in
    Cfg.Truth_test { ifso = true_label; ifnot = false_label }, arg
  | Op { op = Intop (Icomp cmp); args; _ } ->
    let arg = Array.map (get_reg renv) args in
    ( Select_utils.terminator_of_test (Iinttest cmp) ~label_true:true_label
        ~label_false:false_label,
      arg )
  | Op { op = Intop_imm (Icomp cmp, n); args; _ } ->
    let arg = Array.map (get_reg renv) args in
    ( Select_utils.terminator_of_test
        (Iinttest_imm (cmp, n))
        ~label_true:true_label ~label_false:false_label,
      arg )
  | Op { op = Floatop (w, Icompf cmp); args; _ } ->
    let arg = Array.map (get_reg renv) args in
    ( Select_utils.terminator_of_test
        (Ifloattest (w, cmp))
        ~label_true:true_label ~label_false:false_label,
      arg )
  | Op { op = Intop_imm (Iand, 1); args; _ } ->
    let arg = Array.map (get_reg renv) args in
    Cfg.Parity_test { ifso = false_label; ifnot = true_label }, arg
  | _ ->
    let arg = [| get_reg renv cond |] in
    Cfg.Truth_test { ifso = true_label; ifnot = false_label }, arg

let branch_cond_id (block : Ssa.block) : Ssa.InstructionId.t option =
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
  let truncate arr n = if Array.length arr > n then Array.sub arr 0 n else arr

  let emit_op body op dbg rs rd =
    match Target.pseudoregs_for_operation op rs rd with
    | Constrained (rsrc, rdst) ->
      emit_moves body ~src:rs ~dst:(truncate rsrc (Array.length rs));
      DLL.add_end body (make_cfg_instr (Cfg.Op op) rsrc rdst dbg);
      emit_moves body ~src:(truncate rdst (Array.length rd)) ~dst:rd
    | Use_default_regs ->
      DLL.add_end body (make_cfg_instr (Cfg.Op op) rs rd dbg)

  let convert_block (renv : reg_env) ~label_of (block : Ssa.block) :
      Cfg.basic_block =
    let skip_id = branch_cond_id block in
    let body = DLL.make_empty () in
    (match block.desc with
    | CallContinuation _ -> (
      let virt_res = get_block_params_regs renv block in
      match Ssa.Block.Tbl.find_opt renv.call_result_locs block with
      | Some loc_res ->
        emit_moves body ~src:loc_res ~dst:virt_res;
        let stack_ofs =
          match Ssa.Block.Tbl.find_opt renv.call_stack_ofs block with
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
            let arg = Array.map (get_reg renv) args in
            let res = Ssa.InstructionId.Tbl.find renv.op_regs id in
            emit_op body op dbg arg res
        | Block_param _ | Proj _ -> ()
        | Pushtrap { handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Pushtrap { lbl_handler = label_of handler })
               [||] [||] Debuginfo.none)
        | Poptrap { handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Poptrap { lbl_handler = label_of handler })
               [||] [||] Debuginfo.none)
        | Stack_check { max_frame_size_bytes } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Stack_check { max_frame_size_bytes })
               [||] [||] Debuginfo.none)
        | Name_for_debugger { ident; provenance; which_parameter; regs } ->
          let regs = Array.map (get_reg renv) regs in
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Op
                  (Name_for_debugger
                     { ident; provenance; which_parameter; regs }))
               [||] [||] Debuginfo.none))
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
        let dst_regs = get_block_params_regs renv goto in
        let src_regs = Array.map (get_reg renv) args in
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
        make_cfg_instr (Cfg.Always (label_of goto)) [||] [||] dbg, false
      | Branch { conditions; else_goto } -> (
        match conditions with
        | [| (cond_instr, target_block) |] ->
          let term, arg =
            reconstruct_test renv cond_instr ~true_label:(label_of target_block)
              ~false_label:(label_of else_goto)
          in
          make_cfg_instr term arg [||] dbg, false
        | _ ->
          Misc.fatal_error
            "Cfg_of_ssa: multi-condition Branch not yet supported")
      | Switch (targets, args) ->
        let arg = Array.map (get_reg renv) args in
        let labels = Array.map label_of targets in
        make_cfg_instr (Cfg.Switch labels) arg [||] dbg, false
      | Return args ->
        let arg = Array.map (get_reg renv) args in
        let loc_res = Proc.loc_results_return (Reg.typv arg) in
        emit_moves body ~src:arg ~dst:loc_res;
        make_cfg_instr Cfg.Return loc_res [||] dbg, false
      | Raise (k, args, handler_block) ->
        let exn_val = Array.map (get_reg renv) args in
        let exn_bucket = [| Proc.loc_exn_bucket |] in
        let extra_dst =
          match handler_block with
          | Some hb ->
            let handler_regs = get_block_params_regs renv hb in
            if Array.length handler_regs > 1
            then Array.sub handler_regs 1 (Array.length handler_regs - 1)
            else [||]
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
        let virt_args = Array.map (get_reg renv) args in
        let loc_arg = Proc.loc_parameters (Reg.typv virt_args) in
        emit_moves body ~src:virt_args ~dst:loc_arg;
        ( make_cfg_instr
            (Cfg.Tailcall_self { destination = label_of destination })
            loc_arg [||] dbg,
          false )
      | Tailcall_func (call_op, args) ->
        let virt_args = Array.map (get_reg renv) args in
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
        let virt_args = Array.map (get_reg renv) args in
        let virt_res = get_block_params_regs renv continuation in
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
        Ssa.Block.Tbl.replace renv.call_result_locs continuation loc_res;
        Ssa.Block.Tbl.replace renv.call_stack_ofs continuation stack_ofs;
        ( make_cfg_instr
            (Cfg.Call { op = call_op; label_after = label_of continuation })
            call_arg loc_res dbg,
          true )
      | Prim { op = prim_op; args; continuation; exn_continuation = _ } -> (
        let virt_args = Array.map (get_reg renv) args in
        let virt_res = get_block_params_regs renv continuation in
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
            (fun i arg ->
              (* On some architectures, a single SSA argument may map to
                 multiple physical registers. Move the single virtual reg to the
                 first location reg; the rest are handled by the calling
                 convention. *)
              if Array.length locs.(i) = 1
              then emit_moves body ~src:[| arg |] ~dst:locs.(i)
              else
                DLL.add_end body
                  (make_cfg_instr (Cfg.Op Move) [| arg |]
                     [| locs.(i).(0) |]
                     Debuginfo.none))
            virt_args;
          Ssa.Block.Tbl.replace renv.call_result_locs continuation loc_res;
          Ssa.Block.Tbl.replace renv.call_stack_ofs continuation stack_ofs;
          ( make_cfg_instr
              (Cfg.Prim
                 { op = External { ext with stack_ofs; stack_align };
                   label_after = label_of continuation
                 })
              loc_arg loc_res dbg,
            true )
        | Probe _ ->
          ( make_cfg_instr
              (Cfg.Prim { op = prim_op; label_after = label_of continuation })
              virt_args virt_res dbg,
            true ))
      | Invalid { message; args; continuation } ->
        let virt_args = Array.map (get_reg renv) args in
        let ty_args = [Cmm.XInt] in
        let locs, stack_ofs, stack_align =
          Proc.loc_external_arguments ty_args
        in
        let loc_arg = Array.concat (Array.to_list locs) in
        Array.iteri
          (fun i arg ->
            if Array.length locs.(i) = 1
            then emit_moves body ~src:[| arg |] ~dst:locs.(i)
            else
              DLL.add_end body
                (make_cfg_instr (Cfg.Op Move) [| arg |]
                   [| locs.(i).(0) |]
                   Debuginfo.none))
          virt_args;
        let label_after, loc_res =
          match continuation with
          | Some cont_block ->
            let virt_res = get_block_params_regs renv cont_block in
            let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
            Ssa.Block.Tbl.replace renv.call_result_locs cont_block loc_res;
            Ssa.Block.Tbl.replace renv.call_stack_ofs cont_block stack_ofs;
            Some (label_of cont_block), loc_res
          | None -> None, [||]
        in
        ( make_cfg_instr
            (Cfg.Invalid { message; stack_ofs; stack_align; label_after })
            loc_arg loc_res dbg,
          true )
    in
    let is_trap_handler =
      match block.desc with TrapHandler _ -> true | _ -> false
    in
    let exn = None in
    { Cfg.start = label_of block;
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
    (* Assign a Label.t to each SSA block *)
    let block_labels = Ssa.Block.Tbl.create 64 in
    List.iter
      (fun (blk : Ssa.block) ->
        Ssa.Block.Tbl.replace block_labels blk (Cmm.new_label ()))
      ssa.blocks;
    let label_of blk = Ssa.Block.Tbl.find block_labels blk in
    let renv = create_reg_env () in
    List.iter
      (fun (block : Ssa.block) ->
        allocate_block_params_regs renv block;
        Array.iter (allocate_body_regs renv) block.body)
      ssa.blocks;
    (* Pre-compute calling convention locations for all calls *)
    List.iter
      (fun (block : Ssa.block) ->
        match block.terminator with
        | Call { op = call_op; args; continuation; _ } ->
          let virt_args = Array.map (get_reg renv) args in
          let virt_res = get_block_params_regs renv continuation in
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
          Ssa.Block.Tbl.replace renv.call_result_locs continuation loc_res;
          Ssa.Block.Tbl.replace renv.call_stack_ofs continuation stack_ofs
        | Prim { op = External { ty_args; _ }; args; continuation; _ } ->
          let virt_args = Array.map (get_reg renv) args in
          let virt_res = get_block_params_regs renv continuation in
          let ty_args =
            if ty_args = []
            then Array.to_list (Array.map (fun _ -> Cmm.XInt) virt_args)
            else ty_args
          in
          let _locs, stack_ofs, _align = Proc.loc_external_arguments ty_args in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          Ssa.Block.Tbl.replace renv.call_result_locs continuation loc_res;
          Ssa.Block.Tbl.replace renv.call_stack_ofs continuation stack_ofs
        | _ -> ())
      ssa.blocks;
    let fun_arg_regs = get_block_params_regs renv ssa.entry in
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
               [||] [||] Debuginfo.none))
      ssa.fun_args_names;
    let entry_label = label_of ssa.entry in
    let entry_block =
      { Cfg.start = Cfg.entry_label cfg;
        body = entry_body;
        terminator =
          make_cfg_instr (Cfg.Always entry_label) [||] [||] Debuginfo.none;
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
      (fun (ssa_block : Ssa.block) ->
        let label = label_of ssa_block in
        let cfg_block = convert_block renv ~label_of ssa_block in
        cfg_block.can_raise
          <- Cfg.can_raise_terminator cfg_block.terminator.desc;
        if Ssa.block_equal ssa_block ssa.entry
        then (
          let poll_instr =
            make_cfg_instr (Cfg.Op Poll) [||] [||] Debuginfo.none
          in
          prologue_poll_instr_id := Some poll_instr.id;
          DLL.add_begin cfg_block.body poll_instr;
          Array.iter2
            (fun s d ->
              if not (Reg.same s d)
              then
                DLL.add_begin cfg_block.body
                  (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
            (Array.of_list (List.rev (Array.to_list loc_arg)))
            (Array.of_list (List.rev (Array.to_list fun_arg_regs)));
          let param_namings = ref [] in
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
                param_namings
                  := make_cfg_instr
                       (Cfg.Op
                          (Name_for_debugger
                             { ident;
                               provenance = prov;
                               which_parameter = Some pi;
                               regs
                             }))
                       [||] [||] Debuginfo.none
                     :: !param_namings)
            ssa.fun_args_names;
          List.iter (DLL.add_begin cfg_block.body) !param_namings);
        if Cfg.is_return_terminator cfg_block.terminator.desc
        then
          DLL.add_end cfg_block.body
            (make_cfg_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
        Cfg.add_block_exn cfg cfg_block;
        DLL.add_end layout label)
      ssa.blocks;
    Select_utils.Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
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
    let cfg_with_layout = Cfg_simplify.run cfg_with_layout in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let fun_contains_calls =
      Label.Tbl.fold
        (fun _label block acc -> acc || Cfg.basic_block_contains_calls block)
        cfg.blocks false
    in
    let cfg = { cfg with fun_contains_calls } in
    Cfg_with_layout.create cfg ~layout:(Cfg_with_layout.layout cfg_with_layout)
end
