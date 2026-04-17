[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let instr_seq = Sub_cfg.instr_id

type env =
  { op_regs : Reg.t array Ssa.InstructionId.Tbl.t;
    block_params_regs : Reg.t array Ssa.Block.Tbl.t;
    block_labels : Label.t Ssa.Block.Tbl.t;
    call_result_locs : Reg.t array Ssa.Block.Tbl.t;
    call_stack_ofs : int Ssa.Block.Tbl.t
  }

let create_env () =
  { op_regs = Ssa.InstructionId.Tbl.create 256;
    block_params_regs = Ssa.Block.Tbl.create 64;
    block_labels = Ssa.Block.Tbl.create 64;
    call_result_locs = Ssa.Block.Tbl.create 16;
    call_stack_ofs = Ssa.Block.Tbl.create 16
  }

let label_of env blk = Ssa.Block.Tbl.find env.block_labels blk

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
    (Ssa.InstructionId.Tbl.find env.op_regs id).(index)
  | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ | Proj _ ->
    Misc.fatal_error "Cfg_of_ssa.get_reg: unexpected instruction"

let get_block_params_regs env (block : Ssa.block) =
  try Ssa.Block.Tbl.find env.block_params_regs block
  with Not_found ->
    Misc.fatal_errorf "Cfg_of_ssa: no regs for block params of %d"
      (Ssa.block_id block)

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

(* A Branch condition that can be folded into the CFG test terminator, consuming
   the Op's args directly. *)
let fuse_comparison (cond : Ssa.instruction) ~true_label ~false_label :
    (Cfg.terminator * Ssa.instruction array) option =
  match cond with
  | Op { op = Intop_imm (Icomp Cne, 0); args; _ } ->
    Some (Cfg.Truth_test { ifso = true_label; ifnot = false_label }, args)
  | Op { op = Intop (Icomp cmp); args; _ } ->
    Some
      ( Select_utils.terminator_of_test (Iinttest cmp) ~label_true:true_label
          ~label_false:false_label,
        args )
  | Op { op = Intop_imm (Icomp cmp, n); args; _ } ->
    Some
      ( Select_utils.terminator_of_test
          (Iinttest_imm (cmp, n))
          ~label_true:true_label ~label_false:false_label,
        args )
  | Op { op = Floatop (w, Icompf cmp); args; _ } ->
    Some
      ( Select_utils.terminator_of_test
          (Ifloattest (w, cmp))
          ~label_true:true_label ~label_false:false_label,
        args )
  | Op { op = Intop_imm (Iand, 1); args; _ } ->
    Some (Cfg.Parity_test { ifso = false_label; ifnot = true_label }, args)
  | _ -> None

(* Decrement usage counts of comparisons that will be fused into branch
   terminators. Must run before block conversion so the body emission pass sees
   the post-fusion counts. After fusion, the branch terminator uses the
   comparison's args directly, so we pre-increment their use counts to keep
   them alive across the (potentially recursive) decrement. *)
let decrement_fused_branch_conditions (ssa : Ssa.t) =
  List.iter
    (fun (block : Ssa.block) ->
      match[@warning "-fragile-match"] block.terminator with
      | Branch { cond; _ }
        when Option.is_some
               (fuse_comparison cond ~true_label:Label.none
                  ~false_label:Label.none) -> (
        match[@warning "-fragile-match"] cond with
        | Op { args; _ } ->
          Array.iter Ssa.increment_use args;
          Ssa.decrement_use cond
        | _ -> ())
      | _ -> ())
    ssa.blocks

(* Bump every Op with usage_count = 0 up to 1. This disables dead-code
   elimination for naturally-unused Ops, matching the non-SSA pipeline's
   behaviour (and keeping cfg_compare happy). Ops that were decremented by the
   fusion pre-pass are unaffected. *)
let bump_unused_op_counts (ssa : Ssa.t) =
  List.iter
    (fun (block : Ssa.block) ->
      Array.iter
        (fun (i : Ssa.instruction) ->
          match i with
          | Op r when r.usage_count = 0 -> Ssa.increment_use i
          | Op _ | Block_param _ | Proj _ | Push_trap _ | Pop_trap _
          | Stack_check _ | Name_for_debugger _ ->
            ())
        block.body)
    ssa.blocks

let truncate arr n = if Array.length arr > n then Array.sub arr 0 n else arr

let emit_op body op dbg rs rd : Cfg.basic Cfg.instruction =
  match Cfg_selection.pseudoregs_for_operation op rs rd with
  | Constrained (rsrc, rdst) ->
    emit_moves body ~src:rs ~dst:(truncate rsrc (Array.length rs));
    let main = make_cfg_instr (Cfg.Op op) rsrc rdst dbg in
    DLL.add_end body main;
    emit_moves body ~src:(truncate rdst (Array.length rd)) ~dst:rd;
    main
  | Use_default_regs ->
    let main = make_cfg_instr (Cfg.Op op) rs rd dbg in
    DLL.add_end body main;
    main

let convert_block (env : env) (block : Ssa.block) : Cfg.basic_block =
  let body = DLL.make_empty () in
  (match block.desc with
  | Call_continuation _ -> (
    let virt_res = get_block_params_regs env block in
    match Ssa.Block.Tbl.find_opt env.call_result_locs block with
    | Some loc_res ->
      emit_moves body ~src:loc_res ~dst:virt_res;
      let stack_ofs =
        match Ssa.Block.Tbl.find_opt env.call_stack_ofs block with
        | Some ofs -> ofs
        | None -> 0
      in
      if stack_ofs <> 0
      then
        DLL.add_end body
          (make_cfg_instr (Cfg.Op (Stackoffset (-stack_ofs))) [||] [||]
             Debuginfo.none)
    | None -> ())
  | Trap_handler _ ->
    let virt_res = get_block_params_regs env block in
    let exn_bucket = [| Proc.loc_exn_bucket |] in
    let first_param = [| virt_res.(0) |] in
    emit_moves body ~src:exn_bucket ~dst:first_param
  | Merge _ | Loop _ | Branch_target _ | Function_start -> ());
  Array.iter
    (fun (i : Ssa.instruction) ->
      match i with
      | Op { id; op; args; dbg; typ; usage_count } ->
        let regs = Reg.createv typ in
        Ssa.InstructionId.Tbl.replace env.op_regs id regs;
        if usage_count > 0
        then
          let arg = Array.map (get_reg env) args in
          let (_ : Cfg.basic Cfg.instruction) = emit_op body op dbg arg regs in
          ()
      | Block_param _ | Proj _ -> ()
      | Push_trap { handler } ->
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Pushtrap { lbl_handler = label_of env handler })
             [||] [||] Debuginfo.none)
      | Pop_trap { handler } ->
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Poptrap { lbl_handler = label_of env handler })
             [||] [||] Debuginfo.none)
      | Stack_check { max_frame_size_bytes } ->
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Stack_check { max_frame_size_bytes })
             [||] [||] Debuginfo.none)
      | Name_for_debugger { ident; provenance; which_parameter; regs } ->
        let regs = Array.map (get_reg env) regs in
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Op
                (Name_for_debugger { ident; provenance; which_parameter; regs }))
             [||] [||] Debuginfo.none))
    block.body;
  let dbg = block.terminator_dbg in
  let terminator, can_raise =
    match block.terminator with
    | Pending_construction ->
      Misc.fatal_error
        "Cfg_of_ssa: Pending_construction terminator in finished graph"
    | Goto { goto; args } ->
      let dst_regs = get_block_params_regs env goto in
      let src_regs = Array.map (get_reg env) args in
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
      make_cfg_instr (Cfg.Always (label_of env goto)) [||] [||] dbg, false
    | Branch { cond; ifso; ifnot } -> (
      let true_label = label_of env ifso in
      let false_label = label_of env ifnot in
      match fuse_comparison cond ~true_label ~false_label with
      | Some (term, args) ->
        make_cfg_instr term (Array.map (get_reg env) args) [||] dbg, false
      | None ->
        ( make_cfg_instr
            (Cfg.Truth_test { ifso = true_label; ifnot = false_label })
            [| get_reg env cond |]
            [||] dbg,
          false ))
    | Switch (targets, args) ->
      let arg = Array.map (get_reg env) args in
      let labels = Array.map (label_of env) targets in
      make_cfg_instr (Cfg.Switch labels) arg [||] dbg, false
    | Return args ->
      let arg = Array.map (get_reg env) args in
      let loc_res = Proc.loc_results_return (Reg.typv arg) in
      emit_moves body ~src:arg ~dst:loc_res;
      make_cfg_instr Cfg.Return loc_res [||] dbg, false
    | Raise (k, args, handler_block) ->
      let exn_val = Array.map (get_reg env) args in
      let exn_bucket = [| Proc.loc_exn_bucket |] in
      let extra_dst =
        match handler_block with
        | Some hb ->
          let handler_regs = get_block_params_regs env hb in
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
      let virt_args = Array.map (get_reg env) args in
      let loc_arg = Proc.loc_parameters (Reg.typv virt_args) in
      emit_moves body ~src:virt_args ~dst:loc_arg;
      ( make_cfg_instr
          (Cfg.Tailcall_self { destination = label_of env destination })
          loc_arg [||] dbg,
        false )
    | Tailcall_func (call_op, args) ->
      let virt_args = Array.map (get_reg env) args in
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
      let virt_args = Array.map (get_reg env) args in
      let virt_res = get_block_params_regs env continuation in
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
      let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv virt_res) in
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
      Ssa.Block.Tbl.replace env.call_result_locs continuation loc_res;
      Ssa.Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
      ( make_cfg_instr
          (Cfg.Call { op = call_op; label_after = label_of env continuation })
          call_arg loc_res dbg,
        true )
    | Prim { op = prim_op; args; continuation; exn_continuation = _ } -> (
      let virt_args = Array.map (get_reg env) args in
      let virt_res = get_block_params_regs env continuation in
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
        let ty_args_arr = Array.of_list ty_args in
        Array.iteri
          (fun i arg ->
            let src = [| arg |] in
            let dst = locs.(i) in
            if Array.length dst = 1
            then
              let ty_arg = ty_args_arr.(i) in
              match Cfg_selection.insert_move_extcall_arg ty_arg src dst with
              | Rewritten (basic, src, dst) ->
                DLL.add_end body (make_cfg_instr basic src dst dbg)
              | Use_default -> emit_moves body ~src ~dst
            else
              DLL.add_end body
                (make_cfg_instr (Cfg.Op Move) [| arg |]
                   [| dst.(0) |]
                   Debuginfo.none))
          virt_args;
        Ssa.Block.Tbl.replace env.call_result_locs continuation loc_res;
        Ssa.Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
        ( make_cfg_instr
            (Cfg.Prim
               { op = External { ext with stack_ofs; stack_align };
                 label_after = label_of env continuation
               })
            loc_arg loc_res dbg,
          true )
      | Probe _ ->
        ( make_cfg_instr
            (Cfg.Prim { op = prim_op; label_after = label_of env continuation })
            virt_args virt_res dbg,
          true ))
    | Invalid { message; args; continuation } ->
      let virt_args = Array.map (get_reg env) args in
      let ty_args = [Cmm.XInt] in
      let locs, stack_ofs, stack_align = Proc.loc_external_arguments ty_args in
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
          let virt_res = get_block_params_regs env cont_block in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          Ssa.Block.Tbl.replace env.call_result_locs cont_block loc_res;
          Ssa.Block.Tbl.replace env.call_stack_ofs cont_block stack_ofs;
          Some (label_of env cont_block), loc_res
        | None -> None, [||]
      in
      ( make_cfg_instr
          (Cfg.Invalid { message; stack_ofs; stack_align; label_after })
          loc_arg loc_res dbg,
        true )
  in
  let is_trap_handler =
    match block.desc with Trap_handler _ -> true | _ -> false
  in
  let exn = None in
  { Cfg.start = label_of env block;
    body;
    terminator;
    predecessors = Label.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    exn;
    can_raise;
    is_trap_handler;
    cold = false
  }

let convert ~keep_unused_ops ~future_funcnames (ssa : Ssa.t) : Cfg_with_layout.t
    =
  if keep_unused_ops then bump_unused_op_counts ssa;
  decrement_fused_branch_conditions ssa;
  let env = create_env () in
  ssa.blocks
  |> List.iter (fun (block : Ssa.block) ->
      Ssa.Block.Tbl.replace env.block_labels block (Cmm.new_label ());
      Ssa.Block.Tbl.replace env.block_params_regs block
        (Reg.createv block.params));
  let fun_arg_regs = get_block_params_regs env ssa.entry in
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
  let entry_label = label_of env ssa.entry in
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
      let label = label_of env ssa_block in
      let cfg_block = convert_block env ssa_block in
      cfg_block.can_raise <- Cfg.can_raise_terminator cfg_block.terminator.desc;
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
           ~fun_name:ssa.fun_name ~optimistic_prologue_poll_instr_id:poll_id cfg)
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
