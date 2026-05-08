open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

(** SSA → CFG lowering.

    Walks an {!Ssa.Finished_graph} and produces a [Cfg_with_layout.t] suitable
    for the rest of the backend pipeline. The conversion is mostly mechanical:
    each SSA block becomes a CFG block at a fresh label, instructions are
    translated 1-1, and SSA references (op results, block params) become Reg
    arrays.

    Every block parameter gets one virtual register, every [Op] gets a virtual
    reg vector sized by its [typ], and the conversion routes values between them
    with explicit Move instructions.

    Block edges:
    - [Goto] args are realised as parallel moves into the destination block's
      param regs. When src and dst overlap (cyclic or otherwise), we route
      through a fresh temp vector to avoid clobbering live values; otherwise the
      moves go directly. {!Cfg_selectgen} always uses a temp vector.
    - [Call] results are moved from [Proc.loc_results_call] to the
      continuation's block param regs at the start of the continuation.

    Trap stack: {!Ssa.finish} populates each block's [block_end_trap_stack]; we
    use it to (a) decide which blocks are trap predecessors (their [Raise] /
    may-raise [Call] branches to the topmost handler), and (b) emit [Poptrap]
    instructions before [Return] terminators so the runtime trap stack is empty
    on function exit.

    Dead code: the [keep_unused_ops] flag controls whether [Op]s with
    [usage_count = 0] are emitted. With the flag, naturally unused ops have
    their counts bumped so they appear in the CFG (matching the baseline
    pipeline's output, used before [Cfg_compare]). Without it, dead ops are
    skipped to keep the CFG lean.

    Comparisons feeding [Branch]: [collect_fusion_adjustments] decrements the
    use count of the comparison [Op] so [fuse_comparison] can splice it directly
    into the CFG test terminator without leaving the now-dead Op in the body.

    Unreachable trap handlers: [Push_trap] / [Pop_trap] referencing a handler
    block that was dropped (because it became unreachable) are skipped during
    lowering rather than routed through a stub. The matching push/pop pair both
    target the same handler, so they are dropped together and trap-stack balance
    is preserved. [Cfg_compare] knows to ignore [Pushtrap]/[Poptrap] whose
    target has no predecessors so the baseline pipeline (which keeps unreachable
    handlers as segfault stubs) still matches. *)

module DLL = Oxcaml_utils.Doubly_linked_list

let instr_seq = Sub_cfg.instr_id

module Make (Ssa_graph : Ssa.Finished_graph) = struct
  open Ssa_graph

  type env =
    { op_regs : Reg.t array Instruction_id.Tbl.t;
      block_params_regs : Reg.t array Block.Tbl.t;
      block_labels : Label.t Block.Tbl.t;
      call_result_locs : Reg.t array Block.Tbl.t;
      call_stack_ofs : int Block.Tbl.t;
      use_count_adjustments : int Instruction_id.Tbl.t
          (* Per-Op adjustments applied to [op_data.usage_count] to decide which
             Ops to emit. The combination [usage_count + adjustment] determines
             liveness; an Op with adjusted count [<= 0] is skipped. Populated
             before block conversion to (a) decrement Ops consumed by fused
             branch terminators and (b) bump naturally unused Ops to keep them
             in the CFG when [keep_unused_ops] is set. *)
    }

  let create_env () =
    { op_regs = Instruction_id.Tbl.create 256;
      block_params_regs = Block.Tbl.create 64;
      block_labels = Block.Tbl.create 64;
      call_result_locs = Block.Tbl.create 16;
      call_stack_ofs = Block.Tbl.create 16;
      use_count_adjustments = Instruction_id.Tbl.create 16
    }

  let label_of env (blk : Block.t) =
    try Block.Tbl.find env.block_labels blk
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa.label_of: block %d not in block_labels"
        (blk.id :> int)

  let get_reg env (i : Instruction.t) : Reg.t =
    match i with
    | Op { id; _ } ->
      let regs =
        try Instruction_id.Tbl.find env.op_regs id
        with Not_found ->
          Misc.fatal_errorf "Cfg_of_ssa.get_reg: no regs for Op v%d" (id :> int)
      in
      assert (Array.length regs = 1);
      regs.(0)
    | Block_param { block; index; _ } -> (
      try
        let regs = Block.Tbl.find env.block_params_regs block in
        regs.(index)
      with Not_found ->
        Misc.fatal_errorf "Cfg_of_ssa: no regs for Block_param %d.%d"
          (block.id :> int)
          index)
    | Proj { index; src = Op { id; _ } } ->
      let regs =
        try Instruction_id.Tbl.find env.op_regs id
        with Not_found ->
          Misc.fatal_errorf "Cfg_of_ssa.get_reg: no regs for Op v%d (Proj)"
            (id :> int)
      in
      regs.(index)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
    | Proj _ ->
      Misc.fatal_error "Cfg_of_ssa.get_reg: unexpected instruction"

  let get_block_params_regs env (block : Block.t) =
    try Block.Tbl.find env.block_params_regs block
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa: no regs for block params of %d"
        (block.id :> int)

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

  (** A Branch condition that can be folded into the CFG test terminator,
      consuming the comparison's args directly. *)
  let fuse_comparison (cond : Instruction.t) ~true_label ~false_label :
      (Cfg.terminator * Instruction.t array) option =
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

  (** For a call op, return the args to actually pass on the stack/regs (which
      skips the function-pointer arg for [Indirect]), the matching hard-reg
      locations, and the stack offset. *)
  let args_and_locations (call_op : Cfg_intf.S.func_call_operation) virt_args =
    let rarg =
      match call_op with
      | Indirect _ -> Array.sub virt_args 1 (Array.length virt_args - 1)
      | Direct _ -> virt_args
    in
    let loc_arg, stack_ofs = Proc.loc_arguments (Reg.typv rarg) in
    rarg, loc_arg, stack_ofs

  let call_arg_for_terminator (call_op : Cfg_intf.S.func_call_operation)
      virt_args loc_arg =
    match call_op with
    | Indirect _ -> Array.append [| virt_args.(0) |] loc_arg
    | Direct _ -> loc_arg

  (** Move [virt_args] into the locations dictated by
      [Proc.loc_external_arguments ty_args], extending the stack as needed and
      using the per-arg move sequence produced by
      [Cfg_selection.insert_move_extcall_arg]. Returns the flattened locations,
      the stack offset, and the stack alignment. *)
  let move_to_extcall_arg_locs body (ty_args : Cmm.exttype list) virt_args dbg :
      Reg.t array * int * Cmm.stack_align =
    let ty_args =
      if List.is_empty ty_args
      then List.init (Array.length virt_args) (fun _ -> Cmm.XInt)
      else ty_args
    in
    let locs, stack_ofs, stack_align = Proc.loc_external_arguments ty_args in
    let ty_args_arr = Array.of_list ty_args in
    if stack_ofs <> 0
    then
      DLL.add_end body
        (make_cfg_instr (Cfg.Op (Stackoffset stack_ofs)) [||] [||]
           Debuginfo.none);
    Array.iteri
      (fun i arg ->
        let src = [| arg |] in
        let dst = locs.(i) in
        match Cfg_selection.insert_move_extcall_arg ty_args_arr.(i) src dst with
        | Rewritten (basic, src, dst) ->
          DLL.add_end body (make_cfg_instr basic src dst dbg)
        | Use_default -> emit_moves body ~src ~dst)
      virt_args;
    Array.concat (Array.to_list locs), stack_ofs, stack_align

  let is_trap_predecessor (pred : Block.t) (target : Block.t) =
    match trap_successor pred with
    | Some h -> Block.equal h target
    | None -> false

  let is_call_predecessor (pred : Block.t) (target : Block.t) =
    match[@warning "-fragile-match"] pred.terminator with
    | Invalid { continuation = Some continuation; _ }
    | Call { op = Func _ | Prim (External _); continuation; _ } ->
      Block.equal continuation target
    | _ -> false

  (** The incoming ABI for trap handlers is different. Thus, we require ALL
      predecessors to be trap predecessors. We could relax this in the future by
      inserting a merge block to join the non-trap predecessors. *)
  let is_trap_handler (block : Block.t) =
    let any_exn =
      Block.Set.exists (fun p -> is_trap_predecessor p block) block.predecessors
    in
    let all_exn =
      Block.Set.for_all
        (fun p -> is_trap_predecessor p block)
        block.predecessors
    in
    if any_exn && not all_exn
    then
      Misc.fatal_errorf
        "Cfg_of_ssa: block %d mixes trap and non-trap predecessors."
        (block.id :> int);
    any_exn

  (** A call continuation must be reached via a single call/prim predecessor:
      the runtime puts the call's return values in fixed locations. We could
      relax this in the future by automatically splitting the edges. *)
  let is_call_continuation (block : Block.t) =
    let result =
      Block.Set.exists (fun p -> is_call_predecessor p block) block.predecessors
    in
    if result && Block.Set.cardinal block.predecessors != 1
    then
      Misc.fatal_errorf
        "Cfg_of_ssa: block %d has a call predecessor alongside other \
         predecessors."
        (block.id :> int);
    result

  let convert_block (env : env) (block : Block.t) : Cfg.basic_block =
    let body = DLL.make_empty () in
    if is_call_continuation block
    then begin
      let loc_res = Block.Tbl.find env.call_result_locs block in
      let virt_res = get_block_params_regs env block in
      emit_moves body ~src:loc_res ~dst:virt_res;
      let stack_ofs =
        Block.Tbl.find_opt env.call_stack_ofs block |> Option.value ~default:0
      in
      if stack_ofs <> 0
      then
        DLL.add_end body
          (make_cfg_instr (Cfg.Op (Stackoffset (-stack_ofs))) [||] [||]
             Debuginfo.none)
    end;
    if is_trap_handler block
    then begin
      let virt_res = get_block_params_regs env block in
      let exn_bucket = [| Proc.loc_exn_bucket |] in
      let first_param = [| virt_res.(0) |] in
      emit_moves body ~src:exn_bucket ~dst:first_param
    end;
    Array.iter
      (fun (i : Instruction.t) ->
        match i with
        | Op { id; op; args; dbg; typ; usage_count } ->
          let regs = Reg.createv typ in
          Instruction_id.Tbl.replace env.op_regs id regs;
          let adjustment =
            Instruction_id.Tbl.find_opt env.use_count_adjustments id
            |> Option.value ~default:0
          in
          if usage_count + adjustment > 0
          then
            let arg = Array.map (get_reg env) args in
            let (_ : Cfg.basic Cfg.instruction) =
              emit_op body op dbg arg regs
            in
            ()
        | Block_param _ | Proj _ | Tuple _ ->
          Misc.fatal_error
            "Cfg_of_ssa: virtual instruction (Block_param/Proj/Tuple) must not \
             appear in a block body"
        | (Push_trap { handler } | Pop_trap { handler })
          when Block.Set.is_empty handler.predecessors ->
          ()
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
                  (Name_for_debugger
                     { ident; provenance; which_parameter; regs }))
               [||] [||] Debuginfo.none))
      block.body;
    let dbg = block.terminator_dbg in
    let terminator =
      match block.terminator with
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
        make_cfg_instr (Cfg.Always (label_of env goto)) [||] [||] dbg
      | Branch { cond; ifso; ifnot } -> (
        let true_label = label_of env ifso in
        let false_label = label_of env ifnot in
        match fuse_comparison cond ~true_label ~false_label with
        | Some (term, args) ->
          make_cfg_instr term (Array.map (get_reg env) args) [||] dbg
        | None ->
          make_cfg_instr
            (Cfg.Truth_test { ifso = true_label; ifnot = false_label })
            [| get_reg env cond |]
            [||] dbg)
      | Switch { index; targets } ->
        let index_reg = get_reg env index in
        let labels = Array.map (label_of env) targets in
        make_cfg_instr (Cfg.Switch labels) [| index_reg |] [||] dbg
      | Return { args } ->
        (* Emit a [Poptrap] for each handler still on the trap stack at block
           exit, so the runtime trap stack is empty when the function
           returns. *)
        List.iter
          (fun (h : Block.t) ->
            DLL.add_end body
              (make_cfg_instr
                 (Cfg.Poptrap { lbl_handler = label_of env h })
                 [||] [||] Debuginfo.none))
          block.block_end_trap_stack;
        let arg = Array.map (get_reg env) args in
        let loc_res = Proc.loc_results_return (Reg.typv arg) in
        emit_moves body ~src:arg ~dst:loc_res;
        DLL.add_end body
          (make_cfg_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
        make_cfg_instr Cfg.Return loc_res [||] dbg
      | Raise { raise_kind; args } ->
        let exn_val = Array.map (get_reg env) args in
        let exn_bucket = [| Proc.loc_exn_bucket |] in
        let extra_dst =
          match trap_successor block with
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
        make_cfg_instr (Cfg.Raise raise_kind) exn_bucket [||] dbg
      | Tailcall_self { destination; args } ->
        let virt_args = Array.map (get_reg env) args in
        let loc_arg = Proc.loc_parameters (Reg.typv virt_args) in
        emit_moves body ~src:virt_args ~dst:loc_arg;
        make_cfg_instr
          (Cfg.Tailcall_self { destination = label_of env destination })
          loc_arg [||] dbg
      | Tailcall_func { op = call_op; args } ->
        let virt_args = Array.map (get_reg env) args in
        let rarg, loc_arg, _stack_ofs = args_and_locations call_op virt_args in
        emit_moves body ~src:rarg ~dst:loc_arg;
        make_cfg_instr (Cfg.Tailcall_func call_op)
          (call_arg_for_terminator call_op virt_args loc_arg)
          [||] dbg
      | Call { op; args; continuation; may_raise = _ } -> (
        let virt_args = Array.map (get_reg env) args in
        let virt_res = get_block_params_regs env continuation in
        match op with
        | Func call_op ->
          let rarg, loc_arg, stack_ofs_args =
            args_and_locations call_op virt_args
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
          Block.Tbl.replace env.call_result_locs continuation loc_res;
          Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
          make_cfg_instr
            (Cfg.Call { op = call_op; label_after = label_of env continuation })
            (call_arg_for_terminator call_op virt_args loc_arg)
            loc_res dbg
        | Prim (External ({ ty_args; _ } as ext)) ->
          let loc_arg, stack_ofs, stack_align =
            move_to_extcall_arg_locs body ty_args virt_args dbg
          in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          Block.Tbl.replace env.call_result_locs continuation loc_res;
          Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
          make_cfg_instr
            (Cfg.Prim
               { op = External { ext with stack_ofs; stack_align };
                 label_after = label_of env continuation
               })
            loc_arg loc_res dbg
        | Prim (Probe _ as prim_op) ->
          make_cfg_instr
            (Cfg.Prim { op = prim_op; label_after = label_of env continuation })
            virt_args virt_res dbg)
      | Invalid { message; args; continuation } ->
        let virt_args = Array.map (get_reg env) args in
        let loc_arg, stack_ofs, stack_align =
          move_to_extcall_arg_locs body [Cmm.XInt] virt_args dbg
        in
        let label_after, loc_res =
          match continuation with
          | Some cont_block ->
            let virt_res = get_block_params_regs env cont_block in
            let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
            Block.Tbl.replace env.call_result_locs cont_block loc_res;
            Block.Tbl.replace env.call_stack_ofs cont_block stack_ofs;
            Some (label_of env cont_block), loc_res
          | None -> None, [||]
        in
        make_cfg_instr
          (Cfg.Invalid { message; stack_ofs; stack_align; label_after })
          loc_arg loc_res dbg
    in
    let can_raise = Cfg.can_raise_terminator terminator.desc in
    { Cfg.start = label_of env block;
      body;
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = Cfg.invalid_stack_offset;
      exn = None;
      can_raise;
      is_trap_handler = is_trap_handler block;
      cold = false
    }

  let adjust_use_count env id delta =
    let cur =
      Instruction_id.Tbl.find_opt env.use_count_adjustments id
      |> Option.value ~default:0
    in
    Instruction_id.Tbl.replace env.use_count_adjustments id (cur + delta)

  (** Decrement use counts of comparisons that will be folded into branch
      terminators, and increment their inputs to compensate. The branch
      terminator consumes the comparison's args directly, so the comparison
      itself is no longer needed (provided no other consumer references it), and
      its args must be kept alive across the (now-implicit) drop. *)
  let collect_fusion_adjustments env =
    let rec bump_arg (i : Instruction.t) =
      match i with
      | Op { id; _ } -> adjust_use_count env id 1
      | Proj { src; _ } -> bump_arg src
      | Block_param _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
      | Name_for_debugger _ ->
        ()
    in
    List.iter
      (fun (block : Block.t) ->
        match[@warning "-fragile-match"] block.terminator with
        | Branch { cond; _ } ->
          fuse_comparison cond ~true_label:Label.none ~false_label:Label.none
          |> Option.iter (fun (_, args) ->
              (match[@warning "-fragile-match"] cond with
              | Op { id; _ } -> adjust_use_count env id (-1)
              | _ -> ());
              Array.iter bump_arg args)
        | _ -> ())
      blocks

  (** Bump every Op with [usage_count = 0] up by 1. This disables dead-code
      elimination for unused Ops, matching the non-SSA pipeline's behaviour (and
      keeping cfg_compare happy). *)
  let keep_unused_ops_alive env =
    List.iter
      (fun (block : Block.t) ->
        Array.iter
          (fun (instr : Instruction.t) ->
            match instr with
            | Op r when r.usage_count = 0 -> adjust_use_count env r.id 1
            | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
            | Stack_check _ | Name_for_debugger _ ->
              ())
          block.body)
      blocks

  let allocate_block_labels_and_param_regs env =
    List.iter
      (fun (block : Block.t) ->
        Block.Tbl.replace env.block_labels block (Label.new_label ());
        Block.Tbl.replace env.block_params_regs block
          (Reg.createv (params_machtype block)))
      blocks

  let param_naming_instrs ~loc_arg =
    let idx = ref 0 in
    let param_index = ref (-1) in
    function_info.fun_args_names
    |> List.filter_map (fun (var, ty) ->
        param_index := !param_index + 1;
        let provenance = Backend_var.With_provenance.provenance var in
        let ident = Backend_var.With_provenance.var var in
        let n = Array.length ty in
        let regs = Array.init n (fun i -> loc_arg.(!idx + i)) in
        idx := !idx + n;
        if Option.is_some provenance
        then
          Some
            (make_cfg_instr
               (Cfg.Op
                  (Name_for_debugger
                     { ident;
                       provenance;
                       which_parameter = Some !param_index;
                       regs
                     }))
               [||] [||] Debuginfo.none)
        else None)

  (** Build the CFG-level entry block at [Cfg.entry_label cfg]. Note that this
      is a separate block inserted before the one corresponding to [entry]. *)
  let make_entry_block cfg ~ssa_entry_label ~loc_arg : Cfg.basic_block =
    let body = DLL.make_empty () in
    param_naming_instrs ~loc_arg |> List.iter (DLL.add_end body);
    { Cfg.start = Cfg.entry_label cfg;
      body;
      terminator =
        make_cfg_instr (Cfg.Always ssa_entry_label) [||] [||] Debuginfo.none;
      predecessors = Label.Set.empty;
      stack_offset = Cfg.invalid_stack_offset;
      exn = None;
      can_raise = false;
      is_trap_handler = false;
      cold = false
    }

  (** Prepend the function-entry prologue to [entry]'s CFG block: param
      [Name_for_debugger]s, moves from ABI param locations into the entry's
      virtual param regs, then an optimistic [Poll] (whose id is returned so
      {!drop_optimistic_prologue_poll} can later remove it if unneeded). The
      moves and namings are added with [add_begin] in reverse order so they end
      up in their natural left-to-right order. *)
  let prepend_entry_prologue (cfg_block : Cfg.basic_block) ~loc_arg
      ~fun_arg_regs : InstructionId.t =
    let poll_instr = make_cfg_instr (Cfg.Op Poll) [||] [||] Debuginfo.none in
    DLL.add_begin cfg_block.body poll_instr;
    let loc_arg_rev = Array.to_list loc_arg |> List.rev in
    let fun_arg_regs_rev = Array.to_list fun_arg_regs |> List.rev in
    List.iter2
      (fun s d ->
        if not (Reg.same s d)
        then
          DLL.add_begin cfg_block.body
            (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
      loc_arg_rev fun_arg_regs_rev;
    List.rev (param_naming_instrs ~loc_arg)
    |> List.iter (DLL.add_begin cfg_block.body);
    poll_instr.id

  (** Convert each SSA block to a CFG block and append it to [cfg] and [layout].
  *)
  let convert_and_emit_blocks cfg layout env ~loc_arg ~fun_arg_regs :
      InstructionId.t =
    let prologue_poll_instr_id = ref None in
    List.iter
      (fun (ssa_block : Block.t) ->
        let cfg_block = convert_block env ssa_block in
        cfg_block.can_raise
          <- Cfg.can_raise_terminator cfg_block.terminator.desc;
        if Block.equal ssa_block entry
        then
          prologue_poll_instr_id
            := Some (prepend_entry_prologue cfg_block ~loc_arg ~fun_arg_regs);
        Cfg.add_block_exn cfg cfg_block;
        DLL.add_end layout cfg_block.start)
      blocks;
    match !prologue_poll_instr_id with
    | Some id -> id
    | None ->
      Misc.fatal_error
        "Cfg_of_ssa: function-entry block was not visited by \
         convert_and_emit_blocks"

  (** Remove the optimistic prologue [Poll] inserted by
      {!prepend_entry_prologue} unless [Cfg_polling] determines it is required
      for safe-point coverage. *)
  let drop_optimistic_prologue_poll cfg env ~prologue_poll_instr_id ~funcnames =
    if
      not
        (Cfg_polling.requires_prologue_poll ~future_funcnames:funcnames
           ~fun_name:function_info.fun_name
           ~optimistic_prologue_poll_instr_id:prologue_poll_instr_id cfg)
    then
      let entry = Cfg.get_block_exn cfg (label_of env entry) in
      DLL.filter_left entry.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
          not (InstructionId.equal instr.id prologue_poll_instr_id))

  let convert ~keep_unused_ops ~funcnames : Cfg_with_layout.t =
    (* Start instruction ids at 0, matching [Cfg_selectgen.emit_fundecl]. *)
    Sub_cfg.reset_instr_id ();
    let env = create_env () in
    if keep_unused_ops then keep_unused_ops_alive env;
    collect_fusion_adjustments env;
    allocate_block_labels_and_param_regs env;
    let fun_arg_regs = get_block_params_regs env entry in
    let loc_arg = Proc.loc_parameters (Reg.typv fun_arg_regs) in
    let cfg =
      Cfg.create ~fun_name:function_info.fun_name ~fun_args:loc_arg
        ~fun_codegen_options:
          (Cfg.of_cmm_codegen_option function_info.fun_codegen_options)
        ~fun_dbg:function_info.fun_dbg ~fun_contains_calls:true
        ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
        ~fun_poll:function_info.fun_poll ~next_instruction_id:instr_seq
        ~fun_ret_type:function_info.fun_ret_type
        ~allowed_to_be_irreducible:false
    in
    let layout = DLL.make_empty () in
    let entry_block =
      make_entry_block cfg ~ssa_entry_label:(label_of env entry) ~loc_arg
    in
    Cfg.add_block_exn cfg entry_block;
    DLL.add_end layout entry_block.start;
    let prologue_poll_instr_id =
      convert_and_emit_blocks cfg layout env ~loc_arg ~fun_arg_regs
    in
    Select_utils.Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
    drop_optimistic_prologue_poll cfg env ~prologue_poll_instr_id ~funcnames;
    let fun_contains_calls =
      Label.Tbl.to_seq_values cfg.blocks
      |> Seq.exists Cfg.basic_block_contains_calls
    in
    let cfg = { cfg with fun_contains_calls } in
    let cfg_with_layout = Cfg_with_layout.create cfg ~layout in
    Cfg_simplify.run cfg_with_layout
end

let convert ~keep_unused_ops ~funcnames (m : (module Ssa.Finished_graph)) :
    Cfg_with_layout.t =
  let module S = (val m : Ssa.Finished_graph) in
  let module C = Make (S) in
  C.convert ~keep_unused_ops ~funcnames
