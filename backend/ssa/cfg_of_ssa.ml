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
    use it to (a) decide which blocks are exception predecessors (their [Raise]
    / may-raise [Call] branches to the topmost handler), and (b) emit [Poptrap]
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

    Invalid handler stub: [Push_trap] / [Pop_trap] referencing a [None] handler
    (e.g. an unreachable trap-handler block dropped by [Builder.finish]) routes
    through a single shared invalid-handler block emitted lazily at the end of
    the conversion. The stub mirrors what [cfg_selectgen]'s
    [unreachable_handler] emits. *)

module DLL = Oxcaml_utils.Doubly_linked_list

let instr_seq = Sub_cfg.instr_id

module Make (S : Ssa.Finished_graph) = struct
  type env =
    { op_regs : Reg.t array S.Instruction_id.Tbl.t;
      block_params_regs : Reg.t array S.Block.Tbl.t;
      block_labels : Label.t S.Block.Tbl.t;
      call_result_locs : Reg.t array S.Block.Tbl.t;
      call_stack_ofs : int S.Block.Tbl.t;
      trap_handlers : unit S.Block.Tbl.t;
          (** Blocks referenced as a handler from [Push_trap]/[Pop_trap]. These
              need an exn-bucket move at their entry. *)
      use_count_adjustments : int S.Instruction_id.Tbl.t;
          (** Per-Op adjustments applied to [op_data.usage_count] to decide
              which Ops to emit. The combination [usage_count + adjustment]
              determines liveness; an Op with adjusted count [<= 0] is skipped.
              Populated before block conversion to (a) decrement Ops consumed by
              fused branch terminators and (b) bump naturally unused Ops to keep
              them in the CFG when [keep_unused_ops] is set. *)
      mutable invalid_handler_label : Label.t option
          (** Lazily-allocated label for the shared "invalid handler" block
              emitted for [Push_trap]/[Pop_trap] with [handler = None]. *)
    }

  let create_env () =
    { op_regs = S.Instruction_id.Tbl.create 256;
      block_params_regs = S.Block.Tbl.create 64;
      block_labels = S.Block.Tbl.create 64;
      call_result_locs = S.Block.Tbl.create 16;
      call_stack_ofs = S.Block.Tbl.create 16;
      trap_handlers = S.Block.Tbl.create 16;
      use_count_adjustments = S.Instruction_id.Tbl.create 16;
      invalid_handler_label = None
    }

  let collect_trap_handlers (env : env) =
    List.iter
      (fun (block : S.Block.t) ->
        Array.iter
          (fun (i : S.Instruction.t) ->
            match[@warning "-4"] i with
            | Push_trap { handler = Some handler }
            | Pop_trap { handler = Some handler } ->
              S.Block.Tbl.replace env.trap_handlers handler ()
            | _ -> ())
          block.body)
      S.blocks

  let label_of env (blk : S.Block.t) =
    try S.Block.Tbl.find env.block_labels blk
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa.label_of: block %d not in block_labels"
        (blk.id :> int)

  let invalid_handler_label env =
    match env.invalid_handler_label with
    | Some l -> l
    | None ->
      let l = Label.new_label () in
      env.invalid_handler_label <- Some l;
      l

  let handler_label env (handler : S.Block.t option) =
    (* A [Push_trap]/[Pop_trap] can reference a handler block that has become
       unreachable; such handlers are filtered out by [Builder.finish] and don't
       appear in [env.block_labels]. We treat them like [handler = None] and
       route through the shared invalid-handler stub. *)
    match handler with
    | Some h when S.Block.Tbl.mem env.block_labels h -> label_of env h
    | _ -> invalid_handler_label env

  let get_reg env (i : S.Instruction.t) : Reg.t =
    match i with
    | Op { id; _ } ->
      let regs =
        try S.Instruction_id.Tbl.find env.op_regs id
        with Not_found ->
          Misc.fatal_errorf "Cfg_of_ssa.get_reg: no regs for Op v%d" (id :> int)
      in
      assert (Array.length regs = 1);
      regs.(0)
    | Block_param { block; index; _ } -> (
      try
        let regs = S.Block.Tbl.find env.block_params_regs block in
        regs.(index)
      with Not_found ->
        Misc.fatal_errorf "Cfg_of_ssa: no regs for Block_param %d.%d"
          (block.id :> int)
          index)
    | Proj { index; src = Op { id; _ } } ->
      let regs =
        try S.Instruction_id.Tbl.find env.op_regs id
        with Not_found ->
          Misc.fatal_errorf "Cfg_of_ssa.get_reg: no regs for Op v%d (Proj)"
            (id :> int)
      in
      regs.(index)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
    | Proj _ ->
      Misc.fatal_error "Cfg_of_ssa.get_reg: unexpected instruction"

  let get_block_params_regs env (block : S.Block.t) =
    try S.Block.Tbl.find env.block_params_regs block
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

  (* Build the shared dummy handler referenced by [Push_trap]/[Pop_trap] with
     [handler = None]. This mirrors [cfg_selectgen]'s [unreachable_handler]
     (load from address 0 followed by a [Raise_notrace]) so it can be
     cfg_compared with the baseline. *)
  let make_invalid_handler_block ~label : Cfg.basic_block =
    let body = DLL.make_empty () in
    (* Trap handler entry: copy [Proc.loc_exn_bucket] into a virtual reg, as
       would be done for a regular exn handler's first parameter. *)
    let exn_bucket_virt = Reg.createv Cmm.typ_val in
    emit_moves body ~src:[| Proc.loc_exn_bucket |] ~dst:exn_bucket_virt;
    (* Segfault: load from the constant address [0]. *)
    let zero_reg = Reg.createv Cmm.typ_int in
    DLL.add_end body
      (make_cfg_instr (Cfg.Op (Const_int 0n)) [||] zero_reg Debuginfo.none);
    let load_res = Reg.createv Cmm.typ_int in
    DLL.add_end body
      (make_cfg_instr
         (Cfg.Op
            (Load
               { memory_chunk = Word_int;
                 addressing_mode = Arch.identity_addressing;
                 mutability = Mutable;
                 is_atomic = false
               }))
         zero_reg load_res Debuginfo.none);
    (* Dummy raise of [1]. *)
    let one_reg = Reg.createv Cmm.typ_int in
    DLL.add_end body
      (make_cfg_instr (Cfg.Op (Const_int 1n)) [||] one_reg Debuginfo.none);
    emit_moves body ~src:one_reg ~dst:[| Proc.loc_exn_bucket |];
    let terminator =
      make_cfg_instr (Cfg.Raise Lambda.Raise_notrace) [| Proc.loc_exn_bucket |]
        [||] Debuginfo.none
    in
    { Cfg.start = label;
      body;
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = Cfg.invalid_stack_offset;
      exn = None;
      can_raise = true;
      is_trap_handler = true;
      cold = true
    }

  (* A Branch condition that can be folded into the CFG test terminator,
     consuming the Op's args directly. *)
  let fuse_comparison (cond : S.Instruction.t) ~true_label ~false_label :
      (Cfg.terminator * S.Instruction.t array) option =
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

  let is_exception_predecessor (pred : S.Block.t) (target : S.Block.t) =
    match S.trap_successor pred with
    | Some h -> S.Block.equal h target
    | None -> false

  let is_call_predecessor (pred : S.Block.t) (target : S.Block.t) =
    (* [Probe] uses the [Call] constructor but has no return value to receive in
       its continuation, so [cfg_of_ssa] doesn't register the continuation in
       [env.call_result_locs]. We mirror that here so the predicate stays
       consistent with [is_call_continuation]. *)
    match[@warning "-fragile-match"] pred.terminator with
    | Invalid { continuation = Some continuation; _ }
    | Call { op = Func _ | Prim (External _); continuation; _ } ->
      S.Block.equal continuation target
    | _ -> false

  (* For a call op, return the args to actually pass on the stack/regs (which
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

  let convert_block (env : env) (block : S.Block.t) : Cfg.basic_block =
    let body = DLL.make_empty () in
    let is_trap_handler = S.Block.Tbl.mem env.trap_handlers block in
    let is_call_continuation =
      S.Block.Tbl.find_opt env.call_result_locs block |> Option.is_some
    in
    (* A trap handler block must only be reached via a [Raise] handler or a
       [Call]/[Prim] trap continuation. The runtime supplies the exn bucket on
       those edges; a normal-path predecessor would not, leaving the handler's
       first param undefined. Similarly, call continuation blocks must only be
       reached via a call. *)
    List.iter
      (fun (pred : S.Block.t) ->
        if
          not (Bool.equal (is_exception_predecessor pred block) is_trap_handler)
        then
          Misc.fatal_errorf
            "Cfg_of_ssa: %strap-handler block %d has predecessor %d reaching \
             it via a %strap edge"
            (if is_trap_handler then "" else "non-")
            (block.id :> int)
            (pred.id :> int)
            (if is_exception_predecessor pred block then "" else "non-");
        if
          not (Bool.equal (is_call_predecessor pred block) is_call_continuation)
        then
          Misc.fatal_errorf
            "Cfg_of_ssa: %scall continuation block %d has predecessor %d \
             reaching it via a %s edge"
            (if is_call_continuation then "" else "non ")
            (block.id :> int)
            (pred.id :> int)
            (if is_call_predecessor pred block then "call" else "non-call"))
      (S.predecessors block);
    (match S.Block.Tbl.find_opt env.call_result_locs block with
    | Some loc_res ->
      (* A call continuation must be reached only from the single Call/Prim that
         defined its [loc_res] — multiple predecessors would mean different
         calls' return registers feeding into the same block. *)
      (match[@warning "-fragile-match"] S.predecessors block with
      | [_] -> ()
      | preds ->
        Misc.fatal_errorf
          "Cfg_of_ssa: call-continuation block %d has %d predecessors \
           (expected exactly 1)"
          (block.id :> int)
          (List.length preds));
      let virt_res = get_block_params_regs env block in
      emit_moves body ~src:loc_res ~dst:virt_res;
      let stack_ofs =
        S.Block.Tbl.find_opt env.call_stack_ofs block |> Option.value ~default:0
      in
      if stack_ofs <> 0
      then
        DLL.add_end body
          (make_cfg_instr (Cfg.Op (Stackoffset (-stack_ofs))) [||] [||]
             Debuginfo.none)
    | None -> ());
    if is_trap_handler
    then begin
      let virt_res = get_block_params_regs env block in
      let exn_bucket = [| Proc.loc_exn_bucket |] in
      let first_param = [| virt_res.(0) |] in
      emit_moves body ~src:exn_bucket ~dst:first_param
    end;
    Array.iter
      (fun (i : S.Instruction.t) ->
        match i with
        | Op { id; op; args; dbg; typ; usage_count } ->
          let regs = Reg.createv typ in
          S.Instruction_id.Tbl.replace env.op_regs id regs;
          let adjustment =
            S.Instruction_id.Tbl.find_opt env.use_count_adjustments id
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
        | Push_trap { handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Pushtrap { lbl_handler = handler_label env handler })
               [||] [||] Debuginfo.none)
        | Pop_trap { handler } ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Poptrap { lbl_handler = handler_label env handler })
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
          (fun (h : S.Block.t) ->
            DLL.add_end body
              (make_cfg_instr
                 (Cfg.Poptrap { lbl_handler = label_of env h })
                 [||] [||] Debuginfo.none))
          block.block_end_trap_stack;
        let arg = Array.map (get_reg env) args in
        let loc_res = Proc.loc_results_return (Reg.typv arg) in
        emit_moves body ~src:arg ~dst:loc_res;
        make_cfg_instr Cfg.Return loc_res [||] dbg
      | Raise { raise_kind; args } ->
        let exn_val = Array.map (get_reg env) args in
        let exn_bucket = [| Proc.loc_exn_bucket |] in
        let extra_dst =
          match S.trap_successor block with
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
          S.Block.Tbl.replace env.call_result_locs continuation loc_res;
          S.Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
          make_cfg_instr
            (Cfg.Call { op = call_op; label_after = label_of env continuation })
            (call_arg_for_terminator call_op virt_args loc_arg)
            loc_res dbg
        | Prim (External ({ ty_args; _ } as ext)) ->
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
          S.Block.Tbl.replace env.call_result_locs continuation loc_res;
          S.Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
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
            let virt_res = get_block_params_regs env cont_block in
            let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
            S.Block.Tbl.replace env.call_result_locs cont_block loc_res;
            S.Block.Tbl.replace env.call_stack_ofs cont_block stack_ofs;
            Some (label_of env cont_block), loc_res
          | None -> None, [||]
        in
        make_cfg_instr
          (Cfg.Invalid { message; stack_ofs; stack_align; label_after })
          loc_arg loc_res dbg
    in
    let exn = Option.map (label_of env) (S.trap_successor block) in
    let can_raise = Cfg.can_raise_terminator terminator.desc in
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

  (* Decrement [usage_count] of comparisons that will be folded into branch
     terminators, and increment their inputs to compensate. The branch
     terminator consumes the comparison's args directly, so the comparison
     itself is no longer needed (provided no other consumer references it), and
     its args must be kept alive across the (now-implicit) drop. *)
  let bump_adjustment env id delta =
    let cur =
      S.Instruction_id.Tbl.find_opt env.use_count_adjustments id
      |> Option.value ~default:0
    in
    S.Instruction_id.Tbl.replace env.use_count_adjustments id (cur + delta)

  let collect_fusion_adjustments env =
    let rec bump_arg (i : S.Instruction.t) =
      match i with
      | Op { id; _ } -> bump_adjustment env id 1
      | Proj { src; _ } -> bump_arg src
      | Block_param _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
      | Name_for_debugger _ ->
        ()
    in
    List.iter
      (fun (block : S.Block.t) ->
        match[@warning "-fragile-match"] block.terminator with
        | Branch { cond; _ } ->
          fuse_comparison cond ~true_label:Label.none ~false_label:Label.none
          |> Option.iter (fun (_, args) ->
              (match[@warning "-fragile-match"] cond with
              | Op { id; _ } -> bump_adjustment env id (-1)
              | _ -> ());
              Array.iter bump_arg args)
        | _ -> ())
      S.blocks

  (* Bump every Op with [usage_count = 0] up by 1. This disables dead-code
     elimination for naturally-unused Ops, matching the non-SSA pipeline's
     behaviour (and keeping cfg_compare happy). Ops decremented by
     [collect_fusion_adjustments] are unaffected: their original [usage_count]
     is non-zero (they are consumed by the branch's [cond]). *)
  let collect_unused_op_bumps env =
    List.iter
      (fun (block : S.Block.t) ->
        Array.iter
          (fun (instr : S.Instruction.t) ->
            match instr with
            | Op r when r.usage_count = 0 -> bump_adjustment env r.id 1
            | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
            | Stack_check _ | Name_for_debugger _ ->
              ())
          block.body)
      S.blocks

  let convert ~keep_unused_ops ~future_funcnames : Cfg_with_layout.t =
    (* Reset the instruction-id counter so this conversion starts from 0,
       matching what [Cfg_selectgen.emit_fundecl] does. *)
    Sub_cfg.reset_instr_id ();
    let env = create_env () in
    if keep_unused_ops then collect_unused_op_bumps env;
    collect_fusion_adjustments env;
    collect_trap_handlers env;
    S.blocks
    |> List.iter (fun (block : S.Block.t) ->
        let label = Label.new_label () in
        S.Block.Tbl.replace env.block_labels block label;
        S.Block.Tbl.replace env.block_params_regs block
          (Reg.createv block.params));
    let fun_arg_regs = get_block_params_regs env S.entry in
    let loc_arg = Proc.loc_parameters (Reg.typv fun_arg_regs) in
    let prologue_poll_instr_id = ref None in
    let cfg =
      Cfg.create ~fun_name:S.function_info.fun_name ~fun_args:loc_arg
        ~fun_codegen_options:
          (Cfg.of_cmm_codegen_option S.function_info.fun_codegen_options)
        ~fun_dbg:S.function_info.fun_dbg ~fun_contains_calls:true
        ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
        ~fun_poll:S.function_info.fun_poll ~next_instruction_id:instr_seq
        ~fun_ret_type:S.function_info.fun_ret_type
        ~allowed_to_be_irreducible:false
    in
    let layout = DLL.make_empty () in
    (* For each fun-arg with provenance, build a [Name_for_debugger] CFG instr
       referencing the corresponding slice of [loc_arg]. *)
    let param_naming_instrs () =
      let acc = ref [] in
      let idx = ref 0 in
      List.iteri
        (fun param_index (var, ty) ->
          let provenance = Backend_var.With_provenance.provenance var in
          let ident = Backend_var.With_provenance.var var in
          let n = Array.length ty in
          let regs = Array.init n (fun i -> loc_arg.(!idx + i)) in
          idx := !idx + n;
          if Option.is_some provenance
          then
            acc
              := make_cfg_instr
                   (Cfg.Op
                      (Name_for_debugger
                         { ident;
                           provenance;
                           which_parameter = Some param_index;
                           regs
                         }))
                   [||] [||] Debuginfo.none
                 :: !acc)
        S.function_info.fun_args_names;
      List.rev !acc
    in
    let entry_body = DLL.make_empty () in
    List.iter (DLL.add_end entry_body) (param_naming_instrs ());
    let entry_label = label_of env S.entry in
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
      (fun (ssa_block : S.Block.t) ->
        let label = label_of env ssa_block in
        let cfg_block = convert_block env ssa_block in
        cfg_block.can_raise
          <- Cfg.can_raise_terminator cfg_block.terminator.desc;
        if S.Block.equal ssa_block S.entry
        then begin
          let poll_instr =
            make_cfg_instr (Cfg.Op Poll) [||] [||] Debuginfo.none
          in
          prologue_poll_instr_id := Some poll_instr.id;
          DLL.add_begin cfg_block.body poll_instr;
          (* Reverse so [add_begin] in order produces forward [Move]s. *)
          let move_pairs =
            ( Array.to_list loc_arg |> List.rev,
              Array.to_list fun_arg_regs |> List.rev )
          in
          List.iter2
            (fun s d ->
              if not (Reg.same s d)
              then
                DLL.add_begin cfg_block.body
                  (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
            (fst move_pairs) (snd move_pairs);
          List.iter
            (DLL.add_begin cfg_block.body)
            (List.rev (param_naming_instrs ()))
        end;
        if Cfg.is_return_terminator cfg_block.terminator.desc
        then
          DLL.add_end cfg_block.body
            (make_cfg_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
        Cfg.add_block_exn cfg cfg_block;
        DLL.add_end layout label)
      S.blocks;
    (* If any [Push_trap]/[Pop_trap] was missing a handler block, emit the
       shared invalid handler block now. *)
    Option.iter
      (fun lbl ->
        Cfg.add_block_exn cfg (make_invalid_handler_block ~label:lbl);
        DLL.add_end layout lbl)
      env.invalid_handler_label;
    Select_utils.Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
    Option.iter
      (fun poll_id ->
        if
          not
            (Cfg_polling.requires_prologue_poll ~future_funcnames
               ~fun_name:S.function_info.fun_name
               ~optimistic_prologue_poll_instr_id:poll_id cfg)
        then
          let ssa_entry = Cfg.get_block_exn cfg (label_of env S.entry) in
          DLL.filter_left ssa_entry.body
            ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
              not (InstructionId.equal instr.id poll_id)))
      !prologue_poll_instr_id;
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

let convert ~keep_unused_ops ~future_funcnames (m : (module Ssa.Finished_graph))
    : Cfg_with_layout.t =
  let module S = (val m : Ssa.Finished_graph) in
  let module C = Make (S) in
  C.convert ~keep_unused_ops ~future_funcnames
