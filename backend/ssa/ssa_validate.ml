[@@@ocaml.warning "+a-40-41-42"]

let validate (t : Ssa.t) =
  let error fmt =
    Format.kasprintf
      (fun s -> Misc.fatal_errorf "SSA validation (%s): %s" t.fun_name s)
      fmt
  in
  let pb = Ssa_print.print_block_id in
  (* Build block set for membership checking *)
  let block_set = Ssa.Block.Tbl.create 16 in
  List.iter (fun bl -> Ssa.Block.Tbl.replace block_set bl ()) t.blocks;
  let block_exists b = Ssa.Block.Tbl.mem block_set b in
  (* Check entry block exists *)
  if not (block_exists t.entry) then error "entry block %a not found" pb t.entry;
  (* Compute actual predecessors from terminators *)
  let actual_preds = Ssa.Block.Tbl.create 16 in
  List.iter (fun bl -> Ssa.Block.Tbl.replace actual_preds bl []) t.blocks;
  let add_pred ~src ~dst =
    match Ssa.Block.Tbl.find_opt actual_preds dst with
    | Some ps -> Ssa.Block.Tbl.replace actual_preds dst (src :: ps)
    | None ->
      error "block %a references non-existent successor %a" pb src pb dst
  in
  let successor_blocks (bl : Ssa.block) =
    match bl.terminator with
    | Pending_construction ->
      error "block %a: Pending_construction terminator in finished graph" pb bl
    | Goto { goto; _ } -> add_pred ~src:bl ~dst:goto
    | Branch { ifso; ifnot; _ } ->
      add_pred ~src:bl ~dst:ifso;
      add_pred ~src:bl ~dst:ifnot
    | Switch (targets, _) ->
      Array.iter (fun dst -> add_pred ~src:bl ~dst) targets
    | Return _ | Tailcall_func _ -> ()
    | Raise (_, _, handler) ->
      Option.iter (fun dst -> add_pred ~src:bl ~dst) handler
    | Tailcall_self { destination; _ } -> add_pred ~src:bl ~dst:destination
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } -> (
      add_pred ~src:bl ~dst:continuation;
      match exn_continuation with
      | Some l -> add_pred ~src:bl ~dst:l
      | None -> ())
    | Invalid { continuation; _ } ->
      Option.iter (fun l -> add_pred ~src:bl ~dst:l) continuation
  in
  List.iter successor_blocks t.blocks;
  (* The builder guarantees that reachable blocks are listed in dominator order
     (predecessors before successors, except loop back edges), so we can
     validate by iterating t.blocks directly, registering Op definitions as we
     go. Unreachable blocks are skipped entirely. *)
  let defined_ops = Ssa.InstructionId.Tbl.create 64 in
  let rec check_arg (bl : Ssa.block) (i : Ssa.instruction) =
    match i with
    | Op { id; _ } -> (
      match Ssa.InstructionId.Tbl.find_opt defined_ops id with
      | None ->
        error "block %a: Op v%d used but not defined" pb bl
          (Ssa.InstructionId.hash id)
      | Some def_block ->
        if not (Ssa.dominates def_block bl)
        then
          error "block %a: Op v%d defined in non-dominating block %a" pb bl
            (Ssa.InstructionId.hash id)
            pb def_block)
    | Block_param { block; index; typ } ->
      if not (block_exists block)
      then
        error "block %a: BlockParam references non-existent block %a" pb bl pb
          block;
      if index < 0 || index >= Array.length block.params
      then
        error
          "block %a: BlockParam index %d out of range for block %a (params \
           length %d)"
          pb bl index pb block
          (Array.length block.params);
      let expected = block.params.(index) in
      if not (Cmm.equal_machtype_component expected typ)
      then
        error "block %a: BlockParam %a.%d has type %a but block params say %a"
          pb bl pb block index Printcmm.machtype_component typ
          Printcmm.machtype_component expected;
      if not (Ssa.dominates block bl)
      then
        error "block %a: BlockParam of non-dominating block %a" pb bl pb block
    | Proj { src; _ } -> (
      match src with
      | Op _ -> check_arg bl src
      | Block_param _ | Proj _ | Push_trap _ | Pop_trap _ | Stack_check _
      | Name_for_debugger _ ->
        error "block %a: Proj source must be an Op" pb bl)
    | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
      error "block %a: non-value instruction used as argument" pb bl
  in
  let check_args bl args = Array.iter (check_arg bl) args in
  let visit_block (bl : Ssa.block) =
    (* Invariant 1: recorded predecessors are exactly the reachable predecessors
       derived from terminators. *)
    let actual = Ssa.Block.Tbl.find actual_preds bl in
    let reachable_actual =
      List.filter (fun (p : Ssa.block) -> Ssa.reachable p.dominator_info) actual
    in
    let to_set =
      List.fold_left (fun s b -> Ssa.Block.Set.add b s) Ssa.Block.Set.empty
    in
    if
      not
        (Ssa.Block.Set.equal (to_set reachable_actual) (to_set bl.predecessors))
    then
      error
        "block %a: recorded predecessors {%a} differ from reachable actual \
         predecessors {%a}"
        pb bl
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           pb)
        bl.predecessors
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           pb)
        reachable_actual;
    (* Invariant 2: a block is marked as reachable iff it has predecessors
       (Function_start is reachable with no predecessors). *)
    let recorded_reachable = Ssa.reachable bl.dominator_info in
    let expected_reachable =
      (match[@warning "-fragile-match"] bl.desc with
        | Function_start -> true
        | _ -> false)
      || not (List.is_empty bl.predecessors)
    in
    if not (Bool.equal recorded_reachable expected_reachable)
    then
      error
        "block %a: marked reachable=%b but expected reachable=%b based on \
         predecessors"
        pb bl recorded_reachable expected_reachable;
    (* Unreachable blocks are not validated further: they don't produce emitted
       code, and their contents may be stale or meaningless. *)
    if recorded_reachable
    then begin
      (* Check entry block is Function_start *)
      (if Ssa.block_equal bl t.entry
       then
         match[@warning "-fragile-match"] bl.desc with
         | Function_start -> ()
         | _ -> error "entry block %a is not Function_start" pb bl);
      (match bl.desc with
      | Branch_target ->
        if Array.length bl.params > 0
        then error "block %a: Branch_target must not have parameters" pb bl
      | Merge | Function_start | Call_continuation | Trap_handler -> ());
      (* Check body: validate args then register Op *)
      Array.iter
        (fun (i : Ssa.instruction) ->
          match i with
          | Op { id; args; _ } ->
            check_args bl args;
            if Ssa.InstructionId.Tbl.mem defined_ops id
            then
              error "block %a: duplicate Op id v%d" pb bl
                (Ssa.InstructionId.hash id);
            Ssa.InstructionId.Tbl.replace defined_ops id bl
          | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()
          | Block_param _ | Proj _ -> ())
        bl.body;
      (* Check terminator *)
      match bl.terminator with
      | Pending_construction ->
        error "block %a: Pending_construction terminator in finished graph" pb
          bl
      | Goto { goto; args } ->
        check_args bl args;
        if Array.length args <> Array.length (goto : Ssa.block).params
        then
          error "block %a: goto %a has %d args but target has %d params" pb bl
            pb goto (Array.length args) (Array.length goto.params)
      | Branch { cond; ifso; ifnot } ->
        let check_branch_target (target : Ssa.block) =
          match[@warning "-fragile-match"] target.desc with
          | Branch_target -> ()
          | _ ->
            error "block %a: Branch target %a is not a Branch_target block" pb
              bl pb target
        in
        check_arg bl cond;
        check_branch_target ifso;
        check_branch_target ifnot
      | Switch (targets, args) ->
        check_args bl args;
        Array.iter
          (fun (target : Ssa.block) ->
            match[@warning "-fragile-match"] target.desc with
            | Branch_target -> ()
            | _ ->
              error "block %a: Switch target %a is not a Branch_target block" pb
                bl pb target)
          targets
      | Return args -> check_args bl args
      | Raise (_, args, _) -> check_args bl args
      | Tailcall_self { args; _ } -> check_args bl args
      | Tailcall_func (_, args) -> check_args bl args
      | Call { args; _ } -> check_args bl args
      | Prim { args; _ } -> check_args bl args
      | Invalid { args; _ } -> check_args bl args
    end
  in
  List.iter visit_block t.blocks
