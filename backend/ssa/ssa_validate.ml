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
    | Never -> ()
    | Goto { goto; _ } -> add_pred ~src:bl ~dst:goto
    | Branch { conditions; else_goto } ->
      Array.iter (fun (_, dst) -> add_pred ~src:bl ~dst) conditions;
      add_pred ~src:bl ~dst:else_goto
    | Switch (targets, _) ->
      Array.iter (fun dst -> add_pred ~src:bl ~dst) targets
    | Return _ | Raise _ | Tailcall_func _ -> ()
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
  (* Compute successor blocks for RPO/dominator computation *)
  let get_successors (bl : Ssa.block) =
    match bl.terminator with
    | Never -> []
    | Goto { goto; _ } -> [goto]
    | Branch { conditions; else_goto } ->
      let succs = ref [else_goto] in
      Array.iter (fun (_, dst) -> succs := dst :: !succs) conditions;
      !succs
    | Switch (targets, _) -> Array.to_list targets
    | Return _ | Raise _ | Tailcall_func _ -> []
    | Tailcall_self { destination; _ } -> [destination]
    | Call { continuation; exn_continuation; _ }
    | Prim { continuation; exn_continuation; _ } ->
      continuation :: (match exn_continuation with Some l -> [l] | None -> [])
    | Invalid { continuation; _ } -> (
      match continuation with Some l -> [l] | None -> [])
  in
  (* Compute reverse postorder via DFS *)
  let rpo =
    let visited = Ssa.Block.Tbl.create 16 in
    let order = ref [] in
    let rec dfs bl =
      if not (Ssa.Block.Tbl.mem visited bl)
      then (
        Ssa.Block.Tbl.replace visited bl ();
        List.iter dfs (get_successors bl);
        order := bl :: !order)
    in
    dfs t.entry;
    !order
  in
  (* Compute immediate dominators (Cooper-Harvey-Kennedy algorithm) *)
  let rpo_index = Ssa.Block.Tbl.create 16 in
  List.iteri (fun i bl -> Ssa.Block.Tbl.replace rpo_index bl i) rpo;
  let idom = Ssa.Block.Tbl.create 16 in
  Ssa.Block.Tbl.replace idom t.entry t.entry;
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (Ssa.block_equal !b1 !b2) do
      while
        Ssa.Block.Tbl.find rpo_index !b1 > Ssa.Block.Tbl.find rpo_index !b2
      do
        b1 := Ssa.Block.Tbl.find idom !b1
      done;
      while
        Ssa.Block.Tbl.find rpo_index !b2 > Ssa.Block.Tbl.find rpo_index !b1
      do
        b2 := Ssa.Block.Tbl.find idom !b2
      done
    done;
    !b1
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun bl ->
        if not (Ssa.block_equal bl t.entry)
        then
          let preds = Ssa.Block.Tbl.find actual_preds bl in
          let processed =
            List.filter (fun p -> Ssa.Block.Tbl.mem idom p) preds
          in
          match processed with
          | [] -> ()
          | first :: rest ->
            let new_idom = List.fold_left intersect first rest in
            if
              not
                (match Ssa.Block.Tbl.find_opt idom bl with
                | Some d -> Ssa.block_equal d new_idom
                | None -> false)
            then (
              Ssa.Block.Tbl.replace idom bl new_idom;
              changed := true))
      rpo
  done;
  (* Build dominator tree and compute DFS in/out times for O(1) dominance
     queries *)
  let dom_children = Ssa.Block.Tbl.create 16 in
  Ssa.Block.Tbl.iter
    (fun bl parent ->
      if not (Ssa.block_equal bl parent)
      then
        let kids =
          match Ssa.Block.Tbl.find_opt dom_children parent with
          | Some l -> l
          | None -> []
        in
        Ssa.Block.Tbl.replace dom_children parent (bl :: kids))
    idom;
  let dom_in = Ssa.Block.Tbl.create 16 in
  let dom_out = Ssa.Block.Tbl.create 16 in
  let time = ref 0 in
  let rec compute_dom_times bl =
    Ssa.Block.Tbl.replace dom_in bl !time;
    incr time;
    (match Ssa.Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter compute_dom_times kids
    | None -> ());
    Ssa.Block.Tbl.replace dom_out bl !time;
    incr time
  in
  compute_dom_times t.entry;
  let dominates a b =
    Ssa.Block.Tbl.find dom_in a <= Ssa.Block.Tbl.find dom_in b
    && Ssa.Block.Tbl.find dom_out a >= Ssa.Block.Tbl.find dom_out b
  in
  (* Validate blocks in dominator tree order, building Op definition map as we
     go *)
  let defined_ops = Ssa.InstructionId.Tbl.create 64 in
  let rec check_arg (bl : Ssa.block) (i : Ssa.instruction) =
    match i with
    | Op { id; _ } -> (
      match Ssa.InstructionId.Tbl.find_opt defined_ops id with
      | None ->
        error "block %a: Op v%d used but not defined" pb bl
          (Ssa.InstructionId.hash id)
      | Some def_block ->
        if not (dominates def_block bl)
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
      if not (dominates block bl)
      then
        error "block %a: BlockParam of non-dominating block %a" pb bl pb block
    | Proj { src; _ } -> (
      match src with
      | Op _ -> check_arg bl src
      | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ->
        error "block %a: Proj source must be an Op" pb bl)
    | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
      error "block %a: non-value instruction used as argument" pb bl
  and check_args bl args = Array.iter (check_arg bl) args
  and visit_block (bl : Ssa.block) =
    (* Check entry block is FunctionStart *)
    (if Ssa.block_equal bl t.entry
     then
       match bl.desc with
       | FunctionStart -> ()
       | Merge _ | BranchTarget _ | CallContinuation _ | TrapHandler _ ->
         error "entry block %a is not FunctionStart" pb bl);
    (* Check BranchTarget has exactly one predecessor and it matches the
       declared one *)
    (match bl.desc with
    | BranchTarget { predecessor } ->
      if Array.length bl.params > 0
      then error "block %a: BranchTarget must not have parameters" pb bl;
      let preds = Ssa.Block.Tbl.find actual_preds bl in
      if not (List.exists (Ssa.block_equal predecessor) preds)
      then
        error
          "block %a: BranchTarget declares predecessor %a but it is not an \
           actual predecessor"
          pb bl pb predecessor
    | Merge { predecessors } ->
      let preds = Ssa.Block.Tbl.find actual_preds bl in
      let pred_set =
        List.fold_left
          (fun s b -> Ssa.Block.Set.add b s)
          Ssa.Block.Set.empty preds
      in
      let declared_set =
        List.fold_left
          (fun s b -> Ssa.Block.Set.add b s)
          Ssa.Block.Set.empty predecessors
      in
      if not (Ssa.Block.Set.equal pred_set declared_set)
      then
        error
          "block %a: Merge declares predecessors {%a} but actual predecessors \
           are {%a}"
          pb bl
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             pb)
          predecessors
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             pb)
          preds
    | FunctionStart | CallContinuation _ | TrapHandler _ -> ());
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
        | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> ()
        | Block_param _ | Proj _ -> ())
      bl.body;
    (* Check terminator *)
    (match bl.terminator with
    | Never -> ()
    | Goto { goto; args } ->
      check_args bl args;
      if Array.length args <> Array.length (goto : Ssa.block).params
      then
        error "block %a: goto %a has %d args but target has %d params" pb bl pb
          goto (Array.length args) (Array.length goto.params)
    | Branch { conditions; else_goto } ->
      let check_branch_target (target : Ssa.block) =
        match target.desc with
        | BranchTarget _ -> ()
        | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
          error "block %a: Branch target %a is not a BranchTarget block" pb bl
            pb target
      in
      Array.iter
        (fun (cond, target) ->
          check_arg bl cond;
          check_branch_target target)
        conditions;
      check_branch_target else_goto
    | Switch (targets, args) ->
      check_args bl args;
      Array.iter
        (fun (target : Ssa.block) ->
          match target.desc with
          | BranchTarget _ -> ()
          | Merge _ | FunctionStart | CallContinuation _ | TrapHandler _ ->
            error "block %a: Switch target %a is not a BranchTarget block" pb bl
              pb target)
        targets
    | Return args -> check_args bl args
    | Raise (_, args, _) -> check_args bl args
    | Tailcall_self { args; _ } -> check_args bl args
    | Tailcall_func (_, args) -> check_args bl args
    | Call { args; _ } -> check_args bl args
    | Prim { args; _ } -> check_args bl args
    | Invalid { args; _ } -> check_args bl args);
    (* Visit dominator tree children *)
    match Ssa.Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter visit_block kids
    | None -> ()
  in
  visit_block t.entry
