open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42-45"]

(** Cmm → SSA conversion.

    Walks a [Cmm.fundecl] in expression order, emitting into an
    {!Ssa.Standalone_graph_builder} via the [Make] functor. Operation-level
    lowering is delegated to {!Cfg_selectgen} via [Sel.select_operation].

    Trap handling: [Cexit]'s trap actions are emitted as [Push_trap] /
    [Pop_trap] body instructions. In contrast to {!Cfg_selectgen}, we do not
    compute trap stacks and trap continuations, relying on the SSA machinery to
    do this automatically instead.

    Also in contrast to Cfg_selectgen, we do not emit tail calls.
    {!Ssa_tail_call} later rewrites that pattern into [Tailcall_self] /
    [Tailcall_func] when the calling-convention and trap stack constraints are
    satisfied. The [Capply]'s [region_close] is forwarded as the [Call.nontail]
    flag so [@nontail] annotations suppress the rewrite. *)

module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance
module Sel = Cfg_selectgen.Make (Cfg_selection)

module Make (B : Ssa.Standalone_graph_builder) = struct
  type 'a or_never_returns =
    | Ok of 'a
    | Never_returns

  let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

  (* The result of emitting an expression: just the produced values. The
     [unfinished_block ref] passed through emission is mutated in place; on
     [Never_returns] its contents are no longer meaningful and the caller must
     re-bind it (typically via [B.start_block]) before emitting again. *)
  type result = B.Instruction.t array or_never_returns

  type env =
    { vars : B.Instruction.t array V.Map.t;
      static_exceptions : B.Block.t Static_label.Map.t
    }

  let env_find v env =
    try V.Map.find v env.vars
    with Not_found -> Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

  let env_add v instrs env =
    { env with vars = V.Map.add (VP.var v) instrs env.vars }

  let emit_instruction_no_res (b : B.unfinished_block ref) (i : B.Instruction.t)
      : unit =
    let nb, _ = B.emit_instruction !b i in
    b := nb

  let emit_op (b : B.unfinished_block ref) ~op ~dbg ~typ ~args : B.Instruction.t
      =
    let nb, i = B.emit_op !b ~op ~dbg ~typ ~args in
    b := nb;
    i

  let emit_op_res b ?(dbg = Debuginfo.none) op typ args : B.Instruction.t array
      =
    let i = emit_op b ~op ~dbg ~typ ~args in
    if Array.length typ = 1
    then [| i |]
    else
      Array.init (Array.length typ) (fun index ->
          B.Instruction.make_proj ~index i)

  let bind_let env b v r1 =
    let env = env_add v r1 env in
    let name = V.name (VP.var v) in
    Array.iter (fun i -> B.Instruction.set_name i name) r1;
    let provenance = VP.provenance v in
    if Option.is_some provenance
    then
      emit_instruction_no_res b
        (Name_for_debugger
           { ident = VP.var v; provenance; which_parameter = None; regs = r1 });
    env

  let chunk_of_component (c : Cmm.machtype_component) : Cmm.memory_chunk =
    match c with
    | Float -> Double
    | Float32 -> Single { reg = Float32 }
    | Vec128 -> Onetwentyeight_unaligned
    | Vec256 -> Twofiftysix_unaligned
    | Vec512 -> Fivetwelve_unaligned
    | Val | Int | Addr | Valx2 -> Word_val

  let size_of_cmm_expr env (e : Cmm.expression) =
    let rec size (e : Cmm.expression) =
      match e with
      | Cconst_int _ | Cconst_natint _ -> Arch.size_int
      | Cconst_symbol _ -> Arch.size_addr
      | Cconst_float _ -> Arch.size_float
      | Cconst_float32 _ -> Arch.size_float
      | Cconst_vec128 _ -> Arch.size_vec128
      | Cconst_vec256 _ -> Arch.size_vec256
      | Cconst_vec512 _ -> Arch.size_vec512
      | Cvar id ->
        let instrs = V.Map.find id env.vars in
        Array.fold_left
          (fun acc i -> acc + SU.size_component (B.Instruction.arg_type i))
          0 instrs
      | Ctuple el -> List.fold_left (fun acc e -> acc + size e) 0 el
      | Cop (op, _, _) ->
        let ty = SU.oper_result_type op in
        Array.fold_left (fun acc c -> acc + SU.size_component c) 0 ty
      | Clet (_, _, body) | Csequence (_, body) -> size body
      | Cifthenelse (_, _, e1, _, _, _) -> size e1
      | _ -> Arch.size_addr
    in
    size e

  let emit_branch b (test : Operation.test) rarg ~true_block ~false_block =
    let make_cond op args =
      emit_op b ~op ~dbg:Debuginfo.none ~typ:Cmm.typ_int ~args
    in
    let is_comparison (v : B.Instruction.t) =
      match v with
      | Op
          { op =
              ( Intop (Icomp _)
              | Intop_imm (Icomp _, _)
              | Floatop (_, Icompf _)
              | Intop_imm (Iand, 1) );
            _
          } ->
        true
      | _ -> false
    in
    let wrap_truth_test v =
      if is_comparison v
      then make_cond (Intop_imm (Icomp Cne, 0)) [| v |]
      else v
    in
    let term : B.Terminator.t =
      match test with
      | Itruetest ->
        let cond = wrap_truth_test rarg.(0) in
        Branch { cond; ifso = true_block; ifnot = false_block }
      | Ifalsetest ->
        let cond = wrap_truth_test rarg.(0) in
        Branch { cond; ifso = false_block; ifnot = true_block }
      | Iinttest cmp ->
        let cond = make_cond (Intop (Icomp cmp)) rarg in
        Branch { cond; ifso = true_block; ifnot = false_block }
      | Iinttest_imm (cmp, n) ->
        let cond = make_cond (Intop_imm (Icomp cmp, n)) [| rarg.(0) |] in
        Branch { cond; ifso = true_block; ifnot = false_block }
      | Ifloattest (w, cmp) ->
        let cond = make_cond (Floatop (w, Icompf cmp)) rarg in
        Branch { cond; ifso = true_block; ifnot = false_block }
      | Ioddtest ->
        let cond = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
        Branch { cond; ifso = true_block; ifnot = false_block }
      | Ieventest ->
        let cond = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
        Branch { cond; ifso = false_block; ifnot = true_block }
    in
    B.finish_block !b ~dbg:Debuginfo.none term

  let rec emit_parts env b ~effects_after exp =
    let module EC = SU.Effect_and_coeffect in
    let may_defer_evaluation =
      let ec = Sel.effects_of exp in
      match EC.effect_ ec with
      | Arbitrary | Raise -> EC.pure_and_copure effects_after
      | None -> (
        match EC.coeffect ec with
        | None -> true
        | Read_mutable -> (
          match EC.effect_ effects_after with
          | None | Raise -> true
          | Arbitrary -> false)
        | Arbitrary -> (
          match EC.effect_ effects_after with
          | None -> true
          | Arbitrary | Raise -> false))
    in
    if may_defer_evaluation && Sel.is_simple_expr exp
    then Some (exp, env)
    else
      match emit env b exp ~tail:false with
      | Never_returns -> None
      | Ok r ->
        if Array.length r = 0
        then Some (Cmm.Ctuple [], env)
        else
          let id = V.create_local "bind" in
          Some (Cmm.Cvar id, { env with vars = V.Map.add id r env.vars })

  and emit_parts_list env b exp_list =
    let module EC = SU.Effect_and_coeffect in
    let exp_list_right_to_left, _effect =
      List.fold_left
        (fun (exp_list, effects_after) exp ->
          let exp_effect = Sel.effects_of exp in
          (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
        ([], EC.none) exp_list
    in
    List.fold_left
      (fun acc (exp, effects_after) ->
        match acc with
        | None -> None
        | Some (result, env) -> (
          match emit_parts env b ~effects_after exp with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env)))
      (Some ([], env))
      exp_list_right_to_left

  and emit_tuple_not_flattened env b exp_list : _ or_never_returns =
    let rec emit_list = function
      | [] -> Ok []
      | exp :: rem -> (
        let* loc_rem = emit_list rem in
        match emit env b exp ~tail:false with
        | Never_returns -> Never_returns
        | Ok loc_exp -> Ok (loc_exp :: loc_rem))
    in
    emit_list exp_list

  and emit_tuple env b exp_list : result =
    let* l = emit_tuple_not_flattened env b exp_list in
    Ok (Array.concat l)

  and emit_stores env b dbg (args : Cmm.expression list) regs_addr =
    let byte_offset = ref (-Arch.size_int) in
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing !byte_offset)
    in
    let base = ref regs_addr in
    let reset_addressing () =
      let tmp =
        emit_op b
          ~op:(SU.make_const_int (Nativeint.of_int !byte_offset))
          ~dbg ~typ:Cmm.typ_int ~args:[||]
      in
      assert (!byte_offset > 0);
      let new_base =
        emit_op b ~op:(Operation.Intop Iadd) ~dbg ~typ:Cmm.typ_addr
          ~args:[| !base; tmp |]
      in
      base := new_base;
      byte_offset := 0;
      addressing_mode := Arch.identity_addressing
    in
    let advance bytes =
      byte_offset := !byte_offset + bytes;
      match Cfg_selection.is_offset_out_of_range !byte_offset with
      | Within_range ->
        addressing_mode := Arch.offset_addressing !addressing_mode bytes
      | Out_of_range -> reset_addressing ()
    in
    let is_store (op : Operation.t) =
      match op with Store (_, _, _) -> true | _ -> false
    in
    let for_one_arg arg =
      let original_arg = arg in
      let select_store_result =
        Cfg_selection.select_store ~is_assign:false !addressing_mode arg
      in
      let arg : Cmm.expression =
        match select_store_result with
        | Maybe_out_of_range | Use_default -> arg
        | Rewritten (_, arg) -> arg
      in
      match emit env b arg ~tail:false with
      | Ok regs -> (
        let operation_replacing_store =
          match select_store_result with
          | Maybe_out_of_range -> None
          | Rewritten (op, _) -> if is_store op then None else Some op
          | Use_default -> None
        in
        match operation_replacing_store with
        | None ->
          Array.iter
            (fun (r : B.Instruction.t) ->
              let chunk = chunk_of_component (B.Instruction.arg_type r) in
              ignore
                (emit_op b
                   ~op:(Operation.Store (chunk, !addressing_mode, false))
                   ~dbg ~typ:[||] ~args:[| r; !base |]
                  : B.Instruction.t);
              advance (Select_utils.size_component (B.Instruction.arg_type r)))
            regs
        | Some op ->
          ignore
            (emit_op b ~op ~dbg ~typ:[||] ~args:(Array.append regs [| !base |])
              : B.Instruction.t);
          advance (size_of_cmm_expr env original_arg))
      | Never_returns ->
        Misc.fatal_error "Ssa_of_cmm.emit_stores: never_returns"
    in
    List.iter for_one_arg args

  and emit env b (exp : Cmm.expression) ~tail : result =
    let r =
      match exp with
      | Clet (v, e1, e2) ->
        let* r1 = emit env b e1 ~tail:false in
        let env = bind_let env b v r1 in
        emit env b e2 ~tail
      | Cphantom_let (_var, _defining_expr, body) -> emit env b body ~tail
      | Csequence (e1, e2) ->
        let* _ = emit env b e1 ~tail:false in
        emit env b e2 ~tail
      | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
        emit_ifthenelse env b ~tail econd eif eelse
      | Cswitch (esel, index, ecases, _dbg) ->
        emit_switch env b ~tail esel index ecases
      | Ccatch (_, [], e1) -> emit env b e1 ~tail
      | Ccatch (_flag, handlers, body) -> emit_catch env b ~tail handlers body
      (* Leaf expressions *)
      | Cconst_int (n, _dbg) ->
        Ok
          (emit_op_res b
             (SU.make_const_int (Nativeint.of_int n))
             Cmm.typ_int [||])
      | Cconst_natint (n, _dbg) ->
        Ok (emit_op_res b (SU.make_const_int n) Cmm.typ_int [||])
      | Cconst_float32 (n, _dbg) ->
        Ok
          (emit_op_res b
             (SU.make_const_float32 (Int32.bits_of_float n))
             Cmm.typ_float32 [||])
      | Cconst_float (n, _dbg) ->
        Ok
          (emit_op_res b
             (SU.make_const_float (Int64.bits_of_float n))
             Cmm.typ_float [||])
      | Cconst_vec128 (bits, _dbg) ->
        Ok (emit_op_res b (SU.make_const_vec128 bits) Cmm.typ_vec128 [||])
      | Cconst_vec256 (bits, _dbg) ->
        Ok (emit_op_res b (Operation.Const_vec256 bits) Cmm.typ_vec256 [||])
      | Cconst_vec512 (bits, _dbg) ->
        Ok (emit_op_res b (Operation.Const_vec512 bits) Cmm.typ_vec512 [||])
      | Cconst_symbol (n, _dbg) ->
        Ok (emit_op_res b (SU.make_const_symbol n) Cmm.typ_int [||])
      | Cvar v -> Ok (env_find v env)
      | Ctuple [] -> Ok [||]
      | Ctuple exp_list -> (
        match emit_parts_list env b exp_list with
        | None -> Never_returns
        | Some (simple_list, ext_env) -> emit_tuple ext_env b simple_list)
      | Cop (Craise k, args, dbg) ->
        let* r = emit_tuple env b args in
        B.finish_block !b ~dbg (Raise { raise_kind = k; args = r });
        Never_returns
      | Cop (Copaque, args, dbg) -> (
        match emit_parts_list env b args with
        | None -> Never_returns
        | Some (simple_args, env) ->
          let* rs = emit_tuple env b simple_args in
          let typ = Array.map B.Instruction.arg_type rs in
          Ok (emit_op_res b ~dbg Opaque typ rs))
      | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) ->
        let* loc_exp = emit env b arg ~tail:false in
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        Ok (Array.sub loc_exp size_before (Array.length fields_layout.(field)))
      | Cop (op, args, dbg) -> emit_expr_op env b op args dbg
      | Cexit (lbl, args, traps) -> emit_expr_exit env b lbl args traps
      | Cinvalid { message; symbol } -> emit_invalid env b message symbol
    in
    match r with Ok _ when tail -> insert_return b r | _ -> r

  and insert_return b (r : result) : result =
    match r with
    | Never_returns -> Never_returns
    | Ok r ->
      B.finish_block !b ~dbg:Debuginfo.none (Return { args = r });
      Never_returns

  and emit_invalid env b message symbol =
    let arg_expr = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
    let* arg_instrs = emit_tuple env b [arg_expr] in
    if !SU.current_function_is_check_enabled
    then (
      let { B.block = cont_block; params = cont_params } =
        B.new_block ~params:Cmm.typ_int
      in
      B.finish_block !b ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = Some cont_block });
      b := B.start_block cont_block;
      Ok cont_params)
    else (
      B.finish_block !b ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = None });
      Never_returns)

  and emit_call _env b ~ty ~nontail (new_op : Cfg.terminator) arg_instrs dbg :
      result =
    let ty =
      match new_op with
      | Prim { op = External { ty_res; _ }; _ } -> ty_res
      | _ -> ty
    in
    match new_op with
    | Call { op = call_op; _ } ->
      let { B.block = cont_block; params = cont_params } =
        B.new_block ~params:ty
      in
      B.finish_block !b ~dbg
        (Call
           { op = Func call_op;
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      b := B.start_block cont_block;
      Ok cont_params
    | Prim { op = External ({ ty_res; _ } as ext_call); _ } ->
      let { B.block = cont_block; params = cont_params } =
        B.new_block ~params:ty_res
      in
      B.finish_block !b ~dbg
        (Call
           { op = Prim (External ext_call);
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      b := B.start_block cont_block;
      Ok cont_params
    | Prim { op = Probe _ as probe_op; _ } ->
      let { B.block = cont_block; params = cont_params } =
        B.new_block ~params:ty
      in
      B.finish_block !b ~dbg
        (Call
           { op = Prim probe_op;
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      b := B.start_block cont_block;
      Ok cont_params
    | Call_no_return _ ->
      Misc.fatal_errorf
        "Ssa_of_cmm: Currently unused codepath, implement if hit"
    | Never | Return | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Raise _ | Tailcall_self _ | Tailcall_func _
    | Invalid _ ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"")
        new_op

  and emit_expr_op env b op args dbg : result =
    match emit_parts_list env b args with
    | None -> Never_returns
    | Some (simple_args, env) -> (
      let ty = SU.oper_result_type op in
      let new_op, new_args =
        Sel.select_operation op simple_args dbg ~label_after:Label.none
      in
      match new_op with
      | Basic (Op (Alloc { bytes = _; mode; dbginfo = [placeholder] })) ->
        let bytes =
          List.fold_left
            (fun acc arg -> acc + size_of_cmm_expr env arg)
            0 new_args
        in
        let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
        let op =
          Operation.Alloc
            { bytes = alloc_words * Arch.size_addr;
              dbginfo = [{ placeholder with alloc_words; alloc_dbg = dbg }];
              mode
            }
        in
        let rd = emit_op b ~op ~dbg ~typ:Cmm.typ_val ~args:[||] in
        emit_stores env b dbg new_args rd;
        Ok [| rd |]
      | _ -> (
        let* arg_instrs = emit_tuple env b new_args in
        match new_op with
        | Terminator term ->
          let nontail =
            match[@warning "-4"] op with
            | Cmm.Capply { region; _ } -> (
              match (region : Lambda.region_close) with
              | Rc_normal -> false
              | Rc_nontail -> true
              | Rc_close_at_apply -> assert false (* desugared in Flambda2 *))
            | _ -> true
          in
          emit_call env b ~ty ~nontail term arg_instrs dbg
        | Basic (Op op) -> Ok (emit_op_res b ~dbg op ty arg_instrs)
        | Basic basic ->
          Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic
            basic))

  and emit_ifthenelse env b ~tail econd eif eelse : result =
    let cond, earg = Sel.select_condition econd in
    let* rarg = emit env b earg ~tail:false in
    let { B.block = then_block; _ } = B.new_block ~params:[||] in
    let { B.block = else_block; _ } = B.new_block ~params:[||] in
    emit_branch b cond rarg ~true_block:then_block ~false_block:else_block;
    let then_b = ref (B.start_block then_block) in
    let r_then = emit env then_b eif ~tail in
    let else_b = ref (B.start_block else_block) in
    let r_else = emit env else_b eelse ~tail in
    join_branches b (r_then, then_b) (r_else, else_b)

  and emit_switch env b ~tail esel index ecases : result =
    let* rsel = emit env b esel ~tail:false in
    let case_blocks =
      Array.map
        (fun (_case_expr, _dbg) ->
          let { B.block; _ } = B.new_block ~params:[||] in
          block)
        ecases
    in
    let targets = Array.map (fun idx -> case_blocks.(idx)) index in
    let index =
      assert (Array.length rsel = 1);
      rsel.(0)
    in
    B.finish_block !b ~dbg:Debuginfo.none (Switch { index; targets });
    let case_results =
      Array.mapi
        (fun i (case_expr, _dbg) ->
          let case_b = ref (B.start_block case_blocks.(i)) in
          emit env case_b case_expr ~tail, case_b)
        ecases
    in
    join_array b case_results

  and emit_catch env b ~tail handlers body : result =
    (* Create one block per handler up front to also support mutually recursive
       handlers. All handlers are emitted unconditionally, the SSA graph builder
       will drop unreachable blocks. *)
    let env, handler_blocks =
      List.fold_left_map
        (fun env Cmm.{ label = nfail; params; _ } ->
          let types = List.map (fun (_id, ty) -> ty) params |> Array.concat in
          let new_block = B.new_block ~params:types in
          let block_params = B.Block.params new_block.block in
          let pos = ref 0 in
          List.iter
            (fun (id, ty) ->
              let n = Array.length ty in
              let name = V.name (VP.var id) in
              for i = 0 to n - 1 do
                (block_params.(!pos + i) : Ssa_intf.block_param).name
                  <- Some name
              done;
              pos := !pos + n)
            params;
          let env =
            { env with
              static_exceptions =
                Static_label.Map.add nfail new_block.block env.static_exceptions
            }
          in
          env, new_block)
        env handlers
    in
    let r_body = emit env b body ~tail in
    let translate_handler (handler : Cmm.static_handler)
        (handler_block : B.new_block_result) =
      let handler_env =
        let param_idx = ref 0 in
        List.fold_left
          (fun env (id, ty) ->
            let n = Array.length ty in
            let proj_instrs =
              Array.init n (fun i ->
                  Array.get handler_block.params (!param_idx + i))
            in
            param_idx := !param_idx + n;
            env_add id proj_instrs env)
          env handler.params
      in
      let b = ref (B.start_block handler_block.block) in
      List.iter
        (fun (id, _ty) ->
          let provenance = VP.provenance id in
          if Option.is_some provenance
          then
            let regs = V.Map.find (VP.var id) handler_env.vars in
            emit_instruction_no_res b
              (Name_for_debugger
                 { ident = VP.var id; provenance; which_parameter = None; regs }))
        handler.params;
      emit handler_env b handler.body ~tail, b
    in
    let handler_results = List.map2 translate_handler handlers handler_blocks in
    join_array b (Array.of_list ((r_body, b) :: handler_results))

  and find_handler env handler_id : B.Block.t =
    try Static_label.Map.find handler_id env.static_exceptions
    with Not_found ->
      Misc.fatal_errorf "Ssa_of_cmm: unbound trap handler %a"
        Static_label.format handler_id

  and emit_trap_actions env b traps =
    traps
    |> List.iter (fun (trap : Cmm.trap_action) ->
        emit_instruction_no_res b
          (match trap with
          | Push handler_id ->
            Push_trap { handler = Some (find_handler env handler_id) }
          | Pop handler_id ->
            Pop_trap { handler = Some (find_handler env handler_id) }))

  and emit_expr_exit env b (lbl : Cmm.exit_label) args traps : result =
    match emit_parts_list env b args with
    | None -> Never_returns
    | Some (simple_list, ext_env) -> (
      match lbl with
      | Lbl nfail ->
        let* src = emit_tuple ext_env b simple_list in
        let handler = find_handler env nfail in
        emit_trap_actions env b traps;
        B.finish_block !b ~dbg:Debuginfo.none
          (Goto { goto = handler; args = src });
        Never_returns
      | Return_lbl ->
        let* src = emit_tuple ext_env b simple_list in
        emit_trap_actions env b traps;
        B.finish_block !b ~dbg:Debuginfo.none (Return { args = src });
        Never_returns)

  (* Join two branches into a fresh block. After this call [b] points at the
     joined block (or is unchanged if both branches did not return). *)
  and join_branches b (r1, b1) (r2, b2) : result =
    match r1, r2 with
    | Never_returns, Never_returns -> Never_returns
    | Ok r, Never_returns ->
      b := !b1;
      Ok r
    | Never_returns, Ok r ->
      b := !b2;
      Ok r
    | Ok r1_instrs, Ok r2_instrs ->
      assert (Array.length r1_instrs = Array.length r2_instrs);
      let join_types = Array.map B.Instruction.arg_type r1_instrs in
      let { B.block = join_block; params = join_params } =
        B.new_block ~params:join_types
      in
      B.finish_block !b1 ~dbg:Debuginfo.none
        (Goto { goto = join_block; args = r1_instrs });
      B.finish_block !b2 ~dbg:Debuginfo.none
        (Goto { goto = join_block; args = r2_instrs });
      b := B.start_block join_block;
      Ok join_params

  (* Join a set of branches (each with its own end-cursor) into a fresh block.
     [b] is left pointing at the joined block, or unchanged if every branch did
     not return. *)
  and join_array b (results : (result * B.unfinished_block ref) array) : result
      =
    let join_info = ref None in
    Array.iter
      (fun (r, _) ->
        match r with
        | Never_returns -> ()
        | Ok instrs -> (
          let types = Array.map B.Instruction.arg_type instrs in
          match !join_info with
          | None -> join_info := Some types
          | Some prev ->
            (* Different paths may produce values of compatible but distinct
               machtype components (e.g. a [try val E with _ 456] mixes [val]
               and [int]). Pick the least upper bound so the joined block's
               param types accommodate every incoming arm. *)
            assert (Array.length prev = Array.length types);
            let lub = Array.map2 Cmm.lub_component prev types in
            join_info := Some lub))
      results;
    match !join_info with
    | None -> Never_returns
    | Some join_types ->
      let { B.block = join_block; params = join_params } =
        B.new_block ~params:join_types
      in
      Array.iter
        (fun (r, b_branch) ->
          match r with
          | Never_returns -> ()
          | Ok instrs ->
            B.finish_block !b_branch ~dbg:Debuginfo.none
              (Goto { goto = join_block; args = instrs }))
        results;
      b := B.start_block join_block;
      Ok join_params
end

let convert (f : Cmm.fundecl) : (module Ssa.Finished_graph) =
  SU.current_function_name := f.fun_name.sym_name;
  SU.current_function_is_check_enabled
    := Zero_alloc_checker.is_check_enabled f.fun_codegen_options
         f.fun_name.sym_name f.fun_dbg;
  try
    let fun_arg_types = List.map snd f.fun_args |> Array.concat in
    let fi : Ssa.function_info =
      { fun_name = f.fun_name.sym_name;
        fun_args = fun_arg_types;
        fun_args_names = f.fun_args;
        fun_codegen_options = f.fun_codegen_options;
        fun_dbg = f.fun_dbg;
        fun_poll = f.fun_poll;
        fun_ret_type = f.fun_ret_type
      }
    in
    let module B = (val Ssa.make_builder fi : Ssa.Standalone_graph_builder) in
    let module C = Make (B) in
    let env =
      { C.vars = V.Map.empty; static_exceptions = Static_label.Map.empty }
    in
    let env, _offset =
      List.fold_left
        (fun (env, offset) (id, ty) ->
          let n = Array.length ty in
          let projs =
            Array.init n (fun i -> Array.get B.entry_params (offset + i))
          in
          C.env_add id projs env, offset + n)
        (env, 0) f.fun_args
    in
    let r = C.emit env (ref (B.start_block B.entry)) f.fun_body ~tail:true in
    assert (match r with Never_returns -> true | Ok _ -> false);
    let result = B.finish () in
    Ssa_validate.validate result;
    result
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "*** Ssa_of_cmm error for %s: %s@.*** CMM:@.%a@."
      f.fun_name.sym_name (Printexc.to_string exn) Printcmm.fundecl f;
    Printexc.raise_with_backtrace exn bt
