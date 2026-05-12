open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42-45"]

(** Cmm → SSA conversion.

    Walks a [Cmm.fundecl] in expression order, emitting into an
    {!Ssa.Graph_builder} via the [Make] functor. Operation-level lowering is
    delegated to {!Cfg_selectgen} via [Cfg_selectgen.select_operation].

    Trap handling: [Cexit]'s trap actions are emitted as [Push_trap] /
    [Pop_trap] body instructions. In contrast to {!Cfg_selectgen}, we do not
    compute trap stacks and trap continuations, relying on the SSA machinery to
    do this automatically instead.

    Also in contrast to Cfg_selectgen, we do not emit tail calls.
    {!Ssa_tail_call} later rewrites call+return patterns into [Tailcall_self] /
    [Tailcall_func] when possible. The [Capply]'s [region_close] is forwarded as
    the [Call.nontail] flag so [@nontail] annotations can suppress the rewrite.
*)

module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance
module Sel = Cfg_selectgen.Make (Cfg_selection)

module Make (Builder : Ssa.Graph_builder) = struct
  open Builder

  let current_function_is_check_enabled =
    Zero_alloc_checker.is_check_enabled function_info.fun_codegen_options
      function_info.fun_name function_info.fun_dbg

  type 'a or_never_returns =
    | Ok of 'a
    | Never_returns

  let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

  type result = Instruction.t array or_never_returns

  type env =
    { vars : Instruction.t array V.Map.t;
      static_exceptions : Block.t Static_label.Map.t
    }

  let env_find v env =
    try V.Map.find v env.vars
    with Not_found -> Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

  let env_add v instrs env =
    { env with vars = V.Map.add (VP.var v) instrs env.vars }

  let emit_op_res c ?(dbg = Debuginfo.none) op typ args : Instruction.t array =
    let i = emit_op c ~op ~dbg ~typ ~args in
    if Array.length typ = 1
    then [| i |]
    else
      Array.init (Array.length typ) (fun index ->
          Instruction.make_proj ~index i)

  let bind_let env c v args =
    let env = env_add v args env in
    let name = V.name (VP.var v) in
    Array.iter (fun i -> Instruction.set_name i name) args;
    let provenance = VP.provenance v in
    if Option.is_some provenance
    then
      emit_instruction c
        (Name_for_debugger
           { ident = VP.var v; provenance; which_parameter = None; args });
    env

  let chunk_of_machtype (c : Cmm.machtype_component) : Cmm.memory_chunk =
    match c with
    | Float -> Double
    | Float32 -> Single { reg = Float32 }
    | Vec128 -> Onetwentyeight_unaligned
    | Vec256 -> Twofiftysix_unaligned
    | Vec512 -> Fivetwelve_unaligned
    | Val | Int | Addr -> Word_val
    | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"

  let rec size_of_cmm_expr env (e : Cmm.expression) =
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
        (fun acc i -> acc + SU.size_component (Instruction.arg_type i))
        0 instrs
    | Ctuple el ->
      List.fold_left (fun acc e -> acc + size_of_cmm_expr env e) 0 el
    | Cop (op, _, _) ->
      let ty = SU.oper_result_type op in
      Array.fold_left (fun acc c -> acc + SU.size_component c) 0 ty
    | Clet (_, _, body) | Csequence (_, body) -> size_of_cmm_expr env body
    | Cifthenelse _ | Cphantom_let _ | Cswitch _ | Ccatch _ | Cexit _
    | Cinvalid _ ->
      Misc.fatal_error
        "Ssa_of_cmm.size_of_cmm_expr: unexpected kind of expression"

  let emit_branch c (test : Operation.test) rarg ~true_block ~false_block =
    let make_cond op args =
      emit_op c ~op ~dbg:Debuginfo.none ~typ:Cmm.typ_int ~args
    in
    let is_comparison (v : Instruction.t) =
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
    let term : Terminator.t =
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
    finish_block c ~dbg:Debuginfo.none term

  let rec emit_parts env c ~effects_after exp =
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
    then Ok (exp, env)
    else
      let* r = emit env c exp ~tail:false in
      if Array.length r = 0
      then Ok (Cmm.Ctuple [], env)
      else
        let id = V.create_local "bind" in
        Ok (Cmm.Cvar id, { env with vars = V.Map.add id r env.vars })

  and emit_parts_list env c exp_list =
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
        let* result, env = acc in
        let* exp_result, env = emit_parts env c ~effects_after exp in
        Ok (exp_result :: result, env))
      (Ok ([], env))
      exp_list_right_to_left

  and emit_tuple env c exp_list : result =
    let rec emit_list = function
      | [] -> Ok []
      | exp :: rem ->
        let* loc_rem = emit_list rem in
        let* loc_exp = emit env c exp ~tail:false in
        Ok (loc_exp :: loc_rem)
    in
    let* l = emit_list exp_list in
    Ok (Array.concat l)

  and emit_stores env c dbg (args : Cmm.expression list) regs_addr =
    let byte_offset = ref (-Arch.size_int) in
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing !byte_offset)
    in
    let base = ref regs_addr in
    let reset_addressing () =
      let tmp =
        emit_op c
          ~op:(SU.make_const_int (Nativeint.of_int !byte_offset))
          ~dbg ~typ:Cmm.typ_int ~args:[||]
      in
      assert (!byte_offset > 0);
      let new_base =
        emit_op c ~op:(Operation.Intop Iadd) ~dbg ~typ:Cmm.typ_addr
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
      match emit env c arg ~tail:false with
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
            (fun (r : Instruction.t) ->
              let chunk = chunk_of_machtype (Instruction.arg_type r) in
              ignore
                (emit_op c
                   ~op:(Operation.Store (chunk, !addressing_mode, false))
                   ~dbg ~typ:[||] ~args:[| r; !base |]
                  : Instruction.t);
              advance (Select_utils.size_component (Instruction.arg_type r)))
            regs
        | Some op ->
          ignore
            (emit_op c ~op ~dbg ~typ:[||] ~args:(Array.append regs [| !base |])
              : Instruction.t);
          advance (size_of_cmm_expr env original_arg))
      | Never_returns ->
        Misc.fatal_error "Ssa_of_cmm.emit_stores: never_returns"
    in
    List.iter for_one_arg args

  and emit env c (exp : Cmm.expression) ~tail : result =
    let r =
      match exp with
      | Clet (v, e1, e2) ->
        let* r1 = emit env c e1 ~tail:false in
        let env = bind_let env c v r1 in
        emit env c e2 ~tail
      | Cphantom_let (_var, _defining_expr, body) -> emit env c body ~tail
      | Csequence (e1, e2) ->
        let* _ = emit env c e1 ~tail:false in
        emit env c e2 ~tail
      | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
        emit_ifthenelse env c ~tail econd eif eelse
      | Cswitch (esel, index, ecases, _dbg) ->
        emit_switch env c ~tail esel index ecases
      | Ccatch (_, [], e1) -> emit env c e1 ~tail
      | Ccatch (_flag, handlers, body) -> emit_catch env c ~tail handlers body
      (* Leaf expressions *)
      | Cconst_int (n, _dbg) ->
        Ok
          (emit_op_res c
             (SU.make_const_int (Nativeint.of_int n))
             Cmm.typ_int [||])
      | Cconst_natint (n, _dbg) ->
        Ok (emit_op_res c (SU.make_const_int n) Cmm.typ_int [||])
      | Cconst_float32 (n, _dbg) ->
        Ok
          (emit_op_res c
             (SU.make_const_float32 (Int32.bits_of_float n))
             Cmm.typ_float32 [||])
      | Cconst_float (n, _dbg) ->
        Ok
          (emit_op_res c
             (SU.make_const_float (Int64.bits_of_float n))
             Cmm.typ_float [||])
      | Cconst_vec128 (bits, _dbg) ->
        Ok (emit_op_res c (SU.make_const_vec128 bits) Cmm.typ_vec128 [||])
      | Cconst_vec256 (bits, _dbg) ->
        Ok (emit_op_res c (Operation.Const_vec256 bits) Cmm.typ_vec256 [||])
      | Cconst_vec512 (bits, _dbg) ->
        Ok (emit_op_res c (Operation.Const_vec512 bits) Cmm.typ_vec512 [||])
      | Cconst_symbol (n, _dbg) ->
        Ok (emit_op_res c (SU.make_const_symbol n) Cmm.typ_int [||])
      | Cvar v -> Ok (env_find v env)
      | Ctuple [] -> Ok [||]
      | Ctuple exp_list ->
        let* simple_list, ext_env = emit_parts_list env c exp_list in
        emit_tuple ext_env c simple_list
      | Cop (Craise k, args, dbg) ->
        let* r = emit_tuple env c args in
        finish_block c ~dbg (Raise { raise_kind = k; args = r });
        Never_returns
      | Cop (Copaque, args, dbg) ->
        let* simple_args, env = emit_parts_list env c args in
        let* rs = emit_tuple env c simple_args in
        let typ = Array.map Instruction.arg_type rs in
        Ok (emit_op_res c ~dbg Opaque typ rs)
      | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) ->
        let* loc_exp = emit env c arg ~tail:false in
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        Ok (Array.sub loc_exp size_before (Array.length fields_layout.(field)))
      | Cop (op, args, dbg) -> emit_expr_op env c op args dbg
      | Cexit (lbl, args, traps) -> emit_expr_exit env c lbl args traps
      | Cinvalid { message; symbol } -> emit_invalid env c message symbol
    in
    match r with Ok _ when tail -> insert_return c r | _ -> r

  and insert_return c (r : result) : result =
    let* r = r in
    finish_block c ~dbg:Debuginfo.none (Return { args = r });
    Never_returns

  and emit_invalid env c message symbol =
    let arg_expr = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
    let* arg_instrs = emit_tuple env c [arg_expr] in
    if current_function_is_check_enabled
    then (
      let { block = cont_block; params = cont_params } =
        new_block ~params:Cmm.typ_int
      in
      finish_block c ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = Some cont_block });
      move_cursor c ~new_pos:(start_block cont_block);
      Ok cont_params)
    else (
      finish_block c ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = None });
      Never_returns)

  and emit_call _env c ~ty ~nontail (new_op : Cfg.terminator) arg_instrs dbg :
      result =
    let ty =
      match new_op with
      | Prim { op = External { ty_res; _ }; _ } -> ty_res
      | _ -> ty
    in
    match new_op with
    | Call { op = call_op; _ } ->
      let { block = cont_block; params = cont_params } = new_block ~params:ty in
      finish_block c ~dbg
        (Call
           { op = Func call_op;
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      move_cursor c ~new_pos:(start_block cont_block);
      Ok cont_params
    | Prim { op = External ({ ty_res; _ } as ext_call); _ } ->
      let { block = cont_block; params = cont_params } =
        new_block ~params:ty_res
      in
      finish_block c ~dbg
        (Call
           { op = Prim (External ext_call);
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      move_cursor c ~new_pos:(start_block cont_block);
      Ok cont_params
    | Prim { op = Probe _ as probe_op; _ } ->
      let { block = cont_block; params = cont_params } = new_block ~params:ty in
      finish_block c ~dbg
        (Call
           { op = Prim probe_op;
             args = arg_instrs;
             continuation = cont_block;
             may_raise = Cfg.can_raise_terminator new_op;
             nontail
           });
      move_cursor c ~new_pos:(start_block cont_block);
      Ok cont_params
    | Call_no_return _ ->
      Misc.fatal_errorf
        "Ssa_of_cmm: Currently unused codepath, implement when it becomes \
         reachable"
    | Never | Return | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Raise _ | Tailcall_self _ | Tailcall_func _
    | Invalid _ ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"")
        new_op

  and emit_expr_op env c op args dbg : result =
    let* simple_args, env = emit_parts_list env c args in
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
      let rd = emit_op c ~op ~dbg ~typ:Cmm.typ_val ~args:[||] in
      emit_stores env c dbg new_args rd;
      Ok [| rd |]
    | Basic (Op (Alloc _)) ->
      Misc.fatal_error "Alloc is expected to have exactly one dbginfo"
    | _ -> (
      let* arg_instrs = emit_tuple env c new_args in
      match new_op with
      | Terminator term ->
        let nontail =
          match[@warning "-4"] op with
          | Cmm.Capply { region; _ } -> (
            match (region : Lambda.region_close) with
            | Rc_normal -> false
            | Rc_nontail -> true
            | Rc_close_at_apply ->
              Misc.fatal_error
                "Rc_close_at_apply should have been lowered by Flambda2")
          | _ -> false
        in
        emit_call env c ~ty ~nontail term arg_instrs dbg
      | Basic (Op op) -> Ok (emit_op_res c ~dbg op ty arg_instrs)
      | Basic basic ->
        Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic
          basic)

  and emit_ifthenelse env c ~tail econd eif eelse : result =
    let cond, earg = Sel.select_condition econd in
    let* rarg = emit env c earg ~tail:false in
    let { block = then_block; _ } = new_block ~params:[||] in
    let { block = else_block; _ } = new_block ~params:[||] in
    emit_branch c cond rarg ~true_block:then_block ~false_block:else_block;
    let then_c = start_block then_block in
    let r_then = emit env then_c eif ~tail in
    let else_c = start_block else_block in
    let r_else = emit env else_c eelse ~tail in
    join c [| r_then, then_c; r_else, else_c |]

  and emit_switch env c ~tail esel index ecases : result =
    let* rsel = emit env c esel ~tail:false in
    let case_blocks =
      Array.map
        (fun (_case_expr, _dbg) ->
          let { block; _ } = new_block ~params:[||] in
          block)
        ecases
    in
    let targets = Array.map (fun idx -> case_blocks.(idx)) index in
    let index =
      assert (Array.length rsel = 1);
      rsel.(0)
    in
    finish_block c ~dbg:Debuginfo.none (Switch { index; targets });
    let case_results =
      Array.mapi
        (fun i (case_expr, _dbg) ->
          let case_c = start_block case_blocks.(i) in
          emit env case_c case_expr ~tail, case_c)
        ecases
    in
    join c case_results

  and emit_catch env c ~tail handlers body : result =
    (* Create one block per handler up front to also support mutually recursive
       handlers. All handlers are emitted unconditionally, the SSA graph builder
       will drop unreachable blocks. *)
    let static_exceptions, handler_blocks =
      List.fold_left_map
        (fun static_exceptions Cmm.{ label = nfail; params; _ } ->
          let types =
            params |> List.map (fun (_id, ty) -> ty) |> Array.concat
          in
          let new_block = new_block ~params:types in
          let block_params = Block.params new_block.block in
          let pos = ref 0 in
          params
          |> List.iter (fun (id, ty) ->
              let n = Array.length ty in
              let name = V.name (VP.var id) in
              for i = 0 to n - 1 do
                Block.set_param_name block_params.(!pos + i) name
              done;
              pos := !pos + n);
          ( Static_label.Map.add nfail new_block.block static_exceptions,
            new_block ))
        env.static_exceptions handlers
    in
    let env = { env with static_exceptions } in
    let r_body = emit env c body ~tail in
    let translate_handler (handler : Cmm.static_handler)
        (handler_block : new_block_result) =
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
      let c = start_block handler_block.block in
      List.iter
        (fun (id, _ty) ->
          let provenance = VP.provenance id in
          if Option.is_some provenance
          then
            let args = V.Map.find (VP.var id) handler_env.vars in
            emit_instruction c
              (Name_for_debugger
                 { ident = VP.var id; provenance; which_parameter = None; args }))
        handler.params;
      emit handler_env c handler.body ~tail, c
    in
    let handler_results = List.map2 translate_handler handlers handler_blocks in
    join c (Array.of_list ((r_body, c) :: handler_results))

  and find_handler env handler_id : Block.t =
    try Static_label.Map.find handler_id env.static_exceptions
    with Not_found ->
      Misc.fatal_errorf "Ssa_of_cmm: unbound trap handler %a"
        Static_label.format handler_id

  and emit_trap_actions env c traps =
    traps
    |> List.iter (fun (trap : Cmm.trap_action) ->
        emit_instruction c
          (match trap with
          | Push handler_id ->
            Push_trap { handler = find_handler env handler_id }
          | Pop handler_id -> Pop_trap { handler = find_handler env handler_id }))

  and emit_expr_exit env c (lbl : Cmm.exit_label) args traps : result =
    let* simple_list, ext_env = emit_parts_list env c args in
    match lbl with
    | Lbl nfail ->
      let* src = emit_tuple ext_env c simple_list in
      let handler = find_handler env nfail in
      emit_trap_actions env c traps;
      finish_block c ~dbg:Debuginfo.none
        (Goto { goto = handler; args = src |> Array.map (fun arg -> Some arg) });
      Never_returns
    | Return_lbl ->
      let* src = emit_tuple ext_env c simple_list in
      emit_trap_actions env c traps;
      finish_block c ~dbg:Debuginfo.none (Return { args = src });
      Never_returns

  (* Join a set of branches (each with its own end-cursor) into a fresh block.
     [c] is left pointing at the joined block, or unchanged if no branch
     returns. *)
  and join c (results : (result * cursor) array) : result =
    let join_info = ref None in
    Array.iter
      (fun (r, _) ->
        match r with
        | Never_returns -> ()
        | Ok instrs -> (
          let types = Array.map Instruction.arg_type instrs in
          match !join_info with
          | None -> join_info := Some types
          | Some prev ->
            (* Different paths may produce values of compatible but distinct
               machtype components. Pick the least upper bound so the joined
               block's param types accommodate every incoming arm. *)
            assert (Array.length prev = Array.length types);
            let lub = Array.map2 Cmm.lub_component prev types in
            join_info := Some lub))
      results;
    match !join_info with
    | None -> Never_returns
    | Some join_types ->
      let { block = join_block; params = join_params } =
        new_block ~params:join_types
      in
      Array.iter
        (fun (r, c_branch) ->
          match r with
          | Never_returns -> ()
          | Ok instrs ->
            finish_block c_branch ~dbg:Debuginfo.none
              (Goto
                 { goto = join_block;
                   args = instrs |> Array.map (fun instr -> Some instr)
                 }))
        results;
      move_cursor c ~new_pos:(start_block join_block);
      Ok join_params
end

let convert (f : Cmm.fundecl) ~keep_unused_ops : (module Ssa.Finished_graph) =
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
    let module Builder =
      (val Ssa.make_builder fi ~keep_unused_ops : Ssa.Graph_builder)
    in
    let module M = Make (Builder) in
    let env =
      { M.vars = V.Map.empty; static_exceptions = Static_label.Map.empty }
    in
    let env, _offset =
      List.fold_left
        (fun (env, offset) (id, ty) ->
          let n = Array.length ty in
          let projs =
            Array.init n (fun i -> Array.get Builder.entry_params (offset + i))
          in
          M.env_add id projs env, offset + n)
        (env, 0) f.fun_args
    in
    let r =
      M.emit env (Builder.start_block Builder.entry) f.fun_body ~tail:true
    in
    assert (match r with Never_returns -> true | Ok _ -> false);
    let result = Builder.finish () in
    Ssa_validate.validate result;
    result
  with Misc.Fatal_error as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "*** Ssa_of_cmm error for %s: %s@.*** CMM:@.%a@."
      f.fun_name.sym_name (Printexc.to_string exn) Printcmm.fundecl f;
    Printexc.raise_with_backtrace exn bt
