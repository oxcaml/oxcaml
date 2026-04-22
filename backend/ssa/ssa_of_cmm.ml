open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42-45"]

module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance
module B = Ssa.Builder

type 'a or_never_returns =
  | Ok of 'a
  | Never_returns

let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

(* The result of emitting an expression: the produced values and the builder to
   continue building into. *)
type result = (Ssa.instruction array * B.t) or_never_returns

type static_handler =
  { handler_block : Ssa.block;
    handler_b : B.t;
    handler_types : Cmm.machtype;
    traps_ref : SU.trap_stack_info ref
  }

type env =
  { vars : Ssa.instruction array V.Map.t;
    static_exceptions : static_handler Static_label.Map.t;
    trap_stack : Operation.trap_stack;
    tailrec_block : Ssa.block
  }

let current_exn_continuation env : Ssa.block option =
  match env.trap_stack with
  | Operation.Uncaught -> None
  | Specific_trap (static_label, _) -> (
    match Static_label.Map.find_opt static_label env.static_exceptions with
    | Some handler -> Some handler.handler_block
    | None -> None)

module Make (Target : Cfg_selectgen_target_intf.S) = struct
  module Sel = Cfg_selectgen.Make (Target)

  let is_immediate = Sel.is_immediate

  let is_immediate_test = Sel.is_immediate_test

  let select_condition = Sel.select_condition

  let select_operation = Sel.select_operation

  let effects_of = Sel.effects_of

  let is_simple_expr = Sel.is_simple_expr

  let env_find v env =
    try V.Map.find v env.vars
    with Not_found -> Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

  let env_add v instrs env =
    { env with vars = V.Map.add (VP.var v) instrs env.vars }

  let bind_let env b v r1 =
    let env = env_add v r1 env in
    let provenance = VP.provenance v in
    if Option.is_some provenance
    then
      B.emit_instruction b
        (Name_for_debugger
           { ident = VP.var v; provenance; which_parameter = None; regs = r1 });
    env

  let insert_op_debug b op dbg args typ : Ssa.instruction array =
    let i = B.emit_op b ~op ~dbg ~typ ~args in
    if Array.length typ = 1
    then [| i |]
    else
      Array.init (Array.length typ) (fun index ->
          (Ssa.Proj { index; src = i } : Ssa.instruction))

  let insert_op b op args typ = insert_op_debug b op Debuginfo.none args typ

  let pop_all_traps env =
    let rec pop_all acc = function
      | Operation.Uncaught -> acc
      | Operation.Specific_trap (lbl, t) -> pop_all (Cmm.Pop lbl :: acc) t
    in
    pop_all [] env.trap_stack |> List.rev

  let set_traps_for_raise env =
    match env.trap_stack with
    | Operation.Uncaught -> ()
    | Specific_trap (lbl, _) -> (
      match Static_label.Map.find_opt lbl env.static_exceptions with
      | Some handler ->
        SU.set_traps lbl handler.traps_ref env.trap_stack [Pop lbl]
      | None ->
        Misc.fatal_errorf "Ssa_of_cmm: trap %a not registered in env"
          Static_label.format lbl)

  let chunk_of_component (c : Cmm.machtype_component) : Cmm.memory_chunk =
    match c with
    | Float -> Double
    | Float32 -> Single { reg = Float32 }
    | Vec128 -> Onetwentyeight_unaligned
    | Vec256 -> Twofiftysix_unaligned
    | Vec512 -> Fivetwelve_unaligned
    | Val | Int | Addr | Valx2 -> Word_val

  let typ_of_instruction (r : Ssa.instruction) : Cmm.machtype_component =
    match r with
    | Op { typ; _ } ->
      assert (Array.length typ = 1);
      typ.(0)
    | Block_param { typ; _ } -> typ
    | Proj { index; src } -> (
      match src with
      | Op { typ; _ } -> typ.(index)
      | _ -> Misc.fatal_error "typ_of_instruction: Proj of non-Op")
    | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
      Misc.fatal_error "typ_of_instruction: not a value instruction"

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
          (fun acc i -> acc + SU.size_component (typ_of_instruction i))
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

  let unreachable_handler_body : Cmm.expression =
    let dummy_constant : Cmm.expression = Cconst_int (1, Debuginfo.none) in
    let segfault : Cmm.expression =
      Cop
        ( Cload
            { memory_chunk = Word_int; mutability = Mutable; is_atomic = false },
          [Cconst_int (0, Debuginfo.none)],
          Debuginfo.none )
    in
    let dummy_raise : Cmm.expression =
      Cop (Craise Raise_notrace, [dummy_constant], Debuginfo.none)
    in
    Csequence (segfault, dummy_raise)

  let emit_branch b (test : Operation.test) rarg ~true_block ~false_block :
      Ssa.terminator =
    let make_cond op args =
      B.emit_op b ~op ~dbg:Debuginfo.none ~typ:Cmm.typ_int ~args
    in
    let is_comparison (v : Ssa.instruction) =
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
    match test with
    | Itruetest ->
      let c = wrap_truth_test rarg.(0) in
      Branch { conditions = [| c, true_block |]; else_goto = false_block }
    | Ifalsetest ->
      let c = wrap_truth_test rarg.(0) in
      Branch { conditions = [| c, false_block |]; else_goto = true_block }
    | Iinttest cmp ->
      let c = make_cond (Intop (Icomp cmp)) rarg in
      Branch { conditions = [| c, true_block |]; else_goto = false_block }
    | Iinttest_imm (cmp, n) ->
      let c = make_cond (Intop_imm (Icomp cmp, n)) [| rarg.(0) |] in
      Branch { conditions = [| c, true_block |]; else_goto = false_block }
    | Ifloattest (w, cmp) ->
      let c = make_cond (Floatop (w, Icompf cmp)) rarg in
      Branch { conditions = [| c, true_block |]; else_goto = false_block }
    | Ioddtest ->
      let c = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
      Branch { conditions = [| c, true_block |]; else_goto = false_block }
    | Ieventest ->
      let c = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
      Branch { conditions = [| c, false_block |]; else_goto = true_block }

  let rec emit_parts env b ~effects_after exp =
    let module EC = SU.Effect_and_coeffect in
    let may_defer_evaluation =
      let ec = effects_of exp in
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
    if may_defer_evaluation && is_simple_expr exp
    then Some (exp, env, b)
    else
      match emit_expr env b exp ~bound_name:None with
      | Never_returns -> None
      | Ok (r, b) ->
        if Array.length r = 0
        then Some (Cmm.Ctuple [], env, b)
        else
          let id = V.create_local "bind" in
          Some (Cmm.Cvar id, { env with vars = V.Map.add id r env.vars }, b)

  and emit_parts_list env b exp_list =
    let module EC = SU.Effect_and_coeffect in
    let exp_list_right_to_left, _effect =
      List.fold_left
        (fun (exp_list, effects_after) exp ->
          let exp_effect = effects_of exp in
          (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
        ([], EC.none) exp_list
    in
    List.fold_left
      (fun acc (exp, effects_after) ->
        match acc with
        | None -> None
        | Some (result, env, b) -> (
          match emit_parts env b ~effects_after exp with
          | None -> None
          | Some (exp_result, env, b) -> Some (exp_result :: result, env, b)))
      (Some ([], env, b))
      exp_list_right_to_left

  and emit_tuple_not_flattened env b exp_list : _ or_never_returns =
    let rec emit_list b = function
      | [] -> Ok ([], b)
      | exp :: rem -> (
        let* loc_rem, b = emit_list b rem in
        match emit_expr env b exp ~bound_name:None with
        | Never_returns -> Never_returns
        | Ok (loc_exp, b) -> Ok (loc_exp :: loc_rem, b))
    in
    emit_list b exp_list

  and emit_tuple env b exp_list : result =
    let* l, b = emit_tuple_not_flattened env b exp_list in
    Ok (Array.concat l, b)

  and emit_stores env b dbg (args : Cmm.expression list) regs_addr =
    let byte_offset = ref (-Arch.size_int) in
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing !byte_offset)
    in
    let advance bytes =
      byte_offset := !byte_offset + bytes;
      addressing_mode
        := Arch.offset_addressing Arch.identity_addressing !byte_offset
    in
    let is_store (op : Operation.t) =
      match op with Store (_, _, _) -> true | _ -> false
    in
    let b = ref b in
    let for_one_arg arg =
      let original_arg = arg in
      let select_store_result =
        Target.select_store ~is_assign:false !addressing_mode arg
      in
      let arg : Cmm.expression =
        match select_store_result with
        | Maybe_out_of_range | Use_default -> arg
        | Rewritten (_, arg) -> arg
      in
      match emit_expr env !b arg ~bound_name:None with
      | Ok (regs, new_b) -> (
        b := new_b;
        let operation_replacing_store =
          match select_store_result with
          | Maybe_out_of_range -> None
          | Rewritten (op, _) -> if is_store op then None else Some op
          | Use_default -> None
        in
        match operation_replacing_store with
        | None ->
          Array.iter
            (fun (r : Ssa.instruction) ->
              let chunk = chunk_of_component (typ_of_instruction r) in
              ignore
                (B.emit_op !b
                   ~op:(Store (chunk, !addressing_mode, false))
                   ~dbg ~typ:Cmm.typ_void ~args:[| r; regs_addr |]);
              advance (SU.size_component (typ_of_instruction r)))
            regs
        | Some op ->
          ignore
            (B.emit_op !b ~op ~dbg ~typ:Cmm.typ_void
               ~args:(Array.append regs [| regs_addr |]));
          advance (size_of_cmm_expr env original_arg))
      | Never_returns ->
        Misc.fatal_error
          "emit_expr did not return any registers in [emit_stores]"
    in
    List.iter for_one_arg args;
    !b

  and emit_expr env b (exp : Cmm.expression) ~bound_name : result =
    match exp with
    | Cconst_int (n, _dbg) ->
      Ok
        ( insert_op b (SU.make_const_int (Nativeint.of_int n)) [||] Cmm.typ_int,
          b )
    | Cconst_natint (n, _dbg) ->
      Ok (insert_op b (SU.make_const_int n) [||] Cmm.typ_int, b)
    | Cconst_float32 (n, _dbg) ->
      Ok
        ( insert_op b
            (SU.make_const_float32 (Int32.bits_of_float n))
            [||] Cmm.typ_float32,
          b )
    | Cconst_float (n, _dbg) ->
      Ok
        ( insert_op b
            (SU.make_const_float (Int64.bits_of_float n))
            [||] Cmm.typ_float,
          b )
    | Cconst_vec128 (bits, _dbg) ->
      Ok (insert_op b (SU.make_const_vec128 bits) [||] Cmm.typ_vec128, b)
    | Cconst_vec256 (bits, _dbg) ->
      Ok (insert_op b (Operation.Const_vec256 bits) [||] Cmm.typ_vec256, b)
    | Cconst_vec512 (bits, _dbg) ->
      Ok (insert_op b (Operation.Const_vec512 bits) [||] Cmm.typ_vec512, b)
    | Cconst_symbol (n, _dbg) ->
      Ok (insert_op b (SU.make_const_symbol n) [||] Cmm.typ_int, b)
    | Cvar v -> Ok (env_find v env, b)
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 ~bound_name:(Some v) with
      | Never_returns -> Never_returns
      | Ok (r1, b) -> emit_expr (bind_let env b v r1) b e2 ~bound_name)
    | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr env b body ~bound_name
    | Ctuple [] -> Ok ([||], b)
    | Ctuple exp_list -> (
      match emit_parts_list env b exp_list with
      | None -> Never_returns
      | Some (simple_list, ext_env, b) -> emit_tuple ext_env b simple_list)
    | Cop (Craise k, args, dbg) -> emit_expr_raise env b k args dbg
    | Cop (Copaque, args, dbg) -> (
      match emit_parts_list env b args with
      | None -> Never_returns
      | Some (simple_args, env, b) ->
        let* rs, b = emit_tuple env b simple_args in
        let typ = Array.map typ_of_instruction rs in
        Ok (insert_op_debug b Opaque dbg rs typ, b))
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) -> (
      match emit_expr env b arg ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok (loc_exp, b) ->
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        Ok
          (Array.sub loc_exp size_before (Array.length fields_layout.(field)), b)
      )
    | Cop (op, args, dbg) -> emit_expr_op env b bound_name op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok (_, b) -> emit_expr env b e2 ~bound_name)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg) ->
      emit_expr_ifthenelse env b bound_name econd ifso_dbg eif ifnot_dbg eelse
        dbg
    | Cswitch (esel, index, ecases, dbg) ->
      emit_expr_switch env b bound_name esel index ecases dbg
    | Ccatch (_, [], e1) -> emit_expr env b e1 ~bound_name
    | Ccatch (flag, handlers, body) ->
      emit_expr_catch env b bound_name flag handlers body
    | Cexit (lbl, args, traps) -> emit_expr_exit env b lbl args traps
    | Cinvalid { message; symbol } -> emit_invalid env b message symbol

  and emit_tail env b (exp : Cmm.expression) =
    match exp with
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok (r1, b) -> emit_tail (bind_let env b v r1) b e2)
    | Cphantom_let (_var, _defining_expr, body) -> emit_tail env b body
    | Cop ((Capply { result_type = ty; region = Rc_normal; _ } as op), args, dbg)
      ->
      emit_tail_apply env b ty op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok (_, b) -> emit_tail env b e2)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg) ->
      emit_tail_ifthenelse env b econd ifso_dbg eif ifnot_dbg eelse dbg
    | Cswitch (esel, index, ecases, dbg) ->
      emit_tail_switch env b esel index ecases dbg
    | Ccatch (_, [], e1) -> emit_tail env b e1
    | Ccatch (flag, handlers, e1) -> emit_tail_catch env b flag handlers e1
    | Cinvalid { message; symbol } ->
      let ok = emit_invalid env b message symbol in
      insert_return env ok (pop_all_traps env)
    | Cop _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
    | Cvar _ | Ctuple _ | Cexit _ ->
      emit_return env b exp (pop_all_traps env)

  and insert_return env (r : result) (traps : Cmm.trap_action list) =
    match r with
    | Never_returns -> ()
    | Ok (r, b) ->
      emit_trap_actions env b traps;
      B.finish_block b ~dbg:Debuginfo.none (Return r)

  and emit_invalid env b message symbol =
    let arg_expr = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
    let* arg_instrs, b = emit_tuple env b [arg_expr] in
    if !SU.current_function_is_check_enabled
    then (
      let cont_b = B.create_call_continuation b Cmm.typ_int in
      let cont_block = B.current_block cont_b in
      B.finish_block b ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = Some cont_block });
      set_traps_for_raise env;
      Ok (B.block_params cont_b, cont_b))
    else (
      B.finish_block b ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = None });
      set_traps_for_raise env;
      Never_returns)

  and emit_expr_raise env b k args dbg =
    let* r, b = emit_tuple env b args in
    let handler_block = current_exn_continuation env in
    B.finish_block b ~dbg (Raise (k, r, handler_block));
    set_traps_for_raise env;
    Never_returns

  and emit_expr_op_cont env b ~ty (new_op : Cfg.basic_or_terminator) arg_instrs
      _label_after dbg : result =
    let ty =
      match new_op with
      | Terminator (Prim { op = External { ty_res; _ }; _ }) -> ty_res
      | _ -> ty
    in
    match new_op with
    | Terminator (Call { op = call_op; _ }) ->
      let cont_b = B.create_call_continuation b ty in
      let cont_block = B.current_block cont_b in
      let exn_cont = current_exn_continuation env in
      B.finish_block b ~dbg
        (Call
           { op = call_op;
             args = arg_instrs;
             continuation = cont_block;
             exn_continuation = exn_cont
           });
      set_traps_for_raise env;
      Ok (B.block_params cont_b, cont_b)
    | Terminator (Prim { op = External ({ ty_res; _ } as ext_call); _ }) ->
      let cont_b = B.create_call_continuation b ty_res in
      let cont_block = B.current_block cont_b in
      let exn_cont = current_exn_continuation env in
      B.finish_block b ~dbg
        (Prim
           { op = External ext_call;
             args = arg_instrs;
             continuation = cont_block;
             exn_continuation = exn_cont
           });
      set_traps_for_raise env;
      Ok (B.block_params cont_b, cont_b)
    | Terminator (Prim { op = Probe _ as probe_op; _ }) ->
      let cont_b = B.create_call_continuation b ty in
      let cont_block = B.current_block cont_b in
      B.finish_block b ~dbg
        (Prim
           { op = probe_op;
             args = arg_instrs;
             continuation = cont_block;
             exn_continuation = None
           });
      set_traps_for_raise env;
      Ok (B.block_params cont_b, cont_b)
    | Terminator (Call_no_return _) ->
      B.finish_block b ~dbg Never;
      set_traps_for_raise env;
      Never_returns
    | Basic _ ->
      Misc.fatal_errorf "Ssa_of_cmm.emit_expr_op_cont: unexpected basic"
    | Terminator term ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"")
        term

  and emit_expr_op env b _bound_name op args dbg : result =
    match emit_parts_list env b args with
    | None -> Never_returns
    | Some (simple_args, env, b) -> (
      let ty = SU.oper_result_type op in
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
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
        let rd = B.emit_op b ~op ~dbg ~typ:Cmm.typ_val ~args:[||] in
        set_traps_for_raise env;
        let b = emit_stores env b dbg new_args rd in
        Ok ([| rd |], b)
      | _ -> (
        let* arg_instrs, b = emit_tuple env b new_args in
        match new_op with
        | Terminator _ ->
          emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
        | Basic (Op op) -> Ok (insert_op_debug b op dbg arg_instrs ty, b)
        | Basic basic ->
          Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic
            basic))

  and emit_expr_ifthenelse env b _bound_name econd _ifso_dbg eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) : result =
    let cond, earg = select_condition econd in
    match emit_expr env b earg ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok (rarg, b) ->
      let then_b = B.create_branch_target b in
      let else_b = B.create_branch_target b in
      let then_block = B.current_block then_b in
      let else_block = B.current_block else_b in
      let term =
        emit_branch b cond rarg ~true_block:then_block ~false_block:else_block
      in
      B.finish_block b ~dbg:Debuginfo.none term;
      let r_then = emit_expr env then_b eif ~bound_name:None in
      let r_else = emit_expr env else_b eelse ~bound_name:None in
      join_branches r_then r_else

  and emit_expr_switch env b _bound_name esel index ecases (_dbg : Debuginfo.t)
      : result =
    match emit_expr env b esel ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok (rsel, b) ->
      let case_bs =
        Array.map (fun (_case_expr, _dbg) -> B.create_branch_target b) ecases
      in
      let targets =
        Array.map (fun idx -> B.current_block case_bs.(idx)) index
      in
      B.finish_block b ~dbg:Debuginfo.none (Switch (targets, rsel));
      let case_results =
        Array.mapi
          (fun i (case_expr, _dbg) ->
            emit_expr env case_bs.(i) case_expr ~bound_name:None)
          ecases
      in
      join_array case_results

  and emit_expr_catch env b _bound_name (flag : Cmm.ccatch_flag) handlers body :
      result =
    let handlers_info =
      List.map
        (fun Cmm.
               { label = nfail;
                 params = ids;
                 body = e2;
                 dbg = _dbg;
                 is_cold = _is_cold
               } ->
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let handler_b =
            match flag with
            | Cmm.Exn_handler -> B.create_trap_handler b types
            | Cmm.Normal | Cmm.Recursive -> B.create_merge b types
          in
          let handler_block = B.current_block handler_b in
          let traps_ref = ref SU.Unreachable in
          let handler_info =
            { handler_block; handler_b; handler_types = types; traps_ref }
          in
          nfail, ids, e2, handler_info)
        handlers
    in
    let env =
      List.fold_left
        (fun env (nfail, _ids, _body, info) ->
          { env with
            static_exceptions =
              Static_label.Map.add nfail info env.static_exceptions
          })
        env handlers_info
    in
    let body_b = B.create_merge b [||] in
    let body_block = B.current_block body_b in
    B.finish_block b ~dbg:Debuginfo.none
      (Goto { goto = body_block; args = [||] });
    let translate_one_handler (_, ids, handler_body, info) =
      let trap_stack, handler_body =
        match !(info.traps_ref) with
        | SU.Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Operation.Uncaught, unreachable_handler_body
        | SU.Reachable trap_stack -> trap_stack, handler_body
      in
      let handler_env = { env with trap_stack } in
      let handler_env =
        let param_idx = ref 0 in
        List.fold_left
          (fun env (id, ty) ->
            let n = Array.length ty in
            let proj_instrs =
              Array.init n (fun i ->
                  (Ssa.Block_param
                     { block = info.handler_block;
                       index = !param_idx + i;
                       typ = ty.(i)
                     }
                    : Ssa.instruction))
            in
            param_idx := !param_idx + n;
            env_add id proj_instrs env)
          handler_env ids
      in
      List.iter
        (fun (id, _ty) ->
          let provenance = VP.provenance id in
          if Option.is_some provenance
          then
            let regs = V.Map.find (VP.var id) handler_env.vars in
            B.emit_instruction info.handler_b
              (Name_for_debugger
                 { ident = VP.var id; provenance; which_parameter = None; regs }))
        ids;
      emit_expr handler_env info.handler_b handler_body ~bound_name:None
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        List.partition
          (fun (_, _, _, info) ->
            match !(info.traps_ref) with
            | SU.Unreachable -> true
            | SU.Reachable _ -> false)
          not_built
      in
      if List.compare_length_with to_build 0 = 0
      then already_built
      else
        let already_built =
          List.fold_left
            (fun acc handler -> translate_one_handler handler :: acc)
            already_built to_build
        in
        build_all_reachable_handlers ~already_built ~not_built
    in
    let r_body = emit_expr env body_b body ~bound_name:None in
    let handler_results =
      match flag with
      | Normal | Recursive ->
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_info
      | Exn_handler -> List.map translate_one_handler handlers_info
    in
    join_array (Array.of_list (r_body :: handler_results))

  and find_handler env handler_id =
    try Static_label.Map.find handler_id env.static_exceptions
    with Not_found ->
      Misc.fatal_errorf "Ssa_of_cmm: unbound trap handler %a"
        Static_label.format handler_id

  and emit_trap_actions env b traps =
    List.iter
      (fun (trap : Cmm.trap_action) ->
        let h =
          match trap with
          | Push handler_id -> find_handler env handler_id
          | Pop handler_id -> find_handler env handler_id
        in
        B.emit_instruction b
          (match trap with
          | Push _ -> Pushtrap { handler = h.handler_block }
          | Pop _ -> Poptrap { handler = h.handler_block }))
      traps

  and emit_expr_exit env b (lbl : Cmm.exit_label) args traps : result =
    match emit_parts_list env b args with
    | None -> Never_returns
    | Some (simple_list, ext_env, b) -> (
      match lbl with
      | Lbl nfail ->
        let* src, b = emit_tuple ext_env b simple_list in
        let handler = find_handler env nfail in
        emit_trap_actions env b traps;
        SU.set_traps nfail handler.traps_ref env.trap_stack traps;
        B.finish_block b ~dbg:Debuginfo.none
          (Goto { goto = handler.handler_block; args = src });
        Never_returns
      | Return_lbl ->
        let* src, b = emit_tuple ext_env b simple_list in
        emit_trap_actions env b traps;
        B.finish_block b ~dbg:Debuginfo.none (Return src);
        Never_returns)

  and emit_return env b exp traps =
    insert_return env (emit_expr env b exp ~bound_name:None) traps

  and emit_tail_apply env b _ty op args dbg =
    match emit_parts_list env b args with
    | None -> ()
    | Some (simple_args, env, b) -> (
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match emit_tuple env b new_args with
      | Never_returns -> ()
      | Ok (arg_instrs, b) -> (
        let can_tailcall call_op =
          if not (trap_stack_is_empty env)
          then false
          else
            let rarg =
              match call_op with
              | Cfg.Indirect _ ->
                Array.sub arg_instrs 1 (Array.length arg_instrs - 1)
              | Cfg.Direct _ -> arg_instrs
            in
            let arg_types = Array.map typ_of_instruction rarg in
            let _, stack_ofs_args = Proc.loc_arguments arg_types in
            let res_type = SU.oper_result_type op in
            let _, stack_ofs_res = Proc.loc_results_call res_type in
            Stdlib.Int.max stack_ofs_args stack_ofs_res = 0
        in
        match new_op with
        | Terminator (Call { op = Indirect callees; _ })
          when can_tailcall (Indirect callees) ->
          B.finish_block b ~dbg (Tailcall_func (Indirect callees, arg_instrs))
        | Terminator (Call { op = Direct func; _ })
          when String.equal func.sym_name !SU.current_function_name
               && can_tailcall (Direct func) ->
          B.finish_block b ~dbg
            (Tailcall_self
               { destination = env.tailrec_block; args = arg_instrs })
        | Terminator (Call { op = Direct func; _ })
          when can_tailcall (Direct func) ->
          B.finish_block b ~dbg (Tailcall_func (Direct func, arg_instrs))
        | _ ->
          let ty = SU.oper_result_type op in
          let r =
            emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
          in
          insert_return env r (pop_all_traps env)))

  and trap_stack_is_empty env =
    match env.trap_stack with
    | Operation.Uncaught -> true
    | Specific_trap _ -> false

  and emit_tail_ifthenelse env b econd (_ifso_dbg : Debuginfo.t) eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) =
    let cond, earg = select_condition econd in
    match emit_expr env b earg ~bound_name:None with
    | Never_returns -> ()
    | Ok (rarg, b) ->
      let then_b = B.create_branch_target b in
      let else_b = B.create_branch_target b in
      let then_block = B.current_block then_b in
      let else_block = B.current_block else_b in
      let term =
        emit_branch b cond rarg ~true_block:then_block ~false_block:else_block
      in
      B.finish_block b ~dbg:Debuginfo.none term;
      emit_tail env then_b eif;
      emit_tail env else_b eelse

  and emit_tail_switch env b esel index ecases (_dbg : Debuginfo.t) =
    match emit_expr env b esel ~bound_name:None with
    | Never_returns -> ()
    | Ok (rsel, b) ->
      let case_bs =
        Array.map (fun (_case_expr, _dbg) -> B.create_branch_target b) ecases
      in
      let targets =
        Array.map (fun idx -> B.current_block case_bs.(idx)) index
      in
      B.finish_block b ~dbg:Debuginfo.none (Switch (targets, rsel));
      Array.iteri
        (fun i (case_expr, _dbg) -> emit_tail env case_bs.(i) case_expr)
        ecases

  and emit_tail_catch env b (flag : Cmm.ccatch_flag) handlers e1 =
    let handlers_info =
      List.map
        (fun Cmm.
               { label = nfail;
                 params = ids;
                 body = e2;
                 dbg = _dbg;
                 is_cold = _is_cold
               } ->
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let handler_b =
            match flag with
            | Cmm.Exn_handler -> B.create_trap_handler b types
            | Cmm.Normal | Cmm.Recursive -> B.create_merge b types
          in
          let handler_block = B.current_block handler_b in
          let traps_ref = ref SU.Unreachable in
          let handler_info =
            { handler_block; handler_b; handler_types = types; traps_ref }
          in
          nfail, ids, e2, handler_info)
        handlers
    in
    let env =
      List.fold_left
        (fun env (nfail, _ids, _e2, info) ->
          { env with
            static_exceptions =
              Static_label.Map.add nfail info env.static_exceptions
          })
        env handlers_info
    in
    let body_b = B.create_merge b [||] in
    let body_block = B.current_block body_b in
    B.finish_block b ~dbg:Debuginfo.none
      (Goto { goto = body_block; args = [||] });
    let translate_one_handler (_, ids, e2, info) =
      let trap_stack, e2 =
        match !(info.traps_ref) with
        | SU.Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Operation.Uncaught, unreachable_handler_body
        | SU.Reachable trap_stack -> trap_stack, e2
      in
      let handler_env = { env with trap_stack } in
      let param_idx = ref 0 in
      let handler_env =
        List.fold_left
          (fun env (id, ty) ->
            let n = Array.length ty in
            let proj_instrs =
              Array.init n (fun i ->
                  (Ssa.Block_param
                     { block = info.handler_block;
                       index = !param_idx + i;
                       typ = ty.(i)
                     }
                    : Ssa.instruction))
            in
            param_idx := !param_idx + n;
            env_add id proj_instrs env)
          handler_env ids
      in
      List.iter
        (fun (id, _ty) ->
          let provenance = VP.provenance id in
          if Option.is_some provenance
          then
            let regs = V.Map.find (VP.var id) handler_env.vars in
            B.emit_instruction info.handler_b
              (Name_for_debugger
                 { ident = VP.var id; provenance; which_parameter = None; regs }))
        ids;
      emit_tail handler_env info.handler_b e2
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        List.partition
          (fun (_, _, _, info) ->
            match !(info.traps_ref) with
            | SU.Unreachable -> true
            | SU.Reachable _ -> false)
          not_built
      in
      if List.compare_length_with to_build 0 = 0
      then already_built
      else (
        List.iter translate_one_handler to_build;
        build_all_reachable_handlers ~already_built ~not_built)
    in
    emit_tail env body_b e1;
    match flag with
    | Normal | Recursive ->
      build_all_reachable_handlers ~already_built:() ~not_built:handlers_info
    | Exn_handler -> List.iter translate_one_handler handlers_info

  and join_branches (r1 : result) (r2 : result) : result =
    match r1, r2 with
    | Never_returns, Never_returns -> Never_returns
    | Ok (r, b), Never_returns | Never_returns, Ok (r, b) -> Ok (r, b)
    | Ok (r1_instrs, b1), Ok (r2_instrs, b2) ->
      assert (Array.length r1_instrs = Array.length r2_instrs);
      let join_types = Array.map typ_of_instruction r1_instrs in
      let join_b = B.create_merge b1 join_types in
      let join_block = B.current_block join_b in
      B.finish_block b1 ~dbg:Debuginfo.none
        (Goto { goto = join_block; args = r1_instrs });
      B.finish_block b2 ~dbg:Debuginfo.none
        (Goto { goto = join_block; args = r2_instrs });
      if Array.length join_types = 0
      then Ok ([||], join_b)
      else Ok (B.block_params join_b, join_b)

  and join_array (results : result array) : result =
    let join_info = ref None in
    Array.iter
      (fun r ->
        match r with
        | Never_returns -> ()
        | Ok (instrs, any_b) -> (
          let types = Array.map typ_of_instruction instrs in
          match !join_info with
          | None -> join_info := Some (types, any_b)
          | Some (prev, _) -> assert (Cmm.equal_machtype prev types)))
      results;
    match !join_info with
    | None -> Never_returns
    | Some (join_types, any_b) ->
      let join_b = B.create_merge any_b join_types in
      let join_block = B.current_block join_b in
      Array.iter
        (fun r ->
          match r with
          | Never_returns -> ()
          | Ok (instrs, b) ->
            B.finish_block b ~dbg:Debuginfo.none
              (Goto { goto = join_block; args = instrs }))
        results;
      if Array.length join_types = 0
      then Ok ([||], join_b)
      else Ok (B.block_params join_b, join_b)

  let emit_fundecl (f : Cmm.fundecl) : Ssa.t =
    SU.current_function_name := f.fun_name.sym_name;
    SU.current_function_is_check_enabled
      := Zero_alloc_checker.is_check_enabled f.fun_codegen_options
           f.fun_name.sym_name f.fun_dbg;
    let fun_arg_types = List.map snd f.fun_args |> Array.concat in
    let entry_b = B.make fun_arg_types in
    let entry_block = B.current_block entry_b in
    let env =
      { vars = V.Map.empty;
        static_exceptions = Static_label.Map.empty;
        trap_stack = Operation.Uncaught;
        tailrec_block = entry_block
      }
    in
    let env, _offset =
      List.fold_left
        (fun (env, offset) (id, ty) ->
          let n = Array.length ty in
          let projs =
            Array.init n (fun i ->
                (Ssa.Block_param
                   { block = entry_block; index = offset + i; typ = ty.(i) }
                  : Ssa.instruction))
          in
          env_add id projs env, offset + n)
        (env, 0) f.fun_args
    in
    emit_tail env entry_b f.fun_body;
    let blocks = B.finish entry_b in
    { Ssa.blocks;
      fun_name = f.fun_name.sym_name;
      fun_args = fun_arg_types;
      fun_args_names = f.fun_args;
      fun_codegen_options = f.fun_codegen_options;
      fun_dbg = f.fun_dbg;
      entry = entry_block;
      fun_poll = f.fun_poll;
      fun_ret_type = f.fun_ret_type
    }
end
