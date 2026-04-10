open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42-45"]

module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance

type 'a or_never_returns =
  | Ok of 'a
  | Never_returns

let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

type static_handler =
  { handler_label : Label.t;
    handler_types : Cmm.machtype;
    traps_ref : SU.trap_stack_info ref
  }

type env =
  { vars : Ssa.instruction array V.Map.t;
    static_exceptions : static_handler Static_label.Map.t;
    trap_stack : Operation.trap_stack;
    tailrec_label : Label.t
  }

let current_exn_continuation env : Label.t option =
  match env.trap_stack with
  | Operation.Uncaught -> None
  | Specific_trap (static_label, _) -> (
    match Static_label.Map.find_opt static_label env.static_exceptions with
    | Some handler -> Some handler.handler_label
    | None -> None)

type builder = Ssa.Builder.t

let emit_instruction = Ssa.Builder.emit_instruction

let emit_op = Ssa.Builder.emit_op

let finish_block = Ssa.Builder.finish_block

let start_block = Ssa.Builder.start_block

let fork = Ssa.Builder.fork

let merge_into parent sub = Ssa.Builder.merge_into parent ~from:sub

let block_params = Ssa.Builder.block_params

let current_label = Ssa.Builder.current_label

let transfer_open_block b from = Ssa.Builder.transfer_open_block b ~from

module Make (Target : Cfg_selectgen_target_intf.S) = struct
  (* Reuse pure functions from Cfg_selectgen with the same Target. *)
  module Sel = Cfg_selectgen.Make (Target)

  let is_immediate = Sel.is_immediate

  let is_immediate_test = Sel.is_immediate_test

  let select_condition = Sel.select_condition

  let select_arith_comm = Sel.select_arith_comm

  let select_arith = Sel.select_arith

  let select_arith_comp = Sel.select_arith_comp

  let select_operation = Sel.select_operation

  let effects_of = Sel.effects_of

  let is_simple_expr = Sel.is_simple_expr

  (* In cfg_selectgen, [bind_let], [insert_op_debug], [insert_op],
     [insert_move_extcall_arg] appear here. In the SSA pipeline these are
     replaced by the helpers below. *)

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
      emit_instruction b
        (Name_for_debugger
           { ident = VP.var v; provenance; which_parameter = None; regs = r1 });
    env

  let insert_op_debug b op dbg args typ : Ssa.instruction array =
    let i = emit_op b ~op ~dbg ~typ ~args in
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

  (* [may_defer] is inlined into [emit_parts] to match cfg_selectgen. *)

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

  (* Matching cfg_selectgen's [unreachable_handler]: emit a segfault
     (load from address 0) followed by a notrace raise, with trap stack
     set to Uncaught. The segfault ensures a crash if this code is
     somehow reached at runtime. *)
  let unreachable_handler_body : Cmm.expression =
    let dummy_constant : Cmm.expression = Cconst_int (1, Debuginfo.none) in
    let segfault : Cmm.expression =
      Cop
        ( Cload
            { memory_chunk = Word_int;
              mutability = Mutable;
              is_atomic = false
            },
          [Cconst_int (0, Debuginfo.none)],
          Debuginfo.none )
    in
    let dummy_raise : Cmm.expression =
      Cop (Craise Raise_notrace, [dummy_constant], Debuginfo.none)
    in
    Csequence (segfault, dummy_raise)

  (* select_arith_comm, select_arith, select_arith_comp, select_operation0,
     select_operation: all reused from Cfg_selectgen.Make(Target) via Sel
     above. *)

  (* --- SSA-specific: branch encoding --- *)
  let emit_branch b (test : Operation.test) rarg ~true_label ~false_label :
      Ssa.terminator =
    let make_cond op args =
      emit_op b ~op ~dbg:Debuginfo.none ~typ:Cmm.typ_int ~args
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
      Branch { conditions = [| c, true_label |]; else_goto = false_label }
    | Ifalsetest ->
      let c = wrap_truth_test rarg.(0) in
      Branch { conditions = [| c, false_label |]; else_goto = true_label }
    | Iinttest cmp ->
      let c = make_cond (Intop (Icomp cmp)) rarg in
      Branch { conditions = [| c, true_label |]; else_goto = false_label }
    | Iinttest_imm (cmp, n) ->
      let c = make_cond (Intop_imm (Icomp cmp, n)) [| rarg.(0) |] in
      Branch { conditions = [| c, true_label |]; else_goto = false_label }
    | Ifloattest (w, cmp) ->
      let c = make_cond (Floatop (w, Icompf cmp)) rarg in
      Branch { conditions = [| c, true_label |]; else_goto = false_label }
    | Ioddtest ->
      let c = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
      Branch { conditions = [| c, true_label |]; else_goto = false_label }
    | Ieventest ->
      let c = make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |] in
      Branch { conditions = [| c, false_label |]; else_goto = true_label }

  (* The following two functions, [emit_parts] and [emit_parts_list], force
     right-to-left evaluation order as required by the Flambda [Un_anf] pass
     (and to be consistent with the bytecode compiler). *)

  let rec emit_parts env b ~effects_after exp : _ or_never_returns =
    let module EC = SU.Effect_and_coeffect in
    let may_defer_evaluation =
      let ec = effects_of exp in
      match EC.effect_ ec with
      | Arbitrary | Raise ->
        (* Preserve the ordering of effectful expressions by evaluating them
           early (in the correct order) and assigning their results to
           temporaries. We can avoid this in just one case: if we know that
           every [exp'] in the original expression list (cf. [emit_parts_list])
           to be evaluated after [exp] cannot possibly affect the result of
           [exp] or depend on the result of [exp], then [exp] may be deferred.
           (Checking purity here is not enough: we need to check copurity too to
           avoid e.g. moving mutable reads earlier than the raising of an
           exception.) *)
        EC.pure_and_copure effects_after
      | None -> (
        match EC.coeffect ec with
        | None ->
          (* Pure expressions may be moved. *)
          true
        | Read_mutable -> (
          (* Read-mutable expressions may only be deferred if evaluation of
             every [exp'] (for [exp'] as in the comment above) has no effects
             "worse" (in the sense of the ordering in [t]) than raising an
             exception. *)
          match EC.effect_ effects_after with
          | None | Raise -> true
          | Arbitrary -> false)
        | Arbitrary -> (
          (* Arbitrary expressions may only be deferred if evaluation of every
             [exp'] (for [exp'] as in the comment above) has no effects. *)
          match EC.effect_ effects_after with
          | None -> true
          | Arbitrary | Raise -> false))
    in
    (* Even though some expressions may look like they can be deferred from the
       (co)effect analysis, it may be forbidden to move them. *)
    if may_defer_evaluation && is_simple_expr exp
    then Ok (exp, env)
    else
      match emit_expr env b exp ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok r ->
        if Array.length r = 0
        then Ok (Cmm.Ctuple [], env)
        else
          let id = V.create_local "bind" in
          Ok (Cmm.Cvar id, { env with vars = V.Map.add id r env.vars })

  and emit_parts_list env b exp_list : _ or_never_returns =
    let module EC = SU.Effect_and_coeffect in
    let exp_list_right_to_left, _effect =
      (* Annotate each expression with the (co)effects that happen after it when
         the original expression list is evaluated from right to left. The
         resulting expression list has the rightmost expression first. *)
      List.fold_left
        (fun (exp_list, effects_after) exp ->
          let exp_effect = effects_of exp in
          (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
        ([], EC.none) exp_list
    in
    List.fold_left
      (fun (results_and_env : _ or_never_returns) (exp, effects_after) :
           _ or_never_returns ->
        match results_and_env with
        | Never_returns -> Never_returns
        | Ok (result, env) -> (
          match emit_parts env b exp ~effects_after with
          | Never_returns -> Never_returns
          | Ok (exp_result, env) -> Ok (exp_result :: result, env)))
      (Ok ([], env))
      exp_list_right_to_left

  and emit_tuple_not_flattened env b exp_list =
    let rec emit_list = function
      | [] -> Ok []
      | exp :: rem -> (
        (* Again, force right-to-left evaluation *)
        let* loc_rem = emit_list rem in
        match emit_expr env b exp ~bound_name:None with
        | Never_returns -> Never_returns
        | Ok loc_exp -> Ok (loc_exp :: loc_rem))
    in
    emit_list exp_list

  and emit_tuple env b exp_list =
    let* l = emit_tuple_not_flattened env b exp_list in
    Ok (Array.concat l)

  and emit_stores env b dbg (args : Cmm.expression list) regs_addr =
    let byte_offset = ref (-Arch.size_int) in
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing !byte_offset)
    in
    (* In cfg_selectgen, [advance] handles out-of-range offsets by resetting the
       addressing mode. In the SSA pipeline, addressing modes are resolved
       during lowering (cfg_of_ssa), so we always use identity + offset. *)
    let advance bytes =
      byte_offset := !byte_offset + bytes;
      addressing_mode
        := Arch.offset_addressing Arch.identity_addressing !byte_offset
    in
    let is_store (op : Operation.t) =
      match op with Store (_, _, _) -> true | _ -> false
    in
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
      match emit_expr env b arg ~bound_name:None with
      | Ok regs -> (
        let operation_replacing_store =
          match select_store_result with
          | Maybe_out_of_range -> None
          | Rewritten (op, _) -> if is_store op then None else Some op
          | Use_default -> None (* see above *)
        in
        match operation_replacing_store with
        | None ->
          Array.iter
            (fun (r : Ssa.instruction) ->
              let chunk = chunk_of_component (typ_of_instruction r) in
              ignore
                (emit_op b
                   ~op:(Store (chunk, !addressing_mode, false))
                   ~dbg ~typ:Cmm.typ_void ~args:[| r; regs_addr |]);
              advance (SU.size_component (typ_of_instruction r)))
            regs
        | Some op ->
          ignore
            (emit_op b ~op ~dbg ~typ:Cmm.typ_void
               ~args:(Array.append regs [| regs_addr |]));
          advance (size_of_cmm_expr env original_arg))
      | Never_returns ->
        Misc.fatal_error
          "emit_expr did not return any registers in [emit_stores]"
    in
    List.iter for_one_arg args

  (* Emit an expression.

     [bound_name] is the name that will be bound to the result of evaluating the
     expression, if such exists. This is used for emitting debugging info.

     Returns:

     - [Never_returns] if the expression does not finish normally (e.g. raises)

     - [Ok rs] if the expression yields a result in registers [rs] *)
  and emit_expr env b (exp : Cmm.expression) ~bound_name :
      Ssa.instruction array or_never_returns =
    match exp with
    | Cconst_int (n, _dbg) ->
      Ok (insert_op b (SU.make_const_int (Nativeint.of_int n)) [||] Cmm.typ_int)
    | Cconst_natint (n, _dbg) ->
      Ok (insert_op b (SU.make_const_int n) [||] Cmm.typ_int)
    | Cconst_float32 (n, _dbg) ->
      Ok
        (insert_op b
           (SU.make_const_float32 (Int32.bits_of_float n))
           [||] Cmm.typ_float32)
    | Cconst_float (n, _dbg) ->
      Ok
        (insert_op b
           (SU.make_const_float (Int64.bits_of_float n))
           [||] Cmm.typ_float)
    | Cconst_vec128 (bits, _dbg) ->
      Ok (insert_op b (SU.make_const_vec128 bits) [||] Cmm.typ_vec128)
    | Cconst_vec256 (bits, _dbg) ->
      Ok (insert_op b (Operation.Const_vec256 bits) [||] Cmm.typ_vec256)
    | Cconst_vec512 (bits, _dbg) ->
      Ok (insert_op b (Operation.Const_vec512 bits) [||] Cmm.typ_vec512)
    | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of which
         may point to heap values. However, any such blocks will be registered
         in the compilation unit's global roots structure, so adding this
         register to the frame table would be redundant *)
      Ok (insert_op b (SU.make_const_symbol n) [||] Cmm.typ_int)
    | Cvar v -> Ok (env_find v env)
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 ~bound_name:(Some v) with
      | Never_returns -> Never_returns
      | Ok r1 -> emit_expr (bind_let env b v r1) b e2 ~bound_name)
    | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr env b body ~bound_name
    | Ctuple [] -> Ok [||]
    | Ctuple exp_list -> (
      match emit_parts_list env b exp_list with
      | Never_returns -> Never_returns
      | Ok (simple_list, ext_env) -> emit_tuple ext_env b simple_list)
    | Cop (Craise k, args, dbg) -> emit_expr_raise env b k args dbg
    | Cop (Copaque, args, dbg) -> (
      match emit_parts_list env b args with
      | Never_returns -> Never_returns
      | Ok (simple_args, env) ->
        let* rs = emit_tuple env b simple_args in
        let typ = Array.map typ_of_instruction rs in
        Ok (insert_op_debug b Opaque dbg rs typ))
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) -> (
      match emit_expr env b arg ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok loc_exp ->
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        let field_slice =
          Array.sub loc_exp size_before (Array.length fields_layout.(field))
        in
        Ok field_slice)
    | Cop (op, args, dbg) -> emit_expr_op env b bound_name op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok _ -> emit_expr env b e2 ~bound_name)
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

  (* Emit an expression in tail position of a function. *)
  and emit_tail env b (exp : Cmm.expression) =
    match exp with
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok r1 -> emit_tail (bind_let env b v r1) b e2)
    | Cphantom_let (_var, _defining_expr, body) -> emit_tail env b body
    | Cop ((Capply { result_type = ty; region = Rc_normal; _ } as op), args, dbg)
      ->
      emit_tail_apply env b ty op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env b e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok _ -> emit_tail env b e2)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg) ->
      emit_tail_ifthenelse env b econd ifso_dbg eif ifnot_dbg eelse dbg
    | Cswitch (esel, index, ecases, dbg) ->
      emit_tail_switch env b esel index ecases dbg
    | Ccatch (_, [], e1) -> emit_tail env b e1
    | Ccatch (flag, handlers, e1) -> emit_tail_catch env b flag handlers e1
    | Cinvalid { message; symbol } ->
      let ok = emit_invalid env b message symbol in
      insert_return env b ok (pop_all_traps env)
    | Cop _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
    | Cvar _ | Ctuple _ | Cexit _ ->
      emit_return env b exp (pop_all_traps env)

  and insert_return env b (r : _ or_never_returns)
      (traps : Cmm.trap_action list) =
    match r with
    | Never_returns -> ()
    | Ok r ->
      emit_trap_actions env b traps;
      finish_block b ~dbg:Debuginfo.none (Return r)

  and emit_invalid env b message symbol =
    let arg_expr = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
    let* arg_instrs = emit_tuple env b [arg_expr] in
    if !SU.current_function_is_check_enabled
    then (
      (* For zero alloc checking we need to treat [Invalid] as returning. *)
      let label_after = Cmm.new_label () in
      let pred_label = current_label b in
      finish_block b ~dbg:Debuginfo.none
        (Invalid
           { message;
             args = arg_instrs;
             continuation = Some label_after
           });
      start_block b label_after
        (CallContinuation { predecessor = pred_label })
        Cmm.typ_int;
      set_traps_for_raise env;
      Ok (block_params label_after Cmm.typ_int))
    else (
      (* When not zero alloc checking we treat [Invalid] as non-returning. *)
      finish_block b ~dbg:Debuginfo.none
        (Invalid { message; args = arg_instrs; continuation = None });
      set_traps_for_raise env;
      Never_returns)

  and emit_expr_raise env b k args dbg =
    let* r = emit_tuple env b args in
    let handler_label = current_exn_continuation env in
    finish_block b ~dbg (Raise (k, r, handler_label));
    set_traps_for_raise env;
    Never_returns

  (* SSA-specific: terminator continuation handling *)
  and emit_expr_op_cont env b ~ty (new_op : Cfg.basic_or_terminator) arg_instrs
      _label_after dbg : Ssa.instruction array or_never_returns =
    let ty =
      match new_op with
      | Terminator (Prim { op = External { ty_res; _ }; _ }) -> ty_res
      | _ -> ty
    in
    match new_op with
    | Terminator (Call { op = call_op; label_after }) ->
      let pred_label = current_label b in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      finish_block b ~dbg
        (Call
           { op = call_op;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = exn_cont
           });
      set_traps_for_raise env;
      start_block b cont_label
        (CallContinuation { predecessor = pred_label })
        ty;
      Ok (block_params cont_label ty)
    | Terminator
        (Prim { op = External ({ ty_res; _ } as ext_call); label_after }) ->
      let pred_label = current_label b in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      finish_block b ~dbg
        (Prim
           { op = External ext_call;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = exn_cont
           });
      set_traps_for_raise env;
      start_block b cont_label
        (CallContinuation { predecessor = pred_label })
        ty_res;
      Ok (block_params cont_label ty_res)
    | Terminator (Prim { op = Probe _ as probe_op; label_after }) ->
      let pred_label = current_label b in
      let cont_label = label_after in
      finish_block b ~dbg
        (Prim
           { op = probe_op;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = None
           });
      set_traps_for_raise env;
      start_block b cont_label
        (CallContinuation { predecessor = pred_label })
        ty;
      Ok (block_params cont_label ty)
    | Terminator (Call_no_return _) ->
      finish_block b ~dbg Never;
      set_traps_for_raise env;
      Never_returns
    | Basic _ ->
      Misc.fatal_errorf "Ssa_of_cmm.emit_expr_op_cont: unexpected basic"
    | Terminator term ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"")
        term

  and emit_expr_op env b _bound_name op args dbg :
      Ssa.instruction array or_never_returns =
    match emit_parts_list env b args with
    | Never_returns -> Never_returns
    | Ok (simple_args, env) -> (
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
        let rd = emit_op b ~op ~dbg ~typ:Cmm.typ_val ~args:[||] in
        set_traps_for_raise env;
        emit_stores env b dbg new_args rd;
        Ok [| rd |]
      | _ -> (
        let* arg_instrs = emit_tuple env b new_args in
        match new_op with
        | Terminator _ ->
          emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
        | Basic (Op op) -> Ok (insert_op_debug b op dbg arg_instrs ty)
        | Basic basic ->
          Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic
            basic))

  and emit_expr_ifthenelse env b _bound_name econd _ifso_dbg eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) :
      Ssa.instruction array or_never_returns =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    let cond, earg = select_condition econd in
    match emit_expr env b earg ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok rarg ->
      let then_label = Cmm.new_label () in
      let else_label = Cmm.new_label () in
      let pred_label = current_label b in
      let term =
        emit_branch b cond rarg ~true_label:then_label ~false_label:else_label
      in
      finish_block b ~dbg:Debuginfo.none term;
      let b_then =
        fork then_label (BranchTarget { predecessor = pred_label }) [||]
      in
      let r_then = emit_expr env b_then eif ~bound_name:None in
      let b_else =
        fork else_label (BranchTarget { predecessor = pred_label }) [||]
      in
      let r_else = emit_expr env b_else eelse ~bound_name:None in
      join_branches b r_then b_then r_else b_else

  and emit_expr_switch env b _bound_name esel index ecases (_dbg : Debuginfo.t)
      : Ssa.instruction array or_never_returns =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    match emit_expr env b esel ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok rsel ->
      let pred_label = current_label b in
      let case_results =
        Array.map
          (fun (case_expr, _dbg) ->
            let case_label = Cmm.new_label () in
            let b_case =
              fork case_label (BranchTarget { predecessor = pred_label }) [||]
            in
            let r = emit_expr env b_case case_expr ~bound_name:None in
            case_label, r, b_case)
          ecases
      in
      let labels =
        Array.map
          (fun idx ->
            let label, _, _ = case_results.(idx) in
            label)
          index
      in
      finish_block b ~dbg:Debuginfo.none (Switch (labels, rsel));
      join_array b case_results

  and emit_expr_catch env b _bound_name (flag : Cmm.ccatch_flag) handlers body :
      Ssa.instruction array or_never_returns =
    let handlers_info =
      List.map
        (fun Cmm.
               { label = nfail;
                 params = ids;
                 body = e2;
                 dbg = _dbg;
                 is_cold = _is_cold
               } ->
          let handler_label = Cmm.new_label () in
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let traps_ref = ref SU.Unreachable in
          let handler_info =
            { handler_label; handler_types = types; traps_ref }
          in
          nfail, ids, e2, handler_info)
        handlers
    in
    let env =
      (* Since the handlers may be recursive, and called from the body, the same
         environment is used for translating both the handlers and the body. *)
      List.fold_left
        (fun env (nfail, _ids, _body, info) ->
          { env with
            static_exceptions =
              Static_label.Map.add nfail info env.static_exceptions
          })
        env handlers_info
    in
    let body_label = Cmm.new_label () in
    let pred_label = current_label b in
    let b_body = fork body_label (Merge { predecessors = [pred_label] }) [||] in
    let r_body = emit_expr env b_body body ~bound_name:None in
    finish_block b ~dbg:Debuginfo.none (Goto { goto = body_label; args = [||] });
    let translate_one_handler (_, ids, handler_body, info) =
      let block_desc : Ssa.block_desc =
        match flag with
        | Cmm.Exn_handler -> TrapHandler { predecessors = [] }
        | Cmm.Normal | Cmm.Recursive -> Merge { predecessors = [] }
      in
      let b_handler =
        fork info.handler_label block_desc info.handler_types
      in
      let trap_stack, handler_body =
        match !(info.traps_ref) with
        | SU.Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Operation.Uncaught, unreachable_handler_body
        | SU.Reachable trap_stack -> trap_stack, handler_body
      in
      let r =
        let handler_env = { env with trap_stack } in
          let handler_env =
            let param_idx = ref 0 in
            List.fold_left
              (fun env (id, ty) ->
                let n = Array.length ty in
                let proj_instrs =
                  Array.init n (fun i ->
                      (Ssa.Block_param
                         { block = info.handler_label;
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
                emit_instruction b_handler
                  (Name_for_debugger
                     { ident = VP.var id;
                       provenance;
                       which_parameter = None;
                       regs
                     }))
            ids;
          emit_expr handler_env b_handler handler_body ~bound_name:None
      in
      info.handler_label, r, b_handler
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
    let handler_results =
      match flag with
      | Normal | Recursive ->
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_info
        (* Note: we're dropping unreachable handlers here *)
      | Exn_handler ->
        (* We cannot drop exception handlers as some trap instructions may
           refer to them even if they're unreachable. Instead,
           [translate_one_handler] will generate a dummy handler for the
           unreachable cases. *)
        List.map translate_one_handler handlers_info
    in
    let all_results =
      Array.of_list ((body_label, r_body, b_body) :: handler_results)
    in
    join_array b all_results

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
        emit_instruction b
          (match trap with
          | Push _ -> Pushtrap { lbl_handler = h.handler_label }
          | Pop _ -> Poptrap { lbl_handler = h.handler_label }))
      traps

  and emit_expr_exit env b (lbl : Cmm.exit_label) args traps :
      Ssa.instruction array or_never_returns =
    match emit_parts_list env b args with
    | Never_returns -> Never_returns
    | Ok (simple_list, ext_env) -> (
      match lbl with
      | Lbl nfail ->
        let* src = emit_tuple ext_env b simple_list in
        let handler = find_handler env nfail in
        emit_trap_actions env b traps;
        SU.set_traps nfail handler.traps_ref env.trap_stack traps;
        finish_block b ~dbg:Debuginfo.none
          (Goto { goto = handler.handler_label; args = src });
        Never_returns
      | Return_lbl ->
        let* src = emit_tuple ext_env b simple_list in
        emit_trap_actions env b traps;
        finish_block b ~dbg:Debuginfo.none (Return src);
        Never_returns)

  (* Same, but in tail position *)

  and emit_return env b exp traps =
    insert_return env b (emit_expr env b exp ~bound_name:None) traps

  and emit_tail_apply env b _ty op args dbg =
    match emit_parts_list env b args with
    | Never_returns -> ()
    | Ok (simple_args, env) -> (
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match emit_tuple env b new_args with
      | Never_returns -> ()
      | Ok arg_instrs -> (
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
          finish_block b ~dbg (Tailcall_func (Indirect callees, arg_instrs))
        | Terminator (Call { op = Direct func; _ })
          when String.equal func.sym_name !SU.current_function_name
               && can_tailcall (Direct func) ->
          finish_block b ~dbg
            (Tailcall_self
               { destination = env.tailrec_label; args = arg_instrs })
        | Terminator (Call { op = Direct func; _ })
          when can_tailcall (Direct func) ->
          finish_block b ~dbg (Tailcall_func (Direct func, arg_instrs))
        | _ ->
          let ty = SU.oper_result_type op in
          let r =
            emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
          in
          insert_return env b r (pop_all_traps env)))

  and trap_stack_is_empty env =
    match env.trap_stack with
    | Operation.Uncaught -> true
    | Specific_trap _ -> false

  and emit_tail_ifthenelse env b econd (_ifso_dbg : Debuginfo.t) eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) =
    let cond, earg = select_condition econd in
    match emit_expr env b earg ~bound_name:None with
    | Never_returns -> ()
    | Ok rarg ->
      let then_label = Cmm.new_label () in
      let else_label = Cmm.new_label () in
      let pred_label = current_label b in
      let term =
        emit_branch b cond rarg ~true_label:then_label ~false_label:else_label
      in
      finish_block b ~dbg:Debuginfo.none term;
      let b_then =
        fork then_label (BranchTarget { predecessor = pred_label }) [||]
      in
      emit_tail env b_then eif;
      let b_else =
        fork else_label (BranchTarget { predecessor = pred_label }) [||]
      in
      emit_tail env b_else eelse;
      merge_into b b_then;
      merge_into b b_else

  and emit_tail_switch env b esel index ecases (_dbg : Debuginfo.t) =
    match emit_expr env b esel ~bound_name:None with
    | Never_returns -> ()
    | Ok rsel ->
      let pred_label = current_label b in
      let case_builders =
        Array.map
          (fun (case_expr, _dbg) ->
            let case_label = Cmm.new_label () in
            let b_case =
              fork case_label (BranchTarget { predecessor = pred_label }) [||]
            in
            emit_tail env b_case case_expr;
            case_label, b_case)
          ecases
      in
      let labels =
        Array.map
          (fun idx ->
            let label, _ = case_builders.(idx) in
            label)
          index
      in
      finish_block b ~dbg:Debuginfo.none (Switch (labels, rsel));
      Array.iter (fun (_, b_case) -> merge_into b b_case) case_builders

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
          let handler_label = Cmm.new_label () in
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let traps_ref = ref SU.Unreachable in
          let handler_info =
            { handler_label; handler_types = types; traps_ref }
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
    let body_label = Cmm.new_label () in
    let pred_label = current_label b in
    let b_body = fork body_label (Merge { predecessors = [pred_label] }) [||] in
    emit_tail env b_body e1;
    finish_block b ~dbg:Debuginfo.none (Goto { goto = body_label; args = [||] });
    let translate_one_handler (_, ids, e2, info) =
      let block_desc : Ssa.block_desc =
        match flag with
        | Cmm.Exn_handler -> TrapHandler { predecessors = [] }
        | Cmm.Normal | Cmm.Recursive -> Merge { predecessors = [] }
      in
      let b_handler =
        fork info.handler_label block_desc info.handler_types
      in
      let trap_stack, e2 =
        match !(info.traps_ref) with
        | SU.Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Operation.Uncaught, unreachable_handler_body
        | SU.Reachable trap_stack -> trap_stack, e2
      in
      (let handler_env = { env with trap_stack } in
        let param_idx = ref 0 in
        let handler_env =
          List.fold_left
            (fun env (id, ty) ->
              let n = Array.length ty in
              let proj_instrs =
                Array.init n (fun i ->
                    (Ssa.Block_param
                       { block = info.handler_label;
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
              emit_instruction b_handler
                (Name_for_debugger
                   { ident = VP.var id;
                     provenance;
                     which_parameter = None;
                     regs
                   }))
          ids;
        emit_tail handler_env b_handler e2);
      b_handler
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
    let handler_builders =
      match flag with
      | Normal | Recursive ->
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_info
      | Exn_handler ->
        List.map translate_one_handler handlers_info
    in
    merge_into b b_body;
    List.iter (merge_into b) handler_builders

  (* SSA-specific: join points with block parameters *)
  and join_branches b r1 b1 r2 b2 : Ssa.instruction array or_never_returns =
    match r1, r2 with
    | Never_returns, Never_returns ->
      merge_into b b1;
      merge_into b b2;
      Never_returns
    | Ok r, Never_returns ->
      merge_into b b2;
      merge_into b b1;
      transfer_open_block b b1;
      Ok r
    | Never_returns, Ok r ->
      merge_into b b1;
      merge_into b b2;
      transfer_open_block b b2;
      Ok r
    | Ok r1_instrs, Ok r2_instrs ->
      let join_label = Cmm.new_label () in
      assert (Array.length r1_instrs = Array.length r2_instrs);
      let join_types = Array.map typ_of_instruction r1_instrs in
      finish_block b1 ~dbg:Debuginfo.none
        (Goto { goto = join_label; args = r1_instrs });
      finish_block b2 ~dbg:Debuginfo.none
        (Goto { goto = join_label; args = r2_instrs });
      let preds = [current_label b1; current_label b2] in
      merge_into b b1;
      merge_into b b2;
      start_block b join_label (Merge { predecessors = preds }) join_types;
      if Array.length join_types = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  and join_array b
      (results :
        (Label.t * Ssa.instruction array or_never_returns * builder) array) :
      Ssa.instruction array or_never_returns =
    let join_types = ref None in
    Array.iter
      (fun (_, r, _) ->
        match r with
        | Never_returns -> ()
        | Ok instrs -> (
          let types = Array.map typ_of_instruction instrs in
          match !join_types with
          | None -> join_types := Some types
          | Some prev -> assert (Array.length prev = Array.length types)))
      results;
    match !join_types with
    | None ->
      Array.iter (fun (_, _, sub) -> merge_into b sub) results;
      Never_returns
    | Some join_types ->
      let join_label = Cmm.new_label () in
      let preds = ref [] in
      Array.iter
        (fun (_, r, sub) ->
          (match r with
          | Never_returns -> ()
          | Ok instrs ->
            preds := current_label sub :: !preds;
            finish_block sub ~dbg:Debuginfo.none
              (Goto { goto = join_label; args = instrs }));
          merge_into b sub)
        results;
      start_block b join_label
        (Merge { predecessors = List.rev !preds })
        join_types;
      if Array.length join_types = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  (* --- Entry point --- *)

  let emit_fundecl (f : Cmm.fundecl) : Ssa.t =
    SU.current_function_name := f.fun_name.sym_name;
    SU.current_function_is_check_enabled
      := Zero_alloc_checker.is_check_enabled f.fun_codegen_options
           f.fun_name.sym_name f.fun_dbg;
    let entry_label = Cmm.new_label () in
    let fun_arg_types = List.map snd f.fun_args |> Array.concat in
    let env =
      { vars = V.Map.empty;
        static_exceptions = Static_label.Map.empty;
        trap_stack = Operation.Uncaught;
        tailrec_label = entry_label
      }
    in
    let env, _offset =
      List.fold_left
        (fun (env, offset) (id, ty) ->
          let n = Array.length ty in
          let projs =
            Array.init n (fun i ->
                (Ssa.Block_param
                   { block = entry_label; index = offset + i; typ = ty.(i) }
                  : Ssa.instruction))
          in
          env_add id projs env, offset + n)
        (env, 0) f.fun_args
    in
    let b = fork entry_label FunctionStart fun_arg_types in
    emit_tail env b f.fun_body;
    let blocks, block_order = Ssa.Builder.finish b in
    { Ssa.blocks;
      block_order;
      fun_name = f.fun_name.sym_name;
      fun_args = fun_arg_types;
      fun_args_names = f.fun_args;
      fun_codegen_options = f.fun_codegen_options;
      fun_dbg = f.fun_dbg;
      entry_label;
      fun_poll = f.fun_poll;
      fun_ret_type = f.fun_ret_type
    }
end
