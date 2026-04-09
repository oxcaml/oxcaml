[@@@ocaml.warning "+a-4-9-40-41-42"]

open! Int_replace_polymorphic_compare
module V = Backend_var
module VP = Backend_var.With_provenance

type or_never_returns =
  | Ok of Ssa.instruction array
  | Never_returns

let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

type static_handler =
  { handler_label : Label.t;
    handler_types : Cmm.machtype;
    traps_ref : Select_utils.trap_stack_info ref
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

type builder =
  { mutable blocks : Ssa.basic_block list;
    mutable current_label : Label.t;
    mutable current_desc : Ssa.block_desc;
    mutable current_params : Cmm.machtype;
    mutable body : Ssa.instruction list
  }

let add_op b ~typ ~dbg op args : Ssa.instruction =
  let i : Ssa.instruction =
    Op { id = Ssa.InstructionId.create (); op; typ; args; dbg }
  in
  b.body <- i :: b.body;
  i

let add_body b (i : Ssa.instruction) = b.body <- i :: b.body

let emit_block b ~dbg term =
  let block =
    { Ssa.label = b.current_label;
      desc = b.current_desc;
      params = b.current_params;
      body = Array.of_list (List.rev b.body);
      terminator = term;
      terminator_dbg = dbg
    }
  in
  b.blocks <- block :: b.blocks;
  b.body <- []

let start_block b label desc params =
  b.current_label <- label;
  b.current_desc <- desc;
  b.current_params <- params;
  b.body <- []

let fork label desc params =
  { blocks = [];
    current_label = label;
    current_desc = desc;
    current_params = params;
    body = []
  }

let merge_into parent sub = parent.blocks <- sub.blocks @ parent.blocks

let block_params label (types : Cmm.machtype) =
  Array.mapi
    (fun i typ ->
      (Ssa.Block_param { block = label; index = i; typ } : Ssa.instruction))
    types

module Make (Target : Cfg_selectgen_target_intf.S) = struct
  let is_immediate (op : Operation.integer_operation) n =
    match Target.is_immediate op n with
    | Is_immediate result -> result
    | Use_default -> (
      match op with
      | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
      | _ -> false)

  let is_immediate_test cmp n =
    match Target.is_immediate_test cmp n with
    | Is_immediate result -> result
    | Use_default -> is_immediate (Icomp cmp) n

  let select_condition (arg : Cmm.expression) : Operation.test * Cmm.expression
      =
    match arg with
    | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _) when is_immediate_test cmp n
      ->
      Iinttest_imm (cmp, n), arg1
    | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Cmm.swap_integer_comparison cmp) n ->
      Iinttest_imm (Cmm.swap_integer_comparison cmp, n), arg2
    | Cop (Ccmpi cmp, args, _) -> Iinttest cmp, Ctuple args
    | Cop (Ccmpf (width, cmp), args, _) -> Ifloattest (width, cmp), Ctuple args
    | Cop (Cand, [arg1; Cconst_int (1, _)], _) -> Ioddtest, arg1
    | _ -> Itruetest, arg

  let select_arith_comm (op : Operation.integer_operation) args :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cmm.Cconst_int (n, _)] when is_immediate op n ->
      Select_utils.basic_op (Intop_imm (op, n)), [arg]
    | [Cmm.Cconst_int (n, _); arg] when is_immediate op n ->
      Select_utils.basic_op (Intop_imm (op, n)), [arg]
    | _ -> Select_utils.basic_op (Intop op), args

  let select_arith (op : Operation.integer_operation) args :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cmm.Cconst_int (n, _)] when is_immediate op n ->
      Select_utils.basic_op (Intop_imm (op, n)), [arg]
    | _ -> Select_utils.basic_op (Intop op), args

  let select_arith_comp (cmp : Operation.integer_comparison) args :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cmm.Cconst_int (n, _)] when is_immediate (Operation.Icomp cmp) n ->
      Select_utils.basic_op (Intop_imm (Icomp cmp, n)), [arg]
    | [Cmm.Cconst_int (n, _); arg]
      when is_immediate (Operation.Icomp (Scalar.Integer_comparison.swap cmp)) n
      ->
      ( Select_utils.basic_op
          (Intop_imm (Icomp (Scalar.Integer_comparison.swap cmp), n)),
        [arg] )
    | _ -> Select_utils.basic_op (Intop (Icomp cmp)), args

  let select_operation0 (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    let wrong_num_args n =
      Misc.fatal_errorf
        "Ssa_of_cmm.select_operation: expected %d argument(s) for@ %s" n
        (Printcmm.operation dbg op)
    in
    let[@inline] single_arg () =
      match args with [arg] -> arg | [] | _ :: _ -> wrong_num_args 1
    in
    let[@inline] two_args () =
      match args with
      | [arg1; arg2] -> arg1, arg2
      | [] | _ :: _ -> wrong_num_args 2
    in
    let[@inline] three_args () =
      match args with
      | [arg1; arg2; arg3] -> arg1, arg2, arg3
      | [] | _ :: _ -> wrong_num_args 3
    in
    match[@ocaml.warning "+fragile-match"] op with
    | Capply { callees; _ } -> (
      match[@ocaml.warning "-fragile-match"] args with
      | Cconst_symbol (func, _dbg) :: rem ->
        Terminator (Call { op = Direct func; label_after }), rem
      | _ -> Terminator (Call { op = Indirect callees; label_after }), args)
    | Cextcall { func; alloc; ty; ty_args; returns; builtin = _; effects } ->
      let external_call =
        { Cfg.func_symbol = func;
          alloc;
          effects;
          ty_res = ty;
          ty_args;
          stack_ofs = -1;
          stack_align = Align_16
        }
      in
      if returns
      then Terminator (Prim { op = External external_call; label_after }), args
      else Terminator (Call_no_return external_call), args
    | Cload { memory_chunk; mutability; is_atomic } ->
      let arg = single_arg () in
      let addressing_mode, eloc = Target.select_addressing memory_chunk arg in
      let mutability = Select_utils.select_mutable_flag mutability in
      ( Select_utils.basic_op
          (Load { memory_chunk; addressing_mode; mutability; is_atomic }),
        [eloc] )
    | Cstore (chunk, init) -> (
      let arg1, arg2 = two_args () in
      let addr, eloc = Target.select_addressing chunk arg1 in
      let is_assign =
        match init with Initialization -> false | Assignment -> true
      in
      match[@ocaml.warning "-fragile-match"] chunk with
      | Word_int | Word_val ->
        let op, newarg2 =
          match Target.is_store_out_of_range chunk ~byte_offset:0 with
          | Within_range -> (
            match Target.select_store ~is_assign addr arg2 with
            | Rewritten (op, arg) -> op, arg
            | Use_default | Maybe_out_of_range ->
              Operation.Store (chunk, addr, is_assign), arg2)
          | Out_of_range ->
            Misc.fatal_errorf "Ssa_of_cmm: store out of range:@ %s"
              (Printcmm.operation dbg op)
        in
        Select_utils.basic_op op, [newarg2; eloc]
      | _ -> Select_utils.basic_op (Store (chunk, addr, is_assign)), [arg2; eloc]
      )
    | Cdls_get -> Select_utils.basic_op Dls_get, args
    | Ctls_get -> Select_utils.basic_op Tls_get, args
    | Cdomain_index -> Select_utils.basic_op Domain_index, args
    | Calloc (mode, alloc_block_kind) ->
      let placeholder : Cmm.alloc_dbginfo_item =
        { alloc_words = 0; alloc_block_kind; alloc_dbg = Debuginfo.none }
      in
      ( Select_utils.basic_op
          (Alloc { bytes = 0; dbginfo = [placeholder]; mode }),
        args )
    | Cpoll -> Select_utils.basic_op Poll, args
    | Cpause -> Select_utils.basic_op Pause, args
    | Caddi -> select_arith_comm Iadd args
    | Csubi -> select_arith Isub args
    | Cmuli -> select_arith_comm Imul args
    | Cmulhi { signed } -> select_arith_comm (Imulh { signed }) args
    | Cdivi -> Select_utils.basic_op (Intop Idiv), args
    | Cmodi -> Select_utils.basic_op (Intop Imod), args
    | Caddi128 -> Select_utils.basic_op (Int128op Iadd128), args
    | Csubi128 -> Select_utils.basic_op (Int128op Isub128), args
    | Cmuli64 { signed } ->
      Select_utils.basic_op (Int128op (Imul64 { signed })), args
    | Cand -> select_arith_comm Iand args
    | Cor -> select_arith_comm Ior args
    | Cxor -> select_arith_comm Ixor args
    | Clsl -> select_arith Ilsl args
    | Clsr -> select_arith Ilsr args
    | Casr -> select_arith Iasr args
    | Cclz { arg_is_non_zero } ->
      Select_utils.basic_op (Intop (Iclz { arg_is_non_zero })), args
    | Cctz { arg_is_non_zero } ->
      Select_utils.basic_op (Intop (Ictz { arg_is_non_zero })), args
    | Cpopcnt -> Select_utils.basic_op (Intop Ipopcnt), args
    | Ccmpi comp -> select_arith_comp comp args
    | Caddv -> select_arith_comm Iadd args
    | Cadda -> select_arith_comm Iadd args
    | Ccmpf (w, comp) -> Select_utils.basic_op (Floatop (w, Icompf comp)), args
    | Ccsel _ ->
      let cond, ifso, ifnot = three_args () in
      let cond_test, earg = select_condition cond in
      Select_utils.basic_op (Csel cond_test), [earg; ifso; ifnot]
    | Cnegf w -> Select_utils.basic_op (Floatop (w, Inegf)), args
    | Cabsf w -> Select_utils.basic_op (Floatop (w, Iabsf)), args
    | Caddf w -> Select_utils.basic_op (Floatop (w, Iaddf)), args
    | Csubf w -> Select_utils.basic_op (Floatop (w, Isubf)), args
    | Cmulf w -> Select_utils.basic_op (Floatop (w, Imulf)), args
    | Cdivf w -> Select_utils.basic_op (Floatop (w, Idivf)), args
    | Creinterpret_cast cast ->
      Select_utils.basic_op (Reinterpret_cast cast), args
    | Cstatic_cast cast -> Select_utils.basic_op (Static_cast cast), args
    | Catomic { op = atomic_op; size } -> (
      match atomic_op with
      | Exchange | Fetch_and_add | Add | Sub | Land | Lor | Lxor ->
        let src, dst = two_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        ( Select_utils.basic_op (Intop_atomic { op = atomic_op; size; addr }),
          [src; eloc] )
      | Compare_set | Compare_exchange ->
        let compare_with, set_to, dst = three_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        ( Select_utils.basic_op (Intop_atomic { op = atomic_op; size; addr }),
          [compare_with; set_to; eloc] ))
    | Cprobe { name; handler_code_sym; enabled_at_init } ->
      ( Terminator
          (Prim
             { op = Probe { name; handler_code_sym; enabled_at_init };
               label_after
             }),
        args )
    | Cprobe_is_enabled { name; enabled_at_init } ->
      Select_utils.basic_op (Probe_is_enabled { name; enabled_at_init }), []
    | Cbeginregion -> Select_utils.basic_op Begin_region, []
    | Cendregion -> Select_utils.basic_op End_region, args
    | Cpackf32 | Copaque | Cbswap _ | Cprefetch _ | Craise _
    | Ctuple_field (_, _) ->
      Misc.fatal_error "Ssa_of_cmm.select_operation0"

  let rec select_operation (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    match
      Target.select_operation ~generic_select_condition:select_condition op args
        dbg ~label_after
    with
    | Rewritten (bot, args) -> bot, args
    | Select_operation_then_rewrite (op, args, dbg, rewriter) -> (
      let bot, args = select_operation op args dbg ~label_after in
      match rewriter bot ~args with
      | Rewritten (bot, args) -> bot, args
      | Use_default -> bot, args)
    | Use_default -> select_operation0 op args dbg ~label_after

  let emit_branch b (test : Operation.test) rarg ~true_label ~false_label :
      Ssa.terminator =
    let make_cond op a = add_op b ~typ:Cmm.typ_int ~dbg:Debuginfo.none op a in
    (* For Itruetest/Ifalsetest: if the condition is already a comparison Op,
       wrap it in an extra truth-test comparison so cfg_of_ssa's fold produces
       Truth_test (not Int_test/etc). Otherwise put the raw value — cfg_of_ssa
       won't match it and uses Truth_test directly. *)
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

  let env_find v env =
    try V.Map.find v env.vars
    with Not_found -> Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

  let env_add v instrs env =
    { env with vars = V.Map.add (VP.var v) instrs env.vars }

  (* Mark the current trap handler as reachable, matching cfg_selectgen's
     set_traps_for_raise. Must be called after every operation that can
     raise. *)
  let set_traps_for_raise env =
    match env.trap_stack with
    | Operation.Uncaught -> ()
    | Specific_trap (lbl, _) -> (
      match Static_label.Map.find_opt lbl env.static_exceptions with
      | Some handler ->
        Select_utils.set_traps lbl handler.traps_ref env.trap_stack [Pop lbl]
      | None ->
        Misc.fatal_errorf "Ssa_of_cmm: trap %a not registered in env"
          Static_label.format lbl)

  (* For unreachable exception handlers: emit a dummy raise with trap stack set
     to Uncaught, matching cfg_selectgen's unreachable_handler. This avoids
     spurious control-flow edges. *)
  let emit_unreachable_handler _env b =
    let dummy =
      add_op b ~typ:Cmm.typ_int ~dbg:Debuginfo.none (Const_int 1n) [||]
    in
    emit_block b ~dbg:Debuginfo.none
      (Raise (Lambda.Raise_notrace, [| dummy |], None))

  let emit_naming_op b ~bound_name regs =
    match (bound_name : VP.t option) with
    | None -> ()
    | Some v ->
      let provenance = VP.provenance v in
      if Option.is_some provenance
      then
        add_body b
          (Name_for_debugger
             { ident = VP.var v; provenance; which_parameter = None; regs })

  let rec emit_expr env b (exp : Cmm.expression) : or_never_returns =
    match exp with
    | Cconst_int (n, _) ->
      let i =
        add_op b ~typ:Cmm.typ_int ~dbg:Debuginfo.none
          (Const_int (Nativeint.of_int n))
          [||]
      in
      Ok [| i |]
    | Cconst_natint (n, _) ->
      let i =
        add_op b ~typ:Cmm.typ_int ~dbg:Debuginfo.none (Const_int n) [||]
      in
      Ok [| i |]
    | Cconst_float32 (n, _) ->
      let i =
        add_op b ~typ:Cmm.typ_float32 ~dbg:Debuginfo.none
          (Const_float32 (Int32.bits_of_float n))
          [||]
      in
      Ok [| i |]
    | Cconst_float (n, _) ->
      let i =
        add_op b ~typ:Cmm.typ_float ~dbg:Debuginfo.none
          (Const_float (Int64.bits_of_float n))
          [||]
      in
      Ok [| i |]
    | Cconst_vec128 (bits, _) ->
      let i =
        add_op b ~typ:Cmm.typ_vec128 ~dbg:Debuginfo.none (Const_vec128 bits) [||]
      in
      Ok [| i |]
    | Cconst_vec256 (bits, _) ->
      let i =
        add_op b ~typ:Cmm.typ_vec256 ~dbg:Debuginfo.none (Const_vec256 bits) [||]
      in
      Ok [| i |]
    | Cconst_vec512 (bits, _) ->
      let i =
        add_op b ~typ:Cmm.typ_vec512 ~dbg:Debuginfo.none (Const_vec512 bits) [||]
      in
      Ok [| i |]
    | Cconst_symbol (sym, _) ->
      let i =
        add_op b ~typ:Cmm.typ_int ~dbg:Debuginfo.none (Const_symbol sym) [||]
      in
      Ok [| i |]
    | Cvar v -> Ok (env_find v env)
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 with
      | Never_returns -> Never_returns
      | Ok r1 ->
        emit_naming_op b ~bound_name:(Some v) r1;
        emit_expr (env_add v r1 env) b e2)
    | Cphantom_let (_var, _defining_expr, body) -> emit_expr env b body
    | Ctuple [] -> Ok [||]
    | Ctuple exp_list -> emit_tuple env b exp_list
    | Cop (Craise k, args, dbg) -> emit_expr_raise env b k args dbg
    | Cop (Copaque, args, _dbg) -> (
      match emit_parts_list env b args with
      | None -> Never_returns
      | Some (simple_args, env) -> (
        match emit_tuple env b simple_args with
        | Never_returns -> Never_returns
        | Ok rs ->
          let typ = Array.map typ_of_instruction rs in
          let i = add_op b ~typ ~dbg:Debuginfo.none Opaque rs in
          Ok [| i |]))
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) ->
      let* loc_exp = emit_expr env b arg in
      let flat_size a =
        Array.fold_left (fun acc t -> acc + Array.length t) 0 a
      in
      assert (Array.length loc_exp = flat_size fields_layout);
      let before = Array.sub fields_layout 0 field in
      let size_before = flat_size before in
      Ok (Array.sub loc_exp size_before (Array.length fields_layout.(field)))
    | Cop (op, args, dbg) -> emit_expr_op env b op args dbg
    | Csequence (e1, e2) ->
      let* _ = emit_expr env b e1 in
      emit_expr env b e2
    | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
      emit_expr_ifthenelse env b econd eif eelse
    | Cswitch (esel, index, ecases, _dbg) ->
      emit_expr_switch env b esel index ecases
    | Ccatch (_, [], e1) -> emit_expr env b e1
    | Ccatch (rec_flag, handlers, body) ->
      emit_expr_catch env b rec_flag handlers body
    | Cexit (lbl, args, traps) -> emit_expr_exit env b lbl args traps
    | Cinvalid _ ->
      (* TODO: cfg_selectgen emits a call to caml_flambda2_invalid here for
         better error reporting at runtime. For now, Never is semantically
         correct since Cinvalid represents provably unreachable code. *)
      emit_block b ~dbg:Debuginfo.none Never;
      set_traps_for_raise env;
      Never_returns

  (* Replicates emit_parts/emit_parts_list from cfg_selectgen exactly.
     Pre-evaluates complex args R-to-L, binding results to fresh vars in the
     env. Returns simplified CMM expressions (Cvar for pre-evaluated, original
     for deferred) and the updated env. *)
  and emit_parts env b ~effects_after exp =
    if may_defer ~effects_after exp
    then Some (exp, env)
    else
      match emit_expr env b exp with
      | Never_returns -> None
      | Ok r ->
        if Array.length r = 0
        then Some (Cmm.Ctuple [], env)
        else
          let id = V.create_local "bind" in
          Some (Cmm.Cvar id, { env with vars = V.Map.add id r env.vars })

  and emit_parts_list env b exp_list =
    let module EC = Select_utils.Effect_and_coeffect in
    let exp_list_right_to_left, _ =
      List.fold_left
        (fun (acc, effects_after) exp ->
          let exp_effect = effects_of exp in
          (exp, effects_after) :: acc, EC.join exp_effect effects_after)
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

  and emit_tuple env b exp_list : or_never_returns =
    (* Simple right-to-left evaluation, matching emit_tuple_not_flattened in
       cfg_selectgen. *)
    let rec loop = function
      | [] -> Ok [||]
      | exp :: rest -> (
        match loop rest with
        | Never_returns -> Never_returns
        | Ok rest_regs -> (
          match emit_expr env b exp with
          | Never_returns -> Never_returns
          | Ok exp_regs -> Ok (Array.append exp_regs rest_regs)))
    in
    loop exp_list

  and emit_expr_raise env b k args dbg =
    let* r = emit_tuple env b args in
    let handler_label = current_exn_continuation env in
    emit_block b ~dbg (Raise (k, r, handler_label));
    set_traps_for_raise env;
    Never_returns

  and emit_expr_op_cont env b ~ty (new_op : Cfg.basic_or_terminator) arg_instrs
      _label_after dbg : or_never_returns =
    let ty =
      match new_op with
      | Terminator (Prim { op = External { ty_res; _ }; _ }) -> ty_res
      | _ -> ty
    in
    match new_op with
    | Terminator (Call { op = call_op; label_after }) ->
      let pred_label = b.current_label in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      emit_block b ~dbg
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
      let pred_label = b.current_label in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      emit_block b ~dbg
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
      let pred_label = b.current_label in
      let cont_label = label_after in
      emit_block b ~dbg
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
      emit_block b ~dbg Never;
      set_traps_for_raise env;
      Never_returns
    | Basic _ ->
      Misc.fatal_errorf "Ssa_of_cmm.emit_expr_op_cont: unexpected basic"
    | Terminator term ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"")
        term

  and chunk_of_component (c : Cmm.machtype_component) : Cmm.memory_chunk =
    match c with
    | Float -> Double
    | Float32 -> Single { reg = Float32 }
    | Vec128 -> Onetwentyeight_unaligned
    | Vec256 -> Twofiftysix_unaligned
    | Vec512 -> Fivetwelve_unaligned
    | Val | Int | Addr | Valx2 -> Word_val

  and typ_of_instruction (r : Ssa.instruction) : Cmm.machtype_component =
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

  (* Copied from cfg_selectgen effects_of0/effects_of. Used to decide what needs
     pre-evaluation before an allocation, matching emit_parts_list. *)
  and effects_of0 (exp : Cmm.expression) =
    let module EC = Select_utils.Effect_and_coeffect in
    match exp with
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
    | Cvar _ ->
      EC.none
    | Ctuple el -> EC.join_list_map el effects_of
    | Clet (_, arg, body) -> EC.join (effects_of arg) (effects_of body)
    | Cphantom_let (_, _, body) -> effects_of body
    | Csequence (e1, e2) -> EC.join (effects_of e1) (effects_of e2)
    | Cifthenelse (cond, _, ifso, _, ifnot, _) ->
      EC.join (effects_of cond) (EC.join (effects_of ifso) (effects_of ifnot))
    | Cop (op, args, _) ->
      let from_op =
        match op with
        | Cextcall { effects = e; coeffects = ce } ->
          EC.create
            (Select_utils.select_effects e)
            (Select_utils.select_coeffects ce)
        | Capply _ | Cprobe _ | Copaque | Cpoll | Cpause -> EC.arbitrary
        | Calloc (Heap, _) -> EC.none
        | Calloc (Local, _) -> EC.coeffect_only Arbitrary
        | Cstore _ -> EC.effect_only Arbitrary
        | Cbeginregion | Cendregion -> EC.arbitrary
        | Cprefetch _ -> EC.arbitrary
        | Catomic _ -> EC.arbitrary
        | Craise _ -> EC.effect_only Raise
        | Cload { mutability = Immutable } -> EC.none
        | Cload { mutability = Mutable } | Cdls_get | Ctls_get | Cdomain_index
          ->
          EC.coeffect_only Read_mutable
        | Cprobe_is_enabled _ -> EC.coeffect_only Arbitrary
        | Ctuple_field _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
        | Caddi128 | Csubi128 | Cmuli64 _ | Cand | Cor | Cxor | Cbswap _
        | Ccsel _ | Cclz _ | Cctz _ | Cpopcnt | Clsl | Clsr | Casr | Ccmpi _
        | Caddv | Cadda | Cnegf _ | Cabsf _ | Caddf _ | Csubf _ | Cmulf _
        | Cdivf _ | Cpackf32 | Creinterpret_cast _ | Cstatic_cast _ | Ccmpf _ ->
          EC.none
      in
      EC.join from_op (EC.join_list_map args effects_of)
    | Cswitch _ | Ccatch _ | Cexit _ | Cinvalid _ -> EC.arbitrary

  and effects_of (expr : Cmm.expression) =
    match Target.effects_of expr with
    | Effects_of_all_expressions exprs ->
      Select_utils.Effect_and_coeffect.join_list_map exprs effects_of
    | Use_default -> effects_of0 expr

  (* Copied from cfg_selectgen is_simple_expr0/ is_simple_expr. Used together
     with effects_of to replicate the emit_parts_list logic. *)
  and is_simple_expr0 (exp : Cmm.expression) =
    match exp with
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
    | Cvar _ ->
      true
    | Ctuple el -> List.for_all is_simple_expr el
    | Clet (_, arg, body) -> is_simple_expr arg && is_simple_expr body
    | Cphantom_let (_, _, body) -> is_simple_expr body
    | Csequence (e1, e2) -> is_simple_expr e1 && is_simple_expr e2
    | Cop (op, args, _) -> (
      match op with
      | Cextcall { effects = No_effects; coeffects = No_coeffects } ->
        List.for_all is_simple_expr args
      | Capply _ | Cextcall _ | Calloc _ | Cstore _ | Craise _ | Catomic _
      | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cpoll | Cpause ->
        false
      | Cprefetch _ | Cbeginregion | Cendregion -> false
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Caddi128
      | Csubi128 | Cmuli64 _ | Cand | Cor | Cxor | Clsl | Clsr | Casr | Ccmpi _
      | Caddv | Cadda | Cnegf _ | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccsel _
      | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
      | Creinterpret_cast _ | Cstatic_cast _ | Ctuple_field _ | Ccmpf _
      | Cdls_get | Ctls_get | Cdomain_index ->
        List.for_all is_simple_expr args)
    | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Cinvalid _ -> false

  and is_simple_expr (expr : Cmm.expression) =
    match Target.is_simple_expr expr with
    | Simple_if_all_expressions_are exprs -> List.for_all is_simple_expr exprs
    | Use_default -> is_simple_expr0 expr

  (* Replicates emit_parts from cfg_selectgen: an expression may be deferred if
     its effects allow it AND it is structurally simple. *)
  and may_defer ~effects_after exp =
    let module EC = Select_utils.Effect_and_coeffect in
    let ec = effects_of exp in
    let may_defer_evaluation =
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
    may_defer_evaluation && is_simple_expr exp

  (* emit_stores: matches cfg_selectgen emit_stores. Args are already simplified
     by emit_parts_list, so complex subexprs are Cvar lookups. *)
  and emit_stores env b ~dbg cmm_args addr =
    let byte_offset = ref (-Arch.size_int) in
    let ok = ref true in
    let store_one_reg (r : Ssa.instruction) (typ : Cmm.machtype_component) =
      let chunk = chunk_of_component typ in
      ignore
        (add_op b ~typ:Cmm.typ_void ~dbg
           (Store
              ( chunk,
                Arch.offset_addressing Arch.identity_addressing !byte_offset,
                false ))
           [| r; addr |]);
      byte_offset := !byte_offset + Select_utils.size_component typ
    in
    List.iter
      (fun (arg : Cmm.expression) ->
        if !ok
        then
          let addressing_mode =
            Arch.offset_addressing Arch.identity_addressing !byte_offset
          in
          let store_result =
            Target.select_store ~is_assign:false addressing_mode arg
          in
          match store_result with
          | Rewritten (op, Cmm.Ctuple []) ->
            ignore (add_op b ~typ:Cmm.typ_void ~dbg op [| addr |]);
            byte_offset := !byte_offset + Arch.size_int
          | Rewritten (op, new_arg) -> (
            match emit_expr env b new_arg with
            | Never_returns -> ok := false
            | Ok field ->
              ignore
                (add_op b ~typ:Cmm.typ_void ~dbg op
                   (Array.append field [| addr |]));
              byte_offset := !byte_offset + Arch.size_int)
          | Use_default | Maybe_out_of_range -> (
            match emit_expr env b arg with
            | Never_returns -> ok := false
            | Ok field ->
              Array.iter
                (fun (r : Ssa.instruction) ->
                  store_one_reg r (typ_of_instruction r))
                field))
      cmm_args;
    !ok

  and size_of_cmm_expr env (e : Cmm.expression) =
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
        (fun acc i -> acc + Select_utils.size_component (typ_of_instruction i))
        0 instrs
    | Ctuple el ->
      List.fold_left (fun acc e -> acc + size_of_cmm_expr env e) 0 el
    | Cop (op, _, _) ->
      let ty = Select_utils.oper_result_type op in
      Array.fold_left (fun acc c -> acc + Select_utils.size_component c) 0 ty
    | Clet (_, _, body) | Csequence (_, body) -> size_of_cmm_expr env body
    | Cifthenelse (_, _, e1, _, _, _) -> size_of_cmm_expr env e1
    | _ ->
      (* Conservative fallback *)
      Arch.size_addr

  and emit_alloc env b (cmm_args : Cmm.expression list)
      (ph : Cmm.alloc_dbginfo_item) mode dbg : or_never_returns =
    let bytes =
      List.fold_left (fun acc arg -> acc + size_of_cmm_expr env arg) 0 cmm_args
    in
    let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
    let alloc_op =
      Operation.Alloc
        { bytes; dbginfo = [{ ph with alloc_words; alloc_dbg = dbg }]; mode }
    in
    let addr = add_op b ~typ:Cmm.typ_val ~dbg alloc_op [||] in
    set_traps_for_raise env;
    if emit_stores env b ~dbg cmm_args addr
    then Ok [| addr |]
    else Never_returns

  and emit_expr_op env b op args dbg : or_never_returns =
    (* Matching cfg_selectgen emit_expr_op: emit_parts_list first, then
       select_operation on the simplified args. *)
    match emit_parts_list env b args with
    | None -> Never_returns
    | Some (simple_args, env) -> (
      let ty = Select_utils.oper_result_type op in
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match new_op with
      | Basic (Op (Alloc { bytes = _; mode; dbginfo = [ph] })) ->
        emit_alloc env b new_args ph mode dbg
      | _ -> (
        let* arg_instrs = emit_tuple env b new_args in
        match new_op with
        | Terminator _ ->
          emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
        | Basic (Op basic_op) ->
          let i = add_op b ~typ:ty ~dbg basic_op arg_instrs in
          if Array.length ty = 0 then Ok [||] else Ok [| i |]
        | Basic basic ->
          Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic
            basic))

  and emit_expr_ifthenelse env b econd eif eelse : or_never_returns =
    let cond, earg = select_condition econd in
    match emit_expr env b earg with
    | Never_returns -> Never_returns
    | Ok rarg ->
      let then_label = Cmm.new_label () in
      let else_label = Cmm.new_label () in
      let pred_label = b.current_label in
      let term =
        emit_branch b cond rarg ~true_label:then_label ~false_label:else_label
      in
      emit_block b ~dbg:Debuginfo.none term;
      let b_then =
        fork then_label (BranchTarget { predecessor = pred_label }) [||]
      in
      let r_then = emit_expr env b_then eif in
      let b_else =
        fork else_label (BranchTarget { predecessor = pred_label }) [||]
      in
      let r_else = emit_expr env b_else eelse in
      join_branches b r_then b_then r_else b_else

  and emit_expr_switch env b esel index ecases : or_never_returns =
    match emit_expr env b esel with
    | Never_returns -> Never_returns
    | Ok rsel ->
      let pred_label = b.current_label in
      let case_results =
        Array.map
          (fun (case_expr, _dbg) ->
            let case_label = Cmm.new_label () in
            let b_case =
              fork case_label (BranchTarget { predecessor = pred_label }) [||]
            in
            let r = emit_expr env b_case case_expr in
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
      emit_block b ~dbg:Debuginfo.none (Switch (labels, rsel));
      join_array b case_results

  and emit_expr_catch env b rec_flag handlers catch_body : or_never_returns =
    let handlers_info =
      List.map
        (fun (handler : Cmm.static_handler) ->
          let nfail = handler.label in
          let ids = handler.params in
          let handler_body = handler.body in
          let handler_label = Cmm.new_label () in
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let traps_ref = ref Select_utils.Unreachable in
          let handler_info =
            { handler_label; handler_types = types; traps_ref }
          in
          nfail, ids, handler_body, handler_info)
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
    let body_label = Cmm.new_label () in
    let pred_label = b.current_label in
    let b_body = fork body_label (Merge { predecessors = [pred_label] }) [||] in
    let r_body = emit_expr env b_body catch_body in
    emit_block b ~dbg:Debuginfo.none
      (Goto { goto = body_label; args = [||] });
    let handler_results =
      List.map
        (fun (_, ids, handler_body, info) ->
          let block_desc : Ssa.block_desc =
            match rec_flag with
            | Cmm.Exn_handler -> TrapHandler { predecessors = [] }
            | Cmm.Normal | Cmm.Recursive -> Merge { predecessors = [] }
          in
          let b_handler =
            fork info.handler_label block_desc info.handler_types
          in
          let r =
            match !(info.traps_ref) with
            | Select_utils.Unreachable ->
              emit_unreachable_handler env b_handler;
              Never_returns
            | Select_utils.Reachable trap_stack ->
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
                    add_body b_handler
                      (Name_for_debugger
                         { ident = VP.var id;
                           provenance;
                           which_parameter = None;
                           regs
                         }))
                ids;
              emit_expr handler_env b_handler handler_body
          in
          info.handler_label, r, b_handler)
        handlers_info
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
        add_body b
          (match trap with
          | Push _ -> Pushtrap { lbl_handler = h.handler_label }
          | Pop _ -> Poptrap { lbl_handler = h.handler_label }))
      traps

  and emit_expr_exit env b (lbl : Cmm.exit_label) args traps : or_never_returns
      =
    match lbl with
    | Lbl nfail ->
      (* Matching cfg_selectgen emit_expr_exit: emit_parts_list first, then
         emit_tuple on the simplified args. *)
      let src_result =
        match emit_parts_list env b args with
        | None -> Never_returns
        | Some (simple_args, env) -> emit_tuple env b simple_args
      in
      let* src = src_result in
      let handler = find_handler env nfail in
      emit_trap_actions env b traps;
      Select_utils.set_traps nfail handler.traps_ref env.trap_stack traps;
      emit_block b ~dbg:Debuginfo.none
        (Goto { goto = handler.handler_label; args = src });
      Never_returns
    | Return_lbl ->
      let src_result =
        match emit_parts_list env b args with
        | None -> Never_returns
        | Some (simple_args, env) -> emit_tuple env b simple_args
      in
      let* src = src_result in
      emit_trap_actions env b traps;
      emit_block b ~dbg:Debuginfo.none (Return src);
      Never_returns

  and emit_tail env b (exp : Cmm.expression) : unit =
    match exp with
    | Clet (v, e1, e2) -> (
      match emit_expr env b e1 with
      | Never_returns -> ()
      | Ok r1 ->
        emit_naming_op b ~bound_name:(Some v) r1;
        emit_tail (env_add v r1 env) b e2)
    | Cphantom_let (_, _, body) -> emit_tail env b body
    | Csequence (e1, e2) -> (
      match emit_expr env b e1 with
      | Never_returns -> ()
      | Ok _ -> emit_tail env b e2)
    | Cop ((Capply { result_type = _; region = Rc_normal; _ } as op), args, dbg)
      ->
      emit_tail_apply env b op args dbg
    | Cifthenelse (econd, _, eif, _, eelse, _) ->
      emit_tail_ifthenelse env b econd eif eelse
    | Cswitch (esel, index, ecases, _) ->
      emit_tail_switch env b esel index ecases
    | Ccatch (_, [], e1) -> emit_tail env b e1
    | Ccatch (rec_flag, handlers, body) ->
      emit_tail_catch env b rec_flag handlers body
    | _ -> (
      match emit_expr env b exp with
      | Never_returns -> ()
      | Ok r ->
        emit_pop_all_traps env b;
        emit_block b ~dbg:Debuginfo.none (Return r))

  and trap_stack_is_empty env =
    match env.trap_stack with
    | Operation.Uncaught -> true
    | Specific_trap _ -> false

  and emit_pop_all_traps env b =
    let rec pop = function
      | Operation.Uncaught -> ()
      | Operation.Specific_trap (lbl, rest) ->
        let handler = find_handler env lbl in
        add_body b (Poptrap { lbl_handler = handler.handler_label });
        pop rest
    in
    pop env.trap_stack

  and emit_tail_apply env b op args dbg =
    (* Matching cfg_selectgen emit_tail_apply: emit_parts_list first, then
       select_operation on simplified args. *)
    match emit_parts_list env b args with
    | None -> ()
    | Some (simple_args, env) -> (
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match emit_tuple env b new_args with
      | Never_returns -> ()
      | Ok arg_instrs -> (
        (* Check if a tail call is possible: trap stack must be empty and args
           must fit in registers (no stack args), matching cfg_selectgen. *)
        let can_tailcall call_op =
          if not (trap_stack_is_empty env)
          then false
          else
            (* For indirect calls, exclude the closure pointer (first arg) from
               the stack_ofs calculation, matching cfg_selectgen. *)
            let rarg =
              match call_op with
              | Cfg.Indirect _ ->
                Array.sub arg_instrs 1 (Array.length arg_instrs - 1)
              | Cfg.Direct _ -> arg_instrs
            in
            let arg_types = Array.map typ_of_instruction rarg in
            let _, stack_ofs_args = Proc.loc_arguments arg_types in
            let res_type = Select_utils.oper_result_type op in
            let _, stack_ofs_res = Proc.loc_results_call res_type in
            Stdlib.Int.max stack_ofs_args stack_ofs_res = 0
        in
        match new_op with
        | Terminator (Call { op = Indirect callees; _ })
          when can_tailcall (Indirect callees) ->
          emit_block b ~dbg (Tailcall_func (Indirect callees, arg_instrs))
        | Terminator (Call { op = Direct func; _ })
          when String.equal func.sym_name !Select_utils.current_function_name
               && can_tailcall (Direct func) ->
          emit_block b ~dbg
            (Tailcall_self
               { destination = env.tailrec_label; args = arg_instrs })
        | Terminator (Call { op = Direct func; _ })
          when can_tailcall (Direct func) ->
          emit_block b ~dbg (Tailcall_func (Direct func, arg_instrs))
        | _ -> (
          (* Fall back to call + return *)
          let ty = Select_utils.oper_result_type op in
          let r =
            emit_expr_op_cont env b ~ty new_op arg_instrs label_after dbg
          in
          match r with
          | Never_returns -> ()
          | Ok r ->
            emit_pop_all_traps env b;
            emit_block b ~dbg:Debuginfo.none (Return r))))

  and emit_tail_ifthenelse env b econd eif eelse =
    let cond, earg = select_condition econd in
    match emit_expr env b earg with
    | Never_returns -> ()
    | Ok rarg ->
      let then_label = Cmm.new_label () in
      let else_label = Cmm.new_label () in
      let pred_label = b.current_label in
      let term =
        emit_branch b cond rarg ~true_label:then_label ~false_label:else_label
      in
      emit_block b ~dbg:Debuginfo.none term;
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

  and emit_tail_switch env b esel index ecases =
    match emit_expr env b esel with
    | Never_returns -> ()
    | Ok rsel ->
      let pred_label = b.current_label in
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
      emit_block b ~dbg:Debuginfo.none (Switch (labels, rsel));
      Array.iter (fun (_, b_case) -> merge_into b b_case) case_builders

  and emit_tail_catch env b rec_flag handlers catch_body =
    let handlers_info =
      List.map
        (fun (handler : Cmm.static_handler) ->
          let nfail = handler.label in
          let ids = handler.params in
          let handler_body = handler.body in
          let handler_label = Cmm.new_label () in
          let types = List.map (fun (_id, ty) -> ty) ids |> Array.concat in
          let traps_ref = ref Select_utils.Unreachable in
          let handler_info =
            { handler_label; handler_types = types; traps_ref }
          in
          nfail, ids, handler_body, handler_info)
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
    let body_label = Cmm.new_label () in
    let pred_label = b.current_label in
    let b_body = fork body_label (Merge { predecessors = [pred_label] }) [||] in
    emit_tail env b_body catch_body;
    emit_block b ~dbg:Debuginfo.none
      (Goto { goto = body_label; args = [||] });
    let handler_builders =
      List.map
        (fun (_, ids, handler_body, info) ->
          let block_desc : Ssa.block_desc =
            match rec_flag with
            | Cmm.Exn_handler -> TrapHandler { predecessors = [] }
            | Cmm.Normal | Cmm.Recursive -> Merge { predecessors = [] }
          in
          let b_handler =
            fork info.handler_label block_desc info.handler_types
          in
          (match !(info.traps_ref) with
          | Select_utils.Unreachable -> emit_unreachable_handler env b_handler
          | Select_utils.Reachable trap_stack ->
            let handler_env = { env with trap_stack } in
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
                  add_body b_handler
                    (Name_for_debugger
                       { ident = VP.var id;
                         provenance;
                         which_parameter = None;
                         regs
                       }))
              ids;
            emit_tail handler_env b_handler handler_body);
          b_handler)
        handlers_info
    in
    merge_into b b_body;
    List.iter (merge_into b) handler_builders

  and join_branches b r1 b1 r2 b2 : or_never_returns =
    match r1, r2 with
    | Never_returns, Never_returns ->
      merge_into b b1;
      merge_into b b2;
      Never_returns
    | Ok r, Never_returns ->
      merge_into b b2;
      merge_into b b1;
      b.current_label <- b1.current_label;
      b.current_desc <- b1.current_desc;
      b.current_params <- b1.current_params;
      b.body <- b1.body;
      Ok r
    | Never_returns, Ok r ->
      merge_into b b1;
      merge_into b b2;
      b.current_label <- b2.current_label;
      b.current_desc <- b2.current_desc;
      b.current_params <- b2.current_params;
      b.body <- b2.body;
      Ok r
    | Ok r1_instrs, Ok r2_instrs ->
      let join_label = Cmm.new_label () in
      assert (Array.length r1_instrs = Array.length r2_instrs);
      let join_types = Array.map typ_of_instruction r1_instrs in
      emit_block b1 ~dbg:Debuginfo.none
        (Goto { goto = join_label; args = r1_instrs });
      emit_block b2 ~dbg:Debuginfo.none
        (Goto { goto = join_label; args = r2_instrs });
      let preds = [b1.current_label; b2.current_label] in
      merge_into b b1;
      merge_into b b2;
      start_block b join_label (Merge { predecessors = preds }) join_types;
      if Array.length join_types = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  and join_array b (results : (Label.t * or_never_returns * builder) array) :
      or_never_returns =
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
            preds := sub.current_label :: !preds;
            emit_block sub ~dbg:Debuginfo.none
              (Goto { goto = join_label; args = instrs }));
          merge_into b sub)
        results;
      start_block b join_label
        (Merge { predecessors = List.rev !preds })
        join_types;
      if Array.length join_types = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  let emit_fundecl (f : Cmm.fundecl) : Ssa.t =
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
    let blocks = Label.Tbl.create 31 in
    let fun_contains_calls = ref false in
    let block_order =
      List.rev_map
        (fun (block : Ssa.basic_block) ->
          Label.Tbl.add blocks block.label block;
          (match block.terminator with
          | Call _ | Prim _ -> fun_contains_calls := true
          | Never | Goto _ | Branch _ | Switch _ | Return _ | Raise _
          | Tailcall_self _ | Tailcall_func _ ->
            ());
          block.label)
        b.blocks
    in
    { Ssa.blocks;
      block_order;
      fun_name = f.fun_name.sym_name;
      fun_args = fun_arg_types;
      fun_args_names = f.fun_args;
      fun_codegen_options = f.fun_codegen_options;
      fun_dbg = f.fun_dbg;
      entry_label;
      fun_contains_calls = !fun_contains_calls;
      fun_poll = f.fun_poll;
      fun_ret_type = f.fun_ret_type
    }
end
