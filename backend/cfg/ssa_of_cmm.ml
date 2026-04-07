[@@@ocaml.warning "+a-4-9-40-41-42"]

open! Int_replace_polymorphic_compare

module V = Backend_var
module VP = Backend_var.With_provenance

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | Cmm.Push t :: l ->
    combine_traps (Operation.Specific_trap (t, trap_stack)) l
  | Cmm.Pop _ :: l -> (
    match (trap_stack : Operation.trap_stack) with
    | Uncaught ->
      Misc.fatal_error "Trying to pop a trap from an empty stack"
    | Specific_trap (_, ts) -> combine_traps ts l)

type or_never_returns =
  | Ok of Ssa.instruction array
  | Never_returns

let ( let* ) x f = match x with Never_returns -> Never_returns | Ok x -> f x

type static_handler =
  { handler_label : Label.t;
    handler_types : Cmm.machtype;
    traps_ref : trap_stack_info ref
  }

and trap_stack_info =
  | Unreachable
  | Reachable of Operation.trap_stack

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
    match
      Static_label.Map.find_opt static_label
        env.static_exceptions
    with
    | Some handler -> Some handler.handler_label
    | None -> None)

type builder =
  { mutable blocks : Ssa.basic_block list;
    mutable current_label : Label.t;
    mutable current_desc : Ssa.block_desc;
    mutable current_params : Cmm.machtype;
    mutable body : Ssa.body_instruction list
  }

let make_op op args : Ssa.instruction =
  Op { id = Ssa.InstructionId.create (); op; args }

let add_op b op args : Ssa.instruction =
  let i = make_op op args in
  b.body <- Instr i :: b.body;
  i

let add_body b (bi : Ssa.body_instruction) =
  b.body <- bi :: b.body

let emit_block b term =
  let block =
    { Ssa.label = b.current_label;
      desc = b.current_desc;
      params = b.current_params;
      body = Array.of_list (List.rev b.body);
      terminator = term
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

let merge_into parent sub =
  parent.blocks <- sub.blocks @ parent.blocks

let block_params label types =
  Array.mapi
    (fun i _ ->
      (Ssa.Block_param { block = label; index = i }
        : Ssa.instruction))
    types

module Make (Target : Cfg_selectgen_target_intf.S) = struct
  let is_immediate (op : Operation.integer_operation) n =
    match Target.is_immediate op n with
    | Is_immediate result -> result
    | Use_default -> (
      match op with
      | Ilsl | Ilsr | Iasr ->
        n >= 0 && n < Arch.size_int * 8
      | _ -> false)

  let is_immediate_test cmp n =
    match Target.is_immediate_test cmp n with
    | Is_immediate result -> result
    | Use_default -> is_immediate (Icomp cmp) n

  let select_condition (arg : Cmm.expression) :
      Operation.test * Cmm.expression =
    match arg with
    | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
      when is_immediate_test cmp n ->
      Iinttest_imm (cmp, n), arg1
    | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Cmm.swap_integer_comparison cmp) n ->
      Iinttest_imm (Cmm.swap_integer_comparison cmp, n), arg2
    | Cop (Ccmpi cmp, args, _) -> Iinttest cmp, Ctuple args
    | Cop (Ccmpf (width, cmp), args, _) ->
      Ifloattest (width, cmp), Ctuple args
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
    | [arg; Cmm.Cconst_int (n, _)]
      when is_immediate (Operation.Icomp cmp) n ->
      Select_utils.basic_op (Intop_imm (Icomp cmp, n)), [arg]
    | [Cmm.Cconst_int (n, _); arg]
      when is_immediate
             (Operation.Icomp
                (Scalar.Integer_comparison.swap cmp))
             n ->
      ( Select_utils.basic_op
          (Intop_imm
             ( Icomp (Scalar.Integer_comparison.swap cmp),
               n )),
        [arg] )
    | _ -> Select_utils.basic_op (Intop (Icomp cmp)), args

  let select_operation0 (op : Cmm.operation)
      (args : Cmm.expression list) (dbg : Debuginfo.t)
      ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    let wrong_num_args n =
      Misc.fatal_errorf
        "Ssa_of_cmm.select_operation: expected %d argument(s) \
         for@ %s"
        n
        (Printcmm.operation dbg op)
    in
    let[@inline] single_arg () =
      match args with
      | [arg] -> arg
      | [] | _ :: _ -> wrong_num_args 1
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
      | _ ->
        Terminator (Call { op = Indirect callees; label_after }),
        args)
    | Cextcall
        { func; alloc; ty; ty_args; returns; builtin = _; effects }
      ->
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
      then
        ( Terminator
            (Prim { op = External external_call; label_after }),
          args )
      else Terminator (Call_no_return external_call), args
    | Cload { memory_chunk; mutability; is_atomic } ->
      let arg = single_arg () in
      let addressing_mode, eloc =
        Target.select_addressing memory_chunk arg
      in
      let mutability =
        Select_utils.select_mutable_flag mutability
      in
      ( Select_utils.basic_op
          (Load
             { memory_chunk;
               addressing_mode;
               mutability;
               is_atomic
             }),
        [eloc] )
    | Cstore (chunk, init) -> (
      let arg1, arg2 = two_args () in
      let addr, eloc = Target.select_addressing chunk arg1 in
      let is_assign =
        match init with
        | Initialization -> false
        | Assignment -> true
      in
      match[@ocaml.warning "-fragile-match"] chunk with
      | Word_int | Word_val ->
        let op, newarg2 =
          match
            Target.is_store_out_of_range chunk ~byte_offset:0
          with
          | Within_range -> (
            match Target.select_store ~is_assign addr arg2 with
            | Rewritten (op, arg) -> op, arg
            | Use_default | Maybe_out_of_range ->
              Operation.Store (chunk, addr, is_assign), arg2)
          | Out_of_range ->
            Misc.fatal_errorf
              "Ssa_of_cmm: store out of range:@ %s"
              (Printcmm.operation dbg op)
        in
        Select_utils.basic_op op, [newarg2; eloc]
      | _ ->
        ( Select_utils.basic_op
            (Store (chunk, addr, is_assign)),
          [arg2; eloc] ))
    | Cdls_get -> Select_utils.basic_op Dls_get, args
    | Ctls_get -> Select_utils.basic_op Tls_get, args
    | Cdomain_index -> Select_utils.basic_op Domain_index, args
    | Calloc (mode, alloc_block_kind) ->
      let placeholder : Cmm.alloc_dbginfo_item =
        { alloc_words = 0;
          alloc_block_kind;
          alloc_dbg = Debuginfo.none
        }
      in
      ( Select_utils.basic_op
          (Alloc { bytes = 0; dbginfo = [placeholder]; mode }),
        args )
    | Cpoll -> Select_utils.basic_op Poll, args
    | Cpause -> Select_utils.basic_op Pause, args
    | Caddi -> select_arith_comm Iadd args
    | Csubi -> select_arith Isub args
    | Cmuli -> select_arith_comm Imul args
    | Cmulhi { signed } ->
      select_arith_comm (Imulh { signed }) args
    | Cdivi -> Select_utils.basic_op (Intop Idiv), args
    | Cmodi -> Select_utils.basic_op (Intop Imod), args
    | Caddi128 ->
      Select_utils.basic_op (Int128op Iadd128), args
    | Csubi128 ->
      Select_utils.basic_op (Int128op Isub128), args
    | Cmuli64 { signed } ->
      Select_utils.basic_op (Int128op (Imul64 { signed })), args
    | Cand -> select_arith_comm Iand args
    | Cor -> select_arith_comm Ior args
    | Cxor -> select_arith_comm Ixor args
    | Clsl -> select_arith Ilsl args
    | Clsr -> select_arith Ilsr args
    | Casr -> select_arith Iasr args
    | Cclz { arg_is_non_zero } ->
      ( Select_utils.basic_op
          (Intop (Iclz { arg_is_non_zero })),
        args )
    | Cctz { arg_is_non_zero } ->
      ( Select_utils.basic_op
          (Intop (Ictz { arg_is_non_zero })),
        args )
    | Cpopcnt -> Select_utils.basic_op (Intop Ipopcnt), args
    | Ccmpi comp -> select_arith_comp comp args
    | Caddv -> select_arith_comm Iadd args
    | Cadda -> select_arith_comm Iadd args
    | Ccmpf (w, comp) ->
      Select_utils.basic_op (Floatop (w, Icompf comp)), args
    | Ccsel _ ->
      let cond, ifso, ifnot = three_args () in
      let cond_test, earg = select_condition cond in
      Select_utils.basic_op (Csel cond_test), [earg; ifso; ifnot]
    | Cnegf w ->
      Select_utils.basic_op (Floatop (w, Inegf)), args
    | Cabsf w ->
      Select_utils.basic_op (Floatop (w, Iabsf)), args
    | Caddf w ->
      Select_utils.basic_op (Floatop (w, Iaddf)), args
    | Csubf w ->
      Select_utils.basic_op (Floatop (w, Isubf)), args
    | Cmulf w ->
      Select_utils.basic_op (Floatop (w, Imulf)), args
    | Cdivf w ->
      Select_utils.basic_op (Floatop (w, Idivf)), args
    | Creinterpret_cast cast ->
      Select_utils.basic_op (Reinterpret_cast cast), args
    | Cstatic_cast cast ->
      Select_utils.basic_op (Static_cast cast), args
    | Catomic { op = atomic_op; size } -> (
      match atomic_op with
      | Exchange | Fetch_and_add | Add | Sub | Land | Lor
      | Lxor ->
        let src, dst = two_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc =
          Target.select_addressing dst_size dst
        in
        ( Select_utils.basic_op
            (Intop_atomic { op = atomic_op; size; addr }),
          [src; eloc] )
      | Compare_set | Compare_exchange ->
        let compare_with, set_to, dst = three_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc =
          Target.select_addressing dst_size dst
        in
        ( Select_utils.basic_op
            (Intop_atomic { op = atomic_op; size; addr }),
          [compare_with; set_to; eloc] ))
    | Cprobe { name; handler_code_sym; enabled_at_init } ->
      ( Terminator
          (Prim
             { op =
                 Probe { name; handler_code_sym; enabled_at_init };
               label_after
             }),
        args )
    | Cprobe_is_enabled { name; enabled_at_init } ->
      ( Select_utils.basic_op
          (Probe_is_enabled { name; enabled_at_init }),
        [] )
    | Cbeginregion -> Select_utils.basic_op Begin_region, []
    | Cendregion -> Select_utils.basic_op End_region, args
    | Cpackf32 | Copaque | Cbswap _ | Cprefetch _ | Craise _
    | Ctuple_field (_, _) ->
      Misc.fatal_error "Ssa_of_cmm.select_operation0"

  let rec select_operation (op : Cmm.operation)
      (args : Cmm.expression list) (dbg : Debuginfo.t)
      ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    match
      Target.select_operation
        ~generic_select_condition:select_condition op args dbg
        ~label_after
    with
    | Rewritten (bot, args) -> bot, args
    | Select_operation_then_rewrite (op, args, dbg, rewriter) -> (
      let bot, args =
        select_operation op args dbg ~label_after
      in
      match rewriter bot ~args with
      | Rewritten (bot, args) -> bot, args
      | Use_default -> bot, args)
    | Use_default -> select_operation0 op args dbg ~label_after

  let emit_branch b (test : Operation.test) rarg ~true_label
      ~false_label : Ssa.terminator =
    let make_cond op a = add_op b op a in
    match test with
    | Itruetest ->
      Branch
        { conditions = [| rarg.(0), true_label |];
          else_goto = false_label
        }
    | Ifalsetest ->
      Branch
        { conditions = [| rarg.(0), false_label |];
          else_goto = true_label
        }
    | Iinttest cmp ->
      let c =
        make_cond (Intop (Icomp cmp)) rarg
      in
      Branch
        { conditions = [| c, true_label |];
          else_goto = false_label
        }
    | Iinttest_imm (cmp, n) ->
      let c =
        make_cond
          (Intop_imm (Icomp cmp, n))
          [| rarg.(0) |]
      in
      Branch
        { conditions = [| c, true_label |];
          else_goto = false_label
        }
    | Ifloattest (w, cmp) ->
      let c =
        make_cond (Floatop (w, Icompf cmp)) rarg
      in
      Branch
        { conditions = [| c, true_label |];
          else_goto = false_label
        }
    | Ioddtest ->
      let c =
        make_cond
          (Intop_imm (Iand, 1))
          [| rarg.(0) |]
      in
      Branch
        { conditions = [| c, true_label |];
          else_goto = false_label
        }
    | Ieventest ->
      let c =
        make_cond
          (Intop_imm (Iand, 1))
          [| rarg.(0) |]
      in
      Branch
        { conditions = [| c, false_label |];
          else_goto = true_label
        }

  let env_find v env =
    try V.Map.find v env.vars
    with Not_found ->
      Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

  let env_add v instrs env =
    { env with vars = V.Map.add (VP.var v) instrs env.vars }

  let compute_join_types r1 r2 =
    assert (Array.length r1 = Array.length r2);
    [||]

  let rec emit_expr env b (exp : Cmm.expression) :
      or_never_returns =
    match exp with
    | Cconst_int (n, _) ->
      let i =
        add_op b (Const_int (Nativeint.of_int n)) [||]
      in
      Ok [| i |]
    | Cconst_natint (n, _) ->
      let i = add_op b (Const_int n) [||] in
      Ok [| i |]
    | Cconst_float32 (n, _) ->
      let i =
        add_op b
          (Const_float32 (Int32.bits_of_float n))
          [||]
      in
      Ok [| i |]
    | Cconst_float (n, _) ->
      let i =
        add_op b
          (Const_float (Int64.bits_of_float n))
          [||]
      in
      Ok [| i |]
    | Cconst_vec128 (bits, _) ->
      let i = add_op b (Const_vec128 bits) [||] in
      Ok [| i |]
    | Cconst_vec256 (bits, _) ->
      let i = add_op b (Const_vec256 bits) [||] in
      Ok [| i |]
    | Cconst_vec512 (bits, _) ->
      let i = add_op b (Const_vec512 bits) [||] in
      Ok [| i |]
    | Cconst_symbol (sym, _) ->
      let i = add_op b (Const_symbol sym) [||] in
      Ok [| i |]
    | Cvar v -> Ok (env_find v env)
    | Clet (v, e1, e2) ->
      let* r1 = emit_expr env b e1 in
      emit_expr (env_add v r1 env) b e2
    | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr env b body
    | Ctuple [] -> Ok [||]
    | Ctuple exp_list -> emit_tuple env b exp_list
    | Cop (Craise k, args, _dbg) ->
      emit_expr_raise env b k args
    | Cop (Copaque, args, _dbg) -> (
      match emit_tuple_exprs env b args with
      | Never_returns -> Never_returns
      | Ok rs ->
        let i = add_op b Opaque rs in
        Ok [| i |])
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) ->
      let* loc_exp = emit_expr env b arg in
      let flat_size a =
        Array.fold_left
          (fun acc t -> acc + Array.length t)
          0 a
      in
      assert (
        Array.length loc_exp = flat_size fields_layout);
      let before =
        Array.sub fields_layout 0 field
      in
      let size_before = flat_size before in
      Ok
        (Array.sub loc_exp size_before
           (Array.length fields_layout.(field)))
    | Cop (op, args, dbg) -> emit_expr_op env b op args dbg
    | Csequence (e1, e2) ->
      let* _ = emit_expr env b e1 in
      emit_expr env b e2
    | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg)
      ->
      emit_expr_ifthenelse env b econd eif eelse
    | Cswitch (esel, index, ecases, _dbg) ->
      emit_expr_switch env b esel index ecases
    | Ccatch (_, [], e1) -> emit_expr env b e1
    | Ccatch (rec_flag, handlers, body) ->
      emit_expr_catch env b rec_flag handlers body
    | Cexit (lbl, args, traps) ->
      emit_expr_exit env b lbl args traps
    | Cinvalid { message = _; symbol = _ } ->
      emit_block b Never;
      Never_returns

  and emit_tuple env b exp_list : or_never_returns =
    let rec loop acc = function
      | [] -> Ok (Array.concat (List.rev acc))
      | e :: rest ->
        let* r = emit_expr env b e in
        loop (r :: acc) rest
    in
    loop [] exp_list

  and emit_tuple_exprs env b exp_list : or_never_returns =
    emit_tuple env b exp_list

  and emit_expr_raise env b k args =
    let* r = emit_tuple env b args in
    emit_block b (Raise (k, r));
    Never_returns

  and emit_expr_op env b op args dbg : or_never_returns =
    let ty = Select_utils.oper_result_type op in
    let label_after = Cmm.new_label () in
    let new_op, new_args =
      select_operation op args dbg ~label_after
    in
    let* arg_instrs = emit_tuple env b new_args in
    match new_op with
    | Terminator (Call { op = call_op; label_after }) ->
      let pred_label = b.current_label in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      emit_block b
        (Call
           { op = call_op;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = exn_cont
           });
      start_block b cont_label
        (CallContinuation
           { predecessor = pred_label })
        ty;
      Ok (block_params cont_label ty)
    | Terminator
        (Prim
          { op = External ({ ty_res; _ } as ext_call);
            label_after
          }) ->
      let pred_label = b.current_label in
      let cont_label = label_after in
      let exn_cont = current_exn_continuation env in
      emit_block b
        (Prim
           { op = External ext_call;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = exn_cont
           });
      start_block b cont_label
        (CallContinuation
           { predecessor = pred_label })
        ty_res;
      Ok (block_params cont_label ty_res)
    | Terminator (Prim { op = Probe _ as probe_op; label_after })
      ->
      let pred_label = b.current_label in
      let cont_label = label_after in
      emit_block b
        (Prim
           { op = probe_op;
             args = arg_instrs;
             continuation = cont_label;
             exn_continuation = None
           });
      start_block b cont_label
        (CallContinuation
           { predecessor = pred_label })
        ty;
      Ok (block_params cont_label ty)
    | Terminator (Call_no_return _ext_call) ->
      emit_block b Never;
      Never_returns
    | Basic (Op (Alloc { bytes = _; mode; dbginfo = [ph] }))
      ->
      let* field_instrs = emit_tuple env b new_args in
      let n_fields = Array.length field_instrs in
      let alloc_words = n_fields in
      let bytes = alloc_words * Arch.size_addr in
      let alloc_op =
        Operation.Alloc
          { bytes;
            dbginfo =
              [{ ph with alloc_words; alloc_dbg = dbg }];
            mode
          }
      in
      let addr = add_op b alloc_op [||] in
      let byte_offset = ref (-Arch.size_int) in
      Array.iter
        (fun field ->
          let addressing_mode =
            Arch.offset_addressing
              Arch.identity_addressing !byte_offset
          in
          let chunk : Cmm.memory_chunk =
            match field with
            | Ssa.Op { op = Const_int _ | Const_symbol _; _ }
              ->
              Word_int
            | _ -> Word_val
          in
          ignore
            (add_op b
               (Store (chunk, addressing_mode, false))
               [| field; addr |]);
          byte_offset := !byte_offset + Arch.size_int)
        field_instrs;
      Ok [| addr |]
    | Basic (Op op) ->
      let i = add_op b op arg_instrs in
      if Array.length ty = 0
      then Ok [||]
      else Ok [| i |]
    | Basic basic ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)"
        Cfg.dump_basic basic
    | Terminator term ->
      Misc.fatal_errorf
        "Ssa_of_cmm: unexpected terminator (%a)"
        (Cfg.dump_terminator ~sep:"") term

  and emit_expr_ifthenelse env b econd eif eelse :
      or_never_returns =
    let cond, earg = select_condition econd in
    match emit_expr env b earg with
    | Never_returns -> Never_returns
    | Ok rarg ->
      let then_label = Cmm.new_label () in
      let else_label = Cmm.new_label () in
      let pred_label = b.current_label in
      let term =
        emit_branch b cond rarg ~true_label:then_label
          ~false_label:else_label
      in
      emit_block b term;
      let b_then =
        fork then_label
          (Merge
             { predecessors = [pred_label] })
          [||]
      in
      let r_then = emit_expr env b_then eif in
      let b_else =
        fork else_label
          (Merge
             { predecessors = [pred_label] })
          [||]
      in
      let r_else = emit_expr env b_else eelse in
      join_branches b r_then b_then r_else b_else

  and emit_expr_switch env b esel index ecases :
      or_never_returns =
    match emit_expr env b esel with
    | Never_returns -> Never_returns
    | Ok rsel ->
      let pred_label = b.current_label in
      let case_results =
        Array.map
          (fun (case_expr, _dbg) ->
            let case_label = Cmm.new_label () in
            let b_case =
              fork case_label
                (Merge
                   { predecessors = [pred_label] })
                [||]
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
      ignore rsel;
      emit_block b (Switch labels);
      join_array b case_results

  and emit_expr_catch env b rec_flag handlers catch_body :
      or_never_returns =
    let handlers_info =
      List.map
        (fun (handler : Cmm.static_handler) ->
          let nfail = handler.label in
          let ids = handler.params in
          let handler_body = handler.body in
          let handler_label = Cmm.new_label () in
          let types =
            List.map (fun (_id, ty) -> ty) ids
            |> Array.concat
          in
          let traps_ref = ref Unreachable in
          let handler_info =
            { handler_label; handler_types = types; traps_ref }
          in
          ( nfail,
            ids,
            handler_body,
            handler_info ))
        handlers
    in
    let env =
      List.fold_left
        (fun env (nfail, _ids, _body, info) ->
          { env with
            static_exceptions =
              Static_label.Map.add nfail info
                env.static_exceptions
          })
        env handlers_info
    in
    let body_label = Cmm.new_label () in
    let pred_label = b.current_label in
    let b_body =
      fork body_label
        (Merge
           { predecessors = [pred_label] })
        [||]
    in
    let r_body = emit_expr env b_body catch_body in
    emit_block b
      (Always { goto = body_label; args = [||] });
    let handler_results =
      List.map
        (fun (_, ids, handler_body, info) ->
          let trap_stack =
            match !(info.traps_ref) with
            | Unreachable -> env.trap_stack
            | Reachable ts -> ts
          in
          let handler_env =
            { env with trap_stack }
          in
          let handler_env =
            let param_idx = ref 0 in
            List.fold_left
              (fun env (id, ty) ->
                let n = Array.length ty in
                let proj_instrs =
                  Array.init n (fun i ->
                    (Ssa.Block_param
                       { block = info.handler_label;
                         index = !param_idx + i
                       }
                      : Ssa.instruction))
                in
                param_idx := !param_idx + n;
                env_add id proj_instrs env)
              handler_env ids
          in
          let block_desc : Ssa.block_desc =
            match rec_flag with
            | Cmm.Exn_handler ->
              TrapHandler { predecessors = [] }
            | Cmm.Normal | Cmm.Recursive ->
              Merge { predecessors = [] }
          in
          let b_handler =
            fork info.handler_label block_desc
              info.handler_types
          in
          let r =
            emit_expr handler_env b_handler handler_body
          in
          info.handler_label, r, b_handler)
        handlers_info
    in
    let all_results =
      Array.of_list
        ((body_label, r_body, b_body) :: handler_results)
    in
    join_array b all_results

  and emit_expr_exit env b (lbl : Cmm.exit_label) args traps :
      or_never_returns =
    match lbl with
    | Lbl nfail -> (
      let* src = emit_tuple env b args in
      let handler =
        try
          Static_label.Map.find nfail
            env.static_exceptions
        with Not_found ->
          Misc.fatal_errorf
            "Ssa_of_cmm: unbound label %a"
            Static_label.format nfail
      in
      List.iter
        (fun (trap : Cmm.trap_action) ->
          match trap with
          | Push handler_id -> (
            let h =
              try
                Static_label.Map.find handler_id
                  env.static_exceptions
              with Not_found ->
                Misc.fatal_errorf
                  "Ssa_of_cmm: unbound trap handler %a"
                  Static_label.format handler_id
            in
            add_body b
              (Pushtrap
                 { lbl_handler = h.handler_label }))
          | Pop handler_id -> (
            let h =
              try
                Static_label.Map.find handler_id
                  env.static_exceptions
              with Not_found ->
                Misc.fatal_errorf
                  "Ssa_of_cmm: unbound trap handler %a"
                  Static_label.format handler_id
            in
            add_body b
              (Poptrap
                 { lbl_handler = h.handler_label })))
        traps;
      let new_trap_stack =
        combine_traps env.trap_stack traps
      in
      (match !(handler.traps_ref) with
      | Unreachable ->
        handler.traps_ref := Reachable new_trap_stack
      | Reachable _ -> ());
      emit_block b
        (Always
           { goto = handler.handler_label; args = src });
      Never_returns)
    | Return_lbl ->
      let* src = emit_tuple env b args in
      emit_block b (Return src);
      Never_returns

  and emit_tail env b (exp : Cmm.expression) : unit =
    match emit_expr env b exp with
    | Never_returns -> ()
    | Ok r ->
      emit_block b (Return r)

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
      b.body <- b1.body;
      Ok r
    | Never_returns, Ok r ->
      merge_into b b1;
      merge_into b b2;
      b.current_label <- b2.current_label;
      b.current_desc <- b2.current_desc;
      b.body <- b2.body;
      Ok r
    | Ok r1_instrs, Ok r2_instrs ->
      let join_label = Cmm.new_label () in
      let n = Array.length r1_instrs in
      assert (n = Array.length r2_instrs);
      let join_types =
        Array.init n (fun _i -> Cmm.Val)
      in
      emit_block b1
        (Always { goto = join_label; args = r1_instrs });
      emit_block b2
        (Always { goto = join_label; args = r2_instrs });
      let preds =
        [b1.current_label; b2.current_label]
      in
      merge_into b b1;
      merge_into b b2;
      start_block b join_label
        (Merge { predecessors = preds })
        join_types;
      if n = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  and join_array b
      (results :
        (Label.t * or_never_returns * builder) array) :
      or_never_returns =
    let some_ok = ref None in
    Array.iter
      (fun (_, r, _) ->
        match r with
        | Never_returns -> ()
        | Ok instrs -> (
          match !some_ok with
          | None -> some_ok := Some (Array.length instrs)
          | Some n ->
            assert (n = Array.length instrs)))
      results;
    match !some_ok with
    | None ->
      Array.iter
        (fun (_, _, sub) -> merge_into b sub)
        results;
      Never_returns
    | Some n ->
      let join_label = Cmm.new_label () in
      let join_types =
        Array.init n (fun _i -> Cmm.Val)
      in
      let preds = ref [] in
      Array.iter
        (fun (_, r, sub) ->
          (match r with
          | Never_returns -> ()
          | Ok instrs ->
            preds := sub.current_label :: !preds;
            emit_block sub
              (Always
                 { goto = join_label; args = instrs }));
          merge_into b sub)
        results;
      start_block b join_label
        (Merge
           { predecessors = List.rev !preds })
        join_types;
      if n = 0
      then Ok [||]
      else Ok (block_params join_label join_types)

  let emit_fundecl (f : Cmm.fundecl) : Ssa.t =
    let entry_label = Cmm.new_label () in
    let fun_arg_types =
      List.map snd f.fun_args |> Array.concat
    in
    let tailrec_label = Cmm.new_label () in
    let env =
      { vars = V.Map.empty;
        static_exceptions = Static_label.Map.empty;
        trap_stack = Operation.Uncaught;
        tailrec_label
      }
    in
    let env, _offset =
      List.fold_left
        (fun (env, offset) (id, ty) ->
          let n = Array.length ty in
          let projs =
            Array.init n (fun i ->
              (Ssa.Block_param
                 { block = entry_label;
                   index = offset + i
                 }
                : Ssa.instruction))
          in
          env_add id projs env, offset + n)
        (env, 0) f.fun_args
    in
    let b =
      fork entry_label FunctionStart fun_arg_types
    in
    emit_tail env b f.fun_body;
    let blocks = Label.Tbl.create 31 in
    List.iter
      (fun (block : Ssa.basic_block) ->
        Label.Tbl.add blocks block.label block)
      b.blocks;
    { Ssa.blocks;
      fun_name = f.fun_name.sym_name;
      fun_args = fun_arg_types;
      fun_codegen_options = f.fun_codegen_options;
      fun_dbg = f.fun_dbg;
      entry_label;
      fun_contains_calls = true;
      fun_poll = f.fun_poll;
      fun_ret_type = f.fun_ret_type
    }
end
