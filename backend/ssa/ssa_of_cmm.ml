(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2026 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** Cmm → SSA conversion.

    Walks a [Cmm.fundecl] in expression order, emitting into an
    [under_construction Ssa.graph]. Operation-level lowering is delegated to
    {!Cfg_selectgen} via [Cfg_selectgen.select_operation].

    Trap handling: [Cexit]'s trap actions are emitted as [Push_trap] /
    [Pop_trap] body instructions. In contrast to {!Cfg_selectgen}, we do not
    compute trap stacks and trap continuations, relying on the SSA machinery to
    do this automatically instead.

    Also in contrast to Cfg_selectgen, we do not emit tail calls.
    {!Ssa_tail_call} later rewrites call+return patterns into a self-recursive
    [Continue (Goto entry)] back-edge or a tail [Call] (continuation [Return])
    when possible. The [Capply]'s [region_close] is forwarded as the
    [Call.nontail] flag so [@nontail] annotations can suppress the rewrite. *)

module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance
module Sel = Cfg_selectgen.Make (Cfg_selection)
module Or_never_returns = SU.Or_never_returns
open! Or_never_returns.Syntax
open Ssa.Export

type block = under_construction Block.t

type value = under_construction Ssa.Value.t

type result = value array Or_never_returns.t

type env =
  { graph : under_construction Ssa.graph;
    vars : value array V.Map.t;
    static_exceptions : block Static_label.Map.t
  }

let env_find v env =
  try V.Map.find v env.vars
  with Not_found -> Misc.fatal_errorf "Ssa_of_cmm: unbound var %a" V.print v

let env_add v instrs env =
  { env with vars = V.Map.add (VP.var v) instrs env.vars }

let size_of_cmm_expr env e =
  SU.size_expr_with e ~size_of_var:(fun id ->
      env_find id env
      |> Array.fold_left (fun acc i -> acc + SU.size_component (Value.typ i)) 0)

let emit_op env c op dbg typ args = Cursor.emit_op env.graph c op dbg typ args

(* Emit an operation that produces no results, e.g. a store or a
   name-for-debugger marker. *)
let emit_op_nores env c op dbg args =
  ignore (emit_op env c op dbg [||] args : value array)

let finish_block env c ~dbg term = Cursor.finish_block env.graph c ~dbg term

let new_block env ~params = Block.create env.graph ~params

let bind_let env c v args =
  let env = env_add v args env in
  let name = V.name (VP.var v) in
  Array.iter (fun i -> Value.set_name i name) args;
  (match VP.provenance v with
  | None -> ()
  | Some _ as provenance ->
    emit_op_nores env c
      (Operation.Name_for_debugger
         { ident = VP.var v; which_parameter = None; provenance; regs = [||] })
      Debuginfo.none args);
  env

let emit_branch env c (test : Operation.test) rarg ~true_block ~false_block =
  let make_cond op args =
    (emit_op env c op Debuginfo.none Cmm.typ_int args).(0)
  in
  (* A boolean branch is a two-target switch: [targets.(0)] is the false edge,
     [targets.(1)] the true edge. The condition is always a 0/1 comparison. *)
  let branch index ~true_block ~false_block : under_construction Terminator.t =
    Switch { index; targets = [| false_block; true_block |] }
  in
  let term : under_construction Terminator.t =
    match test with
    | Itruetest ->
      branch
        (make_cond (Intop_imm (Icomp Cne, 0)) [| rarg.(0) |])
        ~true_block ~false_block
    | Ifalsetest ->
      branch
        (make_cond (Intop_imm (Icomp Ceq, 0)) [| rarg.(0) |])
        ~true_block ~false_block
    | Iinttest cmp ->
      branch (make_cond (Intop (Icomp cmp)) rarg) ~true_block ~false_block
    | Iinttest_imm (cmp, n) ->
      branch
        (make_cond (Intop_imm (Icomp cmp, n)) [| rarg.(0) |])
        ~true_block ~false_block
    | Ifloattest (w, cmp) ->
      branch (make_cond (Floatop (w, Icompf cmp)) rarg) ~true_block ~false_block
    | Ioddtest ->
      branch
        (make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |])
        ~true_block ~false_block
    | Ieventest ->
      branch
        (make_cond (Intop_imm (Iand, 1)) [| rarg.(0) |])
        ~true_block:false_block ~false_block:true_block
  in
  finish_block env c ~dbg:Debuginfo.none term

let rec emit_parts_list env c exp_list :
    (Cmm.expression list * env) Or_never_returns.t =
  SU.emit_parts_list ~effects_of:Sel.effects_of
    ~is_simple_expr:Sel.is_simple_expr
    ~emit:(fun env exp -> emit env c exp ~tail:false)
    ~bind_result:(fun env id r -> { env with vars = V.Map.add id r env.vars })
    env exp_list

and emit_tuple env c exp_list : result =
  let rec emit_list : Cmm.expression list -> value array list Or_never_returns.t
      = function
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
      (emit_op env c
         (SU.make_const_int (Nativeint.of_int !byte_offset))
         dbg Cmm.typ_int [||]).(0)
    in
    assert (!byte_offset > 0);
    let new_base =
      (emit_op env c (Operation.Intop Iadd) dbg Cmm.typ_addr [| !base; tmp |]).(0)
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
    match[@warning "-fragile-match"] op with
    | Store (_, _, _) -> true
    | _ -> false
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
          (fun (r : value) ->
            let chunk = SU.chunk_of_machtype_component (Value.typ r) in
            emit_op_nores env c
              (Operation.Store (chunk, !addressing_mode, false))
              dbg [| r; !base |];
            advance (Select_utils.size_component (Value.typ r)))
          regs
      | Some op ->
        emit_op_nores env c op dbg (Array.append regs [| !base |]);
        advance (size_of_cmm_expr env original_arg))
    | Never_returns -> Misc.fatal_error "Ssa_of_cmm.emit_stores: never_returns"
  in
  List.iter for_one_arg args

and emit env c (exp : Cmm.expression) ~tail : result =
  let* r =
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
        (emit_op env c
           (SU.make_const_int (Nativeint.of_int n))
           Debuginfo.none Cmm.typ_int [||])
    | Cconst_natint (n, _dbg) ->
      Ok (emit_op env c (SU.make_const_int n) Debuginfo.none Cmm.typ_int [||])
    | Cconst_float32 (n, _dbg) ->
      Ok
        (emit_op env c
           (SU.make_const_float32 (Int32.bits_of_float n))
           Debuginfo.none Cmm.typ_float32 [||])
    | Cconst_float (n, _dbg) ->
      Ok
        (emit_op env c
           (SU.make_const_float (Int64.bits_of_float n))
           Debuginfo.none Cmm.typ_float [||])
    | Cconst_vec128 (bits, _dbg) ->
      Ok
        (emit_op env c
           (SU.make_const_vec128 bits)
           Debuginfo.none Cmm.typ_vec128 [||])
    | Cconst_vec256 (bits, _dbg) ->
      Ok
        (emit_op env c (Operation.Const_vec256 bits) Debuginfo.none
           Cmm.typ_vec256 [||])
    | Cconst_vec512 (bits, _dbg) ->
      Ok
        (emit_op env c (Operation.Const_vec512 bits) Debuginfo.none
           Cmm.typ_vec512 [||])
    | Cconst_symbol (n, _dbg) ->
      Ok
        (emit_op env c (SU.make_const_symbol n) Debuginfo.none Cmm.typ_int [||])
    | Cvar v -> Ok (env_find v env)
    | Ctuple [] -> Ok [||]
    | Ctuple exp_list ->
      let* simple_list, ext_env = emit_parts_list env c exp_list in
      emit_tuple ext_env c simple_list
    | Cop (Craise k, args, dbg) ->
      let* r = emit_tuple env c args in
      finish_block env c ~dbg (Continue { continuation = Raise k; args = r });
      Never_returns
    | Cop (Copaque, args, dbg) ->
      let* simple_args, env = emit_parts_list env c args in
      let* rs = emit_tuple env c simple_args in
      let typ = Array.map Value.typ rs in
      Ok (emit_op env c Opaque dbg typ rs)
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) ->
      let* loc_exp = emit env c arg ~tail:false in
      let flat_size a =
        Array.fold_left (fun acc t -> acc + Array.length t) 0 a
      in
      assert (Array.length loc_exp = flat_size fields_layout);
      let before = Array.sub fields_layout 0 field in
      let size_before = flat_size before in
      Ok (Array.sub loc_exp size_before (Array.length fields_layout.(field)))
    | Cop
        ( (( Ctuple_field _ | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Caddi128
           | Csubi128 | Cand | Cor | Cxor | Clsl | Clsr | Casr | Cclz | Cctz
           | Cpopcnt | Caddv | Cadda | Cpackf32 | Cbeginregion | Cendregion
           | Cdls_get | Ctls_get | Cdomain_index | Cpoll | Cpause | Capply _
           | Cextcall _ | Cload _
           | Calloc (_, _)
           | Cstore (_, _)
           | Cmulhi _ | Cmuli64 _ | Cbswap _ | Ccsel _ | Cprefetch _ | Catomic _
           | Ccmpi _ | Cnegf _ | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _
           | Creinterpret_cast _ | Cstatic_cast _
           | Ccmpf (_, _)
           | Cprobe _ | Cprobe_is_enabled _ ) as op),
          args,
          dbg ) ->
      emit_expr_op env c op args dbg
    | Cexit (lbl, args, traps) -> emit_expr_exit env c lbl args traps
    | Cinvalid { message; symbol } -> emit_invalid env c message symbol
  in
  if tail
  then (
    finish_block env c ~dbg:Debuginfo.none
      (Continue { continuation = Return; args = r });
    Never_returns)
  else Ok r

and emit_invalid env c message symbol =
  let arg_expr = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
  let* arg_instrs = emit_tuple env c [arg_expr] in
  let function_info = Ssa.function_info env.graph in
  let current_function_is_check_enabled =
    Zero_alloc_checker.is_check_enabled function_info.codegen_options
      function_info.sym_name function_info.dbg
  in
  if current_function_is_check_enabled
  then (
    let cont_block = new_block env ~params:Cmm.typ_int in
    finish_block env c ~dbg:Debuginfo.none
      (Invalid { message; args = arg_instrs; continuation = Some cont_block });
    Cursor.move c ~new_pos:cont_block;
    Ok (Block.params cont_block))
  else (
    finish_block env c ~dbg:Debuginfo.none
      (Invalid { message; args = arg_instrs; continuation = None });
    Never_returns)

and emit_call env c ~ty ~nontail (new_op : Cfg.terminator) arg_instrs dbg :
    result =
  let may_raise = Cfg.can_raise_terminator new_op in
  let call_returning_to op (ty : Cmm.machtype) : result =
    let cont_block = new_block env ~params:ty in
    finish_block env c ~dbg
      (Call
         { op;
           args = arg_instrs;
           continuation = Goto cont_block;
           may_raise;
           nontail
         });
    Cursor.move c ~new_pos:cont_block;
    Ok (Block.params cont_block)
  in
  match new_op with
  | Call { op = Direct sym; _ } -> call_returning_to (Direct sym) ty
  | Call { op = Indirect candidates; _ } ->
    call_returning_to (Indirect candidates) ty
  | Prim { op = External ({ ty_res; _ } as ext_call); _ } ->
    call_returning_to (External ext_call) ty_res
  | Prim { op = Probe { name; handler_code_sym; enabled_at_init }; _ } ->
    call_returning_to (Probe { name; handler_code_sym; enabled_at_init }) ty
  | Call_no_return ext_call ->
    finish_block env c ~dbg
      (Call
         { op = External ext_call;
           args = arg_instrs;
           continuation = Unreachable;
           may_raise;
           nontail
         });
    Never_returns
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
      List.fold_left (fun acc arg -> acc + size_of_cmm_expr env arg) 0 new_args
    in
    let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
    let op =
      Operation.Alloc
        { bytes = alloc_words * Arch.size_addr;
          dbginfo = [{ placeholder with alloc_words; alloc_dbg = dbg }];
          mode
        }
    in
    let rd = (emit_op env c op dbg Cmm.typ_val [||]).(0) in
    emit_stores env c dbg new_args rd;
    Ok [| rd |]
  | Basic (Op (Alloc _)) ->
    Misc.fatal_error "Alloc is expected to have exactly one dbginfo"
  | Terminator _
  | Basic
      ( Op
          ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
          | Tls_get | Domain_index | Poll | Pause | Const_int _
          | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
          | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Load _
          | Store (_, _, _)
          | Intop _ | Int128op _
          | Intop_imm (_, _)
          | Intop_atomic _
          | Floatop (_, _)
          | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
          | Specific _ | Name_for_debugger _ )
      | Reloadretaddr | Prologue | Epilogue | Pushtrap _ | Poptrap _
      | Stack_check _ ) -> (
    let* arg_instrs = emit_tuple env c new_args in
    match new_op with
    | Terminator term ->
      let nontail =
        match[@warning "-fragile-match"] op with
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
    | Basic (Op op) -> Ok (emit_op env c op dbg ty arg_instrs)
    | Basic
        (( Reloadretaddr | Prologue | Epilogue | Pushtrap _ | Poptrap _
         | Stack_check _ ) as basic) ->
      Misc.fatal_errorf "Ssa_of_cmm: unexpected basic (%a)" Cfg.dump_basic basic
    )

and emit_ifthenelse env c ~tail econd eif eelse : result =
  let cond, earg = Sel.select_condition econd in
  let* rarg = emit env c earg ~tail:false in
  let then_block = new_block env ~params:[||] in
  let else_block = new_block env ~params:[||] in
  emit_branch env c cond rarg ~true_block:then_block ~false_block:else_block;
  let then_c = Cursor.start then_block in
  let r_then = emit env then_c eif ~tail in
  let else_c = Cursor.start else_block in
  let r_else = emit env else_c eelse ~tail in
  join env c [| r_then, then_c; r_else, else_c |]

and emit_switch env c ~tail esel index ecases : result =
  let* rsel = emit env c esel ~tail:false in
  let case_blocks =
    Array.map (fun (_case_expr, _dbg) -> new_block env ~params:[||]) ecases
  in
  let targets = Array.map (fun idx -> case_blocks.(idx)) index in
  let index =
    assert (Array.length rsel = 1);
    rsel.(0)
  in
  finish_block env c ~dbg:Debuginfo.none (Switch { index; targets });
  let case_results =
    Array.mapi
      (fun i (case_expr, _dbg) ->
        let case_c = Cursor.start case_blocks.(i) in
        emit env case_c case_expr ~tail, case_c)
      ecases
  in
  join env c case_results

and emit_catch env c ~tail handlers body : result =
  (* Create one block per handler up front to also support mutually recursive
     handlers. All handlers are emitted unconditionally, the SSA graph builder
     will drop unreachable blocks. *)
  let static_exceptions, handler_blocks =
    List.fold_left_map
      (fun static_exceptions Cmm.{ label = nfail; params; _ } ->
        let types = params |> List.map (fun (_id, ty) -> ty) |> Array.concat in
        let handler_block = new_block env ~params:types in
        ( Static_label.Map.add nfail handler_block static_exceptions,
          handler_block ))
      env.static_exceptions handlers
  in
  let env = { env with static_exceptions } in
  let r_body = emit env c body ~tail in
  let translate_handler (handler : Cmm.static_handler)
      (handler_block : under_construction Block.t) =
    let c = Cursor.start handler_block in
    (* Bind each handler param to its slice of the flattened block params;
       [bind_let] also sets the param names and emits the [Name_for_debugger]
       markers. *)
    let handler_env, _ =
      List.fold_left
        (fun (env, param_idx) (id, ty) ->
          let n = Array.length ty in
          let proj_instrs =
            Array.init n (fun i -> Block.param handler_block (param_idx + i))
          in
          bind_let env c id proj_instrs, param_idx + n)
        (env, 0) handler.params
    in
    emit handler_env c handler.body ~tail, c
  in
  let handler_results = List.map2 translate_handler handlers handler_blocks in
  join env c (Array.of_list ((r_body, c) :: handler_results))

and find_handler env handler_id : block =
  try Static_label.Map.find handler_id env.static_exceptions
  with Not_found ->
    Misc.fatal_errorf "Ssa_of_cmm: unbound trap handler %a" Static_label.format
      handler_id

and emit_trap_actions env c traps =
  traps
  |> List.iter (fun (trap : Cmm.trap_action) ->
      match trap with
      | Push handler_id ->
        Cursor.emit_push_trap c ~handler:(find_handler env handler_id)
      | Pop handler_id ->
        Cursor.emit_pop_trap c ~handler:(find_handler env handler_id))

and emit_expr_exit env c (lbl : Cmm.exit_label) args traps : result =
  let* simple_list, ext_env = emit_parts_list env c args in
  match lbl with
  | Lbl nfail ->
    let* src = emit_tuple ext_env c simple_list in
    let handler = find_handler env nfail in
    emit_trap_actions env c traps;
    finish_block env c ~dbg:Debuginfo.none
      (Continue { continuation = Goto handler; args = src });
    Never_returns
  | Return_lbl ->
    let* src = emit_tuple ext_env c simple_list in
    emit_trap_actions env c traps;
    finish_block env c ~dbg:Debuginfo.none
      (Continue { continuation = Return; args = src });
    Never_returns

(* Join a set of branches (each with its own end-cursor) into a fresh block. [c]
   is left pointing at the joined block, or unchanged if no branch returns. *)
and join env c (results : (result * Cursor.t) array) : result =
  let join_info = ref None in
  results
  |> Array.iter (fun ((r, _) : result * Cursor.t) ->
      match r with
      | Never_returns -> ()
      | Ok instrs -> (
        let types = Array.map Value.typ instrs in
        match !join_info with
        | None -> join_info := Some types
        | Some prev ->
          (* Different paths may produce values of compatible but distinct
             machtype components. Pick the least upper bound so the joined
             block's param types accommodate every incoming arm. *)
          assert (Array.length prev = Array.length types);
          let lub = Array.map2 Cmm.lub_component prev types in
          join_info := Some lub));
  match !join_info with
  | None -> Never_returns
  | Some join_types ->
    let join_block = new_block env ~params:join_types in
    results
    |> Array.iter (fun ((r, c_branch) : result * Cursor.t) ->
        match r with
        | Never_returns -> ()
        | Ok instrs ->
          finish_block env c_branch ~dbg:Debuginfo.none
            (Continue { continuation = Goto join_block; args = instrs }));
    Cursor.move c ~new_pos:join_block;
    Ok (Block.params join_block)

let emit_function (graph : under_construction Ssa.graph) (cmm : Cmm.fundecl) :
    finished Ssa.graph =
  let env =
    { graph; vars = V.Map.empty; static_exceptions = Static_label.Map.empty }
  in
  let env, _offset =
    List.fold_left
      (fun (env, offset) (id, ty) ->
        let n = Array.length ty in
        let projs =
          Array.init n (fun i -> Block.param (Ssa.entry graph) (offset + i))
        in
        env_add id projs env, offset + n)
      (env, 0) cmm.fun_args
  in
  let r = emit env (Cursor.start (Ssa.entry graph)) cmm.fun_body ~tail:true in
  assert (match r with Never_returns -> true | Ok _ -> false);
  Ssa.finish_graph graph

let convert (cmm : Cmm.fundecl) ~keep_unused_ops : finished Ssa.graph =
  try
    let function_info : Ssa.Function_info.t =
      { sym_name = cmm.fun_name.sym_name;
        parameters = cmm.fun_args;
        codegen_options = cmm.fun_codegen_options;
        dbg = cmm.fun_dbg;
        poll = cmm.fun_poll;
        ret_type = cmm.fun_ret_type
      }
    in
    let g = Ssa.create_graph function_info ~keep_unused_ops in
    emit_function g cmm
  with Misc.Fatal_error as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "*** Ssa_of_cmm error for %s: %s@.*** CMM:@.%a@."
      cmm.fun_name.sym_name (Printexc.to_string exn) Printcmm.fundecl cmm;
    Printexc.raise_with_backtrace exn bt
