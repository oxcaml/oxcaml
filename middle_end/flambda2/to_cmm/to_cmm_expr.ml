(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module Env = To_cmm_env
module Ece = Effects_and_coeffects
module K = Flambda_kind

module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

(* Note about flushing of environments: this module treats the delayed bindings
   in [To_cmm_env] environments (see to_cmm_env.mli for more information) in a
   linear manner. Flushes are inserted to preserve this property. This ensures
   in particular that effectful bindings get placed exactly once and that other
   bindings are not duplicated. *)

(* Bind a Cmm variable to the result of translating a [Simple] into Cmm. *)

let bind_var_to_simple ~dbg_with_inlined:dbg env res v
    ~num_normal_occurrences_of_bound_vars s =
  match Simple.must_be_var s with
  | Some (alias_of, _coercion) ->
    Env.add_alias env res ~var:v ~num_normal_occurrences_of_bound_vars ~alias_of
  | None ->
    let To_cmm_env.
          { env;
            res;
            expr =
              { cmm = defining_expr;
                free_vars = free_vars_of_defining_expr;
                effs = effects_and_coeffects_of_defining_expr
              }
          } =
      C.simple ~dbg env res s
    in
    let env, res =
      Env.bind_variable env res v ~effects_and_coeffects_of_defining_expr
        ~defining_expr ~free_vars_of_defining_expr
        ~num_normal_occurrences_of_bound_vars
    in
    env, res

(* Helpers for the translation of [Apply] expressions. *)

let warn_if_unused_inlined_attribute apply ~dbg_with_inlined =
  match Inlined_attribute.use_info (Apply.inlined apply) with
  | None -> ()
  | Some use_info ->
    let reason =
      Format.asprintf "\n  %s  (the full inlining stack was: %a)"
        (match Inlined_attribute.Use_info.explanation use_info with
        | None -> ""
        | Some explanation -> explanation ^ "\n")
        Debuginfo.print_compact dbg_with_inlined
    in
    Location.prerr_warning
      (Debuginfo.to_location dbg_with_inlined)
      (Warnings.Inlining_impossible reason)

let fail_if_probe apply =
  match Apply.probe apply with
  | None -> ()
  | Some _ ->
    Misc.fatal_errorf
      "[Apply] terms with a [probe] (i.e. that call a tracing probe) must \
       always be direct applications of an OCaml function:@ %a"
      Apply.print apply

let translate_external_call env res ~free_vars apply ~callee_simple ~args
    ~return_arity ~return_ty dbg ~needs_caml_c_call ~is_c_builtin ~effects
    ~coeffects =
  fail_if_probe apply;
  let callee =
    match callee_simple with
    | None ->
      Misc.fatal_errorf
        "Application expression did not provide callee for C call:@ %a"
        Apply.print apply
    | Some callee_simple -> (
      match Simple.must_be_symbol callee_simple with
      | Some (sym, _) -> (To_cmm_result.symbol res sym).sym_name
      | None ->
        Misc.fatal_errorf "Expected a function symbol instead of:@ %a"
          Simple.print callee_simple)
  in
  let returns = Apply.returns apply in
  let return_ty = C.Extended_machtype.to_machtype return_ty in
  let component_tys =
    (* Two notes:

       1. void has been erased in return arities by this point

       2. All of the [machtype_component]s are singleton arrays. *)
    Array.map (fun machtype -> [| machtype |]) return_ty
  in
  (* Returned small integer values need to be sign-extended because it's not
     clear whether C code that returns a small integer returns one that is sign
     extended or not. There is no need to wrap other return arities. *)
  let maybe_sign_extend kind dbg cmm =
    match Flambda_kind.With_subkind.kind kind with
    | Naked_number Naked_int8 -> C.sign_extend ~bits:8 ~dbg cmm
    | Naked_number Naked_int16 -> C.sign_extend ~bits:16 ~dbg cmm
    | Naked_number Naked_int32 -> C.sign_extend ~bits:32 ~dbg cmm
    | Naked_number
        ( Naked_float | Naked_immediate | Naked_int64 | Naked_nativeint
        | Naked_vec128 | Naked_vec256 | Naked_vec512 | Naked_float32 )
    | Value | Rec_info | Region ->
      cmm
  in
  let ty_args =
    List.map C.exttype_of_kind
      (Flambda_arity.unarize (Apply.args_arity apply)
      |> List.map K.With_subkind.kind)
  in
  let effects = To_cmm_effects.transl_c_call_effects effects in
  let coeffects = To_cmm_effects.transl_c_call_coeffects coeffects in
  let extcall =
    C.extcall ~dbg ~alloc:needs_caml_c_call ~is_c_builtin ~effects ~coeffects
      ~returns ~ty_args callee return_ty args
  in
  let wrap return_values =
    let kinds = Flambda_arity.unarized_components return_arity in
    (* As per the comment above, [return_arity] does not mention void
       components. (Unlike parameter arities; see the phantom type parameters on
       the arity fields in [Apply_expr.t], for example.) *)
    assert (List.compare_length_with kinds (Array.length component_tys) = 0);
    match kinds with
    | [] ->
      (* CR mshinwell: this statement would seem to be wrong if we permit void
         returns from extcalls *)
      (* Extcalls of arity 0 are allowed (these never return). *)
      return_values
    | [kind] -> maybe_sign_extend kind dbg return_values
    | [_; _] as kinds ->
      (* CR xclerc: we currently support only pairs as unboxed return values. *)
      (* CR mshinwell: we also currently only support 64 bit integer and float
         values, since on (at least) x86-64 the calling convention differs for
         smaller widths. *)
      List.iter
        (fun kind ->
          match Flambda_kind.With_subkind.kind kind with
          | Naked_number
              (Naked_immediate | Naked_int64 | Naked_nativeint | Naked_float) ->
            ()
          | Naked_number
              ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_vec128
              | Naked_vec256 | Naked_vec512 | Naked_float32 )
          | Value | Region | Rec_info ->
            Misc.fatal_errorf
              "Cannot compile unboxed product return from external C call with \
               a component of kind %a"
              Flambda_kind.With_subkind.print kind)
        kinds;
      (* CR mshinwell: Digest page 35 of this doc:

         https://github.com/ARM-software/abi-aa/releases/download/2024Q3/aapcs64.pdf

         and figure out what happens for mixed int/float struct returns (it
         looks like the floats may be returned in int regs)

         jvanburen: that seems to be what clang does:
         https://godbolt.org/z/snzEoME9h *)
      (match Target_system.architecture () with
      | X86_64 -> ()
      | AArch64 ->
        let kinds = Flambda_kind.With_subkind.Set.of_list kinds in
        if Flambda_kind.With_subkind.Set.cardinal kinds <> 1
        then
          Misc.fatal_errorf
            "Cannot compile unboxed product return from external C call on \
             arm64 unless the components of the product are of the same kind:@ \
             %a"
            Apply.print apply
      | IA32 | ARM | POWER | Z | Riscv ->
        Misc.fatal_error "Only x86-64 and arm64 are supported");
      let get_unarized_return_value exp n =
        C.tuple_field exp ~component_tys n dbg
      in
      C.make_tuple
        (List.mapi
           (fun i kind ->
             maybe_sign_extend kind dbg
               (get_unarized_return_value return_values i))
           kinds)
    | _ ->
      Misc.fatal_errorf
        "C functions are currently limited to a single return value or a pair \
         of return values"
  in
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let cmm =
    C.letin extcall_var ~defining_expr:extcall ~body:(wrap (Cvar extcall_ident))
  in
  cmm, free_vars, env, res, Ece.all

let translate_apply0 ~dbg_with_inlined:dbg env res apply =
  let callee_simple = Apply.callee apply in
  let args = Apply.args apply in
  (* CR mshinwell: When we fix the problem that [prim_effects] and
     [prim_coeffects] are ignored for C calls, we need to take into account the
     effects/coeffects values currently ignored on the following two lines. At
     the moment they can be ignored as we always deem all calls to have
     arbitrary effects and coeffects. *)
  let env, res, callee, callee_free_vars =
    match callee_simple with
    | Some callee_simple ->
      let To_cmm_env.
            { env;
              res;
              expr = { cmm = callee; free_vars = callee_free_vars; effs = _ }
            } =
        C.simple ~dbg env res callee_simple
      in
      env, res, Some callee, callee_free_vars
    | None -> env, res, None, Backend_var.Set.empty
  in
  let args, args_free_vars, env, res, _ = C.simple_list ~dbg env res args in
  let free_vars = Backend_var.Set.union callee_free_vars args_free_vars in
  let pos =
    match Apply.position apply with
    | Normal ->
      (* We always use [Rc_normal] since the [Lambda_to_flambda] pass has
         already taken care of the placement of region begin/end primitives. *)
      Lambda.Rc_normal
    | Nontail -> Lambda.Rc_nontail
  in
  let args_arity =
    Apply.args_arity apply |> Flambda_arity.unarize_per_parameter
  in
  let return_arity = Apply.return_arity apply in
  let args_ty =
    List.map
      (fun kinds -> List.map C.extended_machtype_of_kind kinds |> Array.concat)
      args_arity
  in
  let split_args () =
    let rec aux args args_arity =
      match args_arity, args with
      | [], [] -> []
      | [], _ :: _ ->
        Misc.fatal_errorf
          "[split_args]: [args] and [args_ty] do not have compatible lengths"
      | kinds :: args_arity, args ->
        let group, rest =
          Misc.Stdlib.List.map2_prefix (fun _kind arg -> arg) kinds args
        in
        C.make_tuple group :: aux rest args_arity
    in
    aux args args_arity
  in
  let return_ty = C.extended_machtype_of_return_arity return_arity in
  match Apply.call_kind apply with
  | Function { function_call = Direct code_id; alloc_mode = _ } -> (
    let code_metadata = Env.get_code_metadata env code_id in
    let params_arity = Code_metadata.params_arity code_metadata in
    if not (C.check_arity params_arity args)
    then Misc.fatal_errorf "Wrong arity for direct call";
    let args =
      if Code_metadata.is_my_closure_used code_metadata
      then
        let callee =
          match callee with
          | Some callee -> callee
          | None ->
            Misc.fatal_errorf
              "Need callee to compile call to@ %a@ but application expression \
               did not supply one:@ %a"
              Code_metadata.print code_metadata Apply.print apply
        in
        args @ [callee]
      else args
    in
    let code_sym =
      To_cmm_result.symbol_of_code_id res code_id
        ~currently_in_inlined_body:(Env.currently_in_inlined_body env)
    in
    match Apply.probe apply with
    | None ->
      ( C.direct_call ~dbg
          (C.Extended_machtype.to_machtype return_ty)
          pos (C.symbol ~dbg code_sym) args,
        free_vars,
        env,
        res,
        Ece.all )
    | Some { name; enabled_at_init } ->
      ( C.probe ~dbg ~name ~handler_code_linkage_name:code_sym.sym_name ~args
          ~enabled_at_init
        |> C.return_unit dbg,
        free_vars,
        env,
        res,
        Ece.all ))
  | Function { function_call = Indirect_unknown_arity; alloc_mode } ->
    fail_if_probe apply;
    let callee =
      match callee with
      | Some callee -> callee
      | None ->
        Misc.fatal_errorf
          "Application expression did not provide callee for indirect call:@ %a"
          Apply.print apply
    in
    ( C.indirect_call ~dbg return_ty pos
        (C.alloc_mode_for_applications_to_cmx alloc_mode)
        callee args_ty (split_args ()),
      free_vars,
      env,
      res,
      Ece.all )
  | Function { function_call = Indirect_known_arity; alloc_mode } ->
    fail_if_probe apply;
    let callee =
      match callee with
      | Some callee -> callee
      | None ->
        Misc.fatal_errorf
          "Application expression did not provide callee for indirect call:@ %a"
          Apply.print apply
    in
    if not (C.check_arity (Apply.args_arity apply) args)
    then
      Misc.fatal_errorf
        "To_cmm expects indirect_known_arity calls to be full applications in \
         order to translate them"
    else
      ( C.indirect_full_call ~dbg return_ty pos
          (C.alloc_mode_for_applications_to_cmx alloc_mode)
          callee args_ty args,
        free_vars,
        env,
        res,
        Ece.all )
  | C_call
      { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode = _ } ->
    translate_external_call env res ~free_vars apply ~callee_simple ~args
      ~return_arity ~return_ty dbg ~needs_caml_c_call ~is_c_builtin ~effects
      ~coeffects
  | Method { kind; obj; alloc_mode } ->
    fail_if_probe apply;
    let callee =
      match callee with
      | Some callee -> callee
      | None ->
        Misc.fatal_errorf
          "Application expression did not provide callee for method call:@ %a"
          Apply.print apply
    in
    let To_cmm_env.
          { env;
            res;
            expr = { cmm = obj; free_vars = obj_free_vars; effs = _ }
          } =
      C.simple ~dbg env res obj
    in
    let free_vars = Backend_var.Set.union free_vars obj_free_vars in
    let kind = Call_kind.Method_kind.to_lambda kind in
    let alloc_mode = C.alloc_mode_for_applications_to_cmx alloc_mode in
    ( C.send kind callee obj (split_args ()) args_ty return_ty (pos, alloc_mode)
        dbg,
      free_vars,
      env,
      res,
      Ece.all )
  | Effect op -> (
    let module BV = Backend_var in
    let open To_cmm_env in
    let[@inline] simple s = C.simple ~dbg s in
    match op with
    | Perform { eff } ->
      let { env; res; expr = { cmm = eff; free_vars; effs = _ } } =
        simple env res eff
      in
      C.perform ~dbg eff, free_vars, env, res, Ece.all
    | Reperform { eff; cont; last_fiber } ->
      let { env; res; expr = { cmm = eff; free_vars = fv0; effs = _ } } =
        simple env res eff
      in
      let { env; res; expr = { cmm = cont; free_vars = fv1; effs = _ } } =
        simple env res cont
      in
      let { env; res; expr = { cmm = last_fiber; free_vars = fv2; effs = _ } } =
        simple env res last_fiber
      in
      let free_vars = BV.Set.union (BV.Set.union fv0 fv1) fv2 in
      C.reperform ~dbg ~eff ~cont ~last_fiber, free_vars, env, res, Ece.all
    | Run_stack { stack; f; arg } ->
      let { env; res; expr = { cmm = stack; free_vars = fv0; effs = _ } } =
        simple env res stack
      in
      let { env; res; expr = { cmm = f; free_vars = fv1; effs = _ } } =
        simple env res f
      in
      let { env; res; expr = { cmm = arg; free_vars = fv2; effs = _ } } =
        simple env res arg
      in
      let free_vars = BV.Set.union (BV.Set.union fv0 fv1) fv2 in
      C.run_stack ~dbg ~stack ~f ~arg, free_vars, env, res, Ece.all
    | Resume { stack; f; arg; last_fiber } ->
      let { env; res; expr = { cmm = stack; free_vars = fv0; effs = _ } } =
        simple env res stack
      in
      let { env; res; expr = { cmm = f; free_vars = fv1; effs = _ } } =
        simple env res f
      in
      let { env; res; expr = { cmm = arg; free_vars = fv2; effs = _ } } =
        simple env res arg
      in
      let { env; res; expr = { cmm = last_fiber; free_vars = fv3; effs = _ } } =
        simple env res last_fiber
      in
      let free_vars =
        BV.Set.union (BV.Set.union fv0 fv1) (BV.Set.union fv2 fv3)
      in
      C.resume ~dbg ~stack ~f ~arg ~last_fiber, free_vars, env, res, Ece.all)

let translate_apply env res apply =
  let dbg = Env.add_inlined_debuginfo env (Apply.dbg apply) in
  warn_if_unused_inlined_attribute apply ~dbg_with_inlined:dbg;
  let call, free_vars, env, res, effs =
    translate_apply0 ~dbg_with_inlined:dbg env res apply
  in
  let k_exn = Apply.exn_continuation apply in
  let extra_args = Exn_continuation.extra_args k_exn in
  if Misc.Stdlib.List.is_empty extra_args
  then call, free_vars, env, res, effs
  else
    (* If there are extra args, create a wrapper continuation, as we do when
       inlining a function into a context where the exception continuation takes
       extra args. *)
    let env, res, free_vars, effs, extra_args_rev =
      (* Obtain the Cmm expressions for the extra args *)
      List.fold_left
        (fun (env, res, free_vars, effs, extra_args_rev)
             (extra_arg_simple, _kind) ->
          let { To_cmm_env.env;
                res;
                expr = { cmm = extra_arg; free_vars = free_vars'; effs = effs' }
              } =
            C.simple ~dbg env res extra_arg_simple
          in
          ( env,
            res,
            Backend_var.Set.union free_vars free_vars',
            Ece.join effs effs',
            extra_arg :: extra_args_rev ))
        (env, res, free_vars, effs, [])
        extra_args
    in
    let extra_args = List.rev extra_args_rev in
    let exn_var = Backend_var.create_local "*exn*" in
    let result_var = Backend_var.create_local "*res*" in
    let result_type =
      Apply.return_arity apply |> C.extended_machtype_of_return_arity
      |> C.Extended_machtype.to_machtype
    in
    let pop_handler_params =
      [Backend_var.With_provenance.create result_var, result_type]
    in
    let handler_cont = Lambda.next_raise_count () in
    let push_cont = Lambda.next_raise_count () in
    let pop_cont = Lambda.next_raise_count () in
    let call_and_pop = C.cexit pop_cont [call] [Pop handler_cont] in
    let body =
      C.create_ccatch ~rec_flag:false
        ~body:(C.cexit push_cont [] [Push handler_cont])
        ~handlers:[C.handler ~dbg push_cont [] call_and_pop false (* is_cold *)]
    in
    let body =
      C.create_ccatch ~rec_flag:false ~body
        ~handlers:
          [ C.handler ~dbg pop_cont pop_handler_params (Cvar result_var)
              false (* is_cold *) ]
    in
    let handler =
      (* This exception handler has no extra args, but reraises to the one which
         does. The exception arising from the function application is in
         [exn_var]. *)
      C.raise_prim Raise_reraise (Cvar exn_var) ~extra_args dbg
    in
    let cmm =
      (* Catch any exceptions from the function application and send them to our
         handler *)
      C.trywith
        ~exn_var:(Backend_var.With_provenance.create exn_var)
        ~extra_args:[] ~dbg:(Apply.dbg apply) ~body ~handler_cont ~handler ()
    in
    cmm, free_vars, env, res, effs

(* Helpers for the translation of [Switch] expressions. *)

(* Helpers for translating [Apply_cont] expressions *)

(* Exception continuations always receive the exception value in their first
   argument. Additionally, they may have extra arguments that are passed to the
   handler via mutable variables (expected to be spilled to the stack). *)
let translate_raise ~dbg_with_inlined:dbg env res apply exn_handler args =
  match args with
  | exn :: extra ->
    let raise_kind =
      match Apply_cont.trap_action apply with
      | Some (Pop { raise_kind; _ }) ->
        Trap_action.Raise_kind.option_to_lambda raise_kind
      | Some (Push _) | None ->
        Misc.fatal_errorf
          "Apply_cont calls an exception handler without a Pop trap action:@ %a"
          Apply_cont.print apply
    in
    let To_cmm_env.
          { env;
            res;
            expr = { cmm = exn; free_vars = exn_free_vars; effs = _ }
          } =
      C.simple ~dbg env res exn
    in
    let extra, extra_free_vars, env, res, _ =
      C.simple_list ~dbg env res extra
    in
    let free_vars = Backend_var.Set.union exn_free_vars extra_free_vars in
    let wrap, _, res = Env.flush_delayed_lets ~mode:Branching_point env res in
    let cmm = C.raise_prim raise_kind exn ~extra_args:extra dbg in
    let cmm, free_vars, symbol_inits =
      wrap cmm free_vars Env.Symbol_inits.empty
    in
    cmm, free_vars, symbol_inits, res
  | [] ->
    Misc.fatal_errorf "Exception continuation %a has no arguments:@ \n%a"
      Continuation.print exn_handler Apply_cont.print apply

let translate_jump_to_continuation ~dbg_with_inlined:dbg env res apply types
    cont args =
  if List.compare_lengths types args = 0
  then
    let trap_actions =
      match Apply_cont.trap_action apply with
      | None -> []
      | Some (Pop { exn_handler; _ }) ->
        let cont = Env.get_cmm_continuation env exn_handler in
        [Cmm.Pop cont]
      | Some (Push { exn_handler }) ->
        let cont = Env.get_cmm_continuation env exn_handler in
        [Cmm.Push cont]
    in
    let args = C.remove_skipped_args args types in
    let args, free_vars, env, res, _ = C.simple_list ~dbg env res args in
    let wrap, _, res = Env.flush_delayed_lets ~mode:Branching_point env res in
    let cmm, free_vars, symbol_inits =
      wrap (C.cexit cont args trap_actions) free_vars Env.Symbol_inits.empty
    in
    cmm, free_vars, symbol_inits, res
  else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         (To_cmm_env.print_param_type Printcmm.machtype))
      types Apply_cont.print apply

(* A call to the return continuation of the current block simply is the return
   value for the current block being translated. *)
let translate_jump_to_return_continuation ~dbg_with_inlined:dbg env res apply
    return_cont types args =
  if List.compare_lengths types args = 0
  then
    let return_values, free_vars, env, res, _ =
      C.simple_list ~dbg env res args
    in
    let return_value = C.make_tuple return_values in
    let wrap, _, res = Env.flush_delayed_lets ~mode:Branching_point env res in
    match Apply_cont.trap_action apply with
    | None ->
      let cmm, free_vars, symbol_inits =
        wrap return_value free_vars Env.Symbol_inits.empty
      in
      cmm, free_vars, symbol_inits, res
    | Some (Pop { exn_handler; _ }) ->
      let cont = Env.get_cmm_continuation env exn_handler in
      let cmm, free_vars, symbol_inits =
        wrap
          (C.trap_return return_value [Cmm.Pop cont])
          free_vars Env.Symbol_inits.empty
      in
      cmm, free_vars, symbol_inits, res
    | Some (Push _) ->
      Misc.fatal_errorf
        "Return continuation %a should not be applied with a Push trap action"
        Continuation.print return_cont
  else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Printcmm.machtype)
      types Apply_cont.print apply

(* Invalid expressions *)
let invalid env res ~message =
  let wrap, _empty_env, res =
    Env.flush_delayed_lets ~mode:Branching_point env res
  in
  let cmm_invalid, res = C.invalid res ~message in
  let cmm, free_vars, symbol_inits =
    wrap cmm_invalid Backend_var.Set.empty Env.Symbol_inits.empty
  in
  cmm, free_vars, symbol_inits, res

(* The main set of translation functions for expressions *)

let rec expr env res e :
    Cmm.expression * Backend_var.Set.t * Env.Symbol_inits.t * To_cmm_result.t =
  match Expr.descr e with
  | Let e' -> let_expr env res e'
  | Let_cont e' -> let_cont env res e'
  | Apply e' -> apply_expr env res e'
  | Apply_cont e' -> apply_cont env res e'
  | Switch e' -> switch env res e'
  | Invalid { message } -> invalid env res ~message

and let_prim env res ~num_normal_occurrences_of_bound_vars v p dbg body =
  let dbg = Env.add_inlined_debuginfo env dbg in
  let effects_and_coeffects_of_prim =
    Flambda_primitive.effects_and_coeffects p
  in
  let inline =
    To_cmm_effects.classify_let_binding (Bound_var.var v)
      ~num_normal_occurrences_of_bound_vars
      ~effects_and_coeffects_of_defining_expr:effects_and_coeffects_of_prim
  in
  let simple_case (inline : Env.simple Env.inline) =
    let defining_expr, extra, env, res, args_effs =
      To_cmm_primitive.prim_simple env res dbg p
    in
    let effects_and_coeffects_of_defining_expr =
      Ece.join args_effs effects_and_coeffects_of_prim
    in
    let env, res =
      Env.bind_variable_to_primitive ?extra env res v ~inline
        ~effects_and_coeffects_of_defining_expr ~defining_expr
    in
    expr env res body
  in
  let complex_case (inline : Env.complex Env.inline) =
    let defining_expr, env, res, args_effs =
      To_cmm_primitive.prim_complex env res dbg p
    in
    let effects_and_coeffects_of_defining_expr =
      Ece.join args_effs effects_and_coeffects_of_prim
    in
    let env, res =
      Env.bind_variable_to_primitive env res v ~inline
        ~effects_and_coeffects_of_defining_expr ~defining_expr
    in
    expr env res body
  in
  match inline with
  (* It can be useful to translate a dropped expression because it allows to
     inline (and thus remove from the env) the arguments in it. *)
  | Drop_defining_expr | Regular -> simple_case Do_not_inline
  | May_inline_once -> simple_case May_inline_once
  | Must_inline_once -> complex_case Must_inline_once
  | Must_inline_and_duplicate -> complex_case Must_inline_and_duplicate

and let_expr0 env res let_expr (bound_pattern : Bound_pattern.t)
    ~num_normal_occurrences_of_bound_vars ~body =
  match[@warning "-4"] bound_pattern, Let.defining_expr let_expr with
  | Singleton v, Simple s ->
    (* CR mshinwell: Try to get a proper [dbg] here (although the majority of
       these bindings should have been substituted out). *)
    (* CR gbury: once we get proper debuginfo here, remember to apply
       Env.add_inlined_debuginfo to it *)
    let dbg_with_inlined = Debuginfo.none in
    let env, res =
      bind_var_to_simple ~dbg_with_inlined env res v
        ~num_normal_occurrences_of_bound_vars s
    in
    expr env res body
  | Singleton _, Prim (p, _)
    when (not (Flambda_features.stack_allocation_enabled ()))
         && Flambda_primitive.is_begin_or_end_region p ->
    expr env res body
  | Singleton _, Prim (Nullary (Enter_inlined_apply { dbg }), _) ->
    let env = Env.enter_inlined_apply env dbg in
    expr env res body
  | Singleton v, Prim ((Unary (End_region _, _) as p), dbg) ->
    (* CR gbury: this is a hack to prevent moving of expressions past an
       End_region. We have to do this manually because we currently have effects
       and coeffects that are not precise enough. Particularly, an immutable
       load of a locally allocated block is considered as pure, and thus can be
       moved past an end_region. Here we also need to flush everything,
       including must_inline bindings, particularly projections that may project
       from locally allocated closures (and that must not be moved past an
       end_region). *)
    let wrap, env, res =
      Env.flush_delayed_lets ~mode:Flush_everything env res
    in
    let cmm, free_vars, symbol_inits, res =
      let_prim env res ~num_normal_occurrences_of_bound_vars v p dbg body
    in
    let cmm, free_vars, symbol_inits = wrap cmm free_vars symbol_inits in
    cmm, free_vars, symbol_inits, res
  | Singleton v, Prim (p, dbg) ->
    let_prim env res ~num_normal_occurrences_of_bound_vars v p dbg body
  | Set_of_closures bound_vars, Set_of_closures soc ->
    To_cmm_set_of_closures.let_dynamic_set_of_closures env res ~body ~bound_vars
      ~num_normal_occurrences_of_bound_vars soc ~translate_expr:expr
  | Static bound_static, Static_consts consts -> (
    let env, res, update_opt =
      To_cmm_static.static_consts env res
        ~params_and_body:
          (To_cmm_set_of_closures.params_and_body ~translate_expr:expr)
        bound_static consts
    in
    match update_opt with
    | None -> expr env res body
    | Some { cmm; free_vars; effs = _ } ->
      let wrap, env, res =
        Env.flush_delayed_lets ~mode:Branching_point env res
      in
      let body, body_free_vars, symbol_inits, res = expr env res body in
      let free_vars = Backend_var.Set.union free_vars body_free_vars in
      let cmm, free_vars, symbol_inits =
        wrap (C.sequence cmm body) free_vars symbol_inits
      in
      cmm, free_vars, symbol_inits, res)
  | Singleton _, Rec_info _ -> expr env res body
  | Singleton _, (Set_of_closures _ | Static_consts _)
  | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
  | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
    Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
      Let.print let_expr

and let_expr env res let_expr =
  Let.pattern_match' let_expr
    ~f:(fun bound_pattern ~num_normal_occurrences_of_bound_vars ~body ->
      match Bound_pattern.name_mode bound_pattern with
      | Normal ->
        let_expr0 env res let_expr bound_pattern
          ~num_normal_occurrences_of_bound_vars ~body
      | Phantom -> expr env res body
      | In_types ->
        Misc.fatal_errorf "Cannot bind In_types variables in terms:@ %a"
          Let.print let_expr)

and let_cont env res (let_cont : Flambda.Let_cont.t) =
  match let_cont with
  | Non_recursive { handler; num_free_occurrences; is_applied_with_traps } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let handler = Non_recursive_let_cont_handler.handler handler in
        match
          To_cmm_effects.classify_continuation_handler k handler
            ~num_free_occurrences ~is_applied_with_traps
        with
        | May_inline -> let_cont_inlined env res k handler body
        | Regular -> let_cont_not_inlined env res k handler body)
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body conts ->
        if Continuation_handlers.contains_exn_handler conts
        then
          Misc.fatal_errorf
            "Recursive continuation bindings cannot involve exception \
             handlers:@ %a"
            Let_cont.print let_cont;
        let_cont_rec env res invariant_params conts body)

(* The bound continuation [k] will be inlined. *)
and let_cont_inlined env res k handler body =
  Continuation_handler.pattern_match' handler
    ~f:(fun handler_params ~num_normal_occurrences_of_params ~handler ->
      let env =
        Env.add_inline_cont env k ~handler_params
          ~handler_params_occurrences:num_normal_occurrences_of_params
          ~handler_body:handler
      in
      expr env res body)

and let_cont_not_inlined env res k handler body =
  (* The environment must be flushed to ensure that expressions are not
     duplicated into both the body and the handler. *)
  (* CR gbury: "split" the environment according to which variables the handler
     and the body uses, to allow for inlining to proceed within each
     expression. *)
  let wrap, env, res = Env.flush_delayed_lets ~mode:Branching_point env res in
  let is_exn_handler = Continuation_handler.is_exn_handler handler in
  let is_cold = Continuation_handler.is_cold handler in
  let vars, arity, handler, free_vars_of_handler, handler_symbol_inits, res =
    continuation_handler env res handler
  in
  let catch_id, env =
    Env.add_jump_cont env k ~param_types:(List.map snd vars)
  in
  let cmm, free_vars, symbol_inits, res =
    (* Exception continuations are translated specially -- these will be reached
       via the raising of exceptions, whereas other continuations are reached
       using a normal jump. *)
    if is_exn_handler
    then
      let_cont_exn_handler env res k body vars handler free_vars_of_handler
        handler_symbol_inits ~catch_id arity
    else
      (* CR mshinwell: fix debuginfo *)
      (* CR gbury: once we get proper debuginfo here, remember to apply
         Env.add_inlined_debuginfo to it *)
      let dbg = Debuginfo.none in
      let body, free_vars_of_body, body_symbol_inits, res = expr env res body in
      let symbol_inits =
        Env.Symbol_inits.merge handler_symbol_inits body_symbol_inits
      in
      let free_vars =
        Backend_var.Set.union free_vars_of_body
          (C.remove_vars_with_machtype free_vars_of_handler vars)
      in
      ( C.create_ccatch ~rec_flag:false ~body
          ~handlers:
            [ C.handler ~dbg catch_id
                (C.remove_skipped_params vars)
                handler is_cold ],
        free_vars,
        symbol_inits,
        res )
  in
  let cmm, free_vars, symbol_inits = wrap cmm free_vars symbol_inits in
  cmm, free_vars, symbol_inits, res

(* Exception continuations are translated using delayed Ctrywith blocks. The
   exception handler parts of these blocks are identified by the [catch_id]s. *)
and let_cont_exn_handler env res k body vars handler free_vars_of_handler
    handler_symbol_inits ~catch_id arity =
  let exn_var, extra_params =
    match vars with
    | (v, _) :: rest -> v, rest
    | [] ->
      (* See comment on [translate_raise], above. *)
      Misc.fatal_errorf
        "Exception continuation %a should have at least one argument"
        Continuation.print k
  in
  let extra_params =
    List.filter_map
      (fun (var, (param_type : _ Env.param_type)) ->
        match param_type with
        | Param machtype -> Some (var, machtype)
        | Skip_param -> None)
      extra_params
  in
  let env_body = Env.add_exn_handler env k arity in
  let body, free_vars_of_body, body_symbol_inits, res =
    expr env_body res body
  in
  let symbol_inits =
    Env.Symbol_inits.merge handler_symbol_inits body_symbol_inits
  in
  let free_vars =
    Backend_var.Set.union free_vars_of_body
      (C.remove_vars_with_machtype free_vars_of_handler vars)
  in
  (* CR mshinwell: fix debuginfo *)
  (* CR gbury: once we get proper debuginfo here, remember to apply
     Env.add_inlined_debuginfo to it *)
  let dbg = Debuginfo.none in
  ( C.trywith ~dbg ~body ~exn_var ~extra_args:extra_params
      ~handler_cont:catch_id ~handler (),
    free_vars,
    symbol_inits,
    res )

and let_cont_rec env res invariant_params conts body =
  (* Flush the env now to avoid inlining something inside of a recursive
     continuation (aka a loop), as it would increase the number of times the
     computation is performed (even if there is only one syntactic
     occurrence) *)
  (* CR-someday mshinwell: As discussed, the tradeoff here is not clear, since
     flushing might increase register pressure. *)
  let wrap, env, res = Env.flush_delayed_lets ~mode:Entering_loop env res in
  (* Compute the environment for Ccatch ids *)
  let conts_to_handlers = Continuation_handlers.to_map conts in
  let env =
    Continuation.Lmap.fold
      (fun k handler acc ->
        let continuation_arg_tys =
          Continuation_handler.pattern_match' handler
            ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler:_ ->
              List.map C.param_machtype_of_kinded_parameter
                (Bound_parameters.to_list
                   (Bound_parameters.append invariant_params params)))
        in
        snd (Env.add_jump_cont acc k ~param_types:continuation_arg_tys))
      conts_to_handlers env
  in
  (* Generate variables for the invariant params *)
  let env, invariant_vars =
    C.continuation_bound_parameters env invariant_params
  in
  (* Translate each continuation handler *)
  let conts_to_handlers, res =
    Continuation.Lmap.fold
      (fun k handler (conts_to_handlers, res) ->
        let vars, _arity, handler, free_vars_of_handler, symbol_inits, res =
          continuation_handler env res handler
        in
        (* It should be an flambda2 invariant that symbols are bound at
           top-level, and the handler of a loop is clearly not top-level *)
        if not (Env.Symbol_inits.is_empty symbol_inits)
        then
          Format.eprintf "Found leftover symbol inits in the body of a loop: %a"
            Env.Symbol_inits.print symbol_inits;
        ( Continuation.Map.add k
            (invariant_vars @ vars, handler, free_vars_of_handler)
            conts_to_handlers,
          res ))
      conts_to_handlers
      (Continuation.Map.empty, res)
  in
  (* CR mshinwell: fix debuginfo *)
  (* CR gbury: once we get proper debuginfo here, remember to apply
     Env.add_inlined_debuginfo to it *)
  let dbg = Debuginfo.none in
  let body, free_vars_of_body, symbol_inits, res = expr env res body in
  (* Setup the Cmm handlers for the Ccatch *)
  let handlers, free_vars =
    Continuation.Map.fold
      (fun k (vars, handler, free_vars_of_handler) (handlers, free_vars) ->
        let free_vars =
          Backend_var.Set.union free_vars
            (C.remove_vars_with_machtype free_vars_of_handler vars)
        in
        let id = Env.get_cmm_continuation env k in
        ( C.handler ~dbg id (C.remove_skipped_params vars) handler false
          :: handlers,
          free_vars ))
      conts_to_handlers ([], free_vars_of_body)
  in
  let cmm = C.create_ccatch ~rec_flag:true ~body ~handlers in
  let cmm, free_vars, symbol_inits = wrap cmm free_vars symbol_inits in
  cmm, free_vars, symbol_inits, res

and continuation_handler env res handler =
  Continuation_handler.pattern_match' handler
    ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler ->
      let arity = Bound_parameters.arity params in
      let env, vars = C.continuation_bound_parameters env params in
      let expr, free_vars_of_handler, symbol_inits, res =
        expr env res handler
      in
      let expr, free_vars_of_handler, symbol_inits =
        Env.place_symbol_inits ~params:vars expr free_vars_of_handler
          symbol_inits
      in
      vars, arity, expr, free_vars_of_handler, symbol_inits, res)

and apply_expr env res apply =
  let call, free_vars, env, res, effs = translate_apply env res apply in
  (* With respect to flushing the environment we have three cases:

     1. The call never returns or jumps to another function

     2. The call jumps to somewhere else in the current function, but there is
     more than one incoming control flow edge to that point

     3. The call jumps to somewhere else in the current function and is the only
     thing that jumps to that point. In this case we will inline the return
     continuation of the [Apply].

     In case 1 we can't affect the code generation at the jump target: there is
     no option but to flush now.

     In case 2 we also flush to ensure that effectful bindings are not pushed
     past join points and that no binding is duplicated.

     In case 3 we can avoid flushing the environment due to the linearity of the
     control flow. We know that flushing will eventually happen by virtue of the
     recursive call to [expr]. *)
  match Apply.continuation apply with
  | Never_returns ->
    (* Case 1 *)
    let wrap, _, res = Env.flush_delayed_lets ~mode:Branching_point env res in
    let cmm, free_vars, symbol_inits =
      wrap call free_vars Env.Symbol_inits.empty
    in
    cmm, free_vars, symbol_inits, res
  | Return k -> (
    match Env.get_continuation env k with
    | Return { param_types } ->
      (* Case 1 *)
      let apply_result_arity =
        Flambda_arity.unarized_components (Apply.return_arity apply)
      in
      if List.compare_lengths apply_result_arity param_types = 0
      then
        let wrap, _, res =
          Env.flush_delayed_lets ~mode:Branching_point env res
        in
        let cmm, free_vars, symbol_inits =
          wrap call free_vars Env.Symbol_inits.empty
        in
        cmm, free_vars, symbol_inits, res
      else
        Misc.fatal_errorf
          "Types (%a) do not match arguments for the return cont of@ %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space Printcmm.machtype)
          param_types Apply.print apply
    | Jump { param_types = _; cont } ->
      (* Case 2 *)
      let wrap, _, res = Env.flush_delayed_lets ~mode:Branching_point env res in
      let cmm, free_vars, symbol_inits =
        wrap (C.cexit cont [call] []) free_vars Env.Symbol_inits.empty
      in
      cmm, free_vars, symbol_inits, res
    | Inline
        { handler_params;
          handler_body = body;
          handler_params_occurrences;
          handler_body_inlined_debuginfo
        } -> (
      (* Case 3 *)
      let handler_params = Bound_parameters.to_list handler_params in
      match handler_params with
      | [param] ->
        let param_var, param_uid = Bound_parameter.var_and_uid param in
        let var = Bound_var.create param_var param_uid Name_mode.normal in
        let env, res =
          Env.bind_variable env res var
            ~effects_and_coeffects_of_defining_expr:effs ~defining_expr:call
            ~free_vars_of_defining_expr:free_vars
            ~num_normal_occurrences_of_bound_vars:handler_params_occurrences
        in
        let env =
          Env.set_inlined_debuginfo env handler_body_inlined_debuginfo
        in
        expr env res body
      | params ->
        (* CR ncourant: we create a cexit/ccatch pair here, to be able to
           destruct the output which is a tuple. When we get a way to
           destructure Ctuples, we should use this constructor here instead. *)
        let wrap, env, res =
          Env.flush_delayed_lets ~mode:Branching_point env res
        in
        let env, cmm_params =
          Env.create_bound_parameters env
            (List.map Bound_parameter.var_and_uid params)
        in
        let label = Lambda.next_raise_count () in
        let params_with_machtype =
          List.map2
            (fun cmm_param param ->
              cmm_param, C.machtype_of_kinded_parameter param)
            cmm_params params
        in
        let env =
          Env.set_inlined_debuginfo env handler_body_inlined_debuginfo
        in
        let expr, free_vars_of_handler, handler_symbol_inits, res =
          expr env res body
        in
        let expr, free_vars_of_handler, symbol_inits =
          Env.place_symbol_inits ~params:params_with_machtype expr
            free_vars_of_handler handler_symbol_inits
        in
        (* we know the handler can't be cold, or it wouldn't have been
           inlined. *)
        let handler =
          C.handler ~dbg:(Apply.dbg apply) label params_with_machtype expr false
        in
        let expr =
          C.create_ccatch ~rec_flag:false ~handlers:[handler]
            ~body:(C.cexit label [call] [])
        in
        let free_vars =
          Backend_var.Set.union free_vars
            (C.remove_vars_with_machtype free_vars_of_handler
               params_with_machtype)
        in
        let cmm, free_vars, symbol_inits = wrap expr free_vars symbol_inits in
        cmm, free_vars, symbol_inits, res))

and apply_cont env res apply_cont =
  let dbg_with_inlined =
    Env.add_inlined_debuginfo env (Apply_cont.debuginfo apply_cont)
  in
  let k = Apply_cont.continuation apply_cont in
  let args = Apply_cont.args apply_cont in
  if Env.is_exn_handler env k
  then translate_raise ~dbg_with_inlined env res apply_cont k args
  else
    match Env.get_continuation env k with
    | Return { param_types } ->
      translate_jump_to_return_continuation ~dbg_with_inlined env res apply_cont
        k param_types args
    | Jump { param_types; cont } ->
      translate_jump_to_continuation ~dbg_with_inlined env res apply_cont
        param_types cont args
    | Inline
        { handler_params;
          handler_body;
          handler_params_occurrences;
          handler_body_inlined_debuginfo
        } ->
      if Option.is_some (Apply_cont.trap_action apply_cont)
      then
        Misc.fatal_errorf "This [Apply_cont] should not have a trap action:@ %a"
          Apply_cont.print apply_cont;
      (* Inlining a continuation call simply needs to bind the arguments to the
         variables that the continuation's handler expects. *)
      let handler_params = Bound_parameters.to_list handler_params in
      if List.compare_lengths args handler_params = 0
      then
        let env, res =
          List.fold_left2
            (fun (env, res) param arg ->
              match[@ocaml.warning "-4"]
                Flambda_kind.With_subkind.kind (Bound_parameter.kind param)
              with
              | Rec_info ->
                (* Skip depth variables/parameters *)
                env, res
              | _ ->
                let param_var, param_uid = Bound_parameter.var_and_uid param in
                let var =
                  Bound_var.create param_var param_uid Name_mode.normal
                in
                bind_var_to_simple ~dbg_with_inlined env res var
                  ~num_normal_occurrences_of_bound_vars:
                    handler_params_occurrences arg)
            (env, res) handler_params args
        in
        let env =
          Env.set_inlined_debuginfo env handler_body_inlined_debuginfo
        in
        expr env res handler_body
      else
        Misc.fatal_errorf
          "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
          Continuation.print k Apply_cont.print apply_cont
          (List.length handler_params)
          Apply_cont.print apply_cont

and switch env res switch =
  let scrutinee = Switch.scrutinee switch in
  let dbg = Env.add_inlined_debuginfo env (Switch.condition_dbg switch) in
  let To_cmm_env.
        { env;
          res;
          expr =
            { cmm = untagged_scrutinee_cmm;
              free_vars = scrutinee_free_vars;
              effs = _
            }
        } =
    C.simple ~dbg env res scrutinee
  in
  let arms = Switch.arms switch in
  (* For binary switches, which can be translated to an if-then-else, it can be
     interesting for the scrutinee to be tagged (particularly for switches
     coming from a source level if-then-else on booleans) as that way the
     translation can use 2 instructions instead of 3.

     However, this is only useful to do if the tagged expression is smaller then
     the untagged one (which is not always true due to arithmetic
     simplifications performed by Cmm_helpers).

     Additionally for switches with more than 2 arms, not untagging and
     adjusting the switch to work on tagged integers might be worse. The
     discriminants of the arms might not be successive machine integers anymore,
     thus preventing the use of a table. Alternatively it might not be worth it
     given the already high number of instructions needed for big switches (but
     this might be debatable for small switches with 3 to 5 arms). *)
  let scrutinee, must_tag_discriminant =
    match Targetint_31_63.Map.cardinal arms with
    | 2 -> (
      match Env.extra_info env scrutinee with
      | None -> untagged_scrutinee_cmm, false
      | Some (Untag tagged_scrutinee_cmm) ->
        let size_untagged =
          Option.value
            (C.cmm_arith_size untagged_scrutinee_cmm)
            ~default:max_int
        in
        let size_tagged =
          Option.value (C.cmm_arith_size tagged_scrutinee_cmm) ~default:max_int
        in
        if size_tagged < size_untagged
        then tagged_scrutinee_cmm, true
        else untagged_scrutinee_cmm, false)
    | _ -> untagged_scrutinee_cmm, false
  in
  let wrap, env, res = Env.flush_delayed_lets ~mode:Branching_point env res in
  let prepare_discriminant ~must_tag d =
    let targetint_d = Targetint_31_63.to_targetint d in
    Targetint_32_64.to_int_checked
      (if must_tag then C.tag_targetint targetint_d else targetint_d)
  in
  let make_arm ~must_tag_discriminant env res (d, action) =
    let d = prepare_discriminant ~must_tag:must_tag_discriminant d in
    let cmm_action, action_free_vars, action_symbol_inits, res =
      apply_cont env res action
    in
    ( ( d,
        cmm_action,
        action_free_vars,
        action_symbol_inits,
        Env.add_inlined_debuginfo env (Apply_cont.debuginfo action) ),
      res )
  in
  match Targetint_31_63.Map.cardinal arms with
  (* Binary case: if-then-else *)
  | 2 -> (
    let aux = make_arm ~must_tag_discriminant env in
    let first_arm, res = aux res (Targetint_31_63.Map.min_binding arms) in
    let second_arm, res = aux res (Targetint_31_63.Map.max_binding arms) in
    match first_arm, second_arm with
    (* These switches are actually if-then-elses. On such switches,
       transl_switch_clambda will introduce a let-binding of the scrutinee
       before creating an if-then-else, introducing an indirection that might
       prevent some optimizations performed by Selectgen/Emit when the condition
       is inlined in the if-then-else. Instead we use [C.ite]. *)
    | ( (0, else_, else_free_vars, else_inits, else_dbg),
        (_, then_, then_free_vars, then_inits, then_dbg) )
    | ( (_, then_, then_free_vars, then_inits, then_dbg),
        (0, else_, else_free_vars, else_inits, else_dbg) ) ->
      let free_vars =
        Backend_var.Set.union scrutinee_free_vars
          (Backend_var.Set.union else_free_vars then_free_vars)
      in
      (* See comment below about symbol inits and branches *)
      let symbol_inits = Env.Symbol_inits.merge then_inits else_inits in
      let cmm, free_vars, symbol_inits =
        wrap
          (C.ite ~dbg scrutinee ~then_dbg ~then_ ~else_dbg ~else_)
          free_vars symbol_inits
      in
      cmm, free_vars, symbol_inits, res
    (* Similar case to the previous but none of the arms match 0, so we have to
       generate an equality test, and make sure it is inside the condition to
       ensure Selectgen and Emit can take advantage of it. *)
    | ( (x, if_x, if_x_free_vars, if_x_symbol_inits, if_x_dbg),
        (_, if_not, if_not_free_vars, if_not_symbol_inits, if_not_dbg) ) ->
      let free_vars =
        Backend_var.Set.union scrutinee_free_vars
          (Backend_var.Set.union if_x_free_vars if_not_free_vars)
      in
      let expr =
        C.ite ~dbg
          (C.eq ~dbg (C.int ~dbg x) scrutinee)
          ~then_dbg:if_x_dbg ~then_:if_x ~else_dbg:if_not_dbg ~else_:if_not
      in
      (* See comment below about symbol inits and branches *)
      let symbol_inits =
        Env.Symbol_inits.merge if_x_symbol_inits if_not_symbol_inits
      in
      let cmm, free_vars, symbol_inits = wrap expr free_vars symbol_inits in
      cmm, free_vars, symbol_inits, res)
  (* General case *)
  | n ->
    (* transl_switch_clambda expects an [index] array such that index.(d) is the
       index in [cases] of the expression to execute when [e] matches [d]. *)
    let max_d, _ = Targetint_31_63.Map.max_binding arms in
    let m = prepare_discriminant ~must_tag:must_tag_discriminant max_d in
    let unreachable, res = C.invalid res ~message:"unreachable switch case" in
    let cases = Array.make (n + 1) unreachable in
    let index = Array.make (m + 1) n in
    let _, res, free_vars, symbol_inits =
      Targetint_31_63.Map.fold
        (fun discriminant action (i, res, free_vars, symbol_inits) ->
          let (d, cmm_action, action_free_vars, action_symbol_inits, _dbg), res
              =
            make_arm ~must_tag_discriminant env res (discriminant, action)
          in
          (* Note about symbol inits and branches: symbol allocation can occur
             in branches of a switch/ite, e.g. if there are two branches and one
             of them raises, then the other one can be considered at unit
             top-level and allow for symbols to be defined; after inlining of
             continuation by to_cmm, this means it is expected that there can be
             some symbol inits in branches of a switch (although we'd expect at
             most one branch to have some). Therefore we want to correctly
             handle and propagate up symbol inits that come from branches. *)
          let symbol_inits =
            Env.Symbol_inits.merge symbol_inits action_symbol_inits
          in
          let free_vars = Backend_var.Set.union free_vars action_free_vars in
          cases.(i) <- cmm_action;
          index.(d) <- i;
          i + 1, res, free_vars, symbol_inits)
        arms
        (0, res, scrutinee_free_vars, Env.Symbol_inits.empty)
    in
    (* CR-someday poechsel: Put a more precise value kind here *)
    let expr = C.transl_switch_clambda dbg scrutinee index cases in
    let cmm, free_vars, symbol_inits = wrap expr free_vars symbol_inits in
    cmm, free_vars, symbol_inits, res
