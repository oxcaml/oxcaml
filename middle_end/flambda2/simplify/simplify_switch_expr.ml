(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import
module TE = Flambda2_types.Typing_env
module TI = Target_ocaml_int
module Alias_set = TE.Alias_set

type mergeable_arms =
  | No_arms
  | Mergeable of
      { cont : Continuation.t;
        args : Alias_set.t list
      }
  | Not_mergeable

let find_all_aliases env arg =
  let find_all_aliases () =
    TE.aliases_of_simple env ~min_name_mode:NM.normal arg
  in
  Simple.pattern_match'
    ~var:(fun _var ~coercion:_ ->
      (* We use find alias to find a common simple to different
         simples.

         This simple is already guaranteed to be the cannonical alias.

       * If there is a common alias between variables, the
         cannonical alias must also be a common alias.

       * For constants and symbols there can be a common alias that
         is not cannonical: A variable can have different constant
         values in different branches: this variable is not the
         cannonical alias, the cannonical would be the constant or
         the symbol. But the only common alias could be a variable
         in that case.

         hence there is no loss of generality in returning the
         cannonical alias as the single alias if it is a variable.

         Note that the main reason for this is to allow changing the
         arguments of continuations to variables that where not in
         scope during the downward traversal. In particular for the
         alias rewriting provided by data_flow *)
      TE.Alias_set.singleton arg)
    ~symbol:(fun _sym ~coercion:_ -> find_all_aliases ())
    ~const:(fun _cst -> find_all_aliases ())
    arg

let rebuild_arm uacc arm (action, use_id, arity, env_at_use)
    ( new_let_conts,
      arms,
      (mergeable_arms : mergeable_arms),
      identity_arms,
      not_arms ) =
  let action =
    Simplify_common.clear_demoted_trap_action_and_patch_unused_exn_bucket uacc
      action
  in
  match EB.rewrite_switch_arm uacc action ~use_id arity with
  | Invalid _ ->
    (* The destination is unreachable; delete the [Switch] arm. *)
    new_let_conts, arms, mergeable_arms, identity_arms, not_arms
  | Apply_cont action -> (
    let action =
      let cont = Apply_cont.continuation action in
      let cont_info_from_uenv = UE.find_continuation (UA.uenv uacc) cont in
      (* First try to absorb any [Apply_cont] expression that forms the entirety
         of the arm's action (via an intermediate zero-arity continuation
         without trap action) into the [Switch] expression itself. *)
      match cont_info_from_uenv with
      | Invalid _ -> None
      | Linearly_used_and_inlinable _ | Non_inlinable_zero_arity _
      | Non_inlinable_non_zero_arity _
      | Toplevel_or_function_return_or_exn_continuation _ -> (
        if not (Apply_cont.is_goto action)
        then Some action
        else
          let check_handler ~handler ~action =
            match RE.to_apply_cont handler with
            | Some action -> Some action
            | None -> Some action
          in
          match cont_info_from_uenv with
          | Linearly_used_and_inlinable
              { handler;
                free_names_of_handler = _;
                params;
                cost_metrics_of_handler = _
              } ->
            assert (Bound_parameters.is_empty params);
            check_handler ~handler ~action
          | Non_inlinable_zero_arity { handler = Known handler } ->
            check_handler ~handler ~action
          | Non_inlinable_zero_arity { handler = Unknown } -> Some action
          | Invalid _ -> None
          | Toplevel_or_function_return_or_exn_continuation _ ->
            (* It is legal to call a return continuation with zero arguments; it
               might originally have had layout [void] *)
            Some action
          | Non_inlinable_non_zero_arity _ ->
            Misc.fatal_errorf
              "Inconsistency for %a between [Apply_cont.is_goto] and \
               continuation environment in [UA]:@ %a"
              Continuation.print cont UA.print uacc)
    in
    match action with
    | None ->
      (* The destination is unreachable; delete the [Switch] arm. *)
      new_let_conts, arms, mergeable_arms, identity_arms, not_arms
    | Some action -> (
      (* CR mshinwell/vlaviron: Fix alias handling so that identity switches
         like those in id_switch.ml can be simplified by only using
         [mergeable_arms]. Then remove [identity_arms]. *)
      let maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms =
        let arms = TI.Map.add arm action arms in
        (* Check to see if this arm may be merged with others. *)
        if Option.is_some (Apply_cont.trap_action action)
        then new_let_conts, arms, Not_mergeable, identity_arms, not_arms
        else
          match mergeable_arms with
          | Not_mergeable ->
            new_let_conts, arms, Not_mergeable, identity_arms, not_arms
          | No_arms ->
            let cont = Apply_cont.continuation action in
            let args =
              List.map
                (fun arg -> find_all_aliases env_at_use arg)
                (Apply_cont.args action)
            in
            ( new_let_conts,
              arms,
              Mergeable { cont; args },
              identity_arms,
              not_arms )
          | Mergeable { cont; args } ->
            if not (Continuation.equal cont (Apply_cont.continuation action))
            then new_let_conts, arms, Not_mergeable, identity_arms, not_arms
            else
              let args =
                List.map2
                  (fun arg_set arg ->
                    Alias_set.inter (find_all_aliases env_at_use arg) arg_set)
                  args (Apply_cont.args action)
              in
              ( new_let_conts,
                arms,
                Mergeable { cont; args },
                identity_arms,
                not_arms )
      in
      (* Check to see if the arm is of a form that might mean the whole [Switch]
         is a boolean NOT. *)
      match Apply_cont.to_one_arg_without_trap_action action with
      | None -> maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
      | Some arg ->
        let[@inline always] const arg =
          match Reg_width_const.descr arg with
          | Tagged_immediate arg ->
            if TI.equal arm arg
            then
              let identity_arms = TI.Map.add arm action identity_arms in
              maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
            else
              let machine_width = UE.machine_width (UA.uenv uacc) in
              if TI.equal arm (TI.bool_true machine_width)
                 && TI.equal arg (TI.bool_false machine_width)
                 || TI.equal arm (TI.bool_false machine_width)
                    && TI.equal arg (TI.bool_true machine_width)
              then
                let not_arms = TI.Map.add arm action not_arms in
                maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
              else maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
          | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int8 _
          | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_vec128 _
          | Naked_vec256 _ | Naked_vec512 _ | Naked_nativeint _ | Null ->
            maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
        in
        Simple.pattern_match arg ~const ~name:(fun _ ~coercion:_ ->
            maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms)))
  | New_wrapper new_let_cont ->
    let new_let_conts = new_let_cont :: new_let_conts in
    let action = Apply_cont.goto new_let_cont.cont in
    let arms = TI.Map.add arm action arms in
    new_let_conts, arms, Not_mergeable, identity_arms, not_arms

let filter_and_choose_alias required_names alias_set =
  let available_alias_set =
    Alias_set.filter alias_set ~f:(fun alias ->
        Simple.pattern_match alias
          ~name:(fun name ~coercion:_ -> Name.Set.mem name required_names)
          ~const:(fun _ -> true))
  in
  Alias_set.find_best available_alias_set

let find_cse_simple ?(required = true) dacc required_names prim =
  match P.Eligible_for_cse.create prim with
  | None -> None (* Constant *)
  | Some with_fixed_value -> (
    match DE.find_cse (DA.denv dacc) with_fixed_value with
    | None ->
      if required
      then
        Misc.fatal_errorf
          "Expected@ primitive@ not@ found@ in@ CSE@ environment@ while@ \
           simplifying switch:@ %a"
          P.print prim
      else None
    | Some simple ->
      filter_and_choose_alias required_names
        (find_all_aliases (DA.typing_env dacc) simple))

type must_untag_lookup_table_result =
  | Must_untag
  | Leave_as_tagged_immediate

(* Recognise sufficiently-large Switch expressions where all of the arms provide
   a single argument to a unique destination. These expressions can be compiled
   using lookup tables, which dramatically reduces code size. *)
let recognize_switch_with_single_arg_to_same_destination0 machine_width ~arms =
  let check_arm discr dest dest_and_args_rev_and_expected_discr =
    let dest' = AC.continuation dest in
    match dest_and_args_rev_and_expected_discr with
    | None -> None
    | Some (expected_dest, args_rev, expected_discr) -> (
      match expected_dest with
      | Some expected_dest when not (Continuation.equal dest' expected_dest) ->
        (* All arms must go to the same continuation. *)
        None
      | _ when not (TI.equal discr expected_discr) ->
        (* Discriminants must be 0..(num_arms-1) (note that it is possible to
           have Switches that do not satisfy this criterion in Flambda 2). *)
        None
      | Some _ | None -> (
        match AC.to_one_arg_without_trap_action dest with
        | None ->
          (* The destination continuations must have single constant arguments.
             Trap actions are forbidden. *)
          None
        | Some arg ->
          Simple.pattern_match arg
            ~name:(fun _ ~coercion:_ ->
              (* Aliases should have been followed by now. *) None)
            ~const:(fun const ->
              let expected_discr =
                TI.add (TI.one machine_width) expected_discr
              in
              Some (Some dest', const :: args_rev, expected_discr))))
  in
  match TI.Map.fold check_arm arms (Some (None, [], TI.zero machine_width)) with
  | None | Some (None, _, _) | Some (_, [], _) -> None
  | Some (Some dest, args_rev, _) -> (
    let args = List.rev args_rev in
    assert (List.compare_length_with args 1 >= 0);
    (* For the moment just do this for things that can be put in scannable
       blocks (which might then need untagging depending on how they appeared in
       the original [Switch]). *)
    let[@inline] check_args prover must_untag_lookup_table_result =
      let args' = List.filter_map prover args in
      if List.compare_lengths args args' = 0
      then Some (dest, must_untag_lookup_table_result, args')
      else None
    in
    (* All arguments must be of an appropriate kind and the same kind. *)
    match Reg_width_const.descr (List.hd args) with
    | Naked_immediate _ ->
      check_args Reg_width_const.is_naked_immediate Must_untag
    | Tagged_immediate _ ->
      (* Note that even though the [Reg_width_const] is specifying a tagged
         immediate, the value which we store inside values of that type is still
         a normal untagged [TI.t]. *)
      check_args Reg_width_const.is_tagged_immediate Leave_as_tagged_immediate
    | Naked_float _ | Naked_float32 _ | Naked_int8 _ | Naked_int16 _
    | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _
    | Naked_vec256 _ | Naked_vec512 _ | Null ->
      None)

let recognize_switch_with_single_arg_to_same_destination machine_width ~arms =
  (* Switch must be large enough. *)
  if TI.Map.cardinal arms < 3
  then None
  else recognize_switch_with_single_arg_to_same_destination0 machine_width ~arms

let rebuild_switch_with_single_arg_to_same_destination uacc ~dacc_before_switch
    ~original ~tagged_scrutinee ~dest ~consts ~must_untag_lookup_table_result
    dbg =
  let rebuilding = UA.are_rebuilding_terms uacc in
  let block_sym =
    let var = Variable.create "switch_block" K.value in
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string (Variable.unique_name var))
  in
  let uacc =
    let fields =
      List.map
        (fun const -> Simple.With_debuginfo.create (Simple.const_int const) dbg)
        consts
    in
    let block_type =
      T.immutable_array ~element_kind:(Ok KS.tagged_immediate)
        ~fields:
          (List.map
             (fun const ->
               T.alias_type_of K.value
                 (Simple.const (Reg_width_const.const_int const)))
             consts)
        Alloc_mode.For_types.heap
        ~machine_width:(DE.machine_width (DA.denv dacc_before_switch))
    in
    UA.add_lifted_constant uacc
      (LC.create_definition
         (LC.Definition.block_like
            (DA.denv dacc_before_switch)
            block_sym block_type ~symbol_projections:Variable.Map.empty
            (RSC.create_immutable_value_array rebuilding fields)))
  in
  (* CR mshinwell: consider sharing the constants *)
  let block = Simple.symbol block_sym in
  let load_from_block_prim : P.t =
    Binary (Array_load (Values, Values, Immutable), block, tagged_scrutinee)
  in
  let load_from_block = Named.create_prim load_from_block_prim dbg in
  let arg_var = Variable.create "arg" K.value in
  let arg_var_duid = Flambda_debug_uid.none in
  let arg = Simple.var arg_var in
  let final_arg_var, final_arg_var_duid, final_arg =
    match must_untag_lookup_table_result with
    | Must_untag ->
      let final_arg_var = Variable.create "final_arg" K.naked_immediate in
      let final_arg_var_duid = Flambda_debug_uid.none in
      final_arg_var, final_arg_var_duid, Simple.var final_arg_var
    | Leave_as_tagged_immediate -> arg_var, arg_var_duid, arg
  in
  (* Note that, unlike for the untagging of normal Switch scrutinees, there's no
     problem with CSE and Data_flow here. The reason is that in this case the
     generated primitive always names a fresh variable, so it will never be
     eligible for CSE. *)
  (* CR mshinwell: we could probably expose the actual integer counts of
     continuations in [Name_occurrences] and then try to inline out [dest]. This
     might happen anyway in the backend though so this probably isn't that
     important for now. *)
  let apply_cont = Apply_cont.create dest ~args:[final_arg] ~dbg in
  let free_names_of_body = Apply_cont.free_names apply_cont in
  let untag_arg_prim : P.t = Unary (Untag_immediate, arg) in
  let expr =
    let body =
      let body = RE.create_apply_cont apply_cont in
      match must_untag_lookup_table_result with
      | Leave_as_tagged_immediate -> body
      | Must_untag ->
        let bound =
          BPt.singleton (BV.create final_arg_var final_arg_var_duid NM.normal)
        in
        let untag_arg = Named.create_prim untag_arg_prim dbg in
        RE.create_let rebuilding bound untag_arg ~body ~free_names_of_body
    in
    let bound = BPt.singleton (BV.create arg_var arg_var_duid NM.normal) in
    RE.create_let rebuilding bound load_from_block ~body ~free_names_of_body
  in
  let extra_free_names =
    NO.union
      (Named.free_names load_from_block)
      (NO.remove_var free_names_of_body ~var:final_arg_var)
  in
  let machine_width = DE.machine_width (DA.denv dacc_before_switch) in
  let increase_in_code_size =
    (* Very likely negative. *)
    Code_size.( - )
      (Code_size.( + )
         (Code_size.prim ~machine_width load_from_block_prim)
         (Code_size.( + )
            (Code_size.apply_cont apply_cont)
            (match must_untag_lookup_table_result with
            | Must_untag -> Code_size.prim ~machine_width untag_arg_prim
            | Leave_as_tagged_immediate -> Code_size.zero)))
      (Code_size.switch original)
  in
  let uacc =
    UA.add_free_names uacc extra_free_names
    (* CR mshinwell: it seems we need to fix [Cost_metrics] so we can note that
       we have *added* operations here (load, maybe untagging). *)
    |> UA.notify_added ~code_size:increase_in_code_size
  in
  expr, uacc

let recognize_affine_switch_to_same_destination machine_width consts =
  match consts with
  | [] | [_] -> None
  | const0 :: const1 :: other_consts ->
    let slope = TI.sub const1 const0 in
    let rec check offset slope index = function
      | [] -> Some (offset, slope)
      | const :: _ when not TI.(equal const (add (mul index slope) offset)) ->
        None
      | _ :: consts ->
        check offset slope (TI.add index (TI.one machine_width)) consts
    in
    check const0 slope (TI.of_int machine_width 2) other_consts

(* Tiny DSL to preserve sanity while rebuilding expressions. *)

let bound_prim name kind prim dbg = name, kind, prim, dbg

let ( let$ ) (name, kind, prim, dbg) k uacc ~dacc_before_switch =
  match
    find_cse_simple ~required:false dacc_before_switch (UA.required_names uacc)
      prim
  with
  | Some simple -> k simple uacc ~dacc_before_switch
  | None ->
    let named = Named.create_prim prim dbg in
    let var = Variable.create name kind in
    let uacc = UA.add_free_names uacc (NO.singleton_variable var NM.normal) in
    let body, uacc = k (Simple.var var) uacc ~dacc_before_switch in
    let duid = Flambda_debug_uid.none in
    let machine_width = UE.machine_width (UA.uenv uacc) in
    let binding =
      EB.Keep_binding
        { let_bound = BPt.singleton (BV.create var duid NM.normal);
          simplified_defining_expr =
            Simplified_named.create ~machine_width named;
          original_defining_expr = None
        }
    in
    EB.make_new_let_bindings uacc ~bindings_outermost_first:[binding] ~body

let return ~code_size ~free_names expr uacc ~dacc_before_switch:_ =
  let uacc = UA.notify_added ~code_size uacc in
  let uacc = UA.add_free_names uacc free_names in
  expr, uacc

let run uacc ~dacc_before_switch k = k uacc ~dacc_before_switch

let rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch ~original
    ~scrutinee ~tagged_scrutinee ~dest ~offset ~slope
    ~must_untag_lookup_table_result dbg =
  (* We are creating the following fragment: *)
  (* let scaled = x * slope in
   * let final = scaled + offset in
   * apply_cont k final
   *)
  let scrutinee, kind, standard_int, const =
    match must_untag_lookup_table_result with
    | Must_untag ->
      ( scrutinee,
        K.naked_immediate,
        K.Standard_int.Naked_immediate,
        Reg_width_const.naked_immediate )
    | Leave_as_tagged_immediate ->
      ( tagged_scrutinee,
        K.value,
        K.Standard_int.Tagged_immediate,
        Reg_width_const.tagged_immediate )
  in
  run ~dacc_before_switch uacc
    (let mul_prim : P.t =
       Binary
         (Int_arith (standard_int, Mul), scrutinee, Simple.const (const slope))
     in
     let$ scaled_arg = bound_prim "scaled_arg" kind mul_prim dbg in
     let prim : P.t =
       Binary
         (Int_arith (standard_int, Add), scaled_arg, Simple.const (const offset))
     in
     let$ final_arg = bound_prim "final_arg" kind prim dbg in
     let apply_cont = Apply_cont.create dest ~args:[final_arg] ~dbg in
     let free_names = Apply_cont.free_names apply_cont in
     let increase_in_code_size =
       Code_size.( - )
         (Code_size.apply_cont apply_cont)
         (Code_size.switch original)
     in
     return ~code_size:increase_in_code_size ~free_names
       (RE.create_apply_cont apply_cont))

let rebuild_switch ~original ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
    ~dacc_before_switch uacc ~after_rebuild =
  let new_let_conts, arms, mergeable_arms, identity_arms, not_arms =
    TI.Map.fold (rebuild_arm uacc) arms
      ([], TI.Map.empty, No_arms, TI.Map.empty, TI.Map.empty)
  in
  let switch_merged =
    match mergeable_arms with
    | No_arms | Not_mergeable -> None
    | Mergeable { cont; args } ->
      let num_args = List.length args in
      let required_names = UA.required_names uacc in
      let args =
        List.filter_map (filter_and_choose_alias required_names) args
      in
      if List.compare_length_with args num_args = 0
      then Some (cont, args)
      else None
  in
  let switch_is_identity =
    let arm_discrs = TI.Map.keys arms in
    let identity_arms_discrs = TI.Map.keys identity_arms in
    if not (TI.Set.equal arm_discrs identity_arms_discrs)
    then None
    else
      TI.Map.data identity_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let machine_width = DE.machine_width (DA.denv dacc_before_switch) in
  let switch_is_boolean_not =
    let arm_discrs = TI.Map.keys arms in
    let not_arms_discrs = TI.Map.keys not_arms in
    if (not (TI.Set.equal arm_discrs (TI.all_bools machine_width)))
       || not (TI.Set.equal arm_discrs not_arms_discrs)
    then None
    else
      TI.Map.data not_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let switch_is_single_arg_to_same_destination =
    recognize_switch_with_single_arg_to_same_destination machine_width ~arms
  in
  let body, uacc =
    if TI.Map.cardinal arms < 1
    then
      let uacc = UA.notify_removed ~operation:Removed_operations.branch uacc in
      RE.create_invalid Zero_switch_arms, uacc
    else
      let dbg = Debuginfo.none in
      let[@inline] normal_case0 uacc =
        (* In that case, even though some branches were removed by simplify we
           should not count them in the number of removed operations: these
           branches wouldn't have been taken during execution anyway. *)
        let expr, uacc =
          EB.create_switch uacc ~condition_dbg ~scrutinee ~arms
        in
        if Flambda_features.check_invariants ()
           && Simple.is_const scrutinee
           && TI.Map.cardinal arms > 1
        then
          Misc.fatal_errorf
            "[Switch] with constant scrutinee (type: %a) should have been \
             simplified away:@ %a"
            T.print scrutinee_ty
            (RE.print (UA.are_rebuilding_terms uacc))
            expr;
        expr, uacc
      in
      let[@inline] normal_case uacc =
        match switch_is_single_arg_to_same_destination with
        | None -> normal_case0 uacc
        | Some (dest, must_untag_lookup_table_result, consts) -> (
          assert (List.length consts = TI.Map.cardinal arms);
          let tagging_prim : P.t = Unary (Tag_immediate, scrutinee) in
          match
            find_cse_simple dacc_before_switch (UA.required_names uacc)
              tagging_prim
          with
          | None -> normal_case0 uacc
          | Some tagged_scrutinee -> (
            match
              recognize_affine_switch_to_same_destination machine_width consts
            with
            | None ->
              rebuild_switch_with_single_arg_to_same_destination uacc
                ~dacc_before_switch ~original ~tagged_scrutinee ~dest ~consts
                ~must_untag_lookup_table_result dbg
            | Some (offset, slope) ->
              rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch
                ~original ~scrutinee ~tagged_scrutinee ~dest ~offset ~slope
                ~must_untag_lookup_table_result dbg))
      in
      match switch_merged with
      | Some (dest, args) ->
        let uacc =
          UA.notify_removed ~operation:Removed_operations.branch uacc
        in
        let apply_cont = Apply_cont.create dest ~args ~dbg in
        let expr = RE.create_apply_cont apply_cont in
        let uacc = UA.add_free_names uacc (Apply_cont.free_names apply_cont) in
        expr, uacc
      | None -> (
        match switch_is_identity with
        | Some dest -> (
          let uacc =
            (* CR mshinwell: it seems like this should be registering the
               potentially significant reduction in code size -- likewise in
               other cases here. Plus the fact that some operations are
               *added*. *)
            UA.notify_removed ~operation:Removed_operations.branch uacc
          in
          let tagging_prim : P.t = Unary (Tag_immediate, scrutinee) in
          match
            find_cse_simple dacc_before_switch (UA.required_names uacc)
              tagging_prim
          with
          | None -> normal_case uacc
          | Some tagged_scrutinee ->
            let apply_cont =
              Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg
            in
            let expr = RE.create_apply_cont apply_cont in
            let uacc =
              UA.add_free_names uacc (Apply_cont.free_names apply_cont)
            in
            expr, uacc)
        | None -> (
          match switch_is_boolean_not with
          | Some dest -> (
            let uacc =
              UA.notify_removed ~operation:Removed_operations.branch uacc
            in
            let not_scrutinee = Variable.create "not_scrutinee" K.value in
            let not_scrutinee_duid = Flambda_debug_uid.none in
            let not_scrutinee' = Simple.var not_scrutinee in
            let tagging_prim : P.t = Unary (Tag_immediate, scrutinee) in
            match
              find_cse_simple dacc_before_switch (UA.required_names uacc)
                tagging_prim
            with
            | None -> normal_case uacc
            | Some tagged_scrutinee ->
              (* CR bclement: look it up in CSE environment *)
              let do_tagging =
                Named.create_prim
                  (P.Unary (Boolean_not, tagged_scrutinee))
                  Debuginfo.none
              in
              let bound =
                VB.create not_scrutinee not_scrutinee_duid NM.normal
                |> Bound_pattern.singleton
              in
              let apply_cont =
                Apply_cont.create dest ~args:[not_scrutinee'] ~dbg
              in
              let body = RE.create_apply_cont apply_cont in
              let free_names_of_body = Apply_cont.free_names apply_cont in
              let expr =
                RE.create_let
                  (UA.are_rebuilding_terms uacc)
                  bound do_tagging ~body ~free_names_of_body
              in
              let uacc =
                UA.add_free_names uacc
                  (NO.union
                     (Named.free_names do_tagging)
                     (NO.diff free_names_of_body
                        ~without:(NO.singleton_variable not_scrutinee NM.normal)))
              in
              expr, uacc)
          | None -> normal_case uacc))
  in
  let uacc, expr = EB.bind_let_conts uacc ~body new_let_conts in
  after_rebuild expr uacc

let simplify_arm ~typing_env_at_use ~scrutinee_ty arm action (arms, dacc) =
  let shape = T.this_naked_immediate arm in
  match T.meet typing_env_at_use scrutinee_ty shape with
  | Bottom -> arms, dacc
  | Ok (_meet_ty, env_at_use) ->
    let denv_at_use = DE.with_typing_env (DA.denv dacc) env_at_use in
    let args = AC.args action in
    let use_kind =
      Simplify_common.apply_cont_use_kind ~context:Switch_branch action
    in
    let { S.simples = args; simple_tys = arg_types } =
      S.simplify_simples dacc args
    in
    let dacc, rewrite_id =
      DA.record_continuation_use dacc (AC.continuation action) use_kind
        ~env_at_use:denv_at_use ~arg_types
    in
    let arity =
      arg_types
      |> List.map (fun ty -> K.With_subkind.anything (T.kind ty))
      |> Flambda_arity.create_singletons
    in
    let action = Apply_cont.update_args action ~args in
    let dbg = AC.debuginfo action in
    let dbg = DE.add_inlined_debuginfo (DA.denv dacc) dbg in
    let action = AC.with_debuginfo action ~dbg in
    let dacc =
      DA.map_flow_acc dacc
        ~f:
          (Flow.Acc.add_apply_cont_args ~rewrite_id
             (Apply_cont.continuation action)
             args)
    in
    let arms = TI.Map.add arm (action, rewrite_id, arity, env_at_use) arms in
    arms, dacc

let decide_continuation_specialization0 ~dacc ~switch ~scrutinee =
  match DA.are_lifting_conts dacc with
  | Lifting_out_of _ ->
    Misc.fatal_errorf
      "[Are_lifting_cont] values in the dacc cannot be [Lifting_out_of _] when \
       going downwards through a [Switch] expression. See the explanation in \
       [are_lifting_conts.mli]."
  | Not_lifting _ -> dacc, `Not_lifting
  | Analyzing { continuation; uses; is_exn_handler } -> (
    (* Some preliminary requirements. We do **not** specialize continuations if
       one of the following conditions are true:

       - they have only one (or less) use

       - they are an exception handler. To handle this case, the existing
       mechanism used to rewrite specialized calls on the way up should be
       extended to also rewrite pop_traps and other uses of exn handlers (which
       is not currently the case).

       - we are at toplevel, in which case there can be symbols which we might
       duplicate by specializing (which would be an error). More generally, the
       benefits of specialization at unit toplevel do not seem that great,
       because partial evaluation would be better. *)
    let n_uses = Continuation_uses.number_of_uses uses in
    if n_uses <= 1
    then dacc, `Single_use
    else if is_exn_handler
    then dacc, `Exn_handler
    else if DE.at_unit_toplevel (DA.denv dacc)
    then dacc, `Toplevel
    else
      let denv = DA.denv dacc in
      match DE.specialization_cost denv with
      | Cannot_specialize { reason } ->
        (* CR gbury: we could try and emit something analog to the inlining
           report, but for other optimizations at one point ? *)
        let result =
          match reason with
          | Specialization_disabled -> `Disabled
          | At_toplevel -> `Toplevel
          | Contains_static_consts | Contains_set_of_closures ->
            `Cannot_specialize
        in
        dacc, result
      | Can_specialize spec_cost as spec_cost' -> (
        (* We should never reach here if specialization is disabled, since we
           should never have created a `Can_specialize` value for the
           specialization_cost *)
        if not (Flambda_features.match_in_match ())
        then
          Misc.fatal_errorf
            "Cannot specialize continuations (due to command line arguments), \
             this code path should not have been reached.";
        (* Estimate the cost of lifting: this mainly comes from adding new
           parameters, which increase the work done by the typing env, as well
           as the flow analysis. We then only do the lifting if the cost is
           within the budget for the current function. *)
        let lifting_budget = DA.get_continuation_lifting_budget dacc in
        let lifting_cost =
          DE.cost_of_lifting_continuations_out_of_current_one denv
        in
        (* is_lifting_allowed_by_budget ? *)
        if not (lifting_budget > 0 && lifting_cost <= lifting_budget)
        then dacc, `Insuficient_lifting_budget
        else
          (* Main Criterion: whether all callsites (but one) of the continuation
             determine the value of the scrutinee (and therefore the specialized
             versions will eliminate the switch in favor of an apply_cont
             directly). *)
          let join_analysis_result =
            match DE.join_analysis denv with
            | None -> `Not_enough_join_info
            | Some join_analysis -> (
              match
                Join_analysis.simple_refined_at_join join_analysis
                  (DE.typing_env denv) scrutinee
              with
              | Not_refined_at_join -> `Not_enough_join_info
              | Invariant_in_all_uses _ ->
                (* in this case, we don't need to specialize to know the
                   scrutinee, or to simplify the switch, it will hapen without
                   specialization. *)
                `No_reason_to_spec
              | Variable_refined_at_these_uses var_analysis -> (
                let specialized, generic =
                  Join_analysis.Variable_refined_at_join.fold_values_at_uses
                    (fun id value (specialized, generic) ->
                      match value with
                      | Known _ ->
                        Apply_cont_rewrite_id.Set.add id specialized, generic
                      | Unknown ->
                        specialized, Apply_cont_rewrite_id.Set.add id generic)
                    var_analysis
                    ( Apply_cont_rewrite_id.Set.empty,
                      Apply_cont_rewrite_id.Set.empty )
                in
                match Apply_cont_rewrite_id.Set.cardinal generic with
                | 0 | 1 -> `Spec (join_analysis, specialized, generic)
                | _ ->
                  if Apply_cont_rewrite_id.Set.is_empty specialized
                  then `All_unknown
                  else `Too_many_unknown_uses))
          in
          match join_analysis_result with
          | ( `No_reason_to_spec | `Too_many_unknown_uses | `All_unknown
            | `Not_enough_join_info ) as res ->
            dacc, res
          | `Spec (join_analysis, specialized, generic) ->
            (* Specialization benefit estimation: we use heuristics similar to
               that of inlining to estimate the benefit based on code size and
               removed operations.

               CR gbury/bclement: currently the size of primtives do not include
               some primitives that we expect to be simplified away (e.g.
               Tag_immediate and Get_tag) and thus we often times expect
               [size_of_primitives] to be 0. We should/could try to use info
               from the typing env to know which primitives will have their
               result determined by the values that are known at call_site (and
               thus will disappear from the specialized versions). *)
            let cost_metrics =
              Specialization_cost.cost_metrics (DE.typing_env denv) spec_cost
                ~switch ~join_analysis ~specialized ~generic
            in
            let final_cost =
              Cost_metrics.evaluate
                ~args:(DE.inlining_arguments denv)
                cost_metrics
            in
            let threshold = Flambda_features.Expert.cont_spec_threshold () in
            if debug ()
            then
              Format.eprintf
                "*** Continuation specialization %a@\n\
                 analysis result: %f / %f@\n\
                 known: %a @\n\
                 unknown: %a @\n\
                 spec cost: %a@\n\
                 %a@."
                Continuation.print continuation final_cost threshold
                Apply_cont_rewrite_id.Set.print specialized
                Apply_cont_rewrite_id.Set.print generic
                Specialization_cost.print spec_cost'
                Inlining_report.Context.print_cost_metrics cost_metrics;
            if Float.compare threshold 0. < 0
               || Float.compare final_cost threshold > 0
            then dacc, `Too_costly
            else
              let dacc =
                DA.decrease_continuation_lifting_budget dacc lifting_cost
              in
              let dacc =
                DA.with_are_lifting_conts dacc
                  (Are_lifting_conts.lift_continuations_out_of continuation)
              in
              let dacc = DA.add_continuation_to_specialize dacc continuation in
              dacc, `Specialized))

let decide_continuation_specialization ~dacc ~switch ~scrutinee =
  Profile.record_with_counters ~accumulate:true "continuation_specialization"
    (fun () -> decide_continuation_specialization0 ~dacc ~switch ~scrutinee)
    ()
    ~counter_f:(fun (_, result) ->
      let counters = Profile.Counters.create () in
      match result with
      | `Disabled | `Single_use | `Exn_handler | `Toplevel | `All_unknown
      | `No_reason_to_spec | `Not_lifting ->
        counters
      | `Cannot_specialize -> Profile.Counters.incr "cannot_spec" counters
      | `Insuficient_lifting_budget ->
        Profile.Counters.incr "no_lifting_budget" counters
      | `Not_enough_join_info -> Profile.Counters.incr "no_join_info" counters
      | `Too_many_unknown_uses ->
        Profile.Counters.incr "too_much_unknown" counters
      | `Too_costly -> Profile.Counters.incr "not_beneficial" counters
      | `Specialized -> Profile.Counters.incr "specialized" counters)

let simplify_switch0 dacc switch ~down_to_up =
  let scrutinee = Switch.scrutinee switch in
  let scrutinee_ty, scrutinee =
    S.simplify_simple dacc scrutinee ~min_name_mode:NM.normal
  in
  let dacc_before_switch = dacc in
  let typing_env_at_use = DA.typing_env dacc in
  let arms, dacc =
    TI.Map.fold
      (simplify_arm ~typing_env_at_use ~scrutinee_ty)
      (Switch.arms switch) (TI.Map.empty, dacc)
  in
  let dacc =
    if TI.Map.cardinal arms <= 1
    then dacc
    else
      DA.map_flow_acc dacc
        ~f:(Flow.Acc.add_used_in_current_handler (Simple.free_names scrutinee))
  in
  let condition_dbg =
    DE.add_inlined_debuginfo (DA.denv dacc) (Switch.condition_dbg switch)
  in
  let dacc, _ = decide_continuation_specialization ~dacc ~switch ~scrutinee in
  down_to_up dacc
    ~rebuild:
      (rebuild_switch ~original:switch ~arms ~condition_dbg ~scrutinee
         ~scrutinee_ty ~dacc_before_switch)

let simplify_switch ~simplify_let_with_bound_pattern ~simplify_function_body
    dacc switch ~down_to_up =
  let tagged_scrutinee = Variable.create "tagged_scrutinee" K.value in
  let tagged_scrutinee_duid = Flambda_debug_uid.none in
  let tagging_prim =
    Named.create_prim
      (Unary (Tag_immediate, Switch.scrutinee switch))
      Debuginfo.none
  in
  let let_expr =
    (* [body] won't be looked at (see below). *)
    Let.create
      (Bound_pattern.singleton
         (Bound_var.create tagged_scrutinee tagged_scrutinee_duid NM.normal))
      tagging_prim
      ~body:(Expr.create_switch switch)
      ~free_names_of_body:Unknown
  in
  simplify_let_with_bound_pattern
    ~simplify_expr_with_bound_pattern:
      (fun dacc (bound_pattern, _body) ~down_to_up ->
      let dacc =
        DA.map_flow_acc dacc
          ~f:
            (Flow.Acc.add_used_in_current_handler
               (Bound_pattern.free_names bound_pattern))
      in
      simplify_switch0 dacc switch ~down_to_up)
    ~simplify_function_body dacc let_expr ~down_to_up
