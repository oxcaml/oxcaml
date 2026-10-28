(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module DE = Downwards_env
module DA = Downwards_acc
module T = Flambda2_types
module TE = T.Typing_env
module UA = Upwards_acc
module UE = Upwards_env

(* CR mshinwell for poechsel: We need to emit [Warnings.Inlining_impossible] as
   required.

   When in fallback-inlining mode: if we want to follow Closure we should not
   complain about function declarations with e.g. [@inline always] if the
   function contains other functions and therefore cannot be inlined. We should
   however contain at call sites if inlining is requested but cannot be done for
   this reason. I think this will probably all happen without any specific code
   once [Inlining_impossible] handling is implemented for the
   non-fallback-inlining cases.

   mshinwell 2022-07-11: we should check this when we look at classic mode
   again *)

(* CR-someday mshinwell: Overhaul handling of the inlining depth tracking so
   that it takes into account the depth of closures (or code), as per
   conversation with lwhite. *)

module FT = Flambda2_types.Function_type

let speculative_inlining dacc ~apply ~function_type ~simplify_expr ~rebuild_expr
    ~return_arity =
  let dacc = DA.prepare_for_speculative_inlining dacc in
  (* CR-someday poechsel: [Inlining_transforms.inline] is preparing the body for
     inlining. Right know it may be called twice (once there and once in
     [simplify_apply_expr]) on the same apply expr. It should be possible to
     only call it once and remove some allocations. *)
  let dacc, expr =
    (* The only way for [unroll_to] not to be None is when an explicit Unroll
       annotation is provided by the user. If this is the case then inliner will
       always inline the function and will not call [speculative_inlining]. Thus
       inside of [speculative_inlining] we will always have [unroll_to] = None.
       We are not disabling unrolling when speculating, it just happens that no
       unrolling can happen while speculating right now. *)
    Inlining_transforms.inline dacc ~apply ~unroll_to:None
      ~was_inline_always:false function_type
  in
  let dummy_toplevel_cont =
    Continuation.create ~name:"speculative_inlining_toplevel_continuation" ()
  in
  let dacc =
    DA.with_flow_acc
      (Flow.Acc.init_toplevel ~dummy_toplevel_cont Bound_parameters.empty)
      dacc
  in
  let expr, dacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc expr -> expr, dacc)
  in
  let _, uacc =
    let exn_continuation = Apply.exn_continuation apply in
    let dacc =
      DA.map_flow_acc dacc ~f:(Flow.Acc.exit_continuation dummy_toplevel_cont)
    in
    let data_flow = DA.flow_acc dacc in
    (* The dataflow analysis *)
    let function_return_cont =
      match Apply.continuation apply with
      | Never_returns -> Continuation.create ()
      | Return cont -> cont
    in
    (* When doing the speculative analysis, in order to not blow up, the
       data_flow analysis is only done on the speculatively inlined body;
       however the reachable code_ids part of the data flow analysis is only
       correct at toplevel when all information about the code_age relation and
       used_value slots is available (for the whole compilation unit). Thus we
       here provide empty/dummy values for the used_value_slots and
       code_age_relation, and ignore the reachable_code_id part of the data_flow
       analysis. *)
    let flow_result =
      Flow.Analysis.analyze data_flow ~speculative:true
        ~print_name:"speculative" ~code_age_relation:Code_age_relation.empty
        ~used_value_slots:Unknown ~code_ids_to_never_delete:Code_id.Set.empty
        ~specialization_map:(DA.specialization_map dacc)
        ~return_continuation:function_return_cont
        ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
        ~machine_width:(DE.machine_width (DA.denv dacc))
    in
    let uenv =
      (* Note that we don't need to do anything special if the exception
         continuation takes extra arguments, since we are only simplifying the
         body of the function in question, not substituting it into an existing
         context. *)
      let machine_width = DE.machine_width (DA.denv dacc) in
      UE.add_function_return_or_exn_continuation ~is_cold:false
        (UE.create (DA.are_rebuilding_terms dacc) ~machine_width)
        (Exn_continuation.exn_handler exn_continuation)
        (Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value])
    in
    let uenv =
      match Apply.continuation apply with
      | Never_returns -> uenv
      | Return return_continuation ->
        UE.add_function_return_or_exn_continuation ~is_cold:false uenv
          return_continuation
          (Flambda_arity.unarize_t return_arity)
    in
    let uacc = UA.create ~flow_result ~compute_slot_offsets:false uenv dacc in
    rebuild_expr expr uacc ~after_rebuild:(fun expr uacc -> expr, uacc)
  in
  let cost_metrics_of_lifted_constants =
    if Flambda_features.Inlining.speculative_inlining_track_lifted_constants ()
    then
      (* If we are not at toplevel, there might still be lifted constants to be
         placed in the accumulator whose size must be taken into account for
         speculative inlining. *)
      let lifted_constants = UA.lifted_constants uacc in
      (* CR-someday bclement: Ideally we would simply call
         [place_lifted_constants] in [after_rebuild] above so that we can share
         the code with the non-speculative inlining code path; however, that
         function expects to be called at toplevel and there could be unintended
         consequences -- notably regarding the validity of the used value slots.

         At the time of writing, this means that we incorrectly:

         - Ignore the size of the symbol projections created during speculative
         inlining;

         - Count the size of unused value slots of lifted sets of closures
         created during speculative inlining (but again, it is not clear that it
         is always possible to compute a correct set of "used value slots" at
         the time we are doing speculative inlining, because some value slots
         could be used later in the compilation unit). *)
      Lifted_constant_state.fold lifted_constants ~init:Cost_metrics.zero
        ~f:(fun cost_metrics lifted_constant ->
          List.fold_left
            (fun cost_metrics definition ->
              Cost_metrics.( + ) cost_metrics
                (Rebuilt_static_const.cost_metrics
                   (Lifted_constant.Definition.defining_expr definition)))
            cost_metrics
            (Lifted_constant.definitions lifted_constant))
    else Cost_metrics.zero
  in
  Cost_metrics.( + ) (UA.cost_metrics uacc) cost_metrics_of_lifted_constants

type argument_types_useful =
  | Coarse
  | Fine

let argument_types_useful =
  Oxcaml_args.Extra_options.symbol __LOC__ "argument-types-useful" Coarse
    ["coarse", Coarse; "fine", Fine]

let argument_types_useful dacc ~apply ~code_metadata =
  if
    not
      (Flambda_features.Inlining.speculative_inlining_only_if_arguments_useful
         ())
  then true
  else
    let typing_env = DE.typing_env (DA.denv dacc) in
    match argument_types_useful () with
    | Coarse ->
      List.exists
        (fun simple ->
          Simple.pattern_match simple
            ~name:(fun name ~coercion:_ ->
              let ty = TE.find typing_env name None in
              not (T.is_unknown_maybe_null typing_env ty))
            ~const:(fun _ -> true))
        (Apply.args apply)
    | Fine ->
      let arity = Code_metadata.params_arity code_metadata in
      List.exists2
        (fun full_kind simple ->
          Simple.pattern_match simple
            ~name:(fun name ~coercion:_ ->
              T.type_is_useful full_kind typing_env name)
            ~const:(fun const ->
              (* If the kind already restricts the possible values to a single
                 constant (e.g. unit type), even a constant can be not
                 useful. *)
              match Reg_width_const.is_tagged_immediate const with
              | None -> true
              | Some const ->
                not
                  (Flambda_kind.With_subkind.equal full_kind
                     (Flambda_kind.With_subkind.create Flambda_kind.value
                        (Variant
                           { consts = Target_ocaml_int.Set.singleton const;
                             non_consts = Tag.Scannable.Map.empty
                           })
                        Non_nullable))))
        (Flambda_arity.unarize arity)
        (Apply.args apply)

let inlining_does_decrease_code_size ~code_metadata cost_metrics =
  let[@ocamlformat "break-infix=fit-or-vertical"] original_code_size =
    code_metadata |> Code_metadata.cost_metrics |> Cost_metrics.size
  in
  let inlined_code_size = Cost_metrics.size cost_metrics in
  not (Code_size.( <= ) original_code_size inlined_code_size)

let might_inline dacc ~apply ~code_metadata ~function_type ~simplify_expr
    ~rebuild_expr ~return_arity : Call_site_inlining_decision_type.t =
  let code_present () =
    let code_or_metadata =
      DE.find_code_exn (DA.denv dacc) (Code_metadata.code_id code_metadata)
    in
    Code_or_metadata.code_present code_or_metadata
  in
  let denv = DA.denv dacc in
  let disable_inlining = DE.disable_inlining denv in
  let decision = Code_metadata.inlining_decision code_metadata in
  let is_a_functor = Code_metadata.is_a_functor code_metadata in
  let in_a_stub, doing_speculative_inlining =
    match disable_inlining with
    | Disable_inlining Stub -> true, false
    | Disable_inlining Speculative_inlining -> false, true
    | Do_not_disable_inlining -> false, false
  in
  if in_a_stub
  then In_a_stub
  else if Function_decl_inlining_decision_type.must_be_inlined decision
  then
    if code_present ()
    then
      Definition_says_inline
        { was_inline_always =
            Function_decl_inlining_decision_type.has_attribute_inline decision
        }
    else Missing_code
  else if Function_decl_inlining_decision_type.cannot_be_inlined decision
  then Definition_says_not_to_inline
  else if doing_speculative_inlining
  then Doing_speculative_inlining
  else
    Profile.record_call_with_counters ~accumulate:true "speculative_inlining"
      ~counter_f:(fun (decision : Call_site_inlining_decision_type.t) ->
        let counters = Profile.Counters.create () in
        match decision with
        | Argument_types_not_useful ->
          Profile.Counters.incr "argument_types_not_useful" counters
        | Speculatively_inline { cost_metrics; _ } ->
          let counters =
            Profile.Counters.incr "speculatively_inline" counters
          in
          if inlining_does_decrease_code_size ~code_metadata cost_metrics
          then counters
          else Profile.Counters.incr "same_code_size" counters
        | Speculatively_not_inline _ ->
          Profile.Counters.incr "speculatively_not_inline" counters
        | Missing_code | Definition_says_not_to_inline | In_a_stub
        | Doing_speculative_inlining | Unrolling_depth_exceeded
        | Max_inlining_depth_exceeded | Recursion_depth_exceeded
        | Never_inlined_attribute
        | Forward_inlined_attribute_but_nothing_to_forward | Attribute_always
        | Replay_history_says_must_inline _ | Begin_unrolling _
        | Continue_unrolling | Definition_says_inline _ | Jsir_inlining_disabled
          ->
          (* These can't be returned by the speculative inlining cases below. *)
          if Flambda_features.check_light_invariants ()
          then
            Misc.fatal_error
              "Unexpected call site inlinine decision for speculative inlining";
          counters)
      (fun () : Call_site_inlining_decision_type.t ->
        if not (argument_types_useful dacc ~apply ~code_metadata)
        then Argument_types_not_useful
        else if not (code_present ())
        then Missing_code
        else
          let cost_metrics =
            speculative_inlining ~apply dacc ~simplify_expr ~rebuild_expr
              ~return_arity ~function_type
          in
          let inlining_args =
            Inlining_arguments.combine
              ~from_env:(DE.inlining_arguments denv)
              ~from_metadata:(Apply.inlining_arguments apply)
          in
          let evaluated_to =
            Cost_metrics.evaluate ~args:inlining_args cost_metrics
          in
          let threshold = Inlining_arguments.threshold inlining_args in
          let is_under_inline_threshold =
            Float.compare evaluated_to threshold <= 0
          in
          if is_under_inline_threshold
          then
            Speculatively_inline
              { cost_metrics; evaluated_to; threshold; is_a_functor }
          else
            Speculatively_not_inline
              { cost_metrics; evaluated_to; threshold; is_a_functor })

let get_rec_info dacc ~function_type =
  let rec_info = FT.rec_info function_type in
  match Flambda2_types.meet_rec_info (DA.typing_env dacc) rec_info with
  | Known_result rec_info -> rec_info
  | Need_meet -> Rec_info_expr.unknown
  | Invalid -> (* CR vlaviron: ? *) Rec_info_expr.do_not_inline

let is_self_worker_adapter dacc code_metadata =
  let denv = DA.denv dacc in
  if not (Code_metadata.stub code_metadata)
  then false
  else
    match DE.closure_info denv with
    | Not_in_a_closure | In_a_set_of_closures_but_not_yet_in_a_specific_closure
      ->
      false
    | Closure { code_id = worker_code_id; my_closure = worker_closure; _ } -> (
      try
        let worker_metadata = DE.find_code_metadata_exn denv worker_code_id in
        let wrapper =
          DE.find_code_exn denv (Code_metadata.code_id code_metadata)
        in
        if
          Code_metadata.stub worker_metadata
          || not (Code_or_metadata.code_present wrapper)
        then false
        else
          let remaining = ref 64 in
          let has_numeric_adaptation = ref false in
          let projected_worker =
            let typing_env = DE.typing_env denv in
            let closure_type =
              TE.find typing_env (Name.var worker_closure) None
            in
            match T.meet_single_closures_entry typing_env closure_type with
            | Known_result (function_slot, _, _, function_type) ->
              Some (function_slot, FT.code_id function_type)
            | Need_meet | Invalid -> None
          in
          let calls_worker apply code_id =
            let code_age_relation = TE.code_age_relation (DE.typing_env denv) in
            let rec is_version_of fuel newer older =
              if Code_id.equal newer older
              then true
              else if fuel = 0
              then false
              else
                match
                  Code_age_relation.get_older_version_of code_age_relation newer
                with
                | None -> false
                | Some previous -> is_version_of (fuel - 1) previous older
            in
            if
              is_version_of 64 code_id worker_code_id
              || is_version_of 64 worker_code_id code_id
            then
              let target = DE.find_code_metadata_exn denv code_id in
              (not (Code_metadata.stub target))
              && Flambda_arity.equal_ignoring_subkinds (Apply.args_arity apply)
                   (Code_metadata.params_arity target)
              && Flambda_arity.equal_ignoring_subkinds
                   (Apply.return_arity apply)
                   (Code_metadata.result_arity target)
            else false
          in
          let is_worker_callee worker_callees simple =
            Simple.pattern_match simple
              ~name:(fun name ~coercion:_ ->
                Name.pattern_match name
                  ~var:(fun var -> Variable.Set.mem var worker_callees)
                  ~symbol:(fun _ -> false))
              ~const:(fun _ -> false)
          in
          let adapter_binding wrapper_closure worker_callees (named : Named.t) =
            match named with
            | Simple simple -> true, is_worker_callee worker_callees simple
            | Rec_info _ -> true, false
            | Set_of_closures _ | Static_consts _ -> false, false
            | Prim (primitive, _) -> (
              match primitive with
              | Nullary _ | Binary _ | Ternary _ | Quaternary _ | Variadic _ ->
                false, false
              | Unary (primitive, arg) -> (
                match primitive with
                | Box_number _ | Unbox_number _ ->
                  has_numeric_adaptation := true;
                  true, false
                | Project_function_slot { move_to; _ } ->
                  let is_worker =
                    match projected_worker with
                    | None -> false
                    | Some (worker_slot, _) ->
                      Function_slot.equal move_to worker_slot
                      && Simple.equal
                           (Simple.without_coercion arg)
                           (Simple.var wrapper_closure)
                  in
                  true, is_worker
                | Block_load _ | Duplicate_block _ | Duplicate_array _
                | Is_int _ | Is_null | Get_tag | Array_length _
                | Bigarray_length _ | String_length _ | Int_as_pointer _
                | Opaque_identity _ | Int_arith _ | Float_arith _ | Num_conv _
                | Boolean_not | Reinterpret_64_bit_word _
                | Reinterpret_boxed_vector | Untag_immediate | Tag_immediate
                | Project_value_slot _ | Is_boxed_float | Is_flat_float_array
                | End_region _ | End_try_region _ | Obj_dup _ | Get_header
                | Peek _ | Make_lazy _ ->
                  false, false))
          in
          let rec count_calls wrapper_closure worker_callees expr =
            if !remaining = 0
            then None
            else (
              decr remaining;
              match Expr.descr expr with
              | Let let_expr ->
                let allowed, is_worker_projection =
                  adapter_binding wrapper_closure worker_callees
                    (Let.defining_expr let_expr)
                in
                if allowed
                then
                  Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
                      let worker_callees =
                        match bound_pattern with
                        | Singleton var when is_worker_projection ->
                          Variable.Set.add (Bound_var.var var) worker_callees
                        | Singleton _ | Set_of_closures _ | Static _ ->
                          worker_callees
                      in
                      count_calls wrapper_closure worker_callees body)
                else None
              | Let_cont
                  (Non_recursive { handler; is_applied_with_traps = false; _ })
                -> (
                let continuation_handler =
                  Non_recursive_let_cont_handler.handler handler
                in
                if Continuation_handler.is_exn_handler continuation_handler
                then None
                else
                  let handler_calls =
                    Continuation_handler.pattern_match continuation_handler
                      ~f:(fun _ ~handler ->
                        count_calls wrapper_closure worker_callees handler)
                  in
                  match handler_calls with
                  | None -> None
                  | Some handler_calls ->
                    Non_recursive_let_cont_handler.pattern_match handler
                      ~f:(fun _ ~body ->
                        match
                          count_calls wrapper_closure worker_callees body
                        with
                        | None -> None
                        | Some body_calls -> Some (handler_calls + body_calls)))
              | Apply apply -> (
                match Apply.call_kind apply with
                | Function { function_call = Direct code_id } ->
                  if calls_worker apply code_id then Some 1 else None
                | Function { function_call = Indirect_known_arity _ } -> (
                  match Apply.callee apply, projected_worker with
                  | Some callee, Some (_, worker_code_id)
                    when is_worker_callee worker_callees callee
                         && calls_worker apply worker_code_id ->
                    Some 1
                  | _ -> None)
                | Function { function_call = Indirect_unknown_arity }
                | Method _ | C_call _ | Effect _ ->
                  None)
              | Apply_cont apply_cont -> (
                match Apply_cont.trap_action apply_cont with
                | None -> Some 0
                | Some _ -> None)
              | Let_cont (Non_recursive { is_applied_with_traps = true; _ })
              | Let_cont (Recursive _)
              | Switch _ | Invalid _ ->
                None)
          in
          Function_params_and_body.pattern_match
            (Code.params_and_body (Code_or_metadata.get_code wrapper))
            ~f:(fun
                ~return_continuation:_
                ~exn_continuation:_
                _
                ~body
                ~my_closure
                ~is_my_closure_used:_
                ~my_alloc_mode:_
                ~my_depth:_
                ~free_names_of_body:_
              ->
              match count_calls my_closure Variable.Set.empty body with
              | Some 1 -> !has_numeric_adaptation
              | None | Some _ -> false)
      with Not_found -> false)

let make_decision0 dacc ~simplify_expr ~rebuild_expr ~function_type ~apply
    ~return_arity : Call_site_inlining_decision_type.t =
  let must_inline = DE.must_inline (DA.denv dacc) in
  let fail_if_must_inline () =
    if must_inline
    then
      Misc.fatal_errorf
        "Deciding not to inline an [Apply], but the replay_history says we \
         should inline.@ Replay_history: %a"
        Replay_history.print
        (DE.replay_history (DA.denv dacc))
  in
  let[@local] do_not_inline (decision : Call_site_inlining_decision_type.t) =
    fail_if_must_inline ();
    decision
  in
  let rec_info = get_rec_info dacc ~function_type in
  let inlined = Apply.inlined apply in
  match inlined with
  | Never_inlined -> do_not_inline Never_inlined_attribute
  | Forward_inlined ->
    do_not_inline Forward_inlined_attribute_but_nothing_to_forward
  | Default_inlined | Unroll _ | Always_inlined _ | Hint_inlined -> (
    match
      DE.find_code_metadata_exn (DA.denv dacc) (FT.code_id function_type)
    with
    | exception Not_found -> do_not_inline Missing_code
    | code_metadata -> (
      let code_present () =
        match
          DE.find_code_exn (DA.denv dacc) (Code_metadata.code_id code_metadata)
        with
        | code_or_metadata -> Code_or_metadata.code_present code_or_metadata
        | exception Not_found ->
          Misc.fatal_errorf
            "[DE.find_code_metadata_exn] found a code metadata, but \
             [DE.find_code_exn] returns Not_found for code_id %a"
            Code_id.print
            (Code_metadata.code_id code_metadata)
      in
      let[@local] inline_if_code_present decision =
        if code_present () then decision else do_not_inline Missing_code
      in
      (* The unrolling process is rather subtle, but it boils down to two steps:

         1. We see an [@unrolled n] annotation (with n > 0) on an apply
         expression whose [rec_info] has the unrolling state [Not_unrolling].
         When we inline the body, we bind [my_depth] to a rec_info whose
         unrolling state is [Unrolling { remaining_depth = n }].

         2. When we see that application again, its rec_info will have the
         unrolling state [Unrolling { remaining_depth = n - 1 }] (because its
         depth is [succ my_depth]). At that point, we short-circuit most of the
         inlining logic and inline if and only if n > 0.

         Here we're performing step _2_ (but only, of course, if we performed
         step 1 in a previous call to this function). *)
      let unrolling_depth =
        Simplify_rec_info_expr.known_remaining_unrolling_depth dacc rec_info
      in
      match unrolling_depth with
      | Some 0 -> do_not_inline Unrolling_depth_exceeded
      | Some _ -> inline_if_code_present Continue_unrolling
      | None -> (
        (* lmaurer: This seems semantically dodgy: If we really think of a free
           depth variable as [Unknown], then we shouldn't be considering
           inlining here, because we don't _know_ that we're not unrolling. The
           behavior is what we want, though (and is consistent with FLambda 1):
           If there's a free depth variable, that means this is an internal
           recursive call, which means we consider unrolling if [@unrolled]
           appears. If it's known that the unrolling depth is zero, that means
           we're inlining into another function and we're done unrolling, so we
           immediately stop inlining.

           So this seems to be working for the moment, but I wonder what are the
           ramifications of treating unknown-ness as an observable property this
           way. Are we relying on monotonicity somewhere? *)
        let apply_inlining_state = Apply.inlining_state apply in
        let recursive = Code_metadata.recursive code_metadata in
        if Inlining_state.is_depth_exceeded apply_inlining_state
        then do_not_inline Max_inlining_depth_exceeded
        else
          let policy =
            match inlined with
            | Never_inlined | Forward_inlined -> assert false
            | Default_inlined -> `Heuristic
            | Unroll (to_, _) -> `Unroll to_
            | Always_inlined _ | Hint_inlined -> (
              (* Treat [@inlined] and [@inlined hint] the same as [@unrolled 1]
                 whenever the function is recursive. This is particularly
                 important when the annotation is on a parameter and the
                 function is _usually_ non-recursive: we'd rather behave well in
                 the odd case where it isn't. *)
              match recursive with
              | Recursive -> `Unroll 1
              | Non_recursive -> `Always)
          in
          match policy with
          | `Heuristic ->
            let max_rec_depth =
              Flambda_features.Inlining.max_rec_depth
                (Round (DE.round (DA.denv dacc)))
            in
            if
              Simplify_rec_info_expr.depth_may_exceed dacc rec_info
                max_rec_depth
              (* Expand only a bounded numeric adapter back to this worker. The
                 worker is not a stub and retains its recursion limit. *)
              && not (is_self_worker_adapter dacc code_metadata)
            then do_not_inline Recursion_depth_exceeded
            else if must_inline
            then
              match
                Replay_history.replay_inlining_decision
                  (DE.replay_history (DA.denv dacc))
              with
              | Still_recording ->
                Misc.fatal_errorf
                  "Internal assumption broken: DE.says must_inline\n\
                  \                  (presumably because of the replay \
                   history), but the replay history is still recoding."
              | Replayed decision ->
                if code_present ()
                then Replay_history_says_must_inline decision
                else
                  Misc.fatal_errorf
                    "Replay history says we should inline %a, but its code is \
                     not present"
                    Code_id.print
                    (Code_metadata.code_id code_metadata)
            else
              might_inline dacc ~apply ~code_metadata ~function_type
                ~simplify_expr ~rebuild_expr ~return_arity
          | `Unroll unroll_to ->
            if Simplify_rec_info_expr.can_unroll dacc rec_info
            then
              (* This sets off step 1 in the comment above; see
                 [Inlining_transforms] for how [unroll_to] is ultimately
                 handled. *)
              inline_if_code_present (Begin_unrolling unroll_to)
            else do_not_inline Unrolling_depth_exceeded
          | `Always -> inline_if_code_present Attribute_always)))

let make_decision dacc ~simplify_expr ~rebuild_expr ~function_type ~apply
    ~return_arity : Call_site_inlining_decision_type.t =
  if Clflags.target_is_jsir ()
  then Jsir_inlining_disabled
  else
    make_decision0 dacc ~simplify_expr ~rebuild_expr ~function_type ~apply
      ~return_arity
