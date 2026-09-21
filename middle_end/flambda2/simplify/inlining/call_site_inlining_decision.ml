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

let speculative_inlining dacc ~apply ~function_type ~simplify_expr ~return_arity
    =
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
  let _, uacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
        let exn_continuation = Apply.exn_continuation apply in
        let dacc =
          DA.map_flow_acc dacc
            ~f:(Flow.Acc.exit_continuation dummy_toplevel_cont)
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
           correct at toplevel when all information about the code_age relation
           and used_value slots is available (for the whole compilation unit).
           Thus we here provide empty/dummy values for the used_value_slots and
           code_age_relation, and ignore the reachable_code_id part of the
           data_flow analysis. *)
        let flow_result =
          Flow.Analysis.analyze data_flow ~speculative:true
            ~print_name:"speculative" ~code_age_relation:Code_age_relation.empty
            ~used_value_slots:Unknown
            ~code_ids_to_never_delete:Code_id.Set.empty
            ~specialization_map:(DA.specialization_map dacc)
            ~return_continuation:function_return_cont
            ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
            ~machine_width:(DE.machine_width (DA.denv dacc))
        in
        let uenv =
          (* Note that we don't need to do anything special if the exception
             continuation takes extra arguments, since we are only simplifying
             the body of the function in question, not substituting it into an
             existing context. *)
          let machine_width = DE.machine_width (DA.denv dacc) in
          UE.add_function_return_or_exn_continuation
            (UE.create (DA.are_rebuilding_terms dacc) ~machine_width)
            (Exn_continuation.exn_handler exn_continuation)
            (Flambda_arity.create_singletons
               [Flambda_kind.With_subkind.any_value])
        in
        let uenv =
          match Apply.continuation apply with
          | Never_returns -> uenv
          | Return return_continuation ->
            UE.add_function_return_or_exn_continuation uenv return_continuation
              return_arity
        in
        let uacc =
          UA.create ~flow_result ~compute_slot_offsets:false uenv dacc
        in
        rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  let cost_metrics_of_lifted_constants () =
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

       - Count the size of unused value slots of lifted sets of closures created
       during speculative inlining (but again, it is not clear that it is always
       possible to compute a correct set of "used value slots" at the time we
       are doing speculative inlining, because some value slots could be used
       later in the compilation unit). *)
    Lifted_constant_state.fold lifted_constants ~init:Cost_metrics.zero
      ~f:(fun cost_metrics lifted_constant ->
        List.fold_left
          (fun cost_metrics definition ->
            Cost_metrics.( + ) cost_metrics
              (Rebuilt_static_const.cost_metrics
                 (Lifted_constant.Definition.defining_expr definition)))
          cost_metrics
          (Lifted_constant.definitions lifted_constant))
  in
  let cost_metrics = UA.cost_metrics uacc in
  if Flambda_features.Inlining.speculative_inlining_track_lifted_constants ()
  then
    Cost_metrics.( + ) cost_metrics (cost_metrics_of_lifted_constants ()), None
  else if
    Warnings.is_active
      (Warnings.Inlining_deviates_from_ideal { current = ""; ideal = "" })
  then
    (* Also compute the cost metrics as they would be if
       [speculative_inlining_track_lifted_constants] were enabled, so that we
       can warn if that flag would change the inlining decision. *)
    ( cost_metrics,
      Some
        (Cost_metrics.( + )
           (Cost_metrics.( + ) cost_metrics
              (cost_metrics_of_lifted_constants ()))
           (UA.cost_metrics_of_untracked_static_consts uacc)) )
  else cost_metrics, None

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

(* Result of attempting speculative inlining. [ideal_cost_metrics] gives the
   cost metrics as they would be in the ideal configuration (see below); they
   are equal to [cost_metrics] except when
   [speculative_inlining_track_lifted_constants] is disabled. *)
type speculation =
  | No_useful_argument_types
  | Code_not_present
  | Speculated of
      { cost_metrics : Cost_metrics.t;
        ideal_cost_metrics : Cost_metrics.t
      }

(* The "ideal configuration", against which warning 222
   [Inlining_deviates_from_ideal] compares inlining decisions, is the current
   configuration with [speculative_inlining_track_lifted_constants] enabled and,
   if [ideal_large_functor_size] is set, with [large_functor_size] set to that
   value. *)
let ideal_configuration_may_differ () =
  (not
     (Flambda_features.Inlining.speculative_inlining_track_lifted_constants ()))
  || Option.is_some (Flambda_features.Inlining.ideal_large_functor_size ())

let ideal_function_decl_decision ~inlining_args ~code_metadata =
  let decision = Code_metadata.inlining_decision code_metadata in
  match Flambda_features.Inlining.ideal_large_functor_size () with
  | None -> decision
  | Some ideal_large_functor_size ->
    if not (Code_metadata.is_a_functor code_metadata)
    then decision
    else
      let make_decision ~inlining_arguments =
        Function_decl_inlining_decision.make_decision ~inlining_arguments
          ~inline:(Code_metadata.inline code_metadata)
          ~stub:(Code_metadata.stub code_metadata)
          ~cost_metrics:(Code_metadata.cost_metrics code_metadata)
          ~is_a_functor:true
          ~recursive:(Code_metadata.recursive code_metadata)
      in
      let current = make_decision ~inlining_arguments:inlining_args in
      let ideal =
        make_decision
          ~inlining_arguments:
            (Inlining_arguments.with_large_functor_size inlining_args
               ~large_functor_size:ideal_large_functor_size)
      in
      (* [inlining_args] may differ from the arguments that were in force when
         the decision recorded in [code_metadata] was taken, so [current] is
         only used to detect whether the ideal large functor size changes the
         decision. *)
      if Function_decl_inlining_decision_type.equal current ideal
      then decision
      else ideal

let code_size t = Cost_metrics.size (Code_metadata.cost_metrics t)

let describe_current_behaviour ~code_metadata
    (actual_decision : Call_site_inlining_decision_type.t) =
  match actual_decision with
  | Speculatively_inline { evaluated_to; threshold; _ } ->
    Format.asprintf
      "the function is inlined because its speculative cost %g is at or below \
       the inlining threshold %g"
      evaluated_to threshold
  | Speculatively_not_inline { evaluated_to; threshold; _ } ->
    Format.asprintf
      "the function is not inlined because its speculative cost %g is above \
       the inlining threshold %g"
      evaluated_to threshold
  | Argument_types_not_useful ->
    "the function is not inlined because there is no useful information about \
     its arguments"
  | Missing_code ->
    "the function is not inlined because its code is not available"
  | Definition_says_inline _ -> (
    match Code_metadata.inlining_decision code_metadata with
    | Small_functor { size; small_functor_size } ->
      Format.asprintf
        "the function is always inlined because it is a small functor (size %a \
         <= small functor size %a)"
        Code_size.print size Code_size.print small_functor_size
    | Small_function { size; small_function_size } ->
      Format.asprintf
        "the function is always inlined because it is a small function (size \
         %a <= small function size %a)"
        Code_size.print size Code_size.print small_function_size
    | Not_yet_decided | Never_inline_attribute | Function_body_too_large _
    | Functor_body_too_large _ | Stub | Attribute_inline
    | Speculatively_inlinable _ | Speculatively_inlinable_functor _ | Recursive
    | Jsir_inlining_disabled ->
      "the function is always inlined")
  | Definition_says_not_to_inline -> (
    match Code_metadata.inlining_decision code_metadata with
    | Functor_body_too_large large_functor_size ->
      Format.asprintf
        "the function is not inlined because its body is too large for a \
         functor (size %a >= large functor size %a)"
        Code_size.print (code_size code_metadata) Code_size.print
        large_functor_size
    | Function_body_too_large large_function_size ->
      Format.asprintf
        "the function is not inlined because its body is too large (size %a >= \
         large function size %a)"
        Code_size.print (code_size code_metadata) Code_size.print
        large_function_size
    | Not_yet_decided | Never_inline_attribute | Stub | Attribute_inline
    | Small_function _ | Small_functor _ | Speculatively_inlinable _
    | Speculatively_inlinable_functor _ | Recursive | Jsir_inlining_disabled ->
      "the function is never inlined (as decided at its definition)")
  | In_a_stub | Doing_speculative_inlining | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded | Recursion_depth_exceeded
  | Never_inlined_attribute | Forward_inlined_attribute_but_nothing_to_forward
  | Attribute_always | Replay_history_says_must_inline _ | Begin_unrolling _
  | Continue_unrolling | Jsir_inlining_disabled ->
    (* The warning is never emitted for these decisions. *)
    Format.asprintf "%a" Call_site_inlining_decision_type.print actual_decision

let describe_ideal_must_inline
    (ideal_decl : Function_decl_inlining_decision_type.t) =
  match ideal_decl with
  | Small_functor { size; small_functor_size } ->
    Format.asprintf
      "the function would always be inlined because it would be a small \
       functor (size %a <= small functor size %a)"
      Code_size.print size Code_size.print small_functor_size
  | Small_function { size; small_function_size } ->
    Format.asprintf
      "the function would always be inlined because it is a small function \
       (size %a <= small function size %a)"
      Code_size.print size Code_size.print small_function_size
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _
  | Functor_body_too_large _ | Stub | Attribute_inline
  | Speculatively_inlinable _ | Speculatively_inlinable_functor _ | Recursive
  | Jsir_inlining_disabled ->
    "the function would always be inlined"

let describe_ideal_cannot_inline ~code_metadata
    (ideal_decl : Function_decl_inlining_decision_type.t) =
  match ideal_decl with
  | Functor_body_too_large large_functor_size ->
    Format.asprintf
      "the function would not be inlined because its body would be too large \
       for a functor (size %a >= ideal large functor size %a)"
      Code_size.print (code_size code_metadata) Code_size.print
      large_functor_size
  | Function_body_too_large large_function_size ->
    Format.asprintf
      "the function would not be inlined because its body is too large (size \
       %a >= large function size %a)"
      Code_size.print (code_size code_metadata) Code_size.print
      large_function_size
  | Not_yet_decided | Never_inline_attribute | Stub | Attribute_inline
  | Small_function _ | Small_functor _ | Speculatively_inlinable _
  | Speculatively_inlinable_functor _ | Recursive | Jsir_inlining_disabled ->
    "the function would never be inlined"

let warn_if_ideal_configuration_differs ~apply ~code_metadata ~inlining_args
    ~threshold ~code_present ~actual_decision ~speculation ~speculate =
  let ideal_decl = ideal_function_decl_decision ~inlining_args ~code_metadata in
  let ideal_would_inline, ideal =
    if Function_decl_inlining_decision_type.must_be_inlined ideal_decl
    then
      if code_present ()
      then true, describe_ideal_must_inline ideal_decl
      else
        ( false,
          "the function would not be inlined because its code is not available"
        )
    else if Function_decl_inlining_decision_type.cannot_be_inlined ideal_decl
    then false, describe_ideal_cannot_inline ~code_metadata ideal_decl
    else
      (* In the ideal configuration, the decision would be taken by speculative
         inlining. If the actual decision was also taken by speculative inlining
         then reuse its results, otherwise speculate now. *)
      let speculation =
        match speculation with
        | Some speculation -> speculation
        | None -> speculate ()
      in
      match (speculation : speculation) with
      | No_useful_argument_types ->
        ( false,
          "the function would not be inlined because there is no useful \
           information about its arguments" )
      | Code_not_present ->
        ( false,
          "the function would not be inlined because its code is not available"
        )
      | Speculated { ideal_cost_metrics; cost_metrics = _ } ->
        let evaluated_to =
          Cost_metrics.evaluate ~args:inlining_args ideal_cost_metrics
        in
        if Float.compare evaluated_to threshold <= 0
        then
          ( true,
            Format.asprintf
              "the function would be inlined because its speculative cost %g \
               would be at or below the inlining threshold %g"
              evaluated_to threshold )
        else
          ( false,
            Format.asprintf
              "the function would not be inlined because its speculative cost \
               %g would be above the inlining threshold %g"
              evaluated_to threshold )
  in
  let actually_inlines =
    match Call_site_inlining_decision_type.can_inline actual_decision with
    | Inline _ -> true
    | Do_not_inline _ -> false
  in
  if not (Bool.equal ideal_would_inline actually_inlines)
  then
    Location.prerr_warning
      (Debuginfo.to_location (Apply.dbg apply))
      (Warnings.Inlining_deviates_from_ideal
         { current = describe_current_behaviour ~code_metadata actual_decision;
           ideal
         })

let might_inline dacc ~apply ~code_metadata ~function_type ~simplify_expr
    ~return_arity : Call_site_inlining_decision_type.t =
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
  let inlining_args =
    Inlining_arguments.combine
      ~from_env:(DE.inlining_arguments denv)
      ~from_metadata:(Apply.inlining_arguments apply)
  in
  let threshold = Inlining_arguments.threshold inlining_args in
  let speculate () : speculation =
    if not (argument_types_useful dacc ~apply ~code_metadata)
    then No_useful_argument_types
    else if not (code_present ())
    then Code_not_present
    else
      let cost_metrics, ideal_cost_metrics =
        speculative_inlining ~apply dacc ~simplify_expr ~return_arity
          ~function_type
      in
      Speculated
        { cost_metrics;
          ideal_cost_metrics =
            Option.value ideal_cost_metrics ~default:cost_metrics
        }
  in
  let decision_of_speculation (speculation : speculation) :
      Call_site_inlining_decision_type.t =
    match speculation with
    | No_useful_argument_types -> Argument_types_not_useful
    | Code_not_present -> Missing_code
    | Speculated { cost_metrics; ideal_cost_metrics = _ } ->
      let evaluated_to =
        Cost_metrics.evaluate ~args:inlining_args cost_metrics
      in
      if Float.compare evaluated_to threshold <= 0
      then
        Speculatively_inline
          { cost_metrics; evaluated_to; threshold; is_a_functor }
      else
        Speculatively_not_inline
          { cost_metrics; evaluated_to; threshold; is_a_functor }
  in
  let (actual_decision : Call_site_inlining_decision_type.t), speculation =
    if in_a_stub
    then In_a_stub, None
    else if Function_decl_inlining_decision_type.must_be_inlined decision
    then
      ( (if code_present ()
         then
           Definition_says_inline
             { was_inline_always =
                 Function_decl_inlining_decision_type.has_attribute_inline
                   decision
             }
         else Missing_code),
        None )
    else if Function_decl_inlining_decision_type.cannot_be_inlined decision
    then Definition_says_not_to_inline, None
    else if doing_speculative_inlining
    then Doing_speculative_inlining, None
    else
      Profile.record_call_with_counters ~accumulate:true "speculative_inlining"
        ~counter_f:(fun ((decision : Call_site_inlining_decision_type.t), _) ->
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
          | Continue_unrolling | Definition_says_inline _
          | Jsir_inlining_disabled ->
            (* These can't be returned by the speculative inlining cases
               below. *)
            if Flambda_features.check_light_invariants ()
            then
              Misc.fatal_error
                "Unexpected call site inlinine decision for speculative \
                 inlining";
            counters)
        (fun () ->
          let speculation = speculate () in
          decision_of_speculation speculation, Some speculation)
  in
  if
    (not in_a_stub)
    && (not doing_speculative_inlining)
    && ideal_configuration_may_differ ()
    && Warnings.is_active
         (Warnings.Inlining_deviates_from_ideal { current = ""; ideal = "" })
  then
    warn_if_ideal_configuration_differs ~apply ~code_metadata ~inlining_args
      ~threshold ~code_present ~actual_decision ~speculation ~speculate;
  actual_decision

let get_rec_info dacc ~function_type =
  let rec_info = FT.rec_info function_type in
  match Flambda2_types.meet_rec_info (DA.typing_env dacc) rec_info with
  | Known_result rec_info -> rec_info
  | Need_meet -> Rec_info_expr.unknown
  | Invalid -> (* CR vlaviron: ? *) Rec_info_expr.do_not_inline

let make_decision0 dacc ~simplify_expr ~function_type ~apply ~return_arity :
    Call_site_inlining_decision_type.t =
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
                ~simplify_expr ~return_arity
          | `Unroll unroll_to ->
            if Simplify_rec_info_expr.can_unroll dacc rec_info
            then
              (* This sets off step 1 in the comment above; see
                 [Inlining_transforms] for how [unroll_to] is ultimately
                 handled. *)
              inline_if_code_present (Begin_unrolling unroll_to)
            else do_not_inline Unrolling_depth_exceeded
          | `Always -> inline_if_code_present Attribute_always)))

let make_decision dacc ~simplify_expr ~function_type ~apply ~return_arity :
    Call_site_inlining_decision_type.t =
  if !Clflags.jsir
  then Jsir_inlining_disabled
  else make_decision0 dacc ~simplify_expr ~function_type ~apply ~return_arity
