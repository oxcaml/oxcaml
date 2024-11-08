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
      (* We use find alias to find a common simple to different simples.

         This simple is already guaranteed to be the cannonical alias.

         * If there is a common alias between variables, the cannonical alias
         must also be a common alias.

         * For constants and symbols there can be a common alias that is not
         cannonical: A variable can have different constant values in different
         branches: this variable is not the cannonical alias, the cannonical
         would be the constant or the symbol. But the only common alias could be
         a variable in that case.

         hence there is no loss of generality in returning the cannonical alias
         as the single alias if it is a variable.

         Note that the main reason for this is to allow changing the arguments
         of continuations to variables that where not in scope during the
         downward traversal. In particular for the alias rewriting provided by
         data_flow *)
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
              if
                TI.equal arm (TI.bool_true machine_width)
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
  | No_transformation

type symbol_or_const =
  | Symbol of Symbol.t
  | Const of Reg_width_const.t

type lookup_table_fields =
  | Symbols_or_tagged_immediates of symbol_or_const list
      (** Either all symbols or a mix of symbols and tagged immediates. Both are
          of kind [value], so they share a single value-kind array. *)
  | Tagged_immediates of TI.t list
      (** All arms are tagged immediates (or, with [Must_untag], all arms are
          naked immediates, which are stored as tagged ones and untagged after
          loading). A value array specialised to [Immediates] can be used. *)
  | Naked_float32s of Numeric_types.Float32_by_bit_pattern.t list
  | Naked_floats of Numeric_types.Float_by_bit_pattern.t list
  | Naked_int8s of Numeric_types.Int8.t list
  | Naked_int16s of Numeric_types.Int16.t list
  | Naked_int32s of int32 list
  | Naked_int64s of int64 list
  | Naked_nativeints of Targetint_32_64.t list
  | Naked_vec128s of Vector_types.Vec128.Bit_pattern.t list
  | Naked_vec256s of Vector_types.Vec256.Bit_pattern.t list
  | Naked_vec512s of Vector_types.Vec512.Bit_pattern.t list

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
          (* The destination continuations must have single constant or symbol
             arguments. Trap actions are forbidden. *)
          None
        | Some arg ->
          Simple.pattern_match'
            arg (* CR mshinwell: we could allow variables, if at toplevel *)
            ~var:(fun _ ~coercion:_ ->
              (* Aliases should have been followed by now. *) None)
            ~symbol:(fun sym ~coercion:_ ->
              let expected_discr =
                TI.add (TI.one machine_width) expected_discr
              in
              Some (Some dest', Symbol sym :: args_rev, expected_discr))
            ~const:(fun const ->
              let expected_discr =
                TI.add (TI.one machine_width) expected_discr
              in
              Some (Some dest', Const const :: args_rev, expected_discr))))
  in
  match TI.Map.fold check_arm arms (Some (None, [], TI.zero machine_width)) with
  | None | Some (None, _, _) | Some (_, [], _) -> None
  | Some (Some dest, args_rev, _) -> (
    let args = List.rev args_rev in
    assert (List.compare_length_with args 1 >= 0);
    let[@inline] check_args prover must_untag_lookup_table_result wrapper =
      let args' =
        List.filter_map
          (fun arg -> match arg with Const cst -> Some cst | Symbol _ -> None)
          args
      in
      let args' = List.filter_map prover args' in
      if List.compare_lengths args args' = 0
      then Some (dest, must_untag_lookup_table_result, wrapper args')
      else None
    in
    (* All arguments must be of an appropriate kind; non-value kinds must
       additionally agree with each other. Symbols and tagged immediates may be
       freely mixed because both are of kind [value]. *)
    let module RWC = Reg_width_const in
    let any_symbol =
      List.exists (function Symbol _ -> true | Const _ -> false) args
    in
    let all_value_kind =
      List.for_all
        (fun arg ->
          match arg with
          | Symbol _ -> true
          | Const cst -> (
            match RWC.descr cst with
            | Tagged_immediate _ -> true
            | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int8 _
            | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
            | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Null ->
              false))
        args
    in
    if any_symbol && all_value_kind
    then Some (dest, No_transformation, Symbols_or_tagged_immediates args)
    else
      match List.hd args with
      | Symbol _ ->
        (* Any symbol combined with a non-value-kind constant is rejected. *)
        None
      | Const cst -> (
        match RWC.descr cst with
        | Naked_immediate _ ->
          check_args RWC.is_naked_immediate Must_untag (fun args ->
              Tagged_immediates args)
        | Tagged_immediate _ ->
          (* All arms are tagged immediate [Const]s (any mix with symbols was
             handled above). *)
          (check_args RWC.is_tagged_immediate No_transformation) (fun args ->
              Tagged_immediates args)
        | Naked_float _ ->
          check_args RWC.is_naked_float No_transformation (fun args ->
              Naked_floats args)
        | Naked_float32 _ ->
          check_args RWC.is_naked_float32 No_transformation (fun args ->
              Naked_float32s args)
        | Naked_int8 _ ->
          check_args RWC.is_naked_int8 No_transformation (fun args ->
              Naked_int8s args)
        | Naked_int16 _ ->
          check_args RWC.is_naked_int16 No_transformation (fun args ->
              Naked_int16s args)
        | Naked_int32 _ ->
          check_args RWC.is_naked_int32 No_transformation (fun args ->
              Naked_int32s args)
        | Naked_int64 _ ->
          check_args RWC.is_naked_int64 No_transformation (fun args ->
              Naked_int64s args)
        | Naked_nativeint _ ->
          check_args RWC.is_naked_nativeint No_transformation (fun args ->
              Naked_nativeints args)
        | Naked_vec128 _ ->
          check_args RWC.is_naked_vec128 No_transformation (fun args ->
              Naked_vec128s args)
        | Naked_vec256 _ ->
          check_args RWC.is_naked_vec256 No_transformation (fun args ->
              Naked_vec256s args)
        | Naked_vec512 _ ->
          check_args RWC.is_naked_vec512 No_transformation (fun args ->
              Naked_vec512s args)
        | Null -> None))

let recognize_switch_with_single_arg_to_same_destination machine_width ~arms =
  (* Switch must be large enough. *)
  if TI.Map.cardinal arms < 3
  then None
  else recognize_switch_with_single_arg_to_same_destination0 machine_width ~arms

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

let return ~added_code_size ~free_names expr uacc ~dacc_before_switch:_ =
  let uacc = UA.notify_added ~code_size:added_code_size uacc in
  let uacc = UA.add_free_names uacc free_names in
  expr, uacc

let run uacc ~dacc_before_switch k = k uacc ~dacc_before_switch

let rebuild_switch_with_single_arg_to_same_destination uacc ~dacc_before_switch
    ~scrutinee ~dest ~(lookup_table_fields : lookup_table_fields)
    ~must_untag_lookup_table_result dbg =
  let rebuilding = UA.are_rebuilding_terms uacc in
  let block_sym =
    let var = Variable.create "switch_block" K.value in
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string (Variable.unique_name var))
  in
  let uacc, array_kind, array_load_kind, loaded_kind =
    let simples_from_consts make_simple numbers =
      List.map
        (fun num -> Simple.With_debuginfo.create (make_simple num) dbg)
        numbers
    in
    let constify numbers =
      List.map (fun num -> Or_variable.Const num) numbers
    in
    let field_types_from_consts element_kind make_simple numbers =
      List.map
        (fun num -> T.alias_type_of element_kind (make_simple num))
        numbers
    in
    let array_const, array_kind, array_load_kind, element_kind, fields =
      let module AK = P.Array_kind in
      let module ALK = P.Array_load_kind in
      match lookup_table_fields with
      | Symbols_or_tagged_immediates elts ->
        let make_simple = function
          | Symbol sym -> Simple.symbol sym
          | Const cst -> Simple.const cst
        in
        let simples = simples_from_consts make_simple elts in
        let fields = field_types_from_consts K.value make_simple elts in
        ( RSC.create_immutable_value_array rebuilding simples,
          AK.Values,
          ALK.Values,
          KS.any_value,
          fields )
      | Tagged_immediates imms ->
        (* The discriminants of the original [Switch] may have been naked
           immediates, but we store them as tagged immediates in a value array
           and untag after loading. *)
        let simples = simples_from_consts Simple.const_int imms in
        let fields =
          field_types_from_consts K.value
            (fun i -> Simple.const (Reg_width_const.const_int i))
            imms
        in
        ( RSC.create_immutable_value_array rebuilding simples,
          AK.Values,
          ALK.Immediates,
          KS.tagged_immediate,
          fields )
      | Naked_float32s fs ->
        let fields =
          field_types_from_consts K.naked_float32
            (fun f -> Simple.const (Reg_width_const.naked_float32 f))
            fs
        in
        ( RSC.create_immutable_float32_array rebuilding (constify fs),
          AK.Naked_float32s,
          ALK.Naked_float32s,
          KS.naked_float32,
          fields )
      | Naked_floats fs ->
        let fields =
          field_types_from_consts K.naked_float
            (fun f -> Simple.const (Reg_width_const.naked_float f))
            fs
        in
        ( RSC.create_immutable_float_array rebuilding (constify fs),
          AK.Naked_floats,
          ALK.Naked_floats,
          KS.naked_float,
          fields )
      | Naked_int8s is ->
        let fields =
          field_types_from_consts K.naked_int8
            (fun i -> Simple.const (Reg_width_const.naked_int8 i))
            is
        in
        ( RSC.create_immutable_int8_array rebuilding (constify is),
          AK.Naked_int8s,
          ALK.Naked_int8s,
          KS.naked_int8,
          fields )
      | Naked_int16s is ->
        let fields =
          field_types_from_consts K.naked_int16
            (fun i -> Simple.const (Reg_width_const.naked_int16 i))
            is
        in
        ( RSC.create_immutable_int16_array rebuilding (constify is),
          AK.Naked_int16s,
          ALK.Naked_int16s,
          KS.naked_int16,
          fields )
      | Naked_int32s is ->
        let fields =
          field_types_from_consts K.naked_int32
            (fun i -> Simple.const (Reg_width_const.naked_int32 i))
            is
        in
        ( RSC.create_immutable_int32_array rebuilding (constify is),
          AK.Naked_int32s,
          ALK.Naked_int32s,
          KS.naked_int32,
          fields )
      | Naked_int64s is ->
        let fields =
          field_types_from_consts K.naked_int64
            (fun i -> Simple.const (Reg_width_const.naked_int64 i))
            is
        in
        ( RSC.create_immutable_int64_array rebuilding (constify is),
          AK.Naked_int64s,
          ALK.Naked_int64s,
          KS.naked_int64,
          fields )
      | Naked_nativeints is ->
        let fields =
          field_types_from_consts K.naked_nativeint
            (fun i -> Simple.const (Reg_width_const.naked_nativeint i))
            is
        in
        ( RSC.create_immutable_nativeint_array rebuilding (constify is),
          AK.Naked_nativeints,
          ALK.Naked_nativeints,
          KS.naked_nativeint,
          fields )
      | Naked_vec128s vs ->
        let fields =
          field_types_from_consts K.naked_vec128
            (fun v -> Simple.const (Reg_width_const.naked_vec128 v))
            vs
        in
        ( RSC.create_immutable_vec128_array rebuilding (constify vs),
          AK.Naked_vec128s,
          ALK.Naked_vec128s,
          KS.naked_vec128,
          fields )
      | Naked_vec256s vs ->
        let fields =
          field_types_from_consts K.naked_vec256
            (fun v -> Simple.const (Reg_width_const.naked_vec256 v))
            vs
        in
        ( RSC.create_immutable_vec256_array rebuilding (constify vs),
          AK.Naked_vec256s,
          ALK.Naked_vec256s,
          KS.naked_vec256,
          fields )
      | Naked_vec512s vs ->
        let fields =
          field_types_from_consts K.naked_vec512
            (fun v -> Simple.const (Reg_width_const.naked_vec512 v))
            vs
        in
        ( RSC.create_immutable_vec512_array rebuilding (constify vs),
          AK.Naked_vec512s,
          ALK.Naked_vec512s,
          KS.naked_vec512,
          fields )
    in
    let block_type =
      T.immutable_array ~element_kind:(Ok element_kind) ~fields
        Alloc_mode.For_types.heap
        ~machine_width:(DE.machine_width (DA.denv dacc_before_switch))
    in
    let uacc =
      UA.add_lifted_constant uacc
        (LC.create_definition
           (LC.Definition.block_like
              (DA.denv dacc_before_switch)
              block_sym block_type ~symbol_projections:Variable.Map.empty
              array_const))
    in
    uacc, array_kind, array_load_kind, KS.kind element_kind
  in
  (* CR mshinwell: consider sharing the constants *)
  let block = Simple.symbol block_sym in
  run uacc ~dacc_before_switch
    (let$ tagged_scrutinee =
       bound_prim "tagged_scrutinee" K.value
         (P.Unary (Tag_immediate, scrutinee))
         dbg
     in
     let load_from_block_prim : P.t =
       Binary
         ( Array_load (array_kind, array_load_kind, Immutable),
           block,
           tagged_scrutinee )
     in
     let load_from_block = Named.create_prim load_from_block_prim dbg in
     let arg_var = Variable.create "arg" loaded_kind in
     let arg_var_duid = Flambda_debug_uid.none in
     let arg = Simple.var arg_var in
     let final_arg_var, final_arg_var_duid, final_arg =
       match must_untag_lookup_table_result with
       | Must_untag ->
         let final_arg_var = Variable.create "final_arg" K.naked_immediate in
         let final_arg_var_duid = Flambda_debug_uid.none in
         final_arg_var, final_arg_var_duid, Simple.var final_arg_var
       | No_transformation -> arg_var, arg_var_duid, arg
     in
     (* Note that, unlike for the untagging of normal Switch scrutinees, there's
        no problem with CSE and Data_flow here. The reason is that in this case
        the generated primitive always names a fresh variable, so it will never
        be eligible for CSE. *)
     (* CR mshinwell: we could probably expose the actual integer counts of
        continuations in [Name_occurrences] and then try to inline out [dest].
        This might happen anyway in the backend though so this probably isn't
        that important for now. *)
     let apply_cont = Apply_cont.create dest ~args:[final_arg] ~dbg in
     let free_names_of_body = Apply_cont.free_names apply_cont in
     let untag_arg_prim : P.t = Unary (Untag_immediate, arg) in
     let expr =
       let body =
         let body = RE.create_apply_cont apply_cont in
         match must_untag_lookup_table_result with
         | No_transformation -> body
         | Must_untag ->
           let bound =
             BPt.singleton
               (BV.create final_arg_var final_arg_var_duid NM.normal)
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
     let added_code_size =
       Code_size.( + )
         (Code_size.prim ~machine_width load_from_block_prim)
         (Code_size.( + )
            (Code_size.apply_cont apply_cont)
            (match must_untag_lookup_table_result with
            | Must_untag -> Code_size.prim ~machine_width untag_arg_prim
            | No_transformation -> Code_size.zero))
     in
     (* CR mshinwell: it seems we need to fix [Cost_metrics] so we can note that
        we have *added* operations here (load, maybe untagging). *)
     return ~added_code_size ~free_names:extra_free_names expr)

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

let rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch
    ~scrutinee ~dest ~offset ~slope ~must_untag_lookup_table_result dbg =
  (* We are creating the following fragment: *)
  (* let scaled = x * slope in
   * let final = scaled + offset in
   * apply_cont k final
   *)
  let rebuild_affine_expr scrutinee kind standard_int const =
    let mul_prim : P.t =
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
    let added_code_size = Code_size.apply_cont apply_cont in
    return ~added_code_size ~free_names (RE.create_apply_cont apply_cont)
  in
  run ~dacc_before_switch uacc
    (match must_untag_lookup_table_result with
    | Must_untag ->
      rebuild_affine_expr scrutinee K.naked_immediate
        K.Standard_int.Naked_immediate Reg_width_const.naked_immediate
    | No_transformation ->
      let$ tagged_scrutinee =
        bound_prim "tagged_scrutinee" K.value
          (P.Unary (Tag_immediate, scrutinee))
          dbg
      in
      rebuild_affine_expr tagged_scrutinee K.value
        K.Standard_int.Tagged_immediate Reg_width_const.tagged_immediate)

let rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
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
    if
      (not (TI.Set.equal arm_discrs (TI.all_bools machine_width)))
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
        if
          Flambda_features.check_invariants ()
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
        | Some (dest, must_untag_lookup_table_result, lookup_table_fields) -> (
          let affine =
            match lookup_table_fields with
            | Tagged_immediates consts ->
              assert (List.length consts = TI.Map.cardinal arms);
              recognize_affine_switch_to_same_destination machine_width consts
            | Symbols_or_tagged_immediates _ | Naked_float32s _ | Naked_floats _
            | Naked_int8s _ | Naked_int16s _ | Naked_int32s _ | Naked_int64s _
            | Naked_nativeints _ | Naked_vec128s _ | Naked_vec256s _
            | Naked_vec512s _ ->
              None
          in
          match affine with
          | None ->
            rebuild_switch_with_single_arg_to_same_destination uacc
              ~dacc_before_switch ~scrutinee ~dest ~lookup_table_fields
              ~must_untag_lookup_table_result dbg
          | Some (offset, slope) ->
            rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch
              ~scrutinee ~dest ~offset ~slope ~must_untag_lookup_table_result
              dbg)
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
        | Some dest ->
          let uacc =
            (* CR mshinwell: it seems like this should be registering the
               potentially significant reduction in code size -- likewise in
               other cases here. Plus the fact that some operations are
               *added*. *)
            UA.notify_removed ~operation:Removed_operations.branch uacc
          in
          run uacc ~dacc_before_switch
            (let$ tagged_scrutinee =
               bound_prim "tagged_scrutinee" K.value
                 (P.Unary (Tag_immediate, scrutinee))
                 dbg
             in
             let apply_cont =
               Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg
             in
             let expr = RE.create_apply_cont apply_cont in
             return
               ~added_code_size:(Code_size.apply_cont apply_cont)
               ~free_names:(Apply_cont.free_names apply_cont)
               expr)
        | None -> (
          match switch_is_boolean_not with
          | Some dest ->
            let uacc =
              UA.notify_removed ~operation:Removed_operations.branch uacc
            in
            run uacc ~dacc_before_switch
              (let$ tagged_scrutinee =
                 bound_prim "tagged_scrutinee" K.value
                   (P.Unary (Tag_immediate, scrutinee))
                   dbg
               in
               let$ not_scrutinee =
                 bound_prim "not_scrutinee" K.value
                   (P.Unary (Boolean_not, tagged_scrutinee))
                   dbg
               in
               let apply_cont =
                 Apply_cont.create dest ~args:[not_scrutinee] ~dbg
               in
               let free_names = Apply_cont.free_names apply_cont in
               let added_code_size = Code_size.apply_cont apply_cont in
               return ~added_code_size ~free_names
                 (RE.create_apply_cont apply_cont))
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

let simplify_switch dacc switch ~down_to_up =
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
  let dacc =
    match DA.are_lifting_conts dacc with
    | Lifting_out_of _ ->
      Misc.fatal_errorf
        "[Are_lifting_cont] values in the dacc cannot be [Lifting_out_of _] \
         when going downwards through a [Switch] expression. See the \
         explanation in [are_lifting_conts.mli]."
    | Not_lifting -> dacc
    | Analyzing { continuation; uses; is_exn_handler } -> (
      (* Some preliminary requirements. We do **not** specialize continuations
         if one of the following conditions are true:

         - they have only one (or less) use

         - they are an exception handler. To handle this case, the existing
         mechanism used to rewrite specialized calls on the way up should be
         extended to also rewrite pop_traps and other uses of exn handlers
         (which is not currently the case).

         - we are at toplevel, in which case there can be symbols which we might
         duplicate by specializing (which would be an error). More generally,
         the benefits of specialization at unit toplevel do not seem that great,
         because partial evaluation would be better. *)
      let n_uses = Continuation_uses.number_of_uses uses in
      if is_exn_handler || n_uses <= 1 || DE.at_unit_toplevel (DA.denv dacc)
      then dacc
      else
        let denv = DA.denv dacc in
        match DE.specialization_cost denv with
        | Cannot_specialize { reason = _ } ->
          (* CR gbury: we could try and emit something analog to the inlining
             report, but for other optimizations at one point ? *)
          dacc
        | Can_specialize { size_of_primitives = _ } ->
          (* Estimate the cost of lifting: this mainly comes from adding new
             parameters, which increase the work done by the typing env, as well
             as the flow analysis. We then only do the lifting if the cost is
             within the budget for the current function. *)
          let lifting_budget = DA.get_continuation_lifting_budget dacc in
          let lifting_cost =
            DE.cost_of_lifting_continuations_out_of_current_one denv
          in
          let is_lifting_allowed_by_budget =
            lifting_budget > 0 && lifting_cost <= lifting_budget
          in
          (* very basic specialization budget *)
          let specialization_budget =
            DA.get_continuation_specialization_budget dacc
          in
          let specialization_cost =
            n_uses + 1
            (* specializing requires 'n_uses + 1' traversals of the continuation
               handler *)
          in
          let is_specialization_allowed_by_budget =
            specialization_budget > 0
            && specialization_cost <= specialization_budget
          in
          if
            (not is_lifting_allowed_by_budget)
            || not is_specialization_allowed_by_budget
          then dacc
          else
            (* TODO/FIXME: implement an actual criterion for when to lift
               continuations and specialize them. Currently for testing, we lift
               any continuation that occurs in a handler that ends with a switch
               (if the bduget for lifting and specialization allows it), and we
               specialize the continuation that ends with the switch. *)
            let dacc =
              DA.decrease_continuation_lifting_budget dacc lifting_cost
            in
            let dacc =
              DA.decrease_continuation_specialization_budget dacc
                specialization_cost
            in
            let dacc =
              DA.with_are_lifting_conts dacc
                (Are_lifting_conts.lift_continuations_out_of continuation)
            in
            let dacc = DA.add_continuation_to_specialize dacc continuation in
            dacc)
  in
  down_to_up dacc
    ~rebuild:
      (rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
         ~dacc_before_switch)
