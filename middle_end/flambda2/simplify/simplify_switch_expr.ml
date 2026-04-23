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

type lookup_table_fields =
  | Tagged_immediates of TI.t list
      (** All arms are tagged immediates. The lookup table uses a value array
          specialised to [Immediates]. This case is split out from
          [Static_arguments_of_single_kind] so that the affine-arithmetic
          optimisation can be applied. *)
  | Static_arguments_of_single_kind of
      { array_kind : P.Array_kind.t;
        array_load_kind : P.Array_load_kind.t;
        element_kind : K.With_subkind.t;
        simples : Simple.t list
      }
      (** All arms are symbols or constants of the same [Flambda_kind.t]. For
          the value kind (with [array_kind = Values]), this variant allows a mix
          of symbols (including ones pointing at boxed numbers), tagged
          immediates and nulls; for all other kinds symbols are forbidden, so
          every arm is a constant of the kind described by [element_kind]. *)

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
            ~symbol:(fun _sym ~coercion:_ ->
              let expected_discr =
                TI.add (TI.one machine_width) expected_discr
              in
              Some (Some dest', arg :: args_rev, expected_discr))
            ~const:(fun _const ->
              let expected_discr =
                TI.add (TI.one machine_width) expected_discr
              in
              Some (Some dest', arg :: args_rev, expected_discr))))
  in
  match TI.Map.fold check_arm arms (Some (None, [], TI.zero machine_width)) with
  | None | Some (None, _, _) | Some (_, [], _) -> None
  | Some (Some dest, args_rev, _) -> (
    let args : Simple.t list = List.rev args_rev in
    assert (List.compare_length_with args 1 >= 0);
    let module RWC = Reg_width_const in
    let module ALK = P.Array_load_kind in
    (* Symbols are always of kind [value]; they may be freely mixed with
       [Const]s of kind [value] (i.e. tagged immediates). For all other kinds
       symbols are not permitted and every arm must be a constant of the same
       [Flambda_kind.t]. *)
    let kind_of simple =
      Simple.pattern_match' simple
        ~var:(fun _ ~coercion:_ ->
          (* Variables have already been ruled out above. *) assert false)
        ~symbol:(fun _ ~coercion:_ -> K.value)
        ~const:RWC.kind
    in
    let first_kind = kind_of (List.hd args) in
    if not (List.for_all (fun arg -> K.equal (kind_of arg) first_kind) args)
    then None
    else
      let single_kind array_kind array_load_kind =
        let element_kind = ALK.kind_of_loaded_value array_load_kind in
        Some
          ( dest,
            Static_arguments_of_single_kind
              { array_kind; array_load_kind; element_kind; simples = args } )
      in
      let try_tagged_immediates () =
        (* If no arm is a symbol and all [Const]s are tagged immediates, return
           a [Tagged_immediates] variant so that the affine optimization can
           apply below. Returns [None] otherwise. *)
        List.fold_right
          (fun simple acc ->
            match acc with
            | None -> None
            | Some tagged_imms ->
              Simple.pattern_match' simple
                ~var:(fun _ ~coercion:_ -> assert false)
                ~symbol:(fun _ ~coercion:_ -> None)
                ~const:(fun cst ->
                  Option.map
                    (fun tagged_imm -> tagged_imm :: tagged_imms)
                    (RWC.is_tagged_immediate cst)))
          args (Some [])
      in
      match (first_kind : K.t) with
      | Value -> (
        (* All arms are of kind [value]: either all tagged immediates, or a mix
           of tagged immediates, symbols (which may point at e.g. boxed numbers)
           or nulls. *)
        match try_tagged_immediates () with
        | Some tagged_imms -> Some (dest, Tagged_immediates tagged_imms)
        | None ->
          (* It is possible that this array will contain only boxed floats even
             with the float array optimization enabled. These would not normally
             arise in the presence of such optimization, but if we don't tell
             anyone it will be ok: we explicitly generate the load using array
             load kind [Values] (which does not do any float array optimization
             tests; all of those were expanded in [Lambda_to_flambda]). *)
          single_kind Values Values)
      | Naked_number nn -> (
        match nn with
        | Naked_immediate -> single_kind Naked_ints Naked_ints
        | Naked_float32 -> single_kind Naked_float32s Naked_float32s
        | Naked_float -> single_kind Naked_floats Naked_floats
        | Naked_int8 -> single_kind Naked_int8s Naked_int8s
        | Naked_int16 -> single_kind Naked_int16s Naked_int16s
        | Naked_int32 -> single_kind Naked_int32s Naked_int32s
        | Naked_int64 -> single_kind Naked_int64s Naked_int64s
        | Naked_nativeint -> single_kind Naked_nativeints Naked_nativeints
        | Naked_vec128 -> single_kind Naked_vec128s Naked_vec128s
        | Naked_vec256 -> single_kind Naked_vec256s Naked_vec256s
        | Naked_vec512 -> single_kind Naked_vec512s Naked_vec512s)
      | Region | Rec_info -> None)

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

let fields_to_simples dbg simples =
  List.map (fun simple -> Simple.With_debuginfo.create simple dbg) simples

let create_lookup_table_array_const dbg (array_kind : P.Array_kind.t) rebuilding
    simples =
  let module RWC = Reg_width_const in
  let fields_to_or_variables prover simples =
    ListLabels.map simples ~f:(fun simple ->
        Simple.pattern_match simple
          ~name:(fun _ ~coercion:_ ->
            (* Only constants reach this point. *) assert false)
          ~const:(fun cst ->
            let cst =
              match prover cst with
              | Some v -> v
              | None ->
                Misc.fatal_errorf
                  "Unexpected kind of constant (%a) in switch table at %a"
                  RWC.print cst Debuginfo.print_compact dbg
            in
            Or_variable.Const cst))
  in
  let naked_number_array creator prover =
    creator rebuilding (fields_to_or_variables prover simples)
  in
  match array_kind with
  | Values ->
    RSC.create_immutable_value_array rebuilding (fields_to_simples dbg simples)
  | Naked_float32s ->
    naked_number_array RSC.create_immutable_float32_array RWC.is_naked_float32
  | Naked_floats ->
    naked_number_array RSC.create_immutable_float_array RWC.is_naked_float
  | Naked_ints ->
    naked_number_array RSC.create_immutable_int_array RWC.is_naked_immediate
  | Naked_int8s ->
    naked_number_array RSC.create_immutable_int8_array RWC.is_naked_int8
  | Naked_int16s ->
    naked_number_array RSC.create_immutable_int16_array RWC.is_naked_int16
  | Naked_int32s ->
    naked_number_array RSC.create_immutable_int32_array RWC.is_naked_int32
  | Naked_int64s ->
    naked_number_array RSC.create_immutable_int64_array RWC.is_naked_int64
  | Naked_nativeints ->
    naked_number_array RSC.create_immutable_nativeint_array
      RWC.is_naked_nativeint
  | Naked_vec128s ->
    naked_number_array RSC.create_immutable_vec128_array RWC.is_naked_vec128
  | Naked_vec256s ->
    naked_number_array RSC.create_immutable_vec256_array RWC.is_naked_vec256
  | Naked_vec512s ->
    naked_number_array RSC.create_immutable_vec512_array RWC.is_naked_vec512
  | Immediates | Gc_ignorable_values | Unboxed_product _ ->
    Misc.fatal_errorf
      "Unexpected array kind %a when rebuilding switch lookup table at %a"
      P.Array_kind.print array_kind Debuginfo.print_compact dbg

let rebuild_switch_with_single_arg_to_same_destination uacc ~dacc_before_switch
    ~scrutinee ~dest ~(lookup_table_fields : lookup_table_fields) dbg =
  let rebuilding = UA.are_rebuilding_terms uacc in
  let block_sym =
    let var = Variable.create "switch_block" K.value in
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string (Variable.unique_name var))
  in
  let uacc, array_kind, array_load_kind, loaded_kind =
    let alias_types_of kind simples =
      List.map (fun simple -> T.alias_type_of kind simple) simples
    in
    let array_const, array_kind, array_load_kind, element_kind, fields =
      let module AK = P.Array_kind in
      let module ALK = P.Array_load_kind in
      match lookup_table_fields with
      | Tagged_immediates imms ->
        let simples = List.map Simple.const_int imms in
        ( RSC.create_immutable_value_array rebuilding
            (fields_to_simples dbg simples),
          AK.Values,
          ALK.Immediates,
          KS.tagged_immediate,
          alias_types_of K.value simples )
      | Static_arguments_of_single_kind
          { array_kind; array_load_kind; element_kind; simples } ->
        let fields = alias_types_of (KS.kind element_kind) simples in
        let array_const =
          create_lookup_table_array_const dbg array_kind rebuilding simples
        in
        array_const, array_kind, array_load_kind, element_kind, fields
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
     (* Note that, unlike for the untagging of normal Switch scrutinees, there's
        no problem with CSE and Data_flow here. The reason is that in this case
        the generated primitive always names a fresh variable, so it will never
        be eligible for CSE. *)
     (* CR mshinwell: we could probably expose the actual integer counts of
        continuations in [Name_occurrences] and then try to inline out [dest].
        This might happen anyway in the backend though so this probably isn't
        that important for now. *)
     let apply_cont = Apply_cont.create dest ~args:[arg] ~dbg in
     let free_names_of_body = Apply_cont.free_names apply_cont in
     let expr =
       let body = RE.create_apply_cont apply_cont in
       let bound = BPt.singleton (BV.create arg_var arg_var_duid NM.normal) in
       RE.create_let rebuilding bound load_from_block ~body ~free_names_of_body
     in
     let extra_free_names =
       NO.union
         (Named.free_names load_from_block)
         (NO.remove_var free_names_of_body ~var:arg_var)
     in
     let machine_width = DE.machine_width (DA.denv dacc_before_switch) in
     let added_code_size =
       Code_size.( + )
         (Code_size.prim ~machine_width load_from_block_prim)
         (Code_size.apply_cont apply_cont)
     in
     (* CR mshinwell: it seems we need to fix [Cost_metrics] so we can note that
        we have *added* operations here (load). *)
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

type affine_immediate_kind =
  | Tagged
  | Naked

let rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch
    ~scrutinee ~dest ~offset ~slope ~immediate_kind dbg =
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
    let add_prim : P.t =
      Binary
        (Int_arith (standard_int, Add), scaled_arg, Simple.const (const offset))
    in
    let$ final_arg = bound_prim "final_arg" kind add_prim dbg in
    let apply_cont = Apply_cont.create dest ~args:[final_arg] ~dbg in
    let free_names = Apply_cont.free_names apply_cont in
    let added_code_size = Code_size.apply_cont apply_cont in
    return ~added_code_size ~free_names (RE.create_apply_cont apply_cont)
  in
  run ~dacc_before_switch uacc
    (match (immediate_kind : affine_immediate_kind) with
    | Naked ->
      rebuild_affine_expr scrutinee K.naked_immediate
        K.Standard_int.Naked_immediate Reg_width_const.naked_immediate
    | Tagged ->
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
        | Some (dest, lookup_table_fields) -> (
          let try_affine immediate_kind consts =
            assert (List.length consts = TI.Map.cardinal arms);
            Option.map
              (fun (offset, slope) -> immediate_kind, offset, slope)
              (recognize_affine_switch_to_same_destination machine_width consts)
          in
          let affine =
            match lookup_table_fields with
            | Tagged_immediates consts -> try_affine Tagged consts
            | Static_arguments_of_single_kind
                { array_kind; array_load_kind = _; element_kind = _; simples }
              -> (
              match (array_kind : P.Array_kind.t) with
              | Naked_ints ->
                let consts =
                  List.filter_map
                    (fun simple ->
                      Simple.pattern_match' simple
                        ~var:(fun _ ~coercion:_ -> None)
                        ~symbol:(fun _ ~coercion:_ -> None)
                        ~const:Reg_width_const.is_naked_immediate)
                    simples
                in
                if List.compare_lengths consts simples = 0
                then try_affine Naked consts
                else None
              | Immediates | Gc_ignorable_values | Values | Naked_floats
              | Naked_float32s | Naked_int8s | Naked_int16s | Naked_int32s
              | Naked_int64s | Naked_nativeints | Naked_vec128s | Naked_vec256s
              | Naked_vec512s | Unboxed_product _ ->
                None)
          in
          match affine with
          | None ->
            rebuild_switch_with_single_arg_to_same_destination uacc
              ~dacc_before_switch ~scrutinee ~dest ~lookup_table_fields dbg
          | Some (immediate_kind, offset, slope) ->
            rebuild_affine_switch_to_same_destination uacc ~dacc_before_switch
              ~scrutinee ~dest ~offset ~slope ~immediate_kind dbg)
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
