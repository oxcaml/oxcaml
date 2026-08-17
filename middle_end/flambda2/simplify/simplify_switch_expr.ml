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
module RWC = Reg_width_const

type alias_set =
  | Aliases of Alias_set.t
  | Poison of Flambda_kind.t

type mergeable_arms =
  | No_arms
  | Mergeable of
      { cont : Continuation.t;
        args : alias_set TI.Map.t list
      }
  | Not_mergeable

let inter_alias_set alias1 alias2 =
  match alias1, alias2 with
  | Aliases alias1, Aliases alias2 -> Aliases (Alias_set.inter alias1 alias2)
  | Aliases alias, Poison _ | Poison _, Aliases alias -> Aliases alias
  | Poison kind1, Poison kind2 ->
    if Flambda_kind.equal kind1 kind2
    then Poison kind1
    else
      Misc.fatal_errorf
        "[inter_alias_set]: intersection of poison with different kinds %a and \
         %a"
        Flambda_kind.print kind1 Flambda_kind.print kind2

let find_all_aliases env arg =
  let find_all_aliases () =
    Aliases (TE.aliases_of_simple env ~min_name_mode:NM.normal arg)
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
      Aliases (TE.Alias_set.singleton arg))
    ~symbol:(fun _sym ~coercion:_ -> find_all_aliases ())
    ~const:(fun cst ->
      match Reg_width_const.is_poison cst with
      | Some (kind, _name) -> Poison kind
      | None -> find_all_aliases ())
    arg

let rebuild_arm uacc arm (action, use_id, arity, env_at_use)
    (new_let_conts, arms, (mergeable_arms : mergeable_arms)) =
  let action =
    Simplify_common.clear_demoted_trap_action_and_patch_unused_exn_bucket uacc
      action
  in
  match EB.rewrite_switch_arm uacc action ~use_id arity with
  | Invalid _ ->
    (* The destination is unreachable; delete the [Switch] arm. *)
    new_let_conts, arms, mergeable_arms
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
      new_let_conts, arms, mergeable_arms
    | Some action -> (
      let arms = TI.Map.add arm action arms in
      (* Check to see if this arm may be merged with others. *)
      if Option.is_some (Apply_cont.trap_action action)
      then new_let_conts, arms, Not_mergeable
      else
        match mergeable_arms with
        | Not_mergeable -> new_let_conts, arms, Not_mergeable
        | No_arms ->
          let cont = Apply_cont.continuation action in
          let args =
            List.map
              (fun arg ->
                TI.Map.singleton arm (find_all_aliases env_at_use arg))
              (Apply_cont.args action)
          in
          new_let_conts, arms, Mergeable { cont; args }
        | Mergeable { cont; args } ->
          if not (Continuation.equal cont (Apply_cont.continuation action))
          then new_let_conts, arms, Not_mergeable
          else
            let args =
              List.map2
                (fun arg_set arg ->
                  TI.Map.add arm (find_all_aliases env_at_use arg) arg_set)
                args (Apply_cont.args action)
            in
            new_let_conts, arms, Mergeable { cont; args }))
  | New_wrapper new_let_cont ->
    let new_let_conts = new_let_cont :: new_let_conts in
    let action = Apply_cont.goto new_let_cont.cont in
    let arms = TI.Map.add arm action arms in
    new_let_conts, arms, Not_mergeable

let filter_and_choose_alias required_names alias_set =
  match alias_set with
  | Poison kind ->
    Some (Simple.const (Reg_width_const.const_poison kind "rebuild_switch"))
  | Aliases alias_set ->
    let available_alias_set =
      Alias_set.filter alias_set ~f:(fun alias ->
          Simple.pattern_match alias
            ~name:(fun name ~coercion:_ -> Name.Set.mem name required_names)
            ~const:(fun _ -> true))
    in
    Alias_set.find_best available_alias_set

let find_cse_simple ?(required = true) dacc required_names local_cse prim =
  match P.Eligible_for_cse.create prim with
  | None -> None (* Constant *)
  | Some with_fixed_value -> (
    let[@local] try_local_cse () =
      Common_subexpression_elimination.find local_cse with_fixed_value
    in
    match DE.find_cse (DA.denv dacc) with_fixed_value with
    | None ->
      if required
      then
        Misc.fatal_errorf
          "Expected@ primitive@ not@ found@ in@ CSE@ environment@ while@ \
           simplifying switch:@ %a"
          P.print prim
      else try_local_cse ()
    | Some simple -> (
      match
        filter_and_choose_alias required_names
          (find_all_aliases (DA.typing_env dacc) simple)
      with
      | Some simple -> Some simple
      | None -> try_local_cse ()))

let fields_to_simples dbg simples =
  List.map (fun simple -> Simple.With_debuginfo.create simple dbg) simples

let create_lookup_table_array_const dbg (array_kind : P.Array_kind.t) rebuilding
    simples =
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
  | Values | Immediates ->
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
  | Naked_masks ->
    naked_number_array RSC.create_immutable_mask_array RWC.is_naked_mask
  | Gc_ignorable_values | Unboxed_product _ ->
    Misc.fatal_errorf
      "Unexpected array kind %a when rebuilding switch lookup table at %a"
      P.Array_kind.print array_kind Debuginfo.print_compact dbg

(* Tiny DSL to preserve sanity while rebuilding expressions. *)

type named =
  | Simple of Simple.t
  | Prim of string * K.t * P.t * Debuginfo.t
  | Lookup_table of
      { name : string;
        array_kind : P.Array_kind.t;
        element_kind : KS.t;
        simples : Simple.t list;
        dbg : Debuginfo.t
      }

let bound_simple simple = Simple simple

let bound_prim name kind prim dbg = Prim (name, kind, prim, dbg)

let bound_lookup_table name ~element_kind ~array_kind simples dbg =
  Lookup_table { name; array_kind; element_kind; simples; dbg }

let ( let$ ) expr k uacc ~dacc_before_switch ~local_cse =
  let[@local] already_bound simple =
    k simple uacc ~dacc_before_switch ~local_cse
  in
  match expr with
  | Simple simple -> already_bound simple
  | Lookup_table { name; array_kind; element_kind; simples; dbg } -> (
    let array_const =
      create_lookup_table_array_const dbg array_kind
        (UA.are_rebuilding_terms uacc)
        simples
    in
    let[@local] create_lookup_table static_const =
      let symbol =
        let var = Variable.create name (KS.kind element_kind) in
        Symbol.create
          (Current_unit.get_cu_exn ())
          (Linkage_name.of_string (Variable.unique_name var))
      in
      let dacc_before_switch =
        match static_const with
        | None -> dacc_before_switch
        | Some static_const ->
          (* Note: this only enables sharing of identical arguments for this
             switch -- the modified [dacc_before_switch] gets thrown away. *)
          DA.consider_constant_for_sharing dacc_before_switch symbol
            static_const
      in
      let fields = List.map (T.alias_type_of (KS.kind element_kind)) simples in
      let block_type =
        T.immutable_array ~element_kind:(Ok element_kind) ~fields
          Alloc_mode.For_types.heap
          ~machine_width:(DE.machine_width (DA.denv dacc_before_switch))
      in
      let uacc =
        UA.add_lifted_constant uacc
          (LC.create_block_like symbol array_const
             (DA.denv dacc_before_switch)
             block_type ~symbol_projections:Variable.Map.empty)
      in
      k (Simple.symbol symbol) uacc ~dacc_before_switch ~local_cse
    in
    match RSC.to_const array_const with
    | None ->
      (* Not rebuilding terms *)
      create_lookup_table None
    | Some (Deleted_code | Code _) -> Misc.fatal_error "Cannot bind code"
    | Some (Static_const const) -> (
      match DA.find_shareable_constant dacc_before_switch const with
      | None -> create_lookup_table (Some const)
      | Some symbol -> already_bound (Simple.symbol symbol)))
  | Prim (name, kind, prim, dbg) -> (
    match
      find_cse_simple ~required:false dacc_before_switch
        (UA.required_names uacc) local_cse prim
    with
    | Some simple -> already_bound simple
    | None ->
      let named = Named.create_prim prim dbg in
      let var = Variable.create name kind in
      let uacc = UA.add_free_names uacc (NO.singleton_variable var NM.normal) in
      let local_cse =
        match P.Eligible_for_cse.create prim with
        | None -> local_cse
        | Some with_fixed_value ->
          Common_subexpression_elimination.add local_cse with_fixed_value
            ~bound_to:(Simple.var var)
            (DE.get_continuation_scope (DA.denv dacc_before_switch))
      in
      let body, uacc = k (Simple.var var) uacc ~dacc_before_switch ~local_cse in
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
      EB.make_new_let_bindings uacc ~bindings_outermost_first:[binding] ~body)

let return ~added_code_size ~free_names expr uacc ~dacc_before_switch:_
    ~local_cse:_ =
  let uacc = UA.notify_added ~code_size:added_code_size uacc in
  let uacc = UA.add_free_names uacc free_names in
  expr, uacc

let run uacc ~dacc_before_switch k =
  (* [local_cse] allows sharing between distinct arguments of the same switch.

     We can't update the CSE from the [dacc_before_switch] because that can bind
     to existing names that are not in the [required_names] and we can't use
     anymore. *)
  k uacc ~dacc_before_switch ~local_cse:Common_subexpression_elimination.empty

type affine_immediate_kind =
  | Tagged
  | Naked

let equal_affine_immediate_kind kind1 kind2 =
  match kind1, kind2 with
  | Tagged, Tagged | Naked, Naked -> true
  | (Tagged | Naked), _ -> false

let affine_immediate_kind_to_standard_int = function
  | Tagged -> K.Standard_int.Tagged_immediate
  | Naked -> K.Standard_int.Naked_immediate

type affine_argument_of_mergeable_arm =
  | Not_affine
  | Maybe_affine
  | Constant of affine_immediate_kind * TI.t * TI.t  (** kind, input, output *)
  | Affine of affine_immediate_kind * TI.t * TI.t  (** kind, offset, slope *)

let recognize_affine_immediate_const const =
  match Reg_width_const.descr const with
  | Naked_immediate naked_imm -> Some (Naked, naked_imm)
  | Tagged_immediate tagged_imm -> Some (Tagged, tagged_imm)
  | Naked_float32 _ | Naked_float _ | Naked_int8 _ | Naked_int16 _
  | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _
  | Naked_vec256 _ | Naked_vec512 _ | Naked_mask _ | Null ->
    None
  | Poison (_, _) -> None

let recognize_affine_argument ~machine_width affine_argument discr const =
  let[@local] affine kind ~offset ~slope = Affine (kind, offset, slope) in
  let[@local] maybe_affine kind imm ~offset ~slope =
    if TI.(equal imm (add (mul discr slope) offset))
    then affine kind ~offset ~slope
    else Not_affine
  in
  match affine_argument, recognize_affine_immediate_const const with
  | Not_affine, _ | _, None -> Not_affine
  | Maybe_affine, Some (kind, imm) -> Constant (kind, discr, imm)
  | Constant (kind, arm, arg), Some (imm_kind, imm) ->
    if not (equal_affine_immediate_kind kind imm_kind)
    then Misc.fatal_error "Inconsistent kinds for mergeable switch argument";
    let arg_diff = TI.sub imm arg in
    let arm_diff = TI.sub discr arm in
    if TI.equal (TI.zero machine_width) (TI.mod_ arg_diff arm_diff)
    then
      let slope = TI.div arg_diff arm_diff in
      let offset = TI.sub arg (TI.mul slope arm) in
      affine kind ~offset ~slope
    else Not_affine
  | Affine (kind, offset, slope), Some (imm_kind, imm) ->
    if not (equal_affine_immediate_kind imm_kind kind)
    then Misc.fatal_error "Inconsistent kinds for mergeable switch argument";
    maybe_affine kind imm ~offset ~slope

type mergeable_argument =
  | Invariant_argument of Simple.t
  | Tagged_scrutinee
  | Not_scrutinee (* always tagged immediate *)
  | Affine_argument of
      { kind : affine_immediate_kind;
        offset : TI.t;
        slope : TI.t
      }
  | Lookup_table_argument of
      { array_kind : P.Array_kind.t;
        array_load_kind : P.Array_load_kind.t;
        first_discriminant : TI.t;
        simples : Simple.t list
      }
      (** All arms are symbols or constants of the same [Flambda_kind.t]. For
          the value kind (with [array_kind = Values]), this variant allows a mix
          of symbols (including ones pointing at boxed numbers), tagged
          immediates and nulls; for all other kinds symbols are forbidden, so
          every arm is a constant of the kind described by [array_load_kind]. *)

let enough_arms ~num_arms = function
  | Invariant_argument _ | Tagged_scrutinee | Not_scrutinee -> true
  | Affine_argument _ | Lookup_table_argument _ ->
    (* Only introduce affine computations and lookup tables if the switch is
       large enough *)
    num_arms >= 3

let bound_tagged_scrutinee scrutinee dbg =
  bound_prim "tagged_scrutinee" K.value (P.Unary (Tag_immediate, scrutinee)) dbg

let rebuild_mergeable_argument ~machine_width ~scrutinee mergeable_argument k =
  let dbg = Debuginfo.none in
  match mergeable_argument with
  | Invariant_argument simple -> k simple
  | Affine_argument { kind; offset; slope } -> (
    let[@inline] rebuild_affine_expr scrutinee const =
      let standard_int = affine_immediate_kind_to_standard_int kind in
      let kind = K.Standard_int.to_kind standard_int in
      let must_negate, slope =
        if TI.is_non_negative slope then false, slope else true, TI.neg slope
      in
      let$ scaled_arg =
        if TI.equal slope (TI.one machine_width)
        then bound_simple scrutinee
        else
          bound_prim "scaled_arg" kind
            (Binary
               ( Int_arith (standard_int, Mul),
                 scrutinee,
                 Simple.const (const slope) ))
            dbg
      in
      let$ final_arg =
        if TI.equal offset (TI.zero machine_width) && not must_negate
        then bound_simple scaled_arg
        else
          bound_prim "final_arg" kind
            (Binary
               ( Int_arith (standard_int, if must_negate then Sub else Add),
                 Simple.const (const offset),
                 scaled_arg ))
            dbg
      in
      k final_arg
    in
    match kind with
    | Naked -> rebuild_affine_expr scrutinee RWC.naked_immediate
    | Tagged ->
      let$ tagged_scrutinee = bound_tagged_scrutinee scrutinee dbg in
      rebuild_affine_expr tagged_scrutinee RWC.tagged_immediate)
  | Tagged_scrutinee ->
    let$ tagged_scrutinee = bound_tagged_scrutinee scrutinee dbg in
    k tagged_scrutinee
  | Not_scrutinee ->
    let$ tagged_scrutinee = bound_tagged_scrutinee scrutinee dbg in
    let$ not_scrutinee =
      bound_prim "not_scrutinee" K.value
        (P.Unary (Boolean_not, tagged_scrutinee))
        dbg
    in
    k not_scrutinee
  | Lookup_table_argument
      { array_kind; array_load_kind; first_discriminant; simples } ->
    let module ALK = P.Array_load_kind in
    let element_kind = ALK.kind_of_loaded_value array_load_kind in
    let loaded_kind = KS.kind element_kind in
    let$ switch_block =
      bound_lookup_table "switch_block" ~element_kind ~array_kind simples dbg
    in
    let$ tagged_scrutinee =
      bound_prim "tagged_scrutinee" K.value
        (P.Unary (Tag_immediate, scrutinee))
        dbg
    in
    let$ offset_scrutinee =
      if TI.equal first_discriminant (TI.zero machine_width)
      then bound_simple tagged_scrutinee
      else
        bound_prim "offset_scrutinee" K.value
          (P.Binary
             ( Int_arith (Tagged_immediate, Add),
               tagged_scrutinee,
               Simple.const_int (TI.neg first_discriminant) ))
          dbg
    in
    let$ load_from_block =
      bound_prim "arg" loaded_kind
        (P.Binary
           ( Array_load (array_kind, array_load_kind, Immutable),
             switch_block,
             offset_scrutinee ))
        dbg
    in
    k load_from_block

exception Argument_cannot_be_used_in_lookup_table

let recognize_invariant_argument required_names (arg : alias_set TI.Map.t) =
  let shared_alias_set =
    TI.Map.fold
      (fun _ alias_set shared_alias_set ->
        match shared_alias_set with
        | None -> Some alias_set
        | Some shared_alias_set ->
          Some (inter_alias_set alias_set shared_alias_set))
      arg None
  in
  let shared_alias_set =
    match shared_alias_set with
    | None ->
      (* This means that the argument is always poison. *)
      Misc.fatal_error "Trying to merge zero switch arms"
    | Some shared_alias_set -> shared_alias_set
  in
  filter_and_choose_alias required_names shared_alias_set

let recognize_mergeable_argument ~machine_width ~scrutinee required_names ~dbg
    (arg : alias_set TI.Map.t) =
  let check_arm discr alias_set (affine_arg, args_rev_and_expected_discr) =
    let[@local] maybe_table_lookup_arg constant_arg simple =
      (* These expressions can be compiled using lookup tables, which
         dramatically reduces code size. *)
      match args_rev_and_expected_discr with
      | None -> constant_arg, None
      | Some (args_rev, expected_discr) ->
        if not (TI.equal discr expected_discr)
        then
          (* Discriminants must be d..(d + num_arms-1) (note that it is possible
             to have Switches that do not satisfy this criterion in
             Flambda2). *)
          constant_arg, None
        else
          let expected_discr = TI.add (TI.one machine_width) expected_discr in
          constant_arg, Some (simple :: args_rev, expected_discr)
    in
    let[@local] maybe_constant_arg const =
      let constant_args =
        recognize_affine_argument ~machine_width affine_arg discr const
      in
      maybe_table_lookup_arg constant_args (Simple.const const)
    in
    match alias_set with
    | Poison kind ->
      maybe_table_lookup_arg affine_arg
        (Simple.const (RWC.const_poison kind "rebuild_switch"))
    | Aliases aliases -> (
      match Alias_set.find_best aliases with
      | None -> Misc.fatal_error "Unexpected empty argument"
      | Some simple -> (
        let open struct
          type const_or_symbol =
            | Const of Reg_width_const.t
            | Symbol
        end in
        (* The destination continuations must have constant or symbol
           arguments. *)
        match
          Simple.pattern_match' simple
            ~const:(fun const -> Const const)
            ~symbol:(fun _ ~coercion ->
              if not (NO.no_variables (Coercion.free_names coercion))
              then raise_notrace Argument_cannot_be_used_in_lookup_table;
              Symbol)
            ~var:(fun _var ~coercion:_ ->
              (* CR mshinwell: we could allow variables, if at toplevel *)
              (* Aliases should have been followed by now. *)
              raise_notrace Argument_cannot_be_used_in_lookup_table)
        with
        | Const const -> maybe_constant_arg const
        | Symbol -> maybe_table_lookup_arg Not_affine simple))
  in
  match recognize_invariant_argument required_names arg with
  | Some simple -> Some (Invariant_argument simple)
  | None -> (
    (* First discriminant is not necessarily zero, in which case the array
       lookup will be offset. *)
    let first_discr =
      try TI.Map.min_binding arg |> fst
      with Not_found -> TI.zero machine_width
    in
    let affine_is_identity ~offset ~slope =
      TI.equal offset (TI.zero machine_width)
      && TI.equal slope (TI.one machine_width)
    in
    let affine_is_boolean_not ~offset ~slope =
      TI.equal offset (TI.one machine_width)
      && TI.equal slope (TI.minus_one machine_width)
      && TI.Map.subset_domain arg
           (TI.Map.of_set (fun _ -> ()) (TI.all_bools machine_width))
    in
    match TI.Map.fold check_arm arg (Maybe_affine, Some ([], first_discr)) with
    | (exception Argument_cannot_be_used_in_lookup_table) | Not_affine, None ->
      None
    | (Maybe_affine | Constant _), _ ->
      Misc.fatal_error "Unexpected invariant argument"
    | Affine (kind, offset, slope), _ when affine_is_identity ~offset ~slope
      -> (
      match kind with
      | Naked -> Some (Invariant_argument scrutinee)
      | Tagged -> Some Tagged_scrutinee)
    | Affine (kind, offset, slope), _ -> (
      match kind with
      | Tagged when affine_is_boolean_not ~offset ~slope -> Some Not_scrutinee
      | Tagged | Naked -> Some (Affine_argument { kind; offset; slope }))
    | Not_affine, Some ([], _) -> None
    | Not_affine, Some (args_rev, _) -> (
      let args : Simple.t list = List.rev args_rev in
      assert (List.compare_length_with args 1 >= 0);
      (* Symbols are always of kind [value]; they may be freely mixed with
         [Const]s of kind [value] (i.e. tagged immediates). For all other kinds
         symbols are not permitted and every arm must be a constant of the same
         [Flambda_kind.t]. *)
      let kind_of simple =
        Simple.pattern_match' simple
          ~var:(fun _ ~coercion:_ ->
            (* Variables have already been ruled out above. *)
            Misc.fatal_errorf "Variable (%a) was not expected here: %a"
              Simple.print simple Debuginfo.print_compact dbg)
          ~symbol:(fun _ ~coercion:_ -> K.value)
          ~const:RWC.kind
      in
      let first_kind = kind_of (List.hd args) in
      if not (List.for_all (fun arg -> K.equal (kind_of arg) first_kind) args)
      then None
      else
        let single_kind array_kind array_load_kind =
          Some
            (Lookup_table_argument
               { array_kind;
                 array_load_kind;
                 first_discriminant = first_discr;
                 simples = args
               })
        in
        match first_kind with
        | Value ->
          if
            List.for_all
              (fun simple ->
                Option.bind
                  (Simple.must_be_const simple)
                  RWC.is_tagged_immediate
                |> Option.is_some)
              args
          then single_kind Immediates Immediates
          else
            (* It is possible that this array will contain only boxed floats
               even with the float array optimization enabled. These would not
               normally arise in the presence of such optimization, but if we
               don't tell anyone it will be ok: we explicitly generate the load
               using array load kind [Values] (which does not do any float array
               optimization tests; all of those were expanded in
               [Lambda_to_flambda]). *)
            single_kind Values Values
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
          | Naked_vec512 -> single_kind Naked_vec512s Naked_vec512s
          | Naked_mask -> single_kind Naked_masks Naked_masks)
        | Region | Rec_info -> None))

let rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
    ~dacc_before_switch uacc ~after_rebuild =
  let new_let_conts, arms, mergeable_arms =
    TI.Map.fold (rebuild_arm uacc) arms ([], TI.Map.empty, No_arms)
  in
  let num_arms = TI.Map.cardinal arms in
  let switch_merged =
    match mergeable_arms with
    | No_arms | Not_mergeable -> None
    | Mergeable { cont; args } ->
      let num_args = List.length args in
      let machine_width = DE.machine_width (DA.denv dacc_before_switch) in
      let mergeable_args =
        List.filter_map
          (recognize_mergeable_argument ~machine_width ~scrutinee
             ~dbg:condition_dbg (UA.required_names uacc))
          args
      in
      if
        List.compare_length_with mergeable_args num_args = 0
        && List.for_all (enough_arms ~num_arms) mergeable_args
      then Some (cont, mergeable_args)
      else None
  in
  let machine_width = DE.machine_width (DA.denv dacc_before_switch) in
  let body, uacc =
    if num_arms < 1
    then
      let uacc = UA.notify_removed ~operation:Removed_operations.branch uacc in
      RE.create_invalid Zero_switch_arms, uacc
    else
      let dbg = Debuginfo.none in
      let[@inline] normal_case uacc =
        (* In that case, even though some branches were removed by simplify we
           should not count them in the number of removed operations: these
           branches wouldn't have been taken during execution anyway. *)
        let expr, uacc =
          EB.create_switch uacc ~condition_dbg ~scrutinee ~arms
        in
        if
          Flambda_features.check_invariants ()
          && Simple.is_const scrutinee && num_arms > 1
        then
          Misc.fatal_errorf
            "[Switch] with constant scrutinee (type: %a) should have been \
             simplified away:@ %a"
            T.print scrutinee_ty
            (RE.print (UA.are_rebuilding_terms uacc))
            expr;
        expr, uacc
      in
      match switch_merged with
      | Some (dest, args) ->
        let uacc =
          UA.notify_removed ~operation:Removed_operations.branch uacc
        in
        (* CR bclement: should use a single unboxed product lookup table *)
        let rec rebuild_merged_switch mergeable_args args_rev =
          match mergeable_args with
          | [] ->
            let args = List.rev args_rev in
            (* CR mshinwell: we could probably expose the actual integer counts
               of continuations in [Name_occurrences] and then try to inline out
               [dest]. This might happen anyway in the backend though so this
               probably isn't that important for now. *)
            let apply_cont = Apply_cont.create dest ~args ~dbg in
            return
              (RE.create_apply_cont apply_cont)
              ~added_code_size:(Code_size.apply_cont apply_cont)
              ~free_names:(Apply_cont.free_names apply_cont)
          | special_arg :: special_args ->
            rebuild_mergeable_argument ~machine_width ~scrutinee special_arg
              (fun arg -> rebuild_merged_switch special_args (arg :: args_rev))
        in
        run uacc ~dacc_before_switch (rebuild_merged_switch args [])
      | None -> normal_case uacc
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
      S.simplify_simples (DA.with_denv dacc denv_at_use) args
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
  | Not_lifting _ -> `Not_lifting
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
    then `Single_use
    else if is_exn_handler
    then `Exn_handler
    else if DE.at_unit_toplevel (DA.denv dacc)
    then `Toplevel
    else
      let denv = DA.denv dacc in
      match DE.specialization_cost denv with
      | Cannot_specialize { reason } ->
        (* CR gbury: we could try and emit something analog to the inlining
           report, but for other optimizations at one point ? *)
        begin match reason with
        | Specialization_disabled -> `Disabled
        | At_toplevel -> `Toplevel
        | Contains_static_consts | Contains_set_of_closures ->
          `Cannot_specialize
        end
      | Can_specialize spec_cost -> (
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
        then `Insufficient_lifting_budget
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
                   scrutinee, or to simplify the switch, it will happen without
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
            res
          | `Spec (join_analysis, specialized, generic) ->
            (* Specialization benefit estimation: we use heuristics similar to
               that of inlining to estimate the benefit based on code size and
               removed operations (note that we use the join info in the typing
               env to estimate which operations will be removed during
               specialization, rather that computing it speculatively like is
               done for inlining). *)
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
            if
              Float.compare threshold 0. < 0
              || Float.compare final_cost threshold > 0
            then `Too_costly
            else `Specialized (continuation, lifting_cost)))

let decide_continuation_specialization ~dacc ~switch ~scrutinee =
  Profile.record_with_counters ~accumulate:true "continuation_specialization"
    (fun () -> decide_continuation_specialization0 ~dacc ~switch ~scrutinee)
    ()
    ~counter_f:(fun result ->
      let counters = Profile.Counters.create () in
      match result with
      | `Disabled -> counters
      | `Single_use -> counters
      | `Exn_handler -> counters
      | `Toplevel -> counters
      | `All_unknown -> Profile.Counters.incr "all_unknown" counters
      | `No_reason_to_spec -> Profile.Counters.incr "no_reason" counters
      | `Not_lifting -> Profile.Counters.incr "not_lifting" counters
      | `Cannot_specialize -> Profile.Counters.incr "cannot_spec" counters
      | `Insufficient_lifting_budget ->
        Profile.Counters.incr "no_lifting_budget" counters
      | `Not_enough_join_info -> Profile.Counters.incr "no_join_info" counters
      | `Too_many_unknown_uses ->
        Profile.Counters.incr "too_much_unknown" counters
      | `Too_costly -> Profile.Counters.incr "not_beneficial" counters
      | `Specialized _ -> Profile.Counters.incr "specialized" counters)

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
    match decide_continuation_specialization ~dacc ~switch ~scrutinee with
    | `Specialized (continuation, lifting_cost) ->
      let dacc = DA.decrease_continuation_lifting_budget dacc lifting_cost in
      let dacc =
        DA.with_are_lifting_conts dacc
          (Are_lifting_conts.lift_continuations_out_of continuation)
      in
      let dacc = DA.add_continuation_to_specialize dacc continuation in
      dacc
    | _ -> dacc
  in
  down_to_up dacc
    ~rebuild:
      (rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
         ~dacc_before_switch)
