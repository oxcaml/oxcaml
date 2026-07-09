(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module PTA = Points_to_analysis
module UA = Unboxing_analysis
module Unboxed_fields = UA.Unboxed_fields

type 'a block_or_closure_fields =
  | Empty
  | Block_fields of
      { is_int : 'a option;
        get_tag : 'a option;
        fields : (Flambda_kind.t * 'a) option list
      }
  | Closure_fields of 'a Value_slot.Map.t * 'a Function_slot.Map.t
  | Boxed_number_field of Flambda_kind.Boxable_number.t * 'a
  | Fields_from_distinct_subkinds

let classify_field_map fields =
  let r =
    Field.Map.fold
      (fun field x acc ->
        match acc with
        | `Fields_from_distinct_subkinds -> `Fields_from_distinct_subkinds
        | `Boxed_number_field _ ->
          (* A boxed number has a single field, so it cannot be combined with
             any other field. *)
          `Fields_from_distinct_subkinds
        | (`Block_fields _ | `Closure_fields _ | `Empty) as acc -> (
          let[@inline] block_fields k =
            let[@local] k is_int get_tag fields =
              (k [@inlined hint]) ~is_int ~get_tag ~fields
            in
            match acc with
            | `Closure_fields _ -> `Fields_from_distinct_subkinds
            | `Block_fields (is_int, get_tag, fields) -> k is_int get_tag fields
            | `Empty -> k None None Numeric_types.Int.Map.empty
          in
          let[@inline] closure_fields k =
            let[@local] k value_slots function_slots =
              (k [@inlined hint]) ~value_slots ~function_slots
            in
            match acc with
            | `Closure_fields (value_slots, function_slots) ->
              k value_slots function_slots
            | `Block_fields _ -> `Fields_from_distinct_subkinds
            | `Empty -> k Value_slot.Map.empty Function_slot.Map.empty
          in
          match Field.view field with
          | Call_witness _ | Return_of_call _ | Code_id_of_call_witness ->
            `Fields_from_distinct_subkinds
          | Boxed_number bn -> (
            match acc with
            | `Empty -> `Boxed_number_field (bn, x)
            | `Block_fields _ | `Closure_fields _ ->
              `Fields_from_distinct_subkinds)
          | Value_slot vs ->
            closure_fields (fun ~value_slots ~function_slots ->
                `Closure_fields
                  (Value_slot.Map.add vs x value_slots, function_slots))
          | Function_slot fs ->
            closure_fields (fun ~value_slots ~function_slots ->
                `Closure_fields
                  (value_slots, Function_slot.Map.add fs x function_slots))
          | Is_int ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                assert (Option.is_none is_int);
                `Block_fields (Some x, get_tag, fields))
          | Get_tag ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                assert (Option.is_none get_tag);
                `Block_fields (is_int, Some x, fields))
          | Block (i, kind) ->
            block_fields (fun ~is_int ~get_tag ~fields ->
                if Numeric_types.Int.Map.mem i fields
                then `Fields_from_distinct_subkinds
                else
                  `Block_fields
                    ( is_int,
                      get_tag,
                      Numeric_types.Int.Map.add i (kind, x) fields ))))
      fields `Empty
  in
  match r with
  | `Empty -> Empty
  | `Fields_from_distinct_subkinds -> Fields_from_distinct_subkinds
  | `Boxed_number_field (bn, x) -> Boxed_number_field (bn, x)
  | `Closure_fields (vs, fs) -> Closure_fields (vs, fs)
  | `Block_fields (is_int, get_tag, fields) ->
    let n =
      if Numeric_types.Int.Map.is_empty fields
      then 0
      else fst (Numeric_types.Int.Map.max_binding fields) + 1
    in
    let fields =
      List.init n (fun i -> Numeric_types.Int.Map.find_opt i fields)
    in
    Block_fields { is_int; get_tag; fields }

type rewrite_context =
  { db : Datalog.database;
    unboxing : UA.result;
    sets_of_closures_by_function_slot :
      (Code_id_or_name.t * Code_id.t Or_unknown.t) Function_slot.Map.t list
      Function_slot.Map.t
        (* All the sets of closures defined in the current compilation unit that
           have a given function slot *)
  }

let prepare_rewrite_context result all_sets_of_closures =
  let sets_of_closures_by_function_slot =
    List.fold_left
      (fun acc set_of_closures ->
        let set_of_closures =
          Function_slot.Map.of_list
            (List.map
               (fun (function_slot, (name, code_id)) ->
                 function_slot, (Code_id_or_name.name name, code_id))
               (Function_slot.Lmap.bindings set_of_closures))
        in
        Function_slot.Map.update_many
          (fun _ l _ ->
            match l with
            | None -> Some [set_of_closures]
            | Some l -> Some (set_of_closures :: l))
          acc set_of_closures)
      Function_slot.Map.empty all_sets_of_closures
  in
  { db = result.UA.db; unboxing = result; sets_of_closures_by_function_slot }

(* Note that this depends crucially on the fact that the poison value is not
   nullable. If it was, we could instead keep the subkind but erase the
   nullability part instead. *)
let[@inline] erase kind =
  Flambda_kind.With_subkind.create
    (Flambda_kind.With_subkind.kind kind)
    Flambda_kind.With_subkind.Non_null_value_subkind.Anything
    (Flambda_kind.With_subkind.nullable kind)

let rewrite_boxed_number_kind context usages kind bn =
  (* The contents of boxed int32/int64/nativeint values are tracked via
     [Boxed_number] fields. If the contents are read, the value must really be a
     boxed number (in particular, it cannot have been replaced by a poison
     value), so the subkind can be kept.

     Note that the [Bottom] case below is reachable even though this function is
     only called for values with usages: the value's usages may all read a
     different field, for example when the value flows into a parameter that is
     also fed by values of a different shape and only the fields of those other
     values are read. Such a read is invalid when it is reached with a value of
     this kind, but not immediately invalid (it may be behind a branch), so the
     value here can still have been replaced by a poison value and the subkind
     must be erased. *)
  match PTA.get_one_field_usage context.db (Field.boxed_number bn) usages with
  | Bottom -> erase kind
  | Unknown | Ok _ -> kind

let rec rewrite_kind_with_subkind_not_top_not_bottom context usages kind =
  (* CR ncourant: rewrite changed representation, or at least replace with Top.
     Not needed while we don't change representation of blocks. *)
  match Flambda_kind.With_subkind.non_null_value_subkind kind with
  | Anything -> kind
  | Tagged_immediate ->
    kind (* Always correct, since poison is a tagged immediate *)
  | Boxed_int32 -> rewrite_boxed_number_kind context usages kind Naked_int32
  | Boxed_int64 -> rewrite_boxed_number_kind context usages kind Naked_int64
  | Boxed_nativeint ->
    rewrite_boxed_number_kind context usages kind Naked_nativeint
  | Boxed_float32 | Boxed_float | Boxed_vec128 | Boxed_vec256 | Boxed_vec512
  | Float_block _ | Float_array | Immediate_array | Value_array | Generic_array
  | Unboxed_float32_array | Untagged_int_array | Untagged_int8_array
  | Untagged_int16_array | Unboxed_int32_array | Unboxed_int64_array
  | Unboxed_nativeint_array | Unboxed_vec128_array | Unboxed_vec256_array
  | Unboxed_vec512_array | Unboxed_product_array ->
    (* For all these subkinds, we don't track fields (for now). Thus, being in
       this case without being top or bottom means that we never use this
       particular value, but that it syntactically looks like it could be used.
       We could keep the subkind info, but as this value should not be used, it
       is best to delete it. *)
    erase kind
  | Variant { consts; non_consts } ->
    let fields = PTA.get_fields context.db usages in
    let non_consts =
      Tag.Scannable.Map.map
        (fun (shape, kinds) ->
          let kinds =
            List.mapi
              (fun i kind ->
                let field =
                  Field.block i (Flambda_kind.With_subkind.kind kind)
                in
                match Field.Map.find_opt field fields with
                | None -> (* maybe poison *) erase kind
                | Some Unknown -> (* top *) kind
                | Some (Known flow_to) ->
                  let usages = PTA.get_direct_usages context.db flow_to in
                  rewrite_kind_with_subkind_not_top_not_bottom context usages
                    kind)
              kinds
          in
          shape, kinds)
        non_consts
    in
    Flambda_kind.With_subkind.create Flambda_kind.value
      (Flambda_kind.With_subkind.Non_null_value_subkind.Variant
         { consts; non_consts })
      (Flambda_kind.With_subkind.nullable kind)

let rewrite_kind_with_subkind context var kind =
  let var = Code_id_or_name.name var in
  match PTA.get_usages context.db var with
  | Bottom -> erase kind
  | Unknown -> kind
  | Ok usages ->
    (* We don't need to add usages through function slots, since functions never
       appear in value_kinds. *)
    rewrite_kind_with_subkind_not_top_not_bottom context usages kind

let forget_all_types = lazy (Flambda_features.debug_reaper "forget-types")

let debug_types = lazy (Flambda_features.debug_reaper "types")

module Rewriter = struct
  type t0 =
    | No_source
    | Single_source of Code_id_or_name.t
    | Many_sources_any_usage
    | Many_sources_usages of PTA.usages
    | At_least_one_source_no_usages
  (* When we rewrite a type corresponding to a given value, we will have the
     following invariants:

     - the sources corresponding to the metadata (empty for [No_source], a
     singleton for [Single_source], everything for the other cases) form a
     superset of the possible sources of that value (so if the value can come
     from outside the current compilation unit, [No_source] and [Single_source]
     are impossible).

     - the usages, if provided, correspond to a **subset** of the possible
     usages computed by the reaper of any possible source for that value. *)

  type t = rewrite_context * t0

  let compare_t0 x y =
    match x, y with
    | No_source, No_source -> 0
    | Single_source source1, Single_source source2 ->
      Code_id_or_name.compare source1 source2
    | Many_sources_any_usage, Many_sources_any_usage -> 0
    | Many_sources_usages (Usages usages1), Many_sources_usages (Usages usages2)
      ->
      Code_id_or_name.Map.compare Unit.compare usages1 usages2
    | At_least_one_source_no_usages, At_least_one_source_no_usages -> 0
    | ( No_source,
        ( Single_source _ | Many_sources_any_usage | Many_sources_usages _
        | At_least_one_source_no_usages ) ) ->
      -1
    | ( ( Single_source _ | Many_sources_any_usage | Many_sources_usages _
        | At_least_one_source_no_usages ),
        No_source ) ->
      1
    | ( Single_source _,
        ( Many_sources_any_usage | Many_sources_usages _
        | At_least_one_source_no_usages ) ) ->
      -1
    | ( ( Many_sources_any_usage | Many_sources_usages _
        | At_least_one_source_no_usages ),
        Single_source _ ) ->
      1
    | ( Many_sources_any_usage,
        (Many_sources_usages _ | At_least_one_source_no_usages) ) ->
      -1
    | ( (Many_sources_usages _ | At_least_one_source_no_usages),
        Many_sources_any_usage ) ->
      1
    | Many_sources_usages _, At_least_one_source_no_usages -> -1
    | At_least_one_source_no_usages, Many_sources_usages _ -> 1

  let compare (_context1, t1) (_context2, t2) = compare_t0 t1 t2

  let print_t0 ff t =
    match t with
    | No_source -> Format.fprintf ff "No_source"
    | Single_source source ->
      Format.fprintf ff "(Single_source %a)" Code_id_or_name.print source
    | Many_sources_any_usage -> Format.fprintf ff "Many_sources_any_usage"
    | Many_sources_usages (Usages usages) ->
      Format.fprintf ff "(Many_sources_usages %a)" Code_id_or_name.Set.print
        (Code_id_or_name.Map.keys usages)
    | At_least_one_source_no_usages ->
      Format.fprintf ff "At_least_one_source_no_usages"

  let print ppf (_, t) = print_t0 ppf t

  module T = Container_types.Make (struct
    type nonrec t = t

    let compare = compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash _t = failwith "hash"

    let print ff (_context, t) = print_t0 ff t
  end)

  module Map = T.Map

  let in_coercion (context, _) = context, Many_sources_any_usage

  type use_of_function_slot =
    | Never_called
    | Only_called_with_known_arity
    | Any_call

  let rec patterns_for_unboxed_fields ~machine_width
      ~patterns_for_function_slots db ~var fields unboxed_fields unboxed_block =
    let open Flambda2_types.Rewriter in
    let combined =
      Field.Map.merge
        (fun field field_use unboxed_field ->
          match field_use, unboxed_field with
          | None, None ->
            Misc.fatal_errorf
              "Field %a appeared in neither [fields] nor [unboxed_fields] in \
               [patterns_for_unboxed_fields]"
              Field.print field
          | Some _, None ->
            (match Field.view field with
            | Call_witness _ | Code_id_of_call_witness | Return_of_call _ ->
              (* Virtual fields *) ()
            | Function_slot _ -> (* Shortcut *) ()
            | Block _ | Value_slot _ | Is_int | Get_tag | Boxed_number _ -> (
              let field_source =
                PTA.get_single_field_source db unboxed_block field
              in
              match field_source with
              | No_source -> ()
              | One _ | Many ->
                Misc.fatal_errorf
                  "Field %a appeared in [fields] but not [unboxed_fields] in \
                   [patterns_for_unboxed_fields]"
                  Field.print field));
            None
          | None, Some _ ->
            (* This should not happen if we only start
               [patterns_for_unboxed_fields] on the same name we started
               [mk_unboxed_fields]. *)
            Misc.fatal_errorf
              "In [patterns_for_unboxed_fields], field %a existed in \
               [unboxed_fields] but not in [fields]"
              Field.print field
          | Some field_use, Some unboxed_fields ->
            Some (field_use, unboxed_fields))
        fields unboxed_fields
    in
    let forget unboxed_fields =
      Unboxed_fields.map_u (fun x -> None, x) unboxed_fields
    in
    let for_one_use field (field_use, unboxed_fields) =
      let field_source = PTA.get_single_field_source db unboxed_block field in
      match (unboxed_fields : _ Unboxed_fields.u) with
      | Not_unboxed x ->
        let v = Var.create () in
        ( Unboxed_fields.Not_unboxed (Some v, x),
          Pattern.var v (var x field_source field_use) )
      | Unboxed unboxed_fields -> (
        match (field_use : _ Or_unknown.t) with
        | Unknown ->
          Misc.fatal_errorf
            "In [patterns_for_unboxed_fields], field was unboxed but has \
             [Used_as_top] usage"
        | Known flow_to -> (
          match field_source with
          | No_source ->
            Misc.fatal_errorf
              "In [patterns_for_unboxed_fields], field was unboxed but has no \
               source"
          | Many ->
            Misc.fatal_errorf
              "In [patterns_for_unboxed_fields], field was unboxed but has \
               many sources"
          | One field_source ->
            let usages = PTA.get_direct_usages db flow_to in
            let fields =
              PTA.get_fields db
                (PTA.add_usages_through_function_slots
                   ~follow_known_arity_calls:true db usages)
            in
            let vars, patterns =
              patterns_for_unboxed_fields ~machine_width
                ~patterns_for_function_slots:None db ~var fields unboxed_fields
                field_source
            in
            Unboxed_fields.Unboxed vars, patterns))
    in
    let[@local] closure value_slots =
      let value_slots = Value_slot.Map.bindings value_slots in
      let vars, pats =
        List.fold_left_map
          (fun acc (value_slot, use) ->
            let field = Field.value_slot value_slot in
            let vars, pat = for_one_use field use in
            Field.Map.add field vars acc, Pattern.value_slot value_slot pat)
          Field.Map.empty value_slots
      in
      let pats =
        match patterns_for_function_slots with
        | None -> pats
        | Some p -> p @ pats
      in
      vars, Pattern.closure pats
    in
    match classify_field_map combined with
    | Empty when Option.is_some patterns_for_function_slots ->
      closure Value_slot.Map.empty
    | Empty | Fields_from_distinct_subkinds ->
      ( Field.Map.map (fun (_, unboxed_fields) -> forget unboxed_fields) combined,
        Pattern.any )
    | Boxed_number_field (bn, use) ->
      if Option.is_some patterns_for_function_slots
      then
        Misc.fatal_errorf
          "[patterns_for_unboxed_fields] sees a boxed number but needs to bind \
           function slots";
      let field = Field.boxed_number bn in
      let vars, pat = for_one_use field use in
      Field.Map.singleton field vars, Pattern.boxed_number bn pat
    | Block_fields { is_int; get_tag; fields } ->
      if Option.is_some patterns_for_function_slots
      then
        Misc.fatal_errorf
          "[patterns_for_unboxed_fields] sees a block but needs to bind \
           function slots";
      let acc = Field.Map.empty in
      let pats = [] in
      let acc, pats =
        match is_int with
        | None -> acc, pats
        | Some use ->
          let vars, pat = for_one_use Field.is_int use in
          Field.Map.add Field.is_int vars acc, Pattern.is_int pat :: pats
      in
      let acc, pats =
        match get_tag with
        | None -> acc, pats
        | Some use ->
          let vars, pat = for_one_use Field.get_tag use in
          Field.Map.add Field.get_tag vars acc, Pattern.get_tag pat :: pats
      in
      let acc = ref acc in
      let pats = ref pats in
      List.iteri
        (fun i use ->
          match use with
          | None -> ()
          | Some (kind, use) ->
            let field = Field.block i kind in
            let vars, pat = for_one_use field use in
            acc := Field.Map.add field vars !acc;
            pats
              := Pattern.block_field
                   (Target_ocaml_int.of_int machine_width i)
                   kind pat
                 :: !pats)
        fields;
      !acc, Pattern.block !pats
    | Closure_fields (value_slots, function_slots) ->
      assert (Function_slot.Map.is_empty function_slots);
      closure value_slots

  let follow_field context t field =
    let[@local] for_usages usages =
      match PTA.get_one_field_usage context.db field usages with
      | Unknown -> Many_sources_any_usage
      | Bottom -> At_least_one_source_no_usages
      | Ok vars -> Many_sources_usages (PTA.get_direct_usages context.db vars)
    in
    match t with
    | No_source ->
      Misc.fatal_errorf "Unexpected [No_source] in [follow_field] for field %a"
        Field.print field
    | At_least_one_source_no_usages ->
      Misc.fatal_errorf
        "Unexpected [At_least_one_source_no_usages] in [follow_field] for \
         field %a"
        Field.print field
    | Many_sources_any_usage -> Many_sources_any_usage
    | Many_sources_usages usages -> for_usages usages
    | Single_source source -> (
      if not (PTA.field_used context.db source field)
      then
        At_least_one_source_no_usages
        (* The field has been deleted when building the value *)
      else
        match PTA.get_single_field_source context.db source field with
        | No_source -> No_source
        | One field_source -> Single_source field_source
        | Many -> (
          match PTA.get_usages context.db source with
          | Unknown -> Many_sources_any_usage
          | Bottom -> At_least_one_source_no_usages
          | Ok usages -> for_usages usages))

  let join_t0 db x y =
    let usages_of_t0 x =
      match x with
      | No_source | At_least_one_source_no_usages -> assert false
      | Single_source source -> (
        match PTA.get_usages db source with
        | Unknown -> None
        | Ok usages -> Some usages
        | Bottom -> Some (PTA.Usages Code_id_or_name.Map.empty))
      | Many_sources_any_usage -> None
      | Many_sources_usages usages -> Some usages
    in
    match x, y with
    | No_source, t0 | t0, No_source -> t0
    | At_least_one_source_no_usages, _ | _, At_least_one_source_no_usages ->
      At_least_one_source_no_usages
    | Single_source source1, Single_source source2
      when Code_id_or_name.equal source1 source2 ->
      Single_source source1
    | ( (Single_source _ | Many_sources_any_usage | Many_sources_usages _),
        (Single_source _ | Many_sources_any_usage | Many_sources_usages _) )
      -> (
      match usages_of_t0 x, usages_of_t0 y with
      | None, None -> Many_sources_any_usage
      | Some u, None | None, Some u -> Many_sources_usages u
      | Some (PTA.Usages u1), Some (PTA.Usages u2) ->
        Many_sources_usages
          (PTA.Usages (Code_id_or_name.Map.inter (fun _ () () -> ()) u1 u2)))

  let follow_field_for_set_of_closures context set_of_closures value_slot =
    let field = Field.value_slot value_slot in
    if
      Function_slot.Map.for_all
        (fun _ closure -> not (PTA.field_used context.db closure field))
        set_of_closures
    then At_least_one_source_no_usages
    else
      let sources =
        Function_slot.Map.map
          (fun closure -> PTA.get_single_field_source context.db closure field)
          set_of_closures
      in
      let _, source = Function_slot.Map.choose sources in
      let () =
        assert (
          Function_slot.Map.for_all
            (fun _ (source' : PTA.single_field_source) ->
              match source, source' with
              | No_source, No_source | Many, Many -> true
              | One source, One source' -> Code_id_or_name.equal source source'
              | (No_source | One _ | Many), _ -> false)
            sources)
      in
      match source with
      | No_source -> No_source
      | One source -> Single_source source
      | Many -> (
        let c =
          Function_slot.Map.fold
            (fun _ c acc -> Code_id_or_name.Map.add c () acc)
            set_of_closures Code_id_or_name.Map.empty
        in
        match PTA.get_one_field_usage_of_constructors context.db c field with
        | Unknown -> Many_sources_any_usage
        | Bottom -> At_least_one_source_no_usages
        | Ok vars -> Many_sources_usages (PTA.get_direct_usages context.db vars)
        )

  (* XXX use leapfrog *)
  let inter_list l =
    assert (not (List.is_empty l));
    List.fold_left
      (fun u v -> Code_id_or_name.Map.inter (fun _ () () -> ()) u v)
      (List.hd l) (List.tl l)

  let inter_many m = inter_list (List.map snd (Code_id_or_name.Map.bindings m))

  let usages_of_function_slots_for_set_of_closures db set_of_closures =
    Function_slot.Map.map
      (fun (closure, _code_id) ->
        let constructors = Code_id_or_name.Map.singleton closure () in
        match
          PTA.get_one_field_usage_of_constructors db constructors
            Field.unknown_arity_call_witness
        with
        | Unknown | Ok _ -> Any_call
        | Bottom -> (
          match
            PTA.get_one_field_usage_of_constructors db constructors
              Field.known_arity_call_witness
          with
          | Unknown | Ok _ -> Only_called_with_known_arity
          | Bottom -> Never_called))
      set_of_closures

  type must_be_local =
    | Must_be_local
    | Can_be_external

  let refine_sets_of_closures context t0 current_function_slot
      code_ids_of_function_slots ~must_be_local =
    let can_be_source_for_this_type set_of_closures =
      Function_slot.Map.for_all
        (fun function_slot code_id_in_types ->
          match Function_slot.Map.find_opt function_slot set_of_closures with
          | None -> false
          | Some (_, code_id_in_closure) -> (
            match code_id_in_types, code_id_in_closure with
            | Or_unknown.Unknown, _ | _, Or_unknown.Unknown -> true
            | ( Or_unknown.Known _code_id_in_types,
                Or_unknown.Known _code_id_in_closure ) ->
              (* CR ncourant: use check if code_id_in_closure is a newer version
                 of code_id_in_types here!! *)
              true))
        code_ids_of_function_slots
    in
    let candidates =
      List.filter can_be_source_for_this_type
        (match
           Function_slot.Map.find_opt current_function_slot
             context.sets_of_closures_by_function_slot
         with
        | None -> []
        | Some l -> l)
    in
    let get_source_of_current_function_slot set_of_closures =
      match
        Function_slot.Map.find_opt current_function_slot set_of_closures
      with
      | None ->
        Misc.fatal_errorf
          "In [refine_sets_of_closures], missing function slot %a in set of \
           closures"
          Function_slot.print current_function_slot
      | Some (source, _) -> source
    in
    let[@local] for_known_sources sources =
      let must_be_erased set_of_closures =
        let candidate_source =
          get_source_of_current_function_slot set_of_closures
        in
        Code_id_or_name.Map.mem candidate_source sources
      in
      List.filter must_be_erased candidates, Must_be_local
    in
    let[@local] for_any_source_with_usages usages_or_top =
      let must_be_erased set_of_closures =
        let candidate_source =
          get_source_of_current_function_slot set_of_closures
        in
        (* Check that the uses of [candidate_source] are larger than the uses we
           are considering. If not, this cannot be a possible source. *)
        PTA.any_usage context.db candidate_source
        ||
        match usages_or_top with
        | None -> false
        | Some (PTA.Usages usages) ->
          let (PTA.Usages usages_of_name) =
            PTA.get_direct_usages context.db
              (Code_id_or_name.Map.singleton candidate_source ())
          in
          Code_id_or_name.Map.subset_domain usages usages_of_name
      in
      (* find all allowed sets of closures with code_ids that are
         newer_version_of and for which check_usages is ok *)
      List.filter must_be_erased candidates, must_be_local
    in
    match t0 with
    | No_source ->
      Misc.fatal_errorf
        "Unexpected [No_source] usages in [refine_sets_of_closures] for \
         function slot %a"
        Function_slot.print current_function_slot
    | At_least_one_source_no_usages ->
      Misc.fatal_errorf
        "Unexpected [At_least_one_source_no_usages] usages in \
         [uses_for_set_of_closures] for function slot %a"
        Function_slot.print current_function_slot
    | Many_sources_any_usage -> for_any_source_with_usages None
    | Many_sources_usages (Usages s) ->
      (* Try to recover sources from usages *)
      let usages_sources =
        Code_id_or_name.Map.filter_map
          (fun usage () ->
            match
              PTA.get_direct_sources context.db
                (Code_id_or_name.Map.singleton usage ())
            with
            | Any_source -> None
            | Sources sources -> Some sources)
          s
      in
      if Code_id_or_name.Map.is_empty usages_sources
      then
        (* Every usage is [any_source] *)
        for_any_source_with_usages (Some (Usages s))
      else
        let possible_sources = inter_many usages_sources in
        for_known_sources possible_sources
    | Single_source source ->
      for_known_sources (Code_id_or_name.Map.singleton source ())

  let get_set_of_closures_changed_representation context set_of_closures =
    if
      Function_slot.Map.exists
        (fun _ (clos, _code_id) ->
          Code_id_or_name.Map.mem clos context.unboxing.changed_representation)
        set_of_closures
    then (
      assert (
        Function_slot.Map.for_all
          (fun _ (clos, _code_id) ->
            Code_id_or_name.Map.mem clos context.unboxing.changed_representation)
          set_of_closures);
      let changed_representation =
        List.map
          (fun (_, (clos, _code_id)) ->
            let repr, clos' =
              Code_id_or_name.Map.find clos
                context.unboxing.changed_representation
            in
            assert (Code_id_or_name.equal clos clos');
            repr)
          (Function_slot.Map.bindings set_of_closures)
      in
      let value_slots_reprs, function_slots_reprs =
        match List.hd changed_representation with
        | Closure_representation (value_slots_reprs, function_slots_reprs, _) ->
          value_slots_reprs, function_slots_reprs
        | Block_representation _ ->
          Misc.fatal_errorf
            "Changed representation of a closure with [Block_representation]"
      in
      List.iter
        (function
          | UA.Closure_representation (vs, fs, _) ->
            if vs != value_slots_reprs || fs != function_slots_reprs
            then
              Misc.fatal_errorf
                "In set of closures, all closures do not have the same \
                 representation changes."
          | UA.Block_representation _ ->
            Misc.fatal_errorf
              "Changed representation of a closure with [Block_representation]")
        changed_representation;
      Some (value_slots_reprs, function_slots_reprs))
    else None

  let rewrite (context, usages) typing_env flambda_type =
    let open Flambda2_types.Rewriter in
    let db = context.db in
    let[@local] forget_type () =
      (* CR ncourant: we should preserve the nullability of the type here. *)
      if
        Lazy.force debug_types
        && not (Flambda2_types.is_unknown_maybe_null typing_env flambda_type)
      then
        Format.eprintf "Forgetting: %a@.Usages = %a@." Flambda2_types.print
          flambda_type print_t0 usages;
      Rule.rewrite Pattern.any (Expr.unknown (Flambda2_types.kind flambda_type))
    in
    match usages with
    | _ when Lazy.force forget_all_types -> forget_type ()
    | At_least_one_source_no_usages -> forget_type ()
    | Many_sources_usages (Usages m) when Code_id_or_name.Map.is_empty m ->
      forget_type ()
    | No_source ->
      (* We should be able to use [Expr.bottom] here (if we have something that
         has no source accessible at toplevel, it means that the initialisation
         code of the module necessarily raises).

         However, we currently can't do this because there can be leftover
         symbols in the typing env that no longer have corresponding lets in the
         output post-simplify and returning bottom here would actually be
         unsound in that case.

         So we simply return [unknown] instead of [bottom] here because it would
         be too easy to incorrectly conclude that the environment is
         unsatisfiable for little actual benefit. *)
      Rule.rewrite Pattern.any (Expr.unknown (Flambda2_types.kind flambda_type))
    | Single_source _ | Many_sources_any_usage | Many_sources_usages (Usages _)
      -> (
      match
        Flambda2_types.meet_single_closures_entry typing_env flambda_type
      with
      | Invalid ->
        (* Not a closure. For now, we can never change the representation of
           this, so no rewrite is necessary. *)
        Rule.identity
      | Need_meet ->
        (* Multiple closures are possible. We are never able to use this
           information currently; convert to Unknown. Note that this case
           includes the case where the type is unknown. *)
        forget_type ()
      | Known_result
          (current_function_slot, alloc_mode, closures_entry, _function_type) ->
        let value_slot_types =
          Flambda2_types.Closures_entry.value_slot_types closures_entry
        in
        let function_slot_types =
          Flambda2_types.Closures_entry.function_slot_types closures_entry
        in
        let code_id_of_function_slots =
          Function_slot.Map.mapi
            (fun function_slot _ ->
              let function_type =
                Closures_entry.find_function_type closures_entry function_slot
              in
              Or_unknown.map function_type ~f:Function_type.code_id)
            function_slot_types
        in
        let is_local_value_slot vs _ =
          Compilation_unit.is_current (Value_slot.get_compilation_unit vs)
        in
        let is_local_function_slot fs _ =
          Compilation_unit.is_current (Function_slot.get_compilation_unit fs)
        in
        let has_local_fields =
          Value_slot.Map.exists is_local_value_slot value_slot_types
          || Function_slot.Map.exists is_local_function_slot function_slot_types
        in
        if has_local_fields
        then
          if
            not
              (Value_slot.Map.for_all is_local_value_slot value_slot_types
              && Function_slot.Map.for_all is_local_function_slot
                   function_slot_types)
          then
            Misc.fatal_errorf
              "Some slots in this closure are local while other are not:@\n\
               Value slots: %a@\n\
               Function slots: %a@."
              Value_slot.Set.print
              (Value_slot.Map.keys value_slot_types)
              Function_slot.Set.print
              (Function_slot.Map.keys function_slot_types);
        let must_be_local =
          if
            has_local_fields
            || Function_slot.Map.exists
                 (fun _ code_id ->
                   match code_id with
                   | Or_unknown.Unknown -> false
                   | Or_unknown.Known code_id ->
                     Compilation_unit.is_current
                       (Code_id.get_compilation_unit code_id))
                 code_id_of_function_slots
          then Must_be_local
          else Can_be_external
        in
        (* refine *)
        let local_sets_of_closures, must_be_local =
          refine_sets_of_closures context usages current_function_slot
            code_id_of_function_slots ~must_be_local
        in
        let all_changed_representation =
          List.map
            (get_set_of_closures_changed_representation context)
            local_sets_of_closures
        in
        if List.for_all Option.is_none all_changed_representation
        then begin
          (* No set of closures changed representation. We can keep the shape of
             the type, but might need to delete some value slots. *)
          let from_local_sets_of_closures =
            List.filter_map
              (fun set_of_closures ->
                let set_of_closures =
                  Function_slot.Map.map (fun (name, _) -> name) set_of_closures
                in
                match
                  Value_slot.Map.filter_map
                    (fun value_slot _ ->
                      match
                        follow_field_for_set_of_closures context set_of_closures
                          value_slot
                      with
                      | No_source -> raise Exit
                      | At_least_one_source_no_usages -> None
                      | ( Single_source _ | Many_sources_any_usage
                        | Many_sources_usages _ ) as t0 ->
                        Some t0)
                    value_slot_types
                with
                | exception Exit -> None
                | value_slot_usages ->
                  let function_slot_usages =
                    Function_slot.Map.mapi
                      (fun function_slot _ ->
                        match
                          Function_slot.Map.find_opt function_slot
                            set_of_closures
                        with
                        | None ->
                          Misc.fatal_errorf
                            "Function slot %a not found in closure produced by \
                             [refine_sets_of_closures]"
                            Function_slot.print function_slot
                        | Some source ->
                          let code_id_must_be_erased =
                            not
                              (PTA.field_used context.db source
                                 Field.known_arity_call_witness
                              || PTA.field_used context.db source
                                   Field.unknown_arity_call_witness)
                          in
                          Single_source source, code_id_must_be_erased)
                      function_slot_types
                  in
                  Some (value_slot_usages, function_slot_usages))
              local_sets_of_closures
          in
          let from_external_sources =
            match must_be_local with
            | Must_be_local -> None
            | Can_be_external -> (
              (* There can be external sources, so no local slots, thus making
                 the calls to [follow_field] correct. *)
              assert (not has_local_fields);
              let[@local] any_usage () =
                let value_slots =
                  Value_slot.Map.map
                    (fun _ -> Many_sources_any_usage)
                    value_slot_types
                in
                let function_slots =
                  Function_slot.Map.map
                    (fun _ -> Many_sources_any_usage, false)
                    function_slot_types
                in
                Some (value_slots, function_slots)
              in
              match usages with
              | No_source | At_least_one_source_no_usages -> assert false
              | Single_source _ ->
                Misc.fatal_errorf
                  "While rewriting set of closures, usages was [Single_source] \
                   but [can_have_external_sources] was true."
              | Many_sources_any_usage -> any_usage ()
              | Many_sources_usages usages -> (
                match
                  PTA
                  .compute_usages_by_function_slots_not_following_known_arity_calls
                    context.db usages current_function_slot
                with
                | Unknown -> any_usage ()
                | Known by_function_slot ->
                  let all_usages =
                    Function_slot.Map.fold
                      (fun _ (PTA.Usages x) (PTA.Usages y) ->
                        PTA.Usages (Code_id_or_name.Map.union_left_biased x y))
                      by_function_slot (PTA.Usages Code_id_or_name.Map.empty)
                  in
                  let value_slots =
                    Value_slot.Map.mapi
                      (fun value_slot _ ->
                        (* Even if there are no usages, the field cannot have
                           been deleted, since it comes from an external
                           compilation unit. *)
                        follow_field context (Many_sources_usages all_usages)
                          (Field.value_slot value_slot))
                      value_slot_types
                  in
                  let function_slots =
                    Function_slot.Map.filter_map
                      (fun function_slot _ ->
                        match
                          Function_slot.Map.find_opt function_slot
                            by_function_slot
                        with
                        | None -> None
                        | Some usages -> Some (Many_sources_usages usages, false))
                      function_slot_types
                  in
                  Some (value_slots, function_slots)))
          in
          if
            List.is_empty from_local_sets_of_closures
            && Option.is_none from_external_sources
          then
            (* We should be able to use [Expr.bottom] here (a set of closures
               that can't be local and also can't be external simply cannot
               exist), but we don't because it would be too easy to be unsound
               if there are symbols in the typing env that no longer have a
               corresponding let symbol in the output of simplify -- see the
               discussion for the [No_source] case in [rewrite]. *)
            Rule.rewrite Pattern.any
              (Expr.unknown (Flambda2_types.kind flambda_type))
          else
            let from_everything =
              match from_external_sources with
              | None -> from_local_sets_of_closures
              | Some for_external_sources ->
                for_external_sources :: from_local_sets_of_closures
            in
            let value_slots, function_slots =
              List.fold_left
                (fun (value_slots1, function_slots1)
                     (value_slots2, function_slots2) ->
                  let value_slots =
                    Value_slot.Map.inter
                      (fun _ t1 t2 -> join_t0 context.db t1 t2)
                      value_slots1 value_slots2
                  in
                  let function_slots =
                    Function_slot.Map.inter
                      (fun _ (t1, code_id_must_be_erased1)
                           (t2, code_id_must_be_erased2) ->
                        ( join_t0 context.db t1 t2,
                          code_id_must_be_erased1 || code_id_must_be_erased2 ))
                      function_slots1 function_slots2
                  in
                  value_slots, function_slots)
                (List.hd from_everything) (List.tl from_everything)
            in
            let all_patterns = ref [] in
            let at_least_these_value_slots =
              Value_slot.Map.filter_map
                (fun value_slot metadata ->
                  let v = Var.create () in
                  all_patterns
                    := Pattern.value_slot value_slot
                         (Pattern.var v (context, metadata))
                       :: !all_patterns;
                  Some (Expr.var v))
                value_slots
            in
            let at_least_these_closure_types =
              Function_slot.Map.mapi
                (fun function_slot (metadata, _) ->
                  let v = Var.create () in
                  all_patterns
                    := Pattern.function_slot function_slot
                         (Pattern.var v (context, metadata))
                       :: !all_patterns;
                  Expr.var v)
                function_slots
            in
            let at_least_these_function_slots =
              Function_slot.Map.mapi
                (fun function_slot (_, code_id_must_be_erased) ->
                  if code_id_must_be_erased
                  then Or_unknown.Unknown
                  else
                    let v = Var.create () in
                    all_patterns
                      := Pattern.rec_info function_slot
                           (Pattern.var v (context, Many_sources_any_usage))
                         :: !all_patterns;
                    let function_type =
                      Flambda2_types.Closures_entry.find_function_type
                        closures_entry function_slot
                    in
                    Or_unknown.map function_type ~f:(fun function_type ->
                        Expr.Function_type.create
                          (Function_type.code_id function_type)
                          ~rec_info:(Expr.var v)))
                function_slots
            in
            let expr =
              Expr.at_least_this_closure current_function_slot
                ~at_least_these_function_slots ~at_least_these_closure_types
                ~at_least_these_value_slots alloc_mode
            in
            Rule.rewrite (Pattern.closure !all_patterns) expr
        end
        else if
          List.compare_length_with local_sets_of_closures 1 > 0
          ||
          match must_be_local with
          | Can_be_external -> true
          | Must_be_local -> false
        then
          (* At least one set of closures changed representation, and there are
             more than one sets of closures. They now have incompatible
             representations, since we don't do multi-source representation
             changes/unboxing for now. *)
          forget_type ()
        else
          let set_of_closures = List.hd local_sets_of_closures in
          let value_slots_reprs, function_slots_reprs =
            Option.get (List.hd all_changed_representation)
          in
          let patterns = ref [] in
          let all_function_slots_in_set =
            Function_slot.Map.fold
              (fun function_slot uses m ->
                let new_function_slot =
                  Function_slot.Map.find function_slot function_slots_reprs
                in
                let r =
                  match uses with
                  | Never_called -> Or_unknown.Unknown
                  | Only_called_with_known_arity | Any_call ->
                    let v = Var.create () in
                    patterns
                      := Pattern.rec_info function_slot
                           (Pattern.var v (context, Many_sources_any_usage))
                         :: !patterns;
                    let function_type =
                      Flambda2_types.Closures_entry.find_function_type
                        closures_entry function_slot
                    in
                    Or_unknown.map function_type ~f:(fun function_type ->
                        Expr.Function_type.create
                          (Function_type.code_id function_type)
                          ~rec_info:(Expr.var v))
                in
                Function_slot.Map.add new_function_slot r m)
              (usages_of_function_slots_for_set_of_closures db set_of_closures)
              Function_slot.Map.empty
          in
          let all_closure_types_in_set =
            Function_slot.Map.fold
              (fun function_slot (closure, _code_id) m ->
                let v = Var.create () in
                patterns
                  := Pattern.function_slot function_slot
                       (Pattern.var v (context, Single_source closure))
                     :: !patterns;
                let new_function_slot =
                  Function_slot.Map.find function_slot function_slots_reprs
                in
                Function_slot.Map.add new_function_slot (Expr.var v) m)
              set_of_closures Function_slot.Map.empty
          in
          let patterns_for_function_slots = Some !patterns in
          let fields =
            PTA.get_fields_usage_of_constructors db
              (Function_slot.Map.fold
                 (fun _ (c, _) acc -> Code_id_or_name.Map.add c () acc)
                 set_of_closures Code_id_or_name.Map.empty)
          in
          let closure_source, _code_id =
            Function_slot.Map.find current_function_slot set_of_closures
          in
          let bound, pat =
            patterns_for_unboxed_fields
              ~machine_width:(Typing_env.machine_width typing_env)
              ~patterns_for_function_slots db
              ~var:(fun _ (field_source : PTA.single_field_source) field_use ->
                let metadata =
                  match field_source, field_use with
                  | No_source, _ -> No_source
                  | One source, _ -> Single_source source
                  | Many, Unknown -> Many_sources_any_usage
                  | Many, Known flow_to ->
                    let usages = PTA.get_direct_usages context.db flow_to in
                    Many_sources_usages usages
                in
                context, metadata)
              fields value_slots_reprs closure_source
          in
          let all_value_slots_in_set =
            Unboxed_fields.fold_with_kind
              (fun _kind (var, value_slot) m ->
                let e =
                  match var with
                  | None -> Expr.unknown (Value_slot.kind value_slot)
                  | Some var -> Expr.var var
                in
                Value_slot.Map.add value_slot e m)
              bound Value_slot.Map.empty
          in
          let new_function_slot =
            Function_slot.Map.find current_function_slot function_slots_reprs
          in
          Rule.rewrite pat
            (Expr.exactly_this_closure new_function_slot
               ~all_function_slots_in_set ~all_closure_types_in_set
               ~all_value_slots_in_set alloc_mode))

  let block_slot ?tag:_ (context, t) index _typing_env flambda_type =
    let r =
      let field_kind = Flambda2_types.kind flambda_type in
      let field = Field.block (Target_ocaml_int.to_int index) field_kind in
      follow_field context t field
    in
    context, r

  let array_slot (context, _t) _index _typing_env _flambda_type =
    (* Array primitives are opaque. Thus, anything put inside the array when it
       was created has been treated as escaping, thus giving a
       [Many_sources_any_usage] context. *)
    context, Many_sources_any_usage

  let set_of_closures _t _function_slot _typing_env _closures_entry =
    Misc.fatal_error
      "[set_of_closures] should never be called, because all sets of closures \
       should be handled by [rewrite]"

  type set_of_closures = |

  (* Because all sets of closures are handled by [rewrite], these cases can
     never happen. *)

  let value_slot (s : set_of_closures) _value_slot _typing_env _flambda_type =
    match s with _ -> .

  let function_slot (s : set_of_closures) _function_slot _typing_env
      _flambda_type =
    match s with _ -> .

  let rec_info _typing_env (s : set_of_closures) _function_slot _code_id
      _flambda_type =
    match s with _ -> .
end

module TypesRewrite = Flambda2_types.Rewriter.Make (Rewriter)

let rewrite_typing_env context ~unit_symbol:_ typing_env =
  if Lazy.force debug_types
  then Format.eprintf "OLD typing env: %a@." Typing_env.print typing_env;
  let db = context.db in
  let symbol_metadata sym =
    if not (Compilation_unit.is_current (Symbol.compilation_unit sym))
    then context, Rewriter.Many_sources_any_usage
    else
      let sym = Code_id_or_name.symbol sym in
      match PTA.get_single_source db sym with
      | Bottom -> context, Rewriter.No_source
      | Ok source ->
        if PTA.has_use db sym
        then context, Rewriter.Single_source source
        else context, Rewriter.At_least_one_source_no_usages
      | Unknown -> (
        match PTA.get_usages db sym with
        | Bottom -> context, Rewriter.At_least_one_source_no_usages
        | Unknown -> context, Rewriter.Many_sources_any_usage
        | Ok usages -> context, Rewriter.Many_sources_usages usages)
  in
  let r =
    Profile.record_call ~accumulate:true "types" (fun () ->
        TypesRewrite.rewrite typing_env symbol_metadata)
  in
  if Lazy.force debug_types
  then Format.eprintf "NEW typing env: %a@." Typing_env.print r;
  r

let rewrite_result_types context ~old_typing_env ~my_closure:func_my_closure
    ~params:func_params ~results:func_results result_types =
  if Lazy.force debug_types
  then Format.eprintf "OLD result types: %a@." Result_types.print result_types;
  let params, results, env_extension =
    Result_types.pattern_match result_types ~f:(fun ~params ~results tee ->
        params, results, tee)
  in
  let variable_pattern var to_keep =
    let kind = Variable.kind var in
    let name = Variable.name var in
    let var = Code_id_or_name.var var in
    let db = context.db in
    if Code_id_or_name.Map.mem var context.unboxing.unboxed_fields
    then
      let unboxed_fields =
        Code_id_or_name.Map.find var context.unboxing.unboxed_fields
      in
      match PTA.get_usages db var with
      | Unknown ->
        Misc.fatal_errorf "In [rewrite_result_types], var %a is unboxed but top"
          Code_id_or_name.print var
      | Bottom ->
        Misc.fatal_errorf
          "In [rewrite_result_types], var %a is unboxed but has no usage"
          Code_id_or_name.print var
      | Ok usages ->
        let allocation_point =
          match PTA.get_single_source db var with
          | Bottom | Unknown ->
            Misc.fatal_errorf
              "In [rewrite_result_types], var %a is unboxed but could not get \
               its allocation point"
              Code_id_or_name.print var
          | Ok allocation_point -> allocation_point
        in
        let fields =
          PTA.get_fields db
            (PTA.add_usages_through_function_slots
               ~follow_known_arity_calls:true db usages)
        in
        let bound, pat =
          Rewriter.patterns_for_unboxed_fields
            ~machine_width:(Typing_env.machine_width old_typing_env)
            ~patterns_for_function_slots:None db
            ~var:(fun v field_source field_use ->
              let metadata =
                match field_source, field_use with
                | No_source, _ -> Rewriter.No_source
                | One source, _ -> Rewriter.Single_source source
                | Many, Unknown -> Rewriter.Many_sources_any_usage
                | Many, Known flow_to ->
                  let usages = PTA.get_direct_usages db flow_to in
                  Rewriter.Many_sources_usages usages
              in
              Variable.name v, (context, metadata))
            fields unboxed_fields allocation_point
        in
        let all_vars =
          Unboxed_fields.fold_with_kind
            (fun _kind (pattern_var, v) acc ->
              match pattern_var with
              | None ->
                Misc.fatal_errorf
                  "In [rewrite_result_types], could not get a pattern variable \
                   for unboxed var %a@."
                  Variable.print v
              | Some pattern_var -> pattern_var :: acc)
            bound []
        in
        (pat, kind), all_vars
    else
      match to_keep with
      | PTA.Delete -> (Flambda2_types.Rewriter.Pattern.any, kind), []
      | PTA.Keep ->
        let metadata =
          match PTA.get_single_source db var with
          | Bottom -> context, Rewriter.No_source
          | Ok source ->
            if PTA.has_use db var
            then context, Rewriter.Single_source source
            else context, Rewriter.At_least_one_source_no_usages
          | Unknown -> (
            match PTA.get_usages db var with
            | Bottom -> context, Rewriter.At_least_one_source_no_usages
            | Unknown -> context, Rewriter.Many_sources_any_usage
            | Ok usages -> context, Rewriter.Many_sources_usages usages)
        in
        let v = Flambda2_types.Rewriter.Var.create () in
        let pat = Flambda2_types.Rewriter.Pattern.var v (name, metadata) in
        (pat, kind), [v]
  in
  let patterns_list func_vars type_vars =
    let patterns, vars =
      List.fold_left2
        (fun (patterns, vars) (funcv, to_keep) typev ->
          let pat, vs = variable_pattern funcv to_keep in
          ( Variable.Map.add (Bound_parameter.var typev) pat patterns,
            List.append vs vars ))
        (Variable.Map.empty, []) func_vars
        (Bound_parameters.to_list type_vars)
    in
    patterns, List.rev vars
  in
  let params_patterns, params_vars = patterns_list func_params params in
  let results_patterns, results_vars = patterns_list func_results results in
  let unbox_my_closure_vars =
    match
      Code_id_or_name.Map.find_opt
        (Code_id_or_name.var func_my_closure)
        context.unboxing.unboxed_fields
    with
    | None -> []
    | Some fields ->
      Unboxed_fields.fold_with_kind
        (fun kind v acc -> (v, kind) :: acc)
        fields []
  in
  let new_vars, new_env_extension =
    TypesRewrite.rewrite_env_extension_with_extra_variables old_typing_env
      (Variable.Map.disjoint_union params_patterns results_patterns)
      env_extension
      (params_vars @ results_vars)
  in
  let make_bp vars =
    List.map
      (fun v ->
        let var = Flambda2_types.Rewriter.Var.Map.find v new_vars in
        Bound_parameter.create var
          (Flambda_kind.With_subkind.anything (Variable.kind var))
          Flambda_debug_uid.none)
      vars
  in
  let new_result_types =
    Result_types.create
      ~params:
        (Bound_parameters.create
           (List.map
              (fun (v, k) ->
                Bound_parameter.create v
                  (Flambda_kind.With_subkind.anything k)
                  Flambda_debug_uid.none)
              unbox_my_closure_vars
           @ make_bp params_vars))
      ~results:(Bound_parameters.create (make_bp results_vars))
      new_env_extension
  in
  if Lazy.force debug_types
  then
    Format.eprintf "NEW result types: %a@." Result_types.print new_result_types;
  new_result_types
