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

(* Disable [not-principal] warning in this file. We often write code that looks
   like [let@ [x; y] = a in b] where the list constructor is an [hlist], and [a]
   is used to determine the type of that [hlist]. Unfortunately, due to how
   [let@] is expansed, this is not principal. *)
[@@@ocaml.warning "-not-principal"]

module PTA = Points_to_analysis
open Global_flow_graph.Relations
open! PTA.Relations
module UA = Unboxing_analysis
module Unboxed_fields = UA.Unboxed_fields
open! Datalog_helpers.Syntax
open Datalog_helpers

type 'a block_or_closure_fields =
  | Empty
  | Block_fields of
      { is_int : 'a option;
        get_tag : 'a option;
        fields : (Flambda_kind.t * 'a) option list
      }
  | Closure_fields of 'a Value_slot.Map.t * 'a Function_slot.Map.t
  | Could_not_classify

let classify_field_map fields =
  let r =
    Field.Map.fold
      (fun field x acc ->
        match acc with
        | `Could_not_classify -> `Could_not_classify
        | (`Block_fields _ | `Closure_fields _ | `Empty) as acc -> (
          let[@inline] block_fields k =
            let[@local] k is_int get_tag fields =
              (k [@inlined hint]) ~is_int ~get_tag ~fields
            in
            match acc with
            | `Closure_fields _ -> `Could_not_classify
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
            | `Block_fields _ -> `Could_not_classify
            | `Empty -> k Value_slot.Map.empty Function_slot.Map.empty
          in
          match Field.view field with
          | Call_witness _ | Return_of_call _ | Code_id_of_call_witness ->
            `Could_not_classify
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
                then `Could_not_classify
                else
                  `Block_fields
                    ( is_int,
                      get_tag,
                      Numeric_types.Int.Map.add i (kind, x) fields ))))
      fields `Empty
  in
  match r with
  | `Empty -> Empty
  | `Could_not_classify -> Could_not_classify
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

(* Note that this depends crucially on the fact that the poison value is not
   nullable. If it was, we could instead keep the subkind but erase the
   nullability part instead. *)
let[@inline] erase kind =
  Flambda_kind.With_subkind.create
    (Flambda_kind.With_subkind.kind kind)
    Flambda_kind.With_subkind.Non_null_value_subkind.Anything
    (Flambda_kind.With_subkind.nullable kind)

let rec rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind =
  (* CR ncourant: rewrite changed representation, or at least replace with Top.
     Not needed while we don't change representation of blocks. *)
  match Flambda_kind.With_subkind.non_null_value_subkind kind with
  | Anything -> kind
  | Tagged_immediate ->
    kind (* Always correct, since poison is a tagged immediate *)
  | Boxed_float32 | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
  | Boxed_vec128 | Boxed_vec256 | Boxed_vec512 | Float_block _ | Float_array
  | Immediate_array | Value_array | Generic_array | Unboxed_float32_array
  | Untagged_int_array | Untagged_int8_array | Untagged_int16_array
  | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array
  | Unboxed_vec128_array | Unboxed_vec256_array | Unboxed_vec512_array
  | Unboxed_product_array ->
    (* For all these subkinds, we don't track fields (for now). Thus, being in
       this case without being top or bottom means that we never use this
       particular value, but that it syntactically looks like it could be used.
       We probably could keep the subkind info, but as this value should not be
       used, it is best to delete it. *)
    erase kind
  | Variant { consts; non_consts } ->
    (* CR ncourant: we should make sure poison is in the consts! *)
    (* We don't need to follow indirect code pointers for usage, since functions
       never appear in value_kinds *)
    let usages =
      PTA.get_all_usages ~follow_known_arity_calls:false db flow_to
    in
    let fields = PTA.get_fields db usages in
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
                | Some Used_as_top -> (* top *) kind
                | Some (Used_as_vars flow_to) ->
                  rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind)
              kinds
          in
          shape, kinds)
        non_consts
    in
    Flambda_kind.With_subkind.create Flambda_kind.value
      (Flambda_kind.With_subkind.Non_null_value_subkind.Variant
         { consts; non_consts })
      (Flambda_kind.With_subkind.nullable kind)

let rewrite_kind_with_subkind uses var kind =
  let db = uses.UA.db in
  let var = Code_id_or_name.name var in
  if PTA.any_source db var
  then kind
  else if not (PTA.has_use db var)
  then erase kind
  else
    rewrite_kind_with_subkind_not_top_not_bottom db
      (Code_id_or_name.Map.singleton var ())
      kind

let forget_all_types = Sys.getenv_opt "FORGETALL" <> None

let debug_types = lazy (Flambda_features.debug_reaper "types")

module Rewriter = struct
  type t0 =
    | No_source
    | Single_source of Code_id_or_name.t
    | Many_sources_any_usage
    | Many_sources_usages of unit Code_id_or_name.Map.t
    | No_usages

  type t = UA.result * t0

  let compare_t0 x y =
    match x, y with
    | No_source, No_source -> 0
    | Single_source source1, Single_source source2 ->
      Code_id_or_name.compare source1 source2
    | Many_sources_any_usage, Many_sources_any_usage -> 0
    | Many_sources_usages usages1, Many_sources_usages usages2 ->
      Code_id_or_name.Map.compare Unit.compare usages1 usages2
    | No_usages, No_usages -> 0
    | ( No_source,
        ( Single_source _ | Many_sources_any_usage | Many_sources_usages _
        | No_usages ) ) ->
      -1
    | ( ( Single_source _ | Many_sources_any_usage | Many_sources_usages _
        | No_usages ),
        No_source ) ->
      1
    | ( Single_source _,
        (Many_sources_any_usage | Many_sources_usages _ | No_usages) ) ->
      -1
    | ( (Many_sources_any_usage | Many_sources_usages _ | No_usages),
        Single_source _ ) ->
      1
    | Many_sources_any_usage, (Many_sources_usages _ | No_usages) -> -1
    | (Many_sources_usages _ | No_usages), Many_sources_any_usage -> 1
    | Many_sources_usages _, No_usages -> -1
    | No_usages, Many_sources_usages _ -> 1

  let compare (_result1, t1) (_result2, t2) = compare_t0 t1 t2

  let print_t0 ff t =
    match t with
    | No_source -> Format.fprintf ff "No_source"
    | Single_source source ->
      Format.fprintf ff "(Single_source %a)" Code_id_or_name.print source
    | Many_sources_any_usage -> Format.fprintf ff "Many_sources_any_usage"
    | Many_sources_usages usages ->
      Format.fprintf ff "(Many_sources_usages %a)" Code_id_or_name.Set.print
        (Code_id_or_name.Map.keys usages)
    | No_usages -> Format.fprintf ff "No_usages"

  let print ppf (_, t) = print_t0 ppf t

  module T = Container_types.Make (struct
    type nonrec t = t

    let compare = compare

    let equal t1 t2 = compare t1 t2 = 0

    let hash _t = failwith "hash"

    let print ff (_result, t) = print_t0 ff t
  end)

  module Map = T.Map

  module CNMSet = Stdlib.Set.Make (struct
    type t = unit Code_id_or_name.Map.t

    let compare = Code_id_or_name.Map.compare Unit.compare
  end)

  let in_coercion (result, _) = result, Many_sources_any_usage

  let identify_set_of_closures_with_one_code_id :
      Datalog.database -> Code_id.t -> unit Code_id_or_name.Map.t list =
    let open! Fixit in
    run
      (let@ in_ =
         paramc "in_"
           Cols.[n]
           (fun code_id ->
             Code_id_or_name.Map.singleton (Code_id_or_name.code_id code_id) ())
       in
       let+ out =
         let@ out = fix1' (empty Cols.[n; n]) in
         [ (let$ [code_id; witness; closure; function_slot; all_closures] =
              ["code_id"; "witness"; "closure"; "function_slot"; "all_closures"]
            in
            [ in_ % [code_id];
              rev_constructor ~from:code_id
                !!Field.code_id_of_call_witness
                ~base:witness;
              rev_constructor ~from:witness
                !!Field.known_arity_call_witness
                ~base:closure;
              rev_constructor ~from:closure function_slot ~base:all_closures;
              when1 Field.is_function_slot function_slot ]
            ==> out % [closure; all_closures]) ]
       in
       List.map snd (Code_id_or_name.Map.bindings out))

  let identify_function_slots_of_closure :
      Datalog.database ->
      Code_id_or_name.t ->
      Code_id_or_name.t Function_slot.Map.t =
    (* CR ncourant: we should use [get_set_of_closures_def] here *)
    let q =
      query
        (let^$ [base], [function_slot; target] =
           ["base"], ["function_slot"; "target"]
         in
         [ constructor ~base function_slot ~from:target;
           when1 Field.is_function_slot function_slot ]
         =>? [function_slot; target])
    in
    fun db closure ->
      Cursor.fold_with_parameters q [closure] db ~init:Function_slot.Map.empty
        ~f:(fun [function_slot; c] acc ->
          let function_slot = Field.must_be_function_slot function_slot in
          assert (not (Function_slot.Map.mem function_slot acc));
          Function_slot.Map.add function_slot c acc)

  let identify_set_of_closures_with_code_ids db code_ids =
    let code_ids =
      List.filter
        (fun code_id ->
          Compilation_unit.is_current (Code_id.get_compilation_unit code_id))
        code_ids
    in
    match code_ids with
    | [] -> None
    | code_id :: code_ids ->
      let r =
        List.fold_left
          (fun r code_id ->
            CNMSet.inter r
              (CNMSet.of_list
                 (identify_set_of_closures_with_one_code_id db code_id)))
          (CNMSet.of_list
             (identify_set_of_closures_with_one_code_id db code_id))
          code_ids
      in
      if CNMSet.cardinal r = 1
      then (
        let set_of_closures = CNMSet.min_elt r in
        let one_closure, () = Code_id_or_name.Map.min_binding set_of_closures in
        let by_function_slot =
          identify_function_slots_of_closure db one_closure
        in
        assert (
          Code_id_or_name.Map.equal Unit.equal set_of_closures
            (Function_slot.Map.fold
               (fun _ c acc -> Code_id_or_name.Map.add c () acc)
               by_function_slot Code_id_or_name.Map.empty));
        Some by_function_slot)
      else None

  type use_of_function_slot =
    | Never_called
    | Only_called_with_known_arity
    | Any_call

  type usages_of_value_slots =
    | Dead_code
    | From_set_of_closures of Code_id_or_name.t Function_slot.Map.t
    | Value_slots_usages of unit Code_id_or_name.Map.t
    | Value_slots_any_usages

  let uses_of_function_slots_for_set_of_closures db set_of_closures =
    Function_slot.Map.map
      (fun closure ->
        ( Single_source closure,
          if PTA.field_used db closure Field.unknown_arity_call_witness
          then Any_call
          else if PTA.field_used db closure Field.known_arity_call_witness
          then Only_called_with_known_arity
          else Never_called ))
      set_of_closures

  let uses_for_set_of_closures :
      Datalog.database ->
      t0 ->
      Function_slot.t ->
      Code_id.t Or_unknown.t Function_slot.Map.t ->
      usages_of_value_slots * (t0 * use_of_function_slot) Function_slot.Map.t =
    let open! Fixit in
    let stmt =
      let@ in_ = param "in_" Cols.[n] in
      let@ in_fs =
        paramc "in_fs"
          Cols.[f]
          (fun fs -> Field.Map.singleton (Field.function_slot fs) ())
      in
      let@ mk_any = param0 (fun mk_any -> mk_any) in
      let@ code_ids_of_function_slots =
        param0 (fun code_ids_of_function_slots ->
            Function_slot.Map.fold
              (fun fs code_id m ->
                Field.Map.add (Field.function_slot fs) code_id m)
              code_ids_of_function_slots Field.Map.empty)
      in
      let@ in_all_fs =
        local0
          Cols.[f]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.map (fun _ -> ()) code_ids_of_function_slots)
      in
      let@ in_code_id =
        local0
          Cols.[f; n]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.filter_map
             (fun _ (code_id : _ Or_unknown.t) ->
               match code_id with
               | Unknown -> None
               | Known code_id ->
                 Some
                   (Code_id_or_name.Map.singleton
                      (Code_id_or_name.code_id code_id)
                      ()))
             code_ids_of_function_slots)
      in
      let@ in_unknown_code_id =
        local0
          Cols.[f]
          (let+ code_ids_of_function_slots = code_ids_of_function_slots in
           Field.Map.filter_map
             (fun _ (code_id : _ Or_unknown.t) ->
               match code_id with Unknown -> Some () | Known _ -> None)
             code_ids_of_function_slots)
      in
      let+ ([out1; out2; known_arity; unknown_arity; any] :
             _ Datalog.Constant.hlist) =
        let@ [out1; out2; known_arity; unknown_arity; any] =
          fix'
            [ empty Cols.[n];
              empty Cols.[f; n];
              empty Cols.[f];
              empty Cols.[f];
              empty One.cols ]
        in
        [ (let$ [x; y] = ["x"; "y"] in
           [in_fs % [x]; in_ % [y]] ==> out2 % [x; y]);
          (let$ [fs; usage; field; field_usage] =
             ["fs"; "usage"; "field"; "field_usage"]
           in
           [ out2 % [fs; usage];
             rev_accessor ~base:usage field ~to_:field_usage;
             has_usage field_usage;
             when1 Field.is_value_slot field ]
           ==> out1 % [usage]);
          (let$ [fs0; usage; fs; to_; fs_usage] =
             ["fs0"; "usage"; "fs"; "to_"; "fs_usage"]
           in
           [ out2 % [fs0; usage];
             rev_accessor ~base:usage fs ~to_;
             in_all_fs % [fs];
             usages to_ fs_usage;
             has_usage fs_usage ]
           ==> out2 % [fs; fs_usage]);
          (let$ [fs; usage; v] = ["fs"; "usage"; "v"] in
           [ out2 % [fs; usage];
             rev_accessor ~base:usage !!Field.known_arity_call_witness ~to_:v;
             has_usage v ]
           ==> known_arity % [fs]);
          (let$ [fs; usage; v] = ["fs"; "usage"; "v"] in
           [ out2 % [fs; usage];
             rev_accessor ~base:usage !!Field.unknown_arity_call_witness ~to_:v;
             has_usage v ]
           ==> unknown_arity % [fs]);
          (let$ [fs; code_id; my_closure; usage] =
             ["fs"; "code_id"; "my_closure"; "usage"]
           in
           [ known_arity % [fs];
             in_code_id % [fs; code_id];
             code_id_my_closure ~code_id ~my_closure;
             usages my_closure usage;
             has_usage usage ]
           ==> out2 % [fs; usage]);
          (let$ [fs; code_id; my_closure; usage] =
             ["fs"; "code_id"; "my_closure"; "usage"]
           in
           [ unknown_arity % [fs];
             in_code_id % [fs; code_id];
             code_id_my_closure ~code_id ~my_closure;
             usages my_closure usage;
             has_usage usage ]
           ==> out2 % [fs; usage]);
          (let$ [fs0; x; fs; to_] = ["fs0"; "x"; "fs"; "to_"] in
           [ out2 % [fs0; x];
             rev_accessor ~base:x fs ~to_;
             any_usage to_;
             in_all_fs % [fs] ]
           ==> One.flag any);
          (let$ [fs0; x] = ["fs0"; "x"] in
           [out2 % [fs0; x]; any_usage x] ==> One.flag any);
          (let$ [fs] = ["fs"] in
           [known_arity % [fs]; in_unknown_code_id % [fs]] ==> One.flag any);
          (let$ [fs] = ["fs"] in
           [unknown_arity % [fs]; in_unknown_code_id % [fs]] ==> One.flag any)
        ]
      and+ mk_any = mk_any in
      if One.to_bool any
      then mk_any ()
      else
        ( Value_slots_usages out1,
          Field.Map.fold
            (fun fs uses m ->
              let calls =
                if Field.Map.mem fs unknown_arity
                then Any_call
                else if Field.Map.mem fs known_arity
                then Only_called_with_known_arity
                else Never_called
              in
              Function_slot.Map.add
                (Field.must_be_function_slot fs)
                (Many_sources_usages uses, calls)
                m)
            out2 Function_slot.Map.empty )
    in
    fun db usages current_function_slot code_ids_of_function_slots ->
      let any () =
        ( Value_slots_any_usages,
          Function_slot.Map.map
            (fun _ -> Many_sources_any_usage, Any_call)
            code_ids_of_function_slots )
      in
      let dead_code () =
        ( Dead_code,
          Function_slot.Map.map
            (fun _ -> No_source, Never_called)
            code_ids_of_function_slots )
      in
      match usages with
      | No_source ->
        Misc.fatal_errorf
          "Unexpected [No_source] usages in [uses_for_set_of_closures] for \
           function slot %a"
          Function_slot.print current_function_slot
      | No_usages ->
        Misc.fatal_errorf
          "Unexpected [No_usages] usages in [uses_for_set_of_closures] for \
           function slot %a"
          Function_slot.print current_function_slot
      | Many_sources_any_usage -> any ()
      | Many_sources_usages s ->
        Fixit.run stmt db s current_function_slot any code_ids_of_function_slots
      | Single_source source -> (
        match
          Function_slot.Map.mapi
            (fun function_slot _ ->
              match
                PTA.get_single_field_source db source
                  (Field.function_slot function_slot)
              with
              | No_source -> raise Exit
              | One source -> source
              | Many ->
                Misc.fatal_errorf
                  "[get_single_field_source] of function slot %a of %a \
                   returned [Many]"
                  Function_slot.print function_slot Code_id_or_name.print source)
            code_ids_of_function_slots
        with
        | exception Exit -> dead_code ()
        | function_slot_sources ->
          ( From_set_of_closures function_slot_sources,
            uses_of_function_slots_for_set_of_closures db function_slot_sources
          ))

  let rec patterns_for_unboxed_fields ~machine_width ~bind_function_slots db
      ~var fields unboxed_fields unboxed_block =
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
            None
            (* This should only happen for fields like [Call_witness _], which
               are ignored when creating unboxed fields. TODO: check this? *)
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
        match (field_use : PTA.field_usage) with
        | Used_as_top ->
          Misc.fatal_errorf
            "In [patterns_for_unboxed_fields], field was unboxed but has \
             [Used_as_top] usage"
        | Used_as_vars flow_to -> (
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
            let fields =
              PTA.get_fields db
                (PTA.get_all_usages ~follow_known_arity_calls:true db flow_to)
            in
            let vars, patterns =
              patterns_for_unboxed_fields ~machine_width
                ~bind_function_slots:None db ~var fields unboxed_fields
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
        match bind_function_slots with None -> pats | Some p -> p @ pats
      in
      vars, Pattern.closure pats
    in
    match classify_field_map combined with
    | Empty when Option.is_some bind_function_slots ->
      closure Value_slot.Map.empty
    | Empty | Could_not_classify ->
      ( Field.Map.map (fun (_, unboxed_fields) -> forget unboxed_fields) combined,
        Pattern.any )
    | Block_fields { is_int; get_tag; fields } ->
      if Option.is_some bind_function_slots
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

  let follow_field result t field =
    let[@local] for_usages usages =
      match PTA.get_one_field result.UA.db field (Usages usages) with
      | Used_as_top -> Many_sources_any_usage
      | Used_as_vars vs ->
        let usages = PTA.get_direct_usages result.UA.db vs in
        if Code_id_or_name.Map.is_empty usages
        then No_usages
        else Many_sources_usages usages
    in
    match t with
    | No_source ->
      Misc.fatal_errorf "Unexpected [No_source] in [follow_field] for field %a"
        Field.print field
    | No_usages ->
      Misc.fatal_errorf "Unexpected [No_usages] in [follow_field] for field %a"
        Field.print field
    | Many_sources_any_usage -> Many_sources_any_usage
    | Many_sources_usages usages -> for_usages usages
    | Single_source source -> (
      if not (PTA.field_used result.UA.db source field)
      then No_usages (* The field has been deleted when building the value *)
      else
        match PTA.get_single_field_source result.UA.db source field with
        | No_source -> No_source
        | One field_source -> Single_source field_source
        | Many ->
          if PTA.any_usage result.UA.db source
          then Many_sources_any_usage
          else
            for_usages
              (PTA.get_direct_usages result.db
                 (Code_id_or_name.Map.singleton source ())))

  let follow_field_for_set_of_closures result set_of_closures value_slot =
    let field = Field.value_slot value_slot in
    if
      Function_slot.Map.for_all
        (fun _ closure -> not (PTA.field_used result.UA.db closure field))
        set_of_closures
    then No_usages
    else
      let sources =
        Function_slot.Map.map
          (fun closure ->
            PTA.get_single_field_source result.UA.db closure field)
          set_of_closures
      in
      let _, source = Function_slot.Map.min_binding sources in
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
        match PTA.get_one_field_usage_of_constructors result.db c field with
        | Used_as_top -> Many_sources_any_usage
        | Used_as_vars vs ->
          let usages = PTA.get_direct_usages result.db vs in
          if Code_id_or_name.Map.is_empty usages
          then No_usages
          else Many_sources_usages usages)

  let rewrite (result, usages) typing_env flambda_type =
    let open Flambda2_types.Rewriter in
    let db = result.UA.db in
    let[@local] forget_type () =
      (* CR ncourant: we should preserve the nullability of the type here. *)
      if
        Lazy.force debug_types
        && (not (Flambda2_types.is_unknown typing_env flambda_type))
        && not
             (List.mem
                (Flambda_colours.without_colours ~f:(fun () ->
                     Format.asprintf "%a" Flambda2_types.print flambda_type))
                ["(Val? ⊤)"; "(Val! ⊤)"])
      then
        Format.eprintf "Forgetting: %a@.Usages = %a@." Flambda2_types.print
          flambda_type print_t0 usages;
      Rule.rewrite Pattern.any (Expr.unknown (Flambda2_types.kind flambda_type))
    in
    match usages with
    | _ when forget_all_types -> forget_type ()
    | No_usages -> forget_type ()
    | Many_sources_usages m when Code_id_or_name.Map.is_empty m ->
      forget_type ()
    | No_source ->
      Rule.rewrite Pattern.any (Expr.bottom (Flambda2_types.kind flambda_type))
    | Single_source _ | Many_sources_any_usage | Many_sources_usages _ -> (
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
          (current_function_slot, alloc_mode, closures_entry, _function_type)
        -> (
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
        let usages_for_value_slots, usages_of_function_slots =
          uses_for_set_of_closures db usages current_function_slot
            code_id_of_function_slots
        in
        let[@local] bottom () =
          Rule.rewrite Pattern.any
            (Expr.bottom (Flambda2_types.kind flambda_type))
        in
        let[@local] change_representation_of_closures fields closure_source
            value_slots_reprs function_slots_reprs =
          let patterns = ref [] in
          let all_function_slots_in_set =
            Function_slot.Map.fold
              (fun function_slot (_, uses) m ->
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
                           (Pattern.var v (result, Many_sources_any_usage))
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
              usages_of_function_slots Function_slot.Map.empty
          in
          let all_closure_types_in_set =
            Function_slot.Map.fold
              (fun function_slot (metadata, _uses) m ->
                let v = Var.create () in
                patterns
                  := Pattern.function_slot function_slot
                       (Pattern.var v (result, metadata))
                     :: !patterns;
                let new_function_slot =
                  Function_slot.Map.find function_slot function_slots_reprs
                in
                Function_slot.Map.add new_function_slot (Expr.var v) m)
              usages_of_function_slots Function_slot.Map.empty
          in
          let bind_function_slots = Some !patterns in
          let bound, pat =
            patterns_for_unboxed_fields
              ~machine_width:(Typing_env.machine_width typing_env)
              ~bind_function_slots db
              ~var:(fun _ (field_source : PTA.single_field_source) field_use ->
                let metadata =
                  match field_source, field_use with
                  | No_source, _ -> No_source
                  | One source, _ -> Single_source source
                  | Many, Used_as_top -> Many_sources_any_usage
                  | Many, Used_as_vars flow_to ->
                    let usages = PTA.get_direct_usages result.UA.db flow_to in
                    Many_sources_usages usages
                in
                result, metadata)
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
               ~all_value_slots_in_set alloc_mode)
        in
        let[@local] no_representation_change function_slot value_slots_metadata
            function_slots_metadata_and_uses =
          let all_patterns = ref [] in
          match
            Value_slot.Map.filter_map
              (fun value_slot metadata ->
                match metadata with
                | No_usages -> None
                | No_source -> raise Exit
                | Single_source _ | Many_sources_any_usage
                | Many_sources_usages _ ->
                  let v = Var.create () in
                  all_patterns
                    := Pattern.value_slot value_slot
                         (Pattern.var v (result, metadata))
                       :: !all_patterns;
                  Some (Expr.var v))
              value_slots_metadata
          with
          | exception Exit ->
            (* CR ncourant: should this check be in [uses_for_set_of_closures]
               instead? *)
            bottom ()
          | all_value_slots_in_set ->
            let all_closure_types_in_set =
              Function_slot.Map.mapi
                (fun function_slot (metadata, _uses) ->
                  let v = Var.create () in
                  all_patterns
                    := Pattern.function_slot function_slot
                         (Pattern.var v (result, metadata))
                       :: !all_patterns;
                  Expr.var v)
                function_slots_metadata_and_uses
            in
            let all_function_slots_in_set =
              Function_slot.Map.mapi
                (fun function_slot (_, uses) ->
                  match uses with
                  | Never_called -> Or_unknown.Unknown
                  | Only_called_with_known_arity | Any_call ->
                    let v = Var.create () in
                    all_patterns
                      := Pattern.rec_info function_slot
                           (Pattern.var v (result, Many_sources_any_usage))
                         :: !all_patterns;
                    let function_type =
                      Flambda2_types.Closures_entry.find_function_type
                        closures_entry function_slot
                    in
                    Or_unknown.map function_type ~f:(fun function_type ->
                        Expr.Function_type.create
                          (Function_type.code_id function_type)
                          ~rec_info:(Expr.var v)))
                function_slots_metadata_and_uses
            in
            let known_sources =
              Function_slot.Map.for_all
                (fun _ (use, _) ->
                  match use with
                  | Single_source _ -> true
                  | No_source | No_usages | Many_sources_usages _
                  | Many_sources_any_usage ->
                    false)
                function_slots_metadata_and_uses
            in
            let expr =
              if known_sources
              then
                Expr.exactly_this_closure function_slot
                  ~all_function_slots_in_set ~all_closure_types_in_set
                  ~all_value_slots_in_set alloc_mode
              else
                Expr.at_least_this_closure function_slot
                  ~at_least_these_function_slots:all_function_slots_in_set
                  ~at_least_these_closure_types:all_closure_types_in_set
                  ~at_least_these_value_slots:all_value_slots_in_set alloc_mode
            in
            Rule.rewrite (Pattern.closure !all_patterns) expr
        in
        match usages_for_value_slots with
        | Dead_code -> bottom ()
        | From_set_of_closures set_of_closures ->
          if
            Function_slot.Map.exists
              (fun _ clos ->
                Code_id_or_name.Map.mem clos result.changed_representation)
              set_of_closures
          then (
            assert (
              Function_slot.Map.for_all
                (fun _ clos ->
                  Code_id_or_name.Map.mem clos result.changed_representation)
                set_of_closures);
            let changed_representation =
              List.map
                (fun (_, clos) ->
                  let repr, clos' =
                    Code_id_or_name.Map.find clos result.changed_representation
                  in
                  assert (Code_id_or_name.equal clos clos');
                  repr)
                (Function_slot.Map.bindings set_of_closures)
            in
            let value_slots_reprs, function_slots_reprs =
              match List.hd changed_representation with
              | Closure_representation
                  (value_slots_reprs, function_slots_reprs, _) ->
                value_slots_reprs, function_slots_reprs
              | Block_representation _ ->
                Misc.fatal_errorf
                  "Changed representation of a closure with \
                   [Block_representation]"
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
                    "Changed representation of a closure with \
                     [Block_representation]")
              changed_representation;
            let fields =
              PTA.get_fields_usage_of_constructors db
                (Function_slot.Map.fold
                   (fun _ c acc -> Code_id_or_name.Map.add c () acc)
                   set_of_closures Code_id_or_name.Map.empty)
            in
            change_representation_of_closures fields
              (Function_slot.Map.find current_function_slot set_of_closures)
              value_slots_reprs function_slots_reprs)
          else
            no_representation_change current_function_slot
              (Value_slot.Map.mapi
                 (fun value_slot _value_slot_type ->
                   follow_field_for_set_of_closures result set_of_closures
                     value_slot)
                 value_slot_types)
              usages_of_function_slots
        | Value_slots_usages usages_for_value_slots ->
          if
            Code_id_or_name.Map.exists
              (fun clos () ->
                Code_id_or_name.Map.mem clos result.changed_representation)
              usages_for_value_slots
          then (
            assert (
              Code_id_or_name.Map.for_all
                (fun clos () ->
                  Code_id_or_name.Map.mem clos result.changed_representation)
                usages_for_value_slots);
            let changed_representation =
              Code_id_or_name.Map.bindings
                (Code_id_or_name.Map.mapi
                   (fun clos () ->
                     Code_id_or_name.Map.find clos result.changed_representation)
                   usages_for_value_slots)
            in
            let value_slots_reprs, function_slots_reprs, alloc_point =
              match snd (List.hd changed_representation) with
              | ( Closure_representation
                    (value_slots_reprs, function_slots_reprs, _),
                  alloc_point ) ->
                value_slots_reprs, function_slots_reprs, alloc_point
              | Block_representation _, _ ->
                Misc.fatal_errorf
                  "Changed representation of a closure with \
                   [Block_representation]"
            in
            List.iter
              (fun (_, (_, alloc_point')) ->
                assert (alloc_point == alloc_point'))
              changed_representation;
            let fields =
              PTA.get_fields_usage_of_constructors db
                (Code_id_or_name.Map.singleton alloc_point ())
            in
            change_representation_of_closures fields alloc_point
              value_slots_reprs function_slots_reprs)
          else
            let usages_of_value_slots =
              Value_slot.Map.mapi
                (fun value_slot _value_slot_type ->
                  match
                    PTA.get_one_field db
                      (Field.value_slot value_slot)
                      (Usages usages_for_value_slots)
                  with
                  | Used_as_top -> Many_sources_any_usage
                  | Used_as_vars vs ->
                    Many_sources_usages (PTA.get_direct_usages db vs))
                value_slot_types
            in
            no_representation_change current_function_slot usages_of_value_slots
              usages_of_function_slots
        | Value_slots_any_usages ->
          let is_local_value_slot vs _ =
            Compilation_unit.is_current (Value_slot.get_compilation_unit vs)
          in
          let is_local_function_slot fs _ =
            Compilation_unit.is_current (Function_slot.get_compilation_unit fs)
          in
          if
            Value_slot.Map.exists is_local_value_slot value_slot_types
            || Function_slot.Map.exists is_local_function_slot
                 function_slot_types
          then (
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
            let code_ids =
              Function_slot.Map.fold
                (fun function_slot _ l ->
                  match
                    Flambda2_types.Closures_entry.find_function_type
                      closures_entry function_slot
                  with
                  | Unknown -> l
                  | Known function_type ->
                    Function_type.code_id function_type :: l)
                function_slot_types []
            in
            let set_of_closures =
              identify_set_of_closures_with_code_ids db code_ids
            in
            match set_of_closures with
            | None ->
              if Lazy.force debug_types
              then Format.eprintf "COULD NOT IDENTIFY@.";
              forget_type ()
            | Some set_of_closures ->
              if
                Function_slot.Map.exists
                  (fun _ clos ->
                    Code_id_or_name.Map.mem clos result.changed_representation)
                  set_of_closures
              then (
                assert (
                  Function_slot.Map.for_all
                    (fun _ clos ->
                      Code_id_or_name.Map.mem clos result.changed_representation)
                    set_of_closures);
                let changed_representation =
                  List.map
                    (fun (_, clos) ->
                      let repr, clos' =
                        Code_id_or_name.Map.find clos
                          result.changed_representation
                      in
                      assert (Code_id_or_name.equal clos clos');
                      repr)
                    (Function_slot.Map.bindings set_of_closures)
                in
                let value_slots_reprs, function_slots_reprs =
                  match List.hd changed_representation with
                  | Closure_representation
                      (value_slots_reprs, function_slots_reprs, _) ->
                    value_slots_reprs, function_slots_reprs
                  | Block_representation _ ->
                    Misc.fatal_errorf
                      "Changed representation of a closure with \
                       [Block_representation]"
                in
                List.iter
                  (function
                    | UA.Closure_representation (vs, fs, _) ->
                      if vs != value_slots_reprs || fs != function_slots_reprs
                      then
                        Misc.fatal_errorf
                          "In set of closures, all closures do not have the \
                           same representation changes."
                    | UA.Block_representation _ ->
                      Misc.fatal_errorf
                        "Changed representation of a closure with \
                         [Block_representation]")
                  changed_representation;
                let fields =
                  PTA.get_fields_usage_of_constructors db
                    (Function_slot.Map.fold
                       (fun _ c acc -> Code_id_or_name.Map.add c () acc)
                       set_of_closures Code_id_or_name.Map.empty)
                in
                change_representation_of_closures fields
                  (Function_slot.Map.find current_function_slot set_of_closures)
                  value_slots_reprs function_slots_reprs)
              else
                let usages_of_function_slots =
                  uses_of_function_slots_for_set_of_closures db set_of_closures
                in
                no_representation_change current_function_slot
                  (Value_slot.Map.mapi
                     (fun value_slot _value_slot_type ->
                       follow_field_for_set_of_closures result set_of_closures
                         value_slot)
                     value_slot_types)
                  usages_of_function_slots)
          else
            no_representation_change current_function_slot
              (Value_slot.Map.map
                 (fun _value_slot_type -> Many_sources_any_usage)
                 value_slot_types)
              usages_of_function_slots))

  let block_slot ?tag:_ (result, t) index _typing_env flambda_type =
    let r =
      let field_kind = Flambda2_types.kind flambda_type in
      let field = Field.block (Target_ocaml_int.to_int index) field_kind in
      follow_field result t field
    in
    result, r

  let array_slot (result, _t) _index _typing_env _flambda_type =
    (* Array primitives are opaque. Thus, anything put inside the array when it
       was created has been treated as escaping, thus giving a
       [Many_sources_any_usage] result. *)
    result, Many_sources_any_usage

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

let rewrite_typing_env result ~unit_symbol:_ typing_env =
  if Lazy.force debug_types
  then Format.eprintf "OLD typing env: %a@." Typing_env.print typing_env;
  let db = result.UA.db in
  let symbol_metadata sym =
    if not (Compilation_unit.is_current (Symbol.compilation_unit sym))
    then result, Rewriter.Many_sources_any_usage
    else
      let sym = Code_id_or_name.symbol sym in
      if not (PTA.has_source_query db sym)
      then result, Rewriter.No_source
      else if not (PTA.has_use db sym)
      then result, Rewriter.No_usages
      else
        match PTA.get_allocation_point db sym with
        | Some alloc_point -> result, Rewriter.Single_source alloc_point
        | None ->
          if PTA.any_usage db sym
          then result, Rewriter.Many_sources_any_usage
          else
            ( result,
              Rewriter.Many_sources_usages
                (PTA.get_direct_usages db
                   (Code_id_or_name.Map.singleton sym ())) )
  in
  let r =
    Profile.record_call ~accumulate:true "types" (fun () ->
        TypesRewrite.rewrite typing_env symbol_metadata)
  in
  if Lazy.force debug_types
  then Format.eprintf "NEW typing env: %a@." Typing_env.print r;
  r

let rewrite_result_types result ~old_typing_env ~my_closure:func_my_closure
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
    let db = result.UA.db in
    if Code_id_or_name.Map.mem var result.UA.unboxed_fields
    then (
      let unboxed_fields = Code_id_or_name.Map.find var result.unboxed_fields in
      if PTA.any_usage db var
      then
        Misc.fatal_errorf "In [rewrite_result_types], var %a is unboxed but top"
          Code_id_or_name.print var;
      let fields =
        PTA.get_fields db
          (PTA.get_all_usages ~follow_known_arity_calls:true db
             (Code_id_or_name.Map.singleton var ()))
      in
      let bound, pat =
        Rewriter.patterns_for_unboxed_fields
          ~machine_width:(Typing_env.machine_width old_typing_env)
          ~bind_function_slots:None db
          ~var:(fun v field_source field_use ->
            let metadata =
              match field_source, field_use with
              | No_source, _ -> Rewriter.No_source
              | One source, _ -> Rewriter.Single_source source
              | Many, Used_as_top -> Rewriter.Many_sources_any_usage
              | Many, Used_as_vars flow_to ->
                let usages = PTA.get_direct_usages db flow_to in
                Rewriter.Many_sources_usages usages
            in
            Variable.name v, (result, metadata))
          fields unboxed_fields
          (Option.get (PTA.get_allocation_point db var))
      in
      let all_vars =
        Unboxed_fields.fold_with_kind
          (fun _kind (pattern_var, v) acc ->
            if Option.is_none pattern_var
            then
              Format.eprintf
                "In [rewrite_result_types], could not get a pattern variable \
                 for unboxed var %a@."
                Variable.print v;
            Option.get pattern_var :: acc)
          bound []
      in
      (pat, kind), all_vars)
    else
      match to_keep with
      | PTA.Delete -> (Flambda2_types.Rewriter.Pattern.any, kind), []
      | PTA.Keep ->
        let metadata =
          if not (PTA.has_source_query db var)
          then result, Rewriter.No_source
          else if not (PTA.has_use db var)
          then result, Rewriter.No_usages
          else
            match PTA.get_allocation_point db var with
            | Some alloc_point -> result, Rewriter.Single_source alloc_point
            | None ->
              if PTA.any_usage db var
              then result, Rewriter.Many_sources_any_usage
              else
                ( result,
                  Rewriter.Many_sources_usages
                    (PTA.get_direct_usages db
                       (Code_id_or_name.Map.singleton var ())) )
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
        result.unboxed_fields
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
