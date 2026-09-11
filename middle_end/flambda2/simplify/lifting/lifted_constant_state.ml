(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module DE = Downwards_env
module LC = Lifted_constant
module T = Flambda2_types
module TE = T.Typing_env

type t =
  | Empty
  | Leaf of LC.t
  | Leaf_array of { ts : LC.t array }
  | Union of
      { t1 : t;
        t2 : t
      }

let print ppf t =
  let to_list t =
    let rec to_list t acc =
      match t with
      | Empty -> acc
      | Leaf const -> const :: acc
      | Leaf_array { ts } -> List.rev (Array.to_list ts) @ acc
      | Union { t2; t1 } -> to_list t1 (to_list t2 acc)
    in
    to_list t []
  in
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space LC.print)
    (to_list t)

let empty = Empty

let is_empty t =
  match t with Empty -> true | Leaf _ | Leaf_array _ | Union _ -> false

let singleton const = Leaf const

let singleton_list_of_constants constants =
  match constants with
  | [] -> empty
  | [constant] -> singleton constant
  | _ :: _ -> Leaf_array { ts = Array.of_list constants }

let add t const =
  if is_empty t then Leaf const else Union { t2 = Leaf const; t1 = t }

let union t1 t2 =
  match t1, t2 with
  | Empty, Empty -> Empty
  | Empty, (Leaf _ | Leaf_array _ | Union _) -> t2
  | (Leaf _ | Leaf_array _ | Union _), Empty -> t1
  | (Leaf _ | Leaf_array _ | Union _), t2 -> Union { t2 = t1; t1 = t2 }

let[@inline] fold t ~init ~f =
  let rec loop init = function
    | Empty -> init
    | Leaf const -> f init const
    | Leaf_array { ts } -> ArrayLabels.fold_left ts ~init ~f
    | Union { t2; t1 } ->
      let init = loop init t2 in
      loop init t1
  in
  loop init t

let all_defined_symbols t =
  fold t ~init:Symbol.Set.empty ~f:(fun symbols const ->
      LC.all_defined_symbols const |> Symbol.Set.union symbols)

let add_to_denv ?maybe_already_defined denv lifted =
  let initial_denv = denv in
  let maybe_already_defined =
    match maybe_already_defined with None -> false | Some () -> true
  in
  let denv =
    fold lifted ~init:denv ~f:(fun denv lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold
          (fun sym (_denv, typ) denv ->
            if maybe_already_defined && DE.mem_symbol denv sym
            then denv
            else DE.define_symbol denv sym (T.kind typ))
          types_of_symbols denv)
  in
  let typing_env =
    let typing_env = DE.typing_env denv in
    fold lifted ~init:typing_env ~f:(fun typing_env lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold
          (fun sym (denv_at_definition, typ) typing_env ->
            if maybe_already_defined && DE.mem_symbol initial_denv sym
            then typing_env
            else
              let sym = Name.symbol sym in
              let env_extension =
                (* CR mshinwell: Maybe sometimes this could be done at a time
                   previous to this point. *)
                (* CR pchambart: Maybe some of these make_suitable calls could
                   be combined into one *)
                T.make_suitable_for_environment
                  (DE.typing_env denv_at_definition)
                  (Everything_not_in typing_env)
                  [sym, typ]
              in
              TE.add_env_extension_with_extra_variables typing_env env_extension)
          types_of_symbols typing_env)
  in
  fold lifted ~init:(DE.with_typing_env denv typing_env)
    ~f:(fun denv lifted_constant ->
      let pieces_of_code =
        LC.defining_exprs lifted_constant
        |> Rebuilt_static_const.Group.pieces_of_code_including_those_not_rebuilt
      in
      Code_id.Map.fold
        (fun code_id code denv ->
          if maybe_already_defined && DE.mem_code denv code_id
          then denv
          else DE.define_code denv ~code_id ~code)
        pieces_of_code denv)

module CIS = Code_id_or_symbol
module SCC_lifted_constants = Strongly_connected_components.Make (CIS)

let dependencies ~include_code_age_relation free_names =
  let code_ids =
    if include_code_age_relation
    then Name_occurrences.code_ids_and_newer_version_of_code_ids free_names
    else Name_occurrences.code_ids_in_normal_mode free_names
  in
  let symbols =
    if include_code_age_relation
    then Name_occurrences.symbols free_names
    else
      Name_occurrences.fold_names free_names ~init:Symbol.Set.empty
        ~f:(fun symbols name ->
          match Name_occurrences.greatest_name_mode_name free_names name with
          | Present mode when Name_mode.is_normal mode ->
            Name.pattern_match name
              ~var:(fun _ -> symbols)
              ~symbol:(fun symbol -> Symbol.Set.add symbol symbols)
          | Absent | Present _ -> symbols)
  in
  CIS.Set.union
    (CIS.set_of_symbol_set symbols)
    (CIS.set_of_code_id_set code_ids)

let build_dep_graph t ~include_code_age_relation =
  fold t ~init:(CIS.Lmap.empty, CIS.Map.empty)
    ~f:(fun (dep_graph, code_id_or_symbol_to_const) lifted_constant ->
      ListLabels.fold_left (LC.definitions lifted_constant)
        ~init:(dep_graph, code_id_or_symbol_to_const)
        ~f:(fun (dep_graph, code_id_or_symbol_to_const) definition ->
          let module D = LC.Definition in
          let free_names =
            let free_names = D.free_names definition in
            match D.descr definition with
            | Code _ | Block_like _ -> free_names
            | Set_of_closures { closure_symbols_with_types; _ } ->
              (* To avoid existing sets of closures (with or without associated
                 code) being pulled apart, we add a dependency from each of the
                 closure symbols (in the current set) to all of the others (in
                 the current set). *)
              ListLabels.fold_left
                (Function_slot.Lmap.data closure_symbols_with_types)
                ~init:free_names ~f:(fun free_names (symbol, _) ->
                  Name_occurrences.add_symbol free_names symbol Name_mode.normal)
          in
          let deps = dependencies ~include_code_age_relation free_names in
          let being_defined =
            D.bound_static definition
            |> Bound_static.everything_being_defined_as_list
          in
          List.fold_left
            (fun (dep_graph, code_id_or_symbol_to_const) being_defined ->
              let dep_graph = CIS.Lmap.add being_defined deps dep_graph in
              let code_id_or_symbol_to_const =
                CIS.Map.add being_defined
                  (Lifted_constant.create_definition definition)
                  code_id_or_symbol_to_const
              in
              dep_graph, code_id_or_symbol_to_const)
            (dep_graph, code_id_or_symbol_to_const)
            being_defined))

let fold_reachable t ~roots ~init ~f =
  (* Pending code includes dead siblings and earlier simplification attempts.
     Follow final runtime uses, not age-relation edges or metadata roots. *)
  let graph, constants = build_dep_graph t ~include_code_age_relation:false in
  let rec visit pending seen acc =
    match pending with
    | [] -> acc
    | name :: pending -> (
      if CIS.Set.mem name seen
      then visit pending seen acc
      else
        match CIS.Map.find_opt name constants with
        | None -> visit pending (CIS.Set.add name seen) acc
        | Some constant ->
          let seen =
            CIS.Set.union seen
              (Bound_static.everything_being_defined (LC.bound_static constant))
          in
          let pending =
            CIS.Set.fold
              (fun name pending -> name :: pending)
              (CIS.Lmap.find name graph) pending
          in
          visit pending seen (f acc constant))
  in
  visit
    (CIS.Set.elements (dependencies ~include_code_age_relation:false roots))
    CIS.Set.empty init

let retain_reachable_for_speculation t ~roots =
  fold_reachable t ~roots ~init:empty ~f:add

let cost_metrics t ~roots =
  let cost_of_constant constant =
    List.fold_left
      (fun cost definition ->
        Cost_metrics.( + ) cost
          (Rebuilt_static_const.cost_metrics_for_inlining
             (LC.Definition.defining_expr definition)))
      Cost_metrics.zero (LC.definitions constant)
  in
  let has_cost =
    fold t ~init:false ~f:(fun has_cost constant ->
        has_cost
        || not
             (Cost_metrics.equal (cost_of_constant constant) Cost_metrics.zero))
  in
  if not has_cost
  then Cost_metrics.zero
  else
    fold_reachable t ~roots ~init:Cost_metrics.zero ~f:(fun cost constant ->
        Cost_metrics.( + ) cost (cost_of_constant constant))

let remove_values_not_in_domain (m : CIS.Set.t CIS.Lmap.t) =
  let domain =
    CIS.Lmap.fold
      (fun value _ domain -> CIS.Set.add value domain)
      m CIS.Set.empty
  in
  CIS.Lmap.map
    (fun values ->
      (* CR lmaurer: This should use a map/set intersection function, which
         should be easy to define with the Patricia tree refactor *)
      CIS.Set.filter (fun value -> CIS.Set.mem value domain) values)
    m

type sort_result = { innermost_first : LC.t array }

let sort0 t =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code). We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph t ~include_code_age_relation:true
  in
  let lifted_constants_dep_graph =
    (* This graph has vertices for all code ids and symbols that appear free in
       the definitions, including pre-existing ones. However, any such "external
       vertex" is a leaf, and in fact it won't even appear as a key in the map
       representation of the graph. However, [SCC_lifted_constants] assumes that
       all vertices appear as keys. Fortunately, we're only concerned with the
       interdependencies between code ids and symbols being defined here, so we
       can just filter out external dependencies from the graph. *)
    remove_values_not_in_domain lifted_constants_dep_graph
  in
  let innermost_first =
    CIS.Lmap.bindings lifted_constants_dep_graph
    |> SCC_lifted_constants
       .stable_connected_components_sorted_from_roots_to_leaf
    |> ArrayLabels.map ~f:(fun (group : SCC_lifted_constants.component) ->
        let code_id_or_symbols =
          match group with
          | No_loop code_id_or_symbol -> [code_id_or_symbol]
          | Has_loop code_id_or_symbols -> code_id_or_symbols
        in
        let _, lifted_constants =
          ListLabels.fold_left code_id_or_symbols ~init:(CIS.Set.empty, [])
            ~f:(fun ((already_seen, definitions) as acc) code_id_or_symbol ->
              if CIS.Set.mem code_id_or_symbol already_seen
              then acc
              else
                let lifted_constant =
                  CIS.Map.find code_id_or_symbol code_id_or_symbol_to_const
                in
                let already_seen =
                  (* We may encounter the same defining expression more than
                     once, in the case of sets of closures, which may bind more
                     than one symbol. We must avoid duplicates in the resulting
                     [LC.t]. *)
                  let bound_static = LC.bound_static lifted_constant in
                  CIS.Set.union
                    (Bound_static.everything_being_defined bound_static)
                    already_seen
                in
                already_seen, lifted_constant :: definitions)
        in
        LC.concat lifted_constants)
  in
  { innermost_first }

let sort t =
  match t with
  | Empty -> { innermost_first = [||] }
  | Leaf const -> { innermost_first = [| const |] }
  | Leaf_array _ | Union _ -> sort0 t
