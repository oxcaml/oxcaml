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

module LC = Lifted_constant

type t =
  { constants : LC.t list;
    n : int
  }

let empty = { constants = []; n = 0 }

let is_empty t = t.n = 0

let of_list constants = { constants; n = List.length constants }

let add t const = { constants = const :: t.constants; n = t.n + 1 }

let add_list t l =
  { constants = List.rev_append l t.constants; n = t.n + List.length l }

let diff t t' =
  (* make sure [t] is the "smaller" one *)
  let t, t' = if t.n > t'.n then t', t else t, t' in
  (* Ensure that [t] is indeed an ancestor of [t'] *)
  if false && Flambda_features.check_invariants ()
  then begin
    let rec drop k = function
      | l when k <= 0 -> l
      | [] -> []
      | _ :: r -> drop (k - 1) r
    in
    if not (t.constants == drop (t'.n - t.n) t'.constants)
    then
      Misc.fatal_errorf
        "[Lifted_constant_state.diff]: expected one of the LCS.t to be an \
         ancestor of the other, but that is not the case !"
  end;
  (* Avoid re-allocating a whole list. In practice, the result of the diff is
     short-lived (and in particular is outlived by the arguments), so that's
     fine. *)
  { constants = t'.constants; n = t'.n - t.n }

let print ppf t =
  (* CR gbury: this might be more efficient by going through an array instead of
     a list built twice *)
  let rec take acc n = function
    | [] -> List.rev acc
    | x :: r -> if n <= 0 then List.rev acc else take (x :: acc) (n - 1) r
  in
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space LC.print)
    (take [] t.n t.constants)

let fold t ~init ~f =
  let rec loop acc n = function
    | [] -> acc
    | const :: r -> if n <= 0 then acc else loop (f acc const) (n - 1) r
  in
  loop init t.n t.constants

let all_defined_symbols t =
  fold t ~init:Symbol.Set.empty ~f:(fun symbols const ->
      LC.all_defined_symbols const |> Symbol.Set.union symbols)

module SCC = struct
  module CIS = Code_id_or_symbol
  module SCC_lifted_constants = Strongly_connected_components.Make (CIS)

  let build_dep_graph t =
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
                (* To avoid existing sets of closures (with or without
                   associated code) being pulled apart, we add a dependency from
                   each of the closure symbols (in the current set) to all of
                   the others (in the current set). *)
                ListLabels.fold_left
                  (Function_slot.Lmap.data closure_symbols_with_types)
                  ~init:free_names ~f:(fun free_names (symbol, _) ->
                    Name_occurrences.add_symbol free_names symbol
                      Name_mode.normal)
            in
            let free_syms = Name_occurrences.symbols free_names in
            let free_code_ids =
              free_names
              |> Name_occurrences.code_ids_and_newer_version_of_code_ids
            in
            let deps =
              CIS.Set.union
                (CIS.set_of_symbol_set free_syms)
                (CIS.set_of_code_id_set free_code_ids)
            in
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

  (* This function sorts the lifted constants by order of dependencies,
     returning a list with the innermost lifted constants to place first. *)
  let sort t =
    (* The various lifted constants may exhibit recursion between themselves
       (specifically between closures and/or code). We use SCC to obtain a
       topological sort of groups that must be coalesced into single
       code-and-set-of-closures definitions. *)
    let lifted_constants_dep_graph, code_id_or_symbol_to_const =
      build_dep_graph t
    in
    let lifted_constants_dep_graph =
      (* This graph has vertices for all code ids and symbols that appear free
         in the definitions, including pre-existing ones. However, any such
         "external vertex" is a leaf, and in fact it won't even appear as a key
         in the map representation of the graph. However, [SCC_lifted_constants]
         assumes that all vertices appear as keys. Fortunately, we're only
         concerned with the interdependencies between code ids and symbols being
         defined here, so we can just filter out external dependencies from the
         graph. *)
      remove_values_not_in_domain lifted_constants_dep_graph
    in
    let innermost_first =
      CIS.Lmap.bindings lifted_constants_dep_graph
      |> SCC_lifted_constants
         .stable_connected_components_sorted_from_roots_to_leaf |> Array.to_list
      |> ListLabels.map ~f:(fun (group : SCC_lifted_constants.component) ->
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
                       once, in the case of sets of closures, which may bind
                       more than one symbol. We must avoid duplicates in the
                       resulting [LC.t]. *)
                    let bound_static = LC.bound_static lifted_constant in
                    CIS.Set.union
                      (Bound_static.everything_being_defined bound_static)
                      already_seen
                  in
                  already_seen, lifted_constant :: definitions)
          in
          LC.concat lifted_constants)
    in
    innermost_first
end

module Sorted = struct
  type t =
    { (* The `all` field includes both lifted constants from the `by_name` and
         `by_code` field, as well as lifted constants that are fully static and
         reference no variable, symbol or code_id. It's also used to keep track
         of which constants still need to be placed. *)
      all : LC.Set.t;
      by_name : LC.t list Name.Map.t;
      by_code : LC.t list Code_id.Map.t
    }

  let empty =
    { all = LC.Set.empty;
      by_name = Name.Map.empty;
      by_code = Code_id.Map.empty
    }

  let print_lc_bound ppf lc =
    let bound = LC.bound_static lc in
    Bound_static.print ppf bound

  let[@ocamlformat "disable"] print ppf { all; by_name; by_code; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(by_name@ %a)@]@ \
        @[<hov 1>(by_code_id@ %a)@]@ \
        @[<hov 1>(all@ %a)@]\
        )@]"
      (Name.Map.print (Format.pp_print_list ~pp_sep:Format.pp_print_space print_lc_bound)) by_name
      (Code_id.Map.print (Format.pp_print_list ~pp_sep:Format.pp_print_space print_lc_bound)) by_code
      LC.Set.print all

  let fold_in_any_order t ~init ~f =
    LC.Set.fold (fun lc acc -> f acc lc) t.all init

  let names_and_code_ids_in_defining_expr lc =
    let name_occurrences = LC.free_names lc in
    let free_names =
      Name_occurrences.fold_names name_occurrences ~init:[] ~f:(fun l name ->
          name :: l)
    in
    let free_code_ids =
      Name_occurrences.fold_code_ids name_occurrences ~init:[]
        ~f:(fun l code_id -> code_id :: l)
    in
    free_names, free_code_ids

  let add ~find_code_metadata ~used_value_slots t lc =
    let lc =
      LC.remove_unused_value_slots ~find_code_metadata ~used_value_slots lc
    in
    match names_and_code_ids_in_defining_expr lc with
    | [], [] -> { t with all = LC.Set.add lc t.all }
    | free_names, free_code_ids ->
      let by_name =
        List.fold_left
          (fun acc name ->
            Name.Map.update name
              (function None -> Some [lc] | Some l -> Some (lc :: l))
              acc)
          t.by_name free_names
      in
      let by_code =
        List.fold_left
          (fun acc name ->
            Code_id.Map.update name
              (function None -> Some [lc] | Some l -> Some (lc :: l))
              acc)
          t.by_code free_code_ids
      in
      { by_name; by_code; all = LC.Set.add lc t.all }

  let pop t bound_names =
    let pop_into_queue queue = function
      | None -> None
      | Some l ->
        List.iter (fun lc -> Queue.add lc queue) l;
        None
    in
    let add_to_queue bound_names queue =
      let by_name =
        Name_occurrences.fold_names bound_names ~init:t.by_name
          ~f:(fun map name -> Name.Map.update name (pop_into_queue queue) map)
      in
      let by_code =
        Name_occurrences.fold_code_ids bound_names ~init:t.by_code
          ~f:(fun map code_id ->
            Code_id.Map.update code_id (pop_into_queue queue) map)
      in
      if t.by_name == by_name && t.by_code == by_code
      then t
      else { t with by_name; by_code }
    in
    let rec aux t seen queue =
      match Queue.take_opt queue with
      | None -> t, seen
      | Some lc ->
        if LC.Set.mem lc seen
        then aux t seen queue
        else begin
          let seen = LC.Set.add lc seen in
          let being_defined = LC.bound_static lc in
          let bound_occurrences = Bound_static.free_names being_defined in
          let t = add_to_queue bound_occurrences queue in
          aux t seen queue
        end
    in
    let queue = Queue.create () in
    let t = add_to_queue bound_names queue in
    aux t LC.Set.empty queue

  let return set =
    (* Constants in a set are ordered with the first one being the ones defined
       the earliest. The sorting will return the constant that need to be placed
       first (and we will later do fold_lefts on these). Since the sorting is
       stable, if we want to have in the output constants ordered in the same
       order they were created, we neeed to rev the list here. *)
    let st = of_list (List.rev (LC.Set.elements set)) in
    SCC.sort st

  let pop_lifted_constants_to_place t ~bound_names =
    Profile.record_call ~accumulate:true "lifted-constants-pop" (fun () ->
        let t', lc_set = pop t bound_names in
        let lc_to_place = LC.Set.inter lc_set t'.all in
        let remaining_lc_to_place = LC.Set.diff t'.all lc_to_place in
        let t'' = { t' with all = remaining_lc_to_place } in
        return lc_to_place, t'')

  let pop_all_remaining_lifted_constants t =
    Profile.record_call ~accumulate:true "lifted-constants-pop" (fun () ->
        return t.all)
end

let sort ~find_code_metadata ~used_value_slots t =
  Profile.record_call ~accumulate:true "lifted-constants-sort" (fun () ->
      let l = SCC.sort t in
      List.fold_left
        (Sorted.add ~find_code_metadata ~used_value_slots)
        Sorted.empty l)
