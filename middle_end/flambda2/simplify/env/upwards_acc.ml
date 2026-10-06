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

module ART = Are_rebuilding_terms
module DA = Downwards_acc
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env
module UE = Upwards_env

type lifted_constants = (LCS.Sorted.t, LCS.t) At_toplevel_or_in_a_closure.t

type t =
  { uenv : UE.t;
    creation_dacc : DA.t;
    lifted_constants : lifted_constants;
    all_code : Exported_code.t;
    name_occurrences : Name_occurrences.t;
    cost_metrics : Cost_metrics.t;
    slot_offsets : Slot_offsets.t Or_unknown.t;
    flow_result : Flow_types.Flow_result.t;
    resimplify : bool
  }

let [@ocamlformat "disable"] print ppf
      { uenv; creation_dacc = _; lifted_constants;
        name_occurrences; all_code = _; cost_metrics; slot_offsets; flow_result;
        resimplify;
      } =
  let print_lifted_constants ppf lifted_constants =
    At_toplevel_or_in_a_closure.print ~toplevel:LCS.Sorted.print
      ~in_a_closure:LCS.print ppf lifted_constants
  in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(uenv@ %a)@]@ \
      @[<hov 1>(lifted_constants@ %a)@]@ \
      @[<hov 1>(name_occurrences@ %a)@]@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a@)@]@ \
      @[<hov 1>(flow_result@ %a)@]\
      %a\
      )@]"
    UE.print uenv
    print_lifted_constants lifted_constants
    Name_occurrences.print name_occurrences
    Cost_metrics.print cost_metrics
    (Or_unknown.print Slot_offsets.print) slot_offsets
    Flow_types.Flow_result.print flow_result
    (if resimplify then
       (fun ppf () -> Format.fprintf ppf "@ @[<hov 1>(should_resimplify)@]")
     else
       (fun _ppf () -> ())) ()

let fold_lifted_constants lifted_constants ~init ~f =
  match (lifted_constants : lifted_constants) with
  | At_toplevel sorted -> LCS.Sorted.fold_in_any_order sorted ~init ~f
  | In_a_closure lcs -> LCS.fold lcs ~init ~f

let find_code_metadata dacc code_id =
  let env = DA.denv dacc in
  (try Downwards_env.find_code_exn env code_id
   with Not_found ->
     Misc.fatal_errorf "Could not find code for %a" Code_id.print code_id)
  |> Code_or_metadata.code_metadata

let create ~flow_result ~compute_slot_offsets uenv dacc =
  let slot_offsets : _ Or_unknown.t =
    if compute_slot_offsets then Known Slot_offsets.empty else Unknown
  in
  let lcs = DA.get_lifted_constants dacc in
  let lifted_constants : lifted_constants =
    match DE.closure_info (DA.denv dacc) with
    | Not_in_a_closure ->
      let used_value_slots = DA.used_value_slots dacc in
      let find_code_metadata = find_code_metadata dacc in
      let lifted_constants =
        LCS.sort ~find_code_metadata ~used_value_slots lcs
      in
      At_toplevel lifted_constants
    | Closure _ -> In_a_closure lcs
    | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
      Misc.fatal_errorf
        "We should never be \
         In_a_set_of_closures_but_not_yet_in_a_specific_closure when creating \
         a uacc"
  in
  { uenv;
    creation_dacc = dacc;
    lifted_constants;
    all_code = Exported_code.empty;
    name_occurrences = Name_occurrences.empty;
    (* [used_value_slots] must be kept separate from the normal free names
       tracking in [name_occurrences], since it is always accumulated, and never
       saved and restored (like free name information is when dealing with a
       [Let_cont]). *)
    cost_metrics = Cost_metrics.zero;
    slot_offsets;
    flow_result;
    resimplify = false
  }

let creation_dacc t = t.creation_dacc

let uenv t = t.uenv

let code_age_relation t = TE.code_age_relation (DA.typing_env t.creation_dacc)

let lifted_constants t = t.lifted_constants

let pop_all_remaining_lifted_constants_to_place t =
  match t.lifted_constants with
  | At_toplevel sort_result ->
    let l, sort_result =
      LCS.Sorted.pop_all_remaining_lifted_constants sort_result
    in
    let t = { t with lifted_constants = At_toplevel sort_result } in
    t, l
  | In_a_closure _ ->
    Misc.fatal_error
      "all_remaining_lifted_constants_to_place called inside a closure"

let pop_lifted_constants_for_placement ~bound_names t =
  At_toplevel_or_in_a_closure.map t.lifted_constants
    ~toplevel:(fun sort_result ->
      let lifted_constants_to_place, sort_result =
        LCS.Sorted.pop_lifted_constants_to_place sort_result ~bound_names
      in
      let t = { t with lifted_constants = At_toplevel sort_result } in
      t, lifted_constants_to_place)
    ~in_a_closure:(fun _ -> ())

let add_required_lifted_constant t lc =
  let lifted_constants : lifted_constants =
    match t.lifted_constants with
    | At_toplevel sort_result ->
      let used_value_slots = DA.used_value_slots t.creation_dacc in
      let find_code_metadata = find_code_metadata t.creation_dacc in
      At_toplevel
        (LCS.Sorted.add ~used_value_slots ~find_code_metadata sort_result lc)
    | In_a_closure lcs -> In_a_closure (LCS.add lcs lc)
  in
  let flow_result =
    Flow.Analysis.add_required_lifted_constant t.flow_result lc
  in
  { t with lifted_constants; flow_result }

let cost_metrics t = t.cost_metrics

let are_rebuilding_terms t = DE.are_rebuilding_terms (DA.denv t.creation_dacc)

let map_uenv t ~f = { t with uenv = f t.uenv }

let with_uenv t uenv = { t with uenv }

let remember_code_for_cmx t code =
  if ART.do_not_rebuild_terms (are_rebuilding_terms t)
  then t
  else
    let keep_code code_id =
      Code_id.Set.mem code_id (DA.code_ids_to_remember t.creation_dacc)
    in
    let all_code = Exported_code.add_code ~keep_code code t.all_code in
    { t with all_code }

let all_code t = t.all_code

let name_occurrences t = t.name_occurrences

let with_name_occurrences t ~name_occurrences =
  if name_occurrences == t.name_occurrences
  then t
  else { t with name_occurrences }

let clear_name_occurrences t =
  with_name_occurrences t ~name_occurrences:Name_occurrences.empty

let add_free_names t free_names =
  let name_occurrences = Name_occurrences.union t.name_occurrences free_names in
  { t with name_occurrences }

let used_value_slots t = DA.used_value_slots t.creation_dacc

let shareable_constants t = DA.shareable_constants t.creation_dacc

let remove_all_occurrences_of_free_names t to_remove =
  let name_occurrences =
    Name_occurrences.diff t.name_occurrences ~without:to_remove
  in
  { t with name_occurrences }

let clear_cost_metrics t = { t with cost_metrics = Cost_metrics.zero }

let with_cost_metrics cost_metrics t = { t with cost_metrics }

let notify_added ~code_size t =
  { t with cost_metrics = Cost_metrics.notify_added ~code_size t.cost_metrics }

let notify_removed ~operation t =
  { t with
    cost_metrics = Cost_metrics.notify_removed ~operation t.cost_metrics
  }

let add_cost_metrics cost_metrics t =
  { t with cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics }

let add_cost_metrics_and_with_name_occurrences t cost_metrics name_occurrences =
  { t with
    cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics;
    name_occurrences
  }

let generate_phantom_lets t = DE.generate_phantom_lets (DA.denv t.creation_dacc)

let is_demoted_exn_handler t cont =
  Continuation.Set.mem cont (DA.demoted_exn_handlers t.creation_dacc)

let slot_offsets t = t.slot_offsets

let with_slot_offsets t slot_offsets = { t with slot_offsets }

let required_names t = t.flow_result.data_flow_result.required_names

let reachable_code_ids t = t.flow_result.data_flow_result.reachable_code_ids

let continuation_param_aliases t = t.flow_result.aliases_result

let mutable_unboxing_result t = t.flow_result.mutable_unboxing_result

let set_resimplify t = { t with resimplify = true }

let resimplify t = t.resimplify

let specialization_map t = DA.specialization_map (creation_dacc t)
