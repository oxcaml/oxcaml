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

(* We use unit maps instead of sets, because it allows reuse of the tables
   stored in the Datalog database without copying. *)
type result =
  { has_usage : unit Code_id_or_name.Map.t;
    has_source : unit Code_id_or_name.Map.t;
    field_of_constructor_is_used : unit Field.Map.t Code_id_or_name.Map.t;
    directly_called : Code_id.Set.t Or_unknown.t Code_id_or_name.Map.t;
    known_masks : PTA.keep_or_delete list Code_id_or_name.Map.t;
    unknown_masks : PTA.keep_or_delete list list Code_id_or_name.Map.t;
    unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
    changed_representation :
      (UA.changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

let answer_call_queries db (applications : Traverse_acc.Applications.t) result =
  let dummy_args width = List.init width (fun _ -> ()) in
  Code_id_or_name.Map.fold
    (fun callee ({ known; unknown } : Traverse_acc.Applications.bounds) result
       ->
      let result =
        match known with
        | None -> result
        | Some width ->
          let name =
            Code_id_or_name.pattern_match' callee
              ~name:(fun name -> name)
              ~code_id:(fun _ ->
                Misc.fatal_errorf "Analysis: expected a named callee, found %a"
                  Code_id_or_name.print callee)
          in
          let directly_called = PTA.code_id_actually_directly_called db name in
          let mask =
            PTA.arguments_used_by_known_arity_call db callee (dummy_args width)
            |> List.map snd
          in
          { result with
            directly_called =
              Code_id_or_name.Map.add callee directly_called
                result.directly_called;
            known_masks = Code_id_or_name.Map.add callee mask result.known_masks
          }
      in
      match unknown with
      | None -> result
      | Some widths ->
        let masks =
          PTA.arguments_used_by_unknown_arity_call db callee
            (List.map dummy_args widths)
          |> List.map (List.map snd)
        in
        { result with
          unknown_masks =
            Code_id_or_name.Map.add callee masks result.unknown_masks
        })
    applications result

let fixpoint (graph : Global_flow_graph.graph) ~applications =
  let datalog = Global_flow_graph.to_datalog graph in
  let with_provenance = Flambda_features.debug_reaper "prov" in
  let stats = Datalog.Schedule.create_stats ~with_provenance datalog in
  let db = PTA.perform_analysis datalog ~stats in
  let (unboxing : UA.result) = UA.perform_analysis db ~stats in
  if with_provenance || Flambda_features.debug_reaper "stats"
  then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Flambda_features.debug_reaper "db"
  then Format.eprintf "%a@." Datalog.print db;
  let result =
    { has_usage = Datalog.get_table PTA.Relations.has_usage_tbl db;
      has_source = Datalog.get_table PTA.Relations.has_source_tbl db;
      field_of_constructor_is_used =
        Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl db;
      directly_called = Code_id_or_name.Map.empty;
      known_masks = Code_id_or_name.Map.empty;
      unknown_masks = Code_id_or_name.Map.empty;
      unboxed_fields = unboxing.unboxed_fields;
      changed_representation = unboxing.changed_representation
    }
  in
  unboxing, answer_call_queries db applications result

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Option.map fst (Code_id_or_name.Map.find_opt cn uses.changed_representation)

let has_use uses v = Code_id_or_name.Map.mem v uses.has_usage

let field_used uses v f =
  match Code_id_or_name.Map.find_opt v uses.field_of_constructor_is_used with
  | None -> false
  | Some fields -> Field.Map.mem f fields

let find_answer map callee query =
  match Code_id_or_name.Map.find_opt callee map with
  | Some answer -> answer
  | None ->
    Misc.fatal_errorf "Analysis: no %s request for callee %a" query
      Code_id_or_name.print callee

let code_id_actually_directly_called uses closure =
  find_answer uses.directly_called
    (Code_id_or_name.name closure)
    "direct-call targets"

let rec apply_mask callee query mask args =
  match args, mask with
  | [], _ -> []
  | arg :: args, keep :: mask ->
    (arg, keep) :: apply_mask callee query mask args
  | _ :: _, [] ->
    Misc.fatal_errorf "Analysis: insufficient %s argument width for callee %a"
      query Code_id_or_name.print callee

let arguments_used_by_known_arity_call uses callee args =
  let mask = find_answer uses.known_masks callee "known-arity" in
  apply_mask callee "known-arity" mask args

let arguments_used_by_unknown_arity_call uses callee args =
  let masks = find_answer uses.unknown_masks callee "unknown-arity" in
  let rec apply_groups masks args =
    match args, masks with
    | [], _ -> []
    | args :: rest, mask :: masks ->
      apply_mask callee "unknown-arity" mask args :: apply_groups masks rest
    | _ :: _, [] ->
      Misc.fatal_errorf
        "Analysis: insufficient unknown-arity argument groups for callee %a"
        Code_id_or_name.print callee
  in
  apply_groups masks args

let has_source uses v = Code_id_or_name.Map.mem v uses.has_source
