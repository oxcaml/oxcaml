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

open! Datalog_helpers.Syntax
module PTA = Points_to_analysis
open! Points_to_analysis.Relations
open Unboxing_analysis

type result = Unboxing_analysis.result

let fixpoint (graph : Global_flow_graph.graph) =
  let datalog = Global_flow_graph.to_datalog graph in
  let with_provenance = Flambda_features.debug_reaper "prov" in
  let stats = Datalog.Schedule.create_stats ~with_provenance datalog in
  let db = Points_to_analysis.perform_analysis datalog ~stats in
  let result = Unboxing_analysis.perform_analysis db ~stats in
  if with_provenance || Flambda_features.debug_reaper "stats"
  then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Flambda_features.debug_reaper "db"
  then Format.eprintf "%a@." Datalog.print db;
  result

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Option.map fst (Code_id_or_name.Map.find_opt cn uses.changed_representation)

let has_use uses v = PTA.has_use uses.db v

let any_usage uses v = PTA.any_usage uses.db v

let get_usages uses v = PTA.get_usages uses.db v

let field_used uses v f = PTA.field_used uses.db v f

let not_local_field_has_source uses v f =
  PTA.not_local_field_has_source uses.db v f

let code_id_actually_directly_called uses closure =
  PTA.code_id_actually_directly_called uses.db closure

let arguments_used_by_known_arity_call uses callee args =
  PTA.arguments_used_by_known_arity_call uses.db callee args

let arguments_used_by_unknown_arity_call uses callee args =
  PTA.arguments_used_by_unknown_arity_call uses.db callee args

let has_source uses v = PTA.has_source_query uses.db v

let any_source uses v = PTA.any_source uses.db v

let cannot_change_calling_convention =
  Unboxing_analysis.cannot_change_calling_convention
