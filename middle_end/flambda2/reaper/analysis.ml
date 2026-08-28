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

let slots_used_in_unit uses ~compilation_unit =
  Code_id_or_name.Map.fold
    (fun _base fields acc ->
      Field.Map.fold
        (fun field () ((value_slots, function_slots) as acc) ->
          match Field.view field with
          | Value_slot value_slot
            when Compilation_unit.equal
                   (Value_slot.get_compilation_unit value_slot)
                   compilation_unit ->
            Value_slot.Set.add value_slot value_slots, function_slots
          | Function_slot function_slot
            when Compilation_unit.equal
                   (Function_slot.get_compilation_unit function_slot)
                   compilation_unit ->
            value_slots, Function_slot.Set.add function_slot function_slots
          | Value_slot _ | Function_slot _ | Block _ | Call_witness _ | Is_int
          | Get_tag | Boxed_number _ | Return_of_call _
          | Code_id_of_call_witness ->
            acc)
        fields acc)
    (Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl uses.db)
    (Value_slot.Set.empty, Function_slot.Set.empty)

let used_code_ids_and_symbols_in_unit uses ~compilation_unit =
  Code_id_or_name.Map.fold
    (fun code_id_or_name () roots ->
      Code_id_or_name.pattern_match' code_id_or_name
        ~code_id:(fun code_id ->
          if Code_id.in_compilation_unit code_id compilation_unit
          then Name_occurrences.add_code_id roots code_id Name_mode.normal
          else roots)
        ~name:(fun name ->
          Name.pattern_match name
            ~var:(fun _ -> roots)
            ~symbol:(fun symbol ->
              (* The reaper's synthetic boundary symbols ([le_monde_extérieur]
                 and [all_constants]) belong to the unit but are not real
                 definitions, so they must not become roots. They cannot be
                 recognised by their [any_source] facts: real definitions can
                 carry [any_source] too, because constants (strings, boxed
                 numbers, ...) are aliased to [all_constants] during traversal
                 and [any_source] propagates through aliases. Filtering on
                 [any_source] would drop such constants from the roots even
                 though other units' rebuilt code still references them, causing
                 their symbols to be localised away. *)
              if
                Compilation_unit.equal
                  (Symbol.compilation_unit symbol)
                  compilation_unit
                && not (Global_flow_graph.is_synthetic_boundary_symbol symbol)
              then Name_occurrences.add_symbol roots symbol Name_mode.normal
              else roots)))
    (Datalog.get_table PTA.Relations.has_usage_table uses.db)
    Name_occurrences.empty

let any_usage uses v = PTA.any_usage uses.db v

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
