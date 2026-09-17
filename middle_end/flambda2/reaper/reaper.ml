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

let get_code_metadata ~cmx_loader ~all_code =
  let load_code = Flambda_cmx.get_imported_code cmx_loader in
  fun code_id ->
    Code_or_metadata.code_metadata
      (match Exported_code.find all_code code_id with
      | Some code -> code
      | None -> Exported_code.find_exn (load_code ()) code_id)

(* Import the .cmx file defining [code_id] if its metadata is not already
   available. Only the LTO rebuild gets here: it does not run [Simplify], so the
   metadata of code from units that did not take part in the solve is only
   available from their .cmx files. The metadata of the units in the analysis
   scope comes from the solution instead, and their .cmx files must not be
   read. *)
let load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader ~all_code
    code_id =
  let comp_unit = Code_id.get_compilation_unit code_id in
  if Analysis_scope.contains_unit analysis_scope comp_unit
  then
    Misc.fatal_errorf
      "Code ID %a belongs to the analysis scope, so its metadata must come \
       from the solution"
      Code_id.print code_id;
  if
    (not (Exported_code.mem code_id all_code))
    && not
         (Exported_code.mem code_id
            (Flambda_cmx.get_imported_code cmx_loader ()))
  then
    ignore
      (Flambda_cmx.load_cmx_file_contents cmx_loader comp_unit
        : Typing_env.Serializable.t option)

let get_code_metadata_or_load ~analysis_scope ~cmx_loader ~all_code code_id =
  (match Exported_code.find all_code code_id with
  | Some _ -> ()
  | None ->
    load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader ~all_code
      code_id);
  get_code_metadata ~cmx_loader ~all_code code_id

module Staged = struct
  module Solve_inputs = struct
    type t =
      { deps : Global_flow_graph.graph;
        slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
        code_deps : Traverse_acc.code_dep Code_id.Map.t;
        code_references : Traverse_acc.code_reference list;
        rebuild_queries : Rebuild_queries.Requests.t;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
      }

    let ids_for_export
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          rebuild_queries;
          all_sets_of_closures
        } =
      let ids =
        Code_id.Map.fold
          (fun code_id code_dep ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Traverse_acc.ids_for_export_code_dep code_dep))
          code_deps Ids_for_export.empty
      in
      let ids =
        List.fold_left
          (fun ids set_of_closures ->
            Function_slot.Lmap.fold
              (fun _function_slot (name, code_id) ids ->
                let ids = Ids_for_export.add_name ids name in
                match (code_id : _ Or_unknown.t) with
                | Unknown -> ids
                | Known code_id -> Ids_for_export.add_code_id ids code_id)
              set_of_closures ids)
          ids all_sets_of_closures
      in
      Ids_for_export.union_list
        [ ids;
          Global_flow_graph.ids_for_export deps;
          Slot_offsets_analysis.Inputs.ids_for_export slot_offsets_inputs;
          Traverse_acc.ids_for_export_code_references code_references;
          Rebuild_queries.Requests.ids_for_export rebuild_queries ]

    let prune_for_lto t =
      { t with
        code_deps =
          Code_id.Map.map
            (fun (code_dep : Traverse_acc.code_dep) ->
              { code_dep with
                code_metadata =
                  Code_metadata.with_result_types Unknown code_dep.code_metadata
              })
            t.code_deps;
        all_sets_of_closures = []
      }

    let fields_for_export t = Global_flow_graph.fields_for_export t.deps

    let referenced_compilation_units t =
      Traverse_acc.code_references_compilation_units t.code_references

    let apply_renaming
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          rebuild_queries;
          all_sets_of_closures
        } renaming ~rename_field =
      let code_deps =
        Code_id.Map.fold
          (fun code_id code_dep map ->
            Code_id.Map.add
              (Renaming.apply_code_id renaming code_id)
              (Traverse_acc.apply_renaming_code_dep code_dep renaming)
              map)
          code_deps Code_id.Map.empty
      in
      let all_sets_of_closures =
        List.map
          (Function_slot.Lmap.map (fun (name, code_id) ->
               ( Renaming.apply_name renaming name,
                 Or_unknown.map code_id ~f:(Renaming.apply_code_id renaming) )))
          all_sets_of_closures
      in
      { deps = Global_flow_graph.apply_renaming deps renaming ~rename_field;
        slot_offsets_inputs =
          Slot_offsets_analysis.Inputs.apply_renaming slot_offsets_inputs
            renaming;
        code_deps;
        code_references =
          Traverse_acc.apply_renaming_code_references code_references renaming;
        rebuild_queries =
          Rebuild_queries.Requests.apply_renaming rebuild_queries renaming;
        all_sets_of_closures
      }
  end

  module Rebuild_inputs = struct
    type t =
      { toplevel_expr : Rev_expr.t;
        code : Rev_expr.rev_code Code_id.Map.t;
        ordered_code_ids : Code_id.t array;
        fixed_arity_continuations : Continuation.Set.t;
        continuation_info : Traverse_acc.continuation_info Continuation.Map.t
      }

    let ids_for_export
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info
        } =
      let ids = Rev_expr.ids_for_export toplevel_expr in
      let ids =
        Code_id.Map.fold
          (fun code_id rev_code ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Rev_expr.ids_for_export_code rev_code))
          code ids
      in
      let ids =
        Array.fold_left Ids_for_export.add_code_id ids ordered_code_ids
      in
      let ids =
        Continuation.Set.fold
          (fun cont ids -> Ids_for_export.add_continuation ids cont)
          fixed_arity_continuations ids
      in
      Continuation.Map.fold
        (fun cont info ids ->
          Ids_for_export.union
            (Ids_for_export.add_continuation ids cont)
            (Traverse_acc.ids_for_export_continuation_info info))
        continuation_info ids

    let apply_renaming
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info
        } renaming =
      let toplevel_expr' = Rev_expr.apply_renaming toplevel_expr renaming in
      let code' =
        Code_id.Map.fold
          (fun code_id rev_code code ->
            Code_id.Map.add
              (Renaming.apply_code_id renaming code_id)
              (Rev_expr.apply_renaming_code rev_code renaming)
              code)
          code Code_id.Map.empty
      in
      let ordered_code_ids' =
        Array.map (Renaming.apply_code_id renaming) ordered_code_ids
      in
      let fixed_arity_continuations' =
        Continuation.Set.fold
          (fun cont conts ->
            Continuation.Set.add
              (Renaming.apply_continuation renaming cont)
              conts)
          fixed_arity_continuations Continuation.Set.empty
      in
      let continuation_info' =
        Continuation.Map.fold
          (fun cont info map ->
            Continuation.Map.add
              (Renaming.apply_continuation renaming cont)
              (Traverse_acc.apply_renaming_continuation_info info renaming)
              map)
          continuation_info Continuation.Map.empty
      in
      { toplevel_expr = toplevel_expr';
        code = code';
        ordered_code_ids = ordered_code_ids';
        fixed_arity_continuations = fixed_arity_continuations';
        continuation_info = continuation_info'
      }
  end

  module Solution = struct
    type t =
      { solved_dep : Analysis.result;
        code_changes : Unboxing_analysis.code_changes;
        queries : Rebuild_queries.t;
        slot_offsets : Slot_offsets.result
      }

    let rebuild_data { solved_dep; code_changes; queries; slot_offsets } =
      Rebuild_solution.create_data ~queries ~unboxing:solved_dep ~code_changes
        ~slot_offsets:slot_offsets.exported_offsets
  end

  let traverse ~free_names ~cmx_loader ~all_code ~top_level_return_escapes unit
      =
    let Traverse.
          { toplevel_expr;
            code;
            ordered_code_ids;
            deps;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            code_references;
            rebuild_queries;
            all_sets_of_closures;
            closure_function_decls
          } =
      Traverse.run ~top_level_return_escapes unit
    in
    let slot_offsets_inputs =
      Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
        ~code_deps
        ~get_code_metadata:(get_code_metadata ~cmx_loader ~all_code)
    in
    let solve_inputs =
      Solve_inputs.
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          rebuild_queries;
          all_sets_of_closures
        }
    in
    let rebuild_inputs =
      { Rebuild_inputs.toplevel_expr;
        code;
        ordered_code_ids;
        fixed_arity_continuations;
        continuation_info
      }
    in
    solve_inputs, rebuild_inputs

  let link_code_references ~analysis_scope
      ~(code_deps : Traverse_acc.code_dep Code_id.Map.t) ~solve_inputs deps =
    let module Graph = Global_flow_graph in
    let add_alias_for_caller graph ~caller ~from ~to_ =
      match caller with
      | None -> Graph.add_alias graph ~from ~to_
      | Some code_id ->
        Graph.add_propagate_dep graph
          ~if_used:(Code_id_or_name.code_id code_id)
          ~from ~to_
    in
    let find_in_scope code_id =
      if
        not
          (Analysis_scope.contains_unit analysis_scope
             (Code_id.get_compilation_unit code_id))
      then None
      else
        match Code_id.Map.find_opt code_id code_deps with
        | Some code_dep -> Some code_dep
        | None ->
          Misc.fatal_errorf "Missing participant code interface %a"
            Code_id.print code_id
    in
    let link_reference = function
      | Traverse_acc.Closure { closure; code_id; external_witness } -> (
        match find_in_scope code_id with
        | Some code_dep ->
          Traverse_acc.connect_closure deps ~closure ~code_id code_dep
        | None ->
          Graph.add_any_source deps external_witness;
          Graph.add_constructor_dep deps ~base:closure
            Field.known_arity_call_witness ~from:external_witness;
          Graph.add_constructor_dep deps ~base:closure
            Field.unknown_arity_call_witness ~from:external_witness;
          Graph.add_constructor_dep deps ~base:external_witness
            Field.code_id_of_call_witness ~from:closure)
      | Traverse_acc.Direct_call
          { call;
            code_id;
            closure;
            caller;
            external_call;
            external_closure;
            external_world
          } -> (
        match find_in_scope code_id with
        | Some code_dep ->
          add_alias_for_caller deps ~caller ~to_:call
            ~from:code_dep.known_arity_call_witness;
          Option.iter
            (fun closure ->
              add_alias_for_caller deps ~caller ~from:closure
                ~to_:(Code_id_or_name.var code_dep.my_closure))
            closure
        | None ->
          (match caller with
          | None -> Graph.add_any_source deps external_call
          | Some caller ->
            Graph.add_propagate_dep deps
              ~if_used:(Code_id_or_name.code_id caller)
              ~to_:external_call ~from:external_world);
          Option.iter
            (fun closure ->
              match caller with
              | None -> Graph.add_any_usage deps closure
              | Some caller ->
                Graph.add_use_dep deps
                  ~to_:(Code_id_or_name.code_id caller)
                  ~from:closure)
            external_closure)
    in
    List.iter
      (fun (inputs : Solve_inputs.t) ->
        List.iter link_reference inputs.code_references)
      solve_inputs

  let solve ~analysis_scope (solve_inputs : Solve_inputs.t list) =
    let deps =
      match solve_inputs with
      | [] -> Global_flow_graph.create ()
      | first :: rest ->
        List.fold_left
          (fun deps (inputs : Solve_inputs.t) ->
            Global_flow_graph.union deps inputs.deps)
          first.deps rest
    in
    let slot_offsets_inputs =
      List.fold_left
        (fun combined (inputs : Solve_inputs.t) ->
          Slot_offsets_analysis.Inputs.union combined inputs.slot_offsets_inputs)
        Slot_offsets_analysis.Inputs.empty solve_inputs
    in
    let code_deps =
      List.fold_left
        (fun code_deps (inputs : Solve_inputs.t) ->
          Code_id.Map.disjoint_union code_deps inputs.code_deps)
        Code_id.Map.empty solve_inputs
    in
    let requests =
      List.fold_left
        (fun requests (inputs : Solve_inputs.t) ->
          Rebuild_queries.Requests.union requests inputs.rebuild_queries)
        Rebuild_queries.Requests.empty solve_inputs
    in
    link_code_references ~analysis_scope ~code_deps ~solve_inputs deps;
    let solved_dep =
      Profile.record_call ~accumulate:true "solver" (fun () ->
          Analysis.fixpoint deps ~analysis_scope)
    in
    let () =
      if Flambda_features.debug_reaper "print-solved"
      then (
        Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
        Dot_printer.print_solved_dep solved_dep deps)
    in
    let code_changes =
      Unboxing_analysis.compute_code_changes solved_dep ~analysis_scope
        ~rewrite_kind_with_subkind:
          (Types_rewriter.rewrite_kind_with_subkind solved_dep.db)
        ~code_deps
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~analysis_scope
        ~code_changes solved_dep
    in
    let queries = Rebuild_queries.create solved_dep.db ~requests in
    Solution.{ solved_dep; code_changes; queries; slot_offsets }

  let rebuild ~unit_metadata ~rebuild_inputs ~(solution : Rebuild_solution.t)
      ~(typing : Rebuild.typing option) ~machine_width ~cmx_loader ~all_code =
    let analysis_scope = Rebuild_solution.analysis_scope solution in
    let get_code_metadata =
      get_code_metadata_or_load ~analysis_scope ~cmx_loader ~all_code
    in
    let Rebuild_inputs.
          { toplevel_expr;
            code;
            ordered_code_ids;
            fixed_arity_continuations;
            continuation_info
          } =
      rebuild_inputs
    in
    let Rebuild.
          { body; all_code = rebuilt_code; code_ids_to_remember; free_names } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids
        ~fixed_arity_continuations ~continuation_info ~typing solution
        get_code_metadata toplevel_expr code
    in
    let is_foreign code_id =
      not (Current_unit.is_current (Code_id.get_compilation_unit code_id))
    in
    (* The metadata of foreign code comes from the solution for units that took
       part in the solve, and from their .cmx files otherwise. *)
    let solution_metadata =
      Name_occurrences.fold_code_ids free_names ~init:[]
        ~f:(fun solution_metadata code_id ->
          if not (is_foreign code_id)
          then solution_metadata
          else
            match Rebuild_solution.find_code_metadata solution code_id with
            | Some code_metadata -> code_metadata :: solution_metadata
            | None ->
              load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader
                ~all_code code_id;
              solution_metadata)
    in
    (* Local entries are replaced by rebuilt code below. *)
    let imported_code =
      Exported_code.merge
        (Exported_code.mark_as_imported all_code)
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
      |> Exported_code.filter ~f:is_foreign
    in
    let imported_code =
      List.fold_left Exported_code.add_code_metadata imported_code
        solution_metadata
    in
    let all_code =
      Exported_code.add_code
        ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
        rebuilt_code imported_code
    in
    let final_typing_env =
      match typing with
      | None -> None
      | Some typing ->
        Option.map
          (fun typing_env ->
            Types_rewriter.rewrite_typing_env typing.context
              ~unit_symbol:(Flambda_unit.Metadata.module_symbol unit_metadata)
              typing_env)
          typing.env
    in
    ( Flambda_unit.create_of_metadata_and_body unit_metadata body,
      all_code,
      final_typing_env,
      free_names )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let solve_inputs, rebuild_inputs =
    Staged.traverse ~free_names ~cmx_loader ~all_code
      ~top_level_return_escapes:true unit
  in
  let Staged.Solution.{ solved_dep; code_changes; queries; slot_offsets } =
    Staged.solve ~analysis_scope:Current_unit [solve_inputs]
  in
  let typing =
    Rebuild.
      { context =
          Types_rewriter.prepare_rewrite_context solved_dep
            solve_inputs.Staged.Solve_inputs.all_sets_of_closures;
        code_deps = solve_inputs.Staged.Solve_inputs.code_deps;
        env = final_typing_env
      }
  in
  let solution =
    Rebuild_solution.create ~analysis_scope:Current_unit ~queries
      ~unboxing:solved_dep ~code_changes
      ~slot_offsets:slot_offsets.exported_offsets
  in
  let flambda, all_code, final_typing_env, free_names =
    Staged.rebuild
      ~unit_metadata:(Flambda_unit.metadata unit)
      ~rebuild_inputs ~solution ~typing:(Some typing) ~machine_width ~cmx_loader
      ~all_code
  in
  let exported_offsets =
    Rebuild_solution.offsets_for_free_names solution free_names
  in
  ( flambda,
    all_code,
    { slot_offsets with Slot_offsets.exported_offsets },
    final_typing_env )
