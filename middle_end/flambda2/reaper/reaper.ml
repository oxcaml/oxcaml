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

module Staged = struct
  module Solve_inputs = struct
    type t =
      { code_deps : Traverse_acc.code_dep Code_id.Map.t;
        code_references : Traverse_acc.code_reference list;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
      }

    let ids_for_export { code_deps; code_references; all_sets_of_closures } =
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
      Ids_for_export.union ids
        (Traverse_acc.ids_for_export_code_references code_references)

    let referenced_compilation_units t =
      Traverse_acc.code_references_compilation_units t.code_references

    let apply_renaming { code_deps; code_references; all_sets_of_closures }
        renaming =
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
      let code_references =
        Traverse_acc.apply_renaming_code_references code_references renaming
      in
      { code_deps; code_references; all_sets_of_closures }

    let map_result_types t ~f =
      (* The code metadata stored in [code_deps] is the only part of the solve
         inputs holding Flambda types. *)
      let map_code_dep (code_dep : Traverse_acc.code_dep) =
        { code_dep with
          code_metadata =
            Code_metadata.map_result_types code_dep.code_metadata ~f
        }
      in
      { t with code_deps = Code_id.Map.map map_code_dep t.code_deps }
  end

  module Traverse_rebuild = struct
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

  type solution =
    { uses : Analysis.result;
      code_changes : Unboxing_analysis.code_changes
    }

  let traverse ~free_names ~cmx_loader ~all_code ~closed_world unit =
    let Traverse.
          { toplevel_expr;
            code;
            ordered_code_ids;
            deps;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            code_references;
            all_sets_of_closures;
            closure_function_decls
          } =
      Traverse.run ~closed_world unit
    in
    let slot_offsets_inputs =
      Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
        ~code_deps
        ~get_code_metadata:(get_code_metadata ~cmx_loader ~all_code)
    in
    let solve_inputs =
      Solve_inputs.{ code_deps; code_references; all_sets_of_closures }
    in
    let rebuild_data =
      { Traverse_rebuild.toplevel_expr;
        code;
        ordered_code_ids;
        fixed_arity_continuations;
        continuation_info
      }
    in
    deps, slot_offsets_inputs, solve_inputs, rebuild_data

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

  let solve ~slot_offsets_inputs ~analysis_scope ~solve_inputs deps =
    let code_deps =
      List.fold_left
        (fun code_deps (inputs : Solve_inputs.t) ->
          Code_id.Map.disjoint_union code_deps inputs.code_deps)
        Code_id.Map.empty solve_inputs
    in
    link_code_references ~analysis_scope ~code_deps ~solve_inputs deps;
    let uses =
      Profile.record_call ~accumulate:true "solver" (fun () ->
          Analysis.fixpoint deps ~analysis_scope)
    in
    let () =
      if Flambda_features.debug_reaper "print-solved"
      then Dot_printer.print_solved_dep uses deps
    in
    let code_changes =
      Unboxing_analysis.compute_code_changes ~db:uses.db uses.unboxing
        ~analysis_scope
        ~rewrite_kind_with_subkind:
          (Types_rewriter.rewrite_kind_with_subkind uses.db)
        ~code_deps
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~analysis_scope
        ~code_changes ~db:uses.db uses.unboxing
    in
    { uses; code_changes }, slot_offsets

  let rebuild ~unit_metadata ~traverse_rebuild ~solution:{ uses; code_changes }
      ~code_deps_for_result_types ~all_sets_of_closures ~machine_width
      ~cmx_loader ~all_code ~final_typing_env =
    let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
    let Traverse_rebuild.
          { toplevel_expr;
            code;
            ordered_code_ids;
            fixed_arity_continuations;
            continuation_info
          } =
      traverse_rebuild
    in
    let types_rewrite_context =
      lazy
        (Types_rewriter.prepare_rewrite_context ~db:uses.db uses.unboxing
           all_sets_of_closures)
    in
    let Rebuild.{ body; all_code = rebuilt_code; code_ids_to_remember } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids
        ~fixed_arity_continuations ~continuation_info ~final_typing_env
        ~types_rewrite_context ~code_changes ~code_deps_for_result_types uses
        get_code_metadata toplevel_expr code
    in
    let is_foreign code_id =
      not (Current_unit.is_current (Code_id.get_compilation_unit code_id))
    in
    (* Retain foreign metadata saved in the CMR even if rebuild never reloads
       its CMX. Local entries are replaced by rebuilt code below. *)
    let imported_code =
      Exported_code.merge
        (Exported_code.mark_as_imported all_code)
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
      |> Exported_code.filter ~f:is_foreign
    in
    let imported_code =
      Unboxing_analysis.fold_code_metadata code_changes ~init:imported_code
        ~f:(fun code_metadata imported_code ->
          if is_foreign (Code_metadata.code_id code_metadata)
          then Exported_code.add_code_metadata imported_code code_metadata
          else imported_code)
    in
    let all_code =
      Exported_code.add_code
        ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
        rebuilt_code imported_code
    in
    let final_typing_env =
      Option.map
        (fun typing_env ->
          Types_rewriter.rewrite_typing_env
            (Lazy.force types_rewrite_context)
            ~unit_symbol:(Flambda_unit.Metadata.module_symbol unit_metadata)
            typing_env)
        final_typing_env
    in
    ( Flambda_unit.create_of_metadata_and_body unit_metadata body,
      all_code,
      final_typing_env )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let deps, slot_offsets_inputs, solve_inputs, traverse_rebuild =
    Staged.traverse ~free_names ~cmx_loader ~all_code ~closed_world:false unit
  in
  let solution, slot_offsets =
    Staged.solve ~slot_offsets_inputs ~analysis_scope:Current_unit
      ~solve_inputs:[solve_inputs] deps
  in
  let unit_metadata = Flambda_unit.metadata unit in
  let flambda, all_code, final_typing_env =
    Staged.rebuild ~unit_metadata ~traverse_rebuild ~solution
      ~code_deps_for_result_types:
        (Some solve_inputs.Staged.Solve_inputs.code_deps)
      ~all_sets_of_closures:
        solve_inputs.Staged.Solve_inputs.all_sets_of_closures ~machine_width
      ~cmx_loader ~all_code ~final_typing_env
  in
  flambda, all_code, slot_offsets, final_typing_env
