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

let make_exported_code ~code_ids_to_remember ~all_code ~cmx_loader =
  Exported_code.add_code
    ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
    all_code
    (Exported_code.mark_as_imported
       (Flambda_cmx.get_imported_code cmx_loader ()))

module For_lto = struct
  module Solve_inputs = struct
    type t =
      { deps : Global_flow_graph.graph;
        free_names : Name_occurrences.t;
        code_deps : Traverse_acc.code_dep Code_id.Map.t;
        delayed_deps : Traverse_acc.delayed_deps;
        le_monde_exterieur : Symbol.t;
        applications : Traverse_acc.Applications.t
      }

    let ids_for_export
        { deps;
          free_names;
          code_deps;
          delayed_deps;
          le_monde_exterieur;
          applications
        } =
      let ids =
        Code_id.Map.fold
          (fun code_id code_dep ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Traverse_acc.ids_for_export_code_dep code_dep))
          code_deps Ids_for_export.empty
      in
      Ids_for_export.union_list
        [ Ids_for_export.add_symbol ids le_monde_exterieur;
          Global_flow_graph.ids_for_export deps;
          Name_occurrences.ids_for_export free_names;
          Traverse_acc.ids_for_export_delayed_deps delayed_deps;
          Traverse_acc.Applications.ids_for_export applications ]

    let prune_for_lto t =
      { t with
        code_deps =
          Code_id.Map.map
            (fun (code_dep : Traverse_acc.code_dep) ->
              { code_dep with
                code_metadata =
                  Code_metadata.with_result_types Unknown code_dep.code_metadata
              })
            t.code_deps
      }

    let fields_for_export t = Global_flow_graph.fields_for_export t.deps

    let referenced_compilation_units t =
      Traverse_acc.delayed_deps_compilation_units t.delayed_deps

    let apply_renaming
        { deps;
          free_names;
          code_deps;
          delayed_deps;
          le_monde_exterieur;
          applications
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
      { deps = Global_flow_graph.apply_renaming deps renaming ~rename_field;
        free_names = Name_occurrences.apply_renaming free_names renaming;
        code_deps;
        delayed_deps =
          Traverse_acc.apply_renaming_delayed_deps delayed_deps renaming;
        le_monde_exterieur = Renaming.apply_symbol renaming le_monde_exterieur;
        applications =
          Traverse_acc.Applications.apply_renaming applications renaming
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
      { analysis : Analysis.result;
        code_changes : Unboxing_analysis.code_changes;
        slot_offsets : Slot_offsets.result
      }
  end

  let traverse ~free_names unit =
    let { Traverse.toplevel_expr;
          code;
          ordered_code_ids;
          deps;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          delayed_deps;
          le_monde_exterieur;
          applications;
          all_sets_of_closures = _
        } =
      Traverse.run ~top_level_return_escapes:false unit ~free_names
    in
    let solve_inputs =
      { Solve_inputs.deps;
        free_names;
        code_deps;
        delayed_deps;
        le_monde_exterieur;
        applications
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

  let solve ~analysis_scope
      { Solve_inputs.deps;
        free_names;
        code_deps;
        delayed_deps;
        le_monde_exterieur;
        applications
      } =
    Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps
      ~le_monde_exterieur delayed_deps;
    let solved_dep, analysis =
      Analysis.fixpoint deps ~applications ~analysis_scope
    in
    let code_changes =
      Unboxing_analysis.compute_code_changes solved_dep ~analysis_scope
        ~rewrite_kind_with_subkind:(fun _name kind ->
          Types_rewriter.erase_subkind kind)
        ~rewrite_result_types:(fun ~my_closure:_ ~params:_ ~results:_ _types ->
          Or_unknown_or_bottom.Unknown)
        ~code_deps
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~free_names ~analysis_scope solved_dep
    in
    { Solution.analysis; code_changes; slot_offsets }

  let rebuild ~unit_metadata ~rebuild_inputs ~solution ~machine_width
      ~cmx_loader ~all_code =
    let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
    let { Rebuild_inputs.toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info
        } =
      rebuild_inputs
    in
    let { Solution.analysis; code_changes; slot_offsets } = solution in
    let Rebuild.{ body; all_code; code_ids_to_remember } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids
        ~fixed_arity_continuations ~continuation_info ~final_typing_env:None
        ~rewrite_kind_with_subkind:(fun _ kind ->
          Types_rewriter.erase_subkind kind)
        ~code_changes analysis get_code_metadata toplevel_expr code
    in
    let all_code =
      make_exported_code ~code_ids_to_remember ~all_code ~cmx_loader
    in
    ( Flambda_unit.create_of_metadata_and_body unit_metadata body,
      all_code,
      slot_offsets )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
  let analysis_scope = Analysis_scope.Current_unit in
  let Traverse.
        { toplevel_expr;
          code;
          ordered_code_ids;
          deps;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          delayed_deps;
          le_monde_exterieur;
          applications;
          all_sets_of_closures
        } =
    Traverse.run ~top_level_return_escapes:true unit ~free_names
  in
  Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps
    ~le_monde_exterieur delayed_deps;
  let solved_dep, uses = Analysis.fixpoint deps ~applications ~analysis_scope in
  let types_rewrite_context =
    Types_rewriter.prepare_rewrite_context solved_dep all_sets_of_closures
  in
  let code_changes =
    Unboxing_analysis.compute_code_changes solved_dep ~analysis_scope
      ~rewrite_kind_with_subkind:
        (Types_rewriter.rewrite_kind_with_subkind types_rewrite_context)
      ~rewrite_result_types:(fun ~my_closure ~params ~results types ->
        match final_typing_env with
        | None -> Or_unknown_or_bottom.Unknown
        | Some old_typing_env ->
          Or_unknown_or_bottom.Ok
            (Types_rewriter.rewrite_result_types types_rewrite_context
               ~old_typing_env ~my_closure ~params ~results types))
      ~code_deps
  in
  let slot_offsets =
    Slot_offsets_analysis.compute ~free_names ~analysis_scope solved_dep
  in
  let Rebuild.{ body; all_code; code_ids_to_remember } =
    Rebuild.rebuild ~machine_width ~ordered_code_ids ~fixed_arity_continuations
      ~continuation_info ~final_typing_env
      ~rewrite_kind_with_subkind:
        (Types_rewriter.rewrite_kind_with_subkind types_rewrite_context)
      ~code_changes uses get_code_metadata toplevel_expr code
  in
  let all_code =
    make_exported_code ~code_ids_to_remember ~all_code ~cmx_loader
  in
  let final_typing_env =
    Option.map
      (Types_rewriter.rewrite_typing_env types_rewrite_context
         ~unit_symbol:(Flambda_unit.module_symbol unit))
      final_typing_env
  in
  Flambda_unit.with_body unit body, all_code, slot_offsets, final_typing_env
