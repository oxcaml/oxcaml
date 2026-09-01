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

module Staged = struct
  module Traverse_rebuild = struct
    type t =
      { toplevel_expr : Rev_expr.t;
        code : Rev_expr.rev_code Code_id.Map.t;
        ordered_code_ids : Code_id.t array;
        fixed_arity_continuations : Continuation.Set.t;
        continuation_info : Traverse_acc.continuation_info Continuation.Map.t;
        code_deps : Traverse_acc.code_dep Code_id.Map.t;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
      }

    let ids_for_export
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          all_sets_of_closures
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
      let ids =
        Continuation.Map.fold
          (fun cont info ids ->
            Ids_for_export.union
              (Ids_for_export.add_continuation ids cont)
              (Traverse_acc.ids_for_export_continuation_info info))
          continuation_info ids
      in
      let ids =
        Code_id.Map.fold
          (fun code_id code_dep ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Traverse_acc.ids_for_export_code_dep code_dep))
          code_deps ids
      in
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

    let apply_renaming
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          all_sets_of_closures
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
      let code_deps' =
        Code_id.Map.fold
          (fun code_id code_dep map ->
            Code_id.Map.add
              (Renaming.apply_code_id renaming code_id)
              (Traverse_acc.apply_renaming_code_dep code_dep renaming)
              map)
          code_deps Code_id.Map.empty
      in
      let all_sets_of_closures' =
        List.map
          (Function_slot.Lmap.map (fun (name, code_id) ->
               ( Renaming.apply_name renaming name,
                 Or_unknown.map code_id ~f:(Renaming.apply_code_id renaming) )))
          all_sets_of_closures
      in
      { toplevel_expr = toplevel_expr';
        code = code';
        ordered_code_ids = ordered_code_ids';
        fixed_arity_continuations = fixed_arity_continuations';
        continuation_info = continuation_info';
        code_deps = code_deps';
        all_sets_of_closures = all_sets_of_closures'
      }

    let map_result_types t ~f =
      (* [code] is the only part of the rebuild data holding Flambda types. *)
      let map_rev_code (rev_code : Rev_expr.rev_code) =
        { rev_code with
          code_metadata =
            Code_metadata.map_result_types rev_code.code_metadata ~f
        }
      in
      { t with code = Code_id.Map.map map_rev_code t.code }
  end

  let traverse unit =
    let Traverse.
          { toplevel_expr;
            code;
            ordered_code_ids;
            deps;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            all_sets_of_closures
          } =
      Traverse.run unit
    in
    let rebuild_data =
      Traverse_rebuild.
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          all_sets_of_closures
        }
    in
    deps, rebuild_data

  let solve deps =
    let solved_dep =
      Profile.record_call ~accumulate:true "solver" (fun () ->
          Analysis.fixpoint deps)
    in
    let () =
      if Flambda_features.debug_reaper "print-solved"
      then (
        Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
        Dot_printer.print_solved_dep solved_dep deps)
    in
    solved_dep

  let rebuild ~unit_metadata ~traverse_rebuild ~solved_dep ~machine_width
      ~cmx_loader ~all_code ~final_typing_env =
    let load_code = Flambda_cmx.get_imported_code cmx_loader in
    let get_code_metadata code_id =
      Code_or_metadata.code_metadata
        (match Exported_code.find all_code code_id with
        | Some code -> code
        | None -> Exported_code.find_exn (load_code ()) code_id)
    in
    let Traverse_rebuild.
          { toplevel_expr;
            code;
            ordered_code_ids;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            all_sets_of_closures
          } =
      traverse_rebuild
    in
    let types_rewrite_context =
      Types_rewriter.prepare_rewrite_context solved_dep all_sets_of_closures
    in
    let Rebuild.
          { body; free_names; all_code; code_ids_to_remember; slot_offsets } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids ~code_deps
        ~fixed_arity_continuations ~continuation_info ~final_typing_env
        ~types_rewrite_context solved_dep get_code_metadata toplevel_expr code
    in
    let all_code =
      Exported_code.add_code
        ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
        all_code
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
    in
    let final_typing_env =
      Option.map
        (Types_rewriter.rewrite_typing_env types_rewrite_context
           ~unit_symbol:(Flambda_unit.Metadata.module_symbol unit_metadata))
        final_typing_env
    in
    ( Flambda_unit.create_of_metadata_and_body unit_metadata body,
      free_names,
      all_code,
      slot_offsets,
      final_typing_env )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env
    (unit : Flambda_unit.t) =
  let deps, traverse_rebuild = Staged.traverse unit in
  let solved_dep = Staged.solve deps in
  let unit_metadata = Flambda_unit.metadata unit in
  Staged.rebuild ~unit_metadata ~traverse_rebuild ~solved_dep ~machine_width
    ~cmx_loader ~all_code ~final_typing_env
