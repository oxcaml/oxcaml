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

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let load_code = Flambda_cmx.get_imported_code cmx_loader in
  let get_code_metadata code_id =
    Code_or_metadata.code_metadata
      (match Exported_code.find all_code code_id with
      | Some code -> code
      | None -> Exported_code.find_exn (load_code ()) code_id)
  in
  let Traverse.
        { toplevel_expr;
          code;
          ordered_code_ids;
          deps;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          applications;
          all_sets_of_closures;
          closure_function_decls
        } =
    Traverse.run unit
  in
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
  let types_rewrite_context =
    Types_rewriter.prepare_rewrite_context solved_dep all_sets_of_closures
  in
  let code_changes =
    Unboxing_analysis.compute_code_changes solved_dep
      ~rewrite_kind_with_subkind:
        (Types_rewriter.For_solve.rewrite_kind_with_subkind ~db:solved_dep.db)
      ~code_deps
  in
  let slot_offsets_inputs =
    Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
      ~code_deps ~get_code_metadata
  in
  let slot_offsets =
    Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~code_changes
      solved_dep
  in
  let queries = Rebuild_queries.create solved_dep.db ~applications in
  let solution =
    Rebuild_solution.create ~queries ~unboxing:solved_dep ~code_changes
  in
  let Rebuild.{ body; all_code; code_ids_to_remember } =
    Rebuild.rebuild ~machine_width ~code_deps ~ordered_code_ids
      ~fixed_arity_continuations ~continuation_info ~final_typing_env
      ~types_rewrite_context solution get_code_metadata toplevel_expr code
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
         ~unit_symbol:(Flambda_unit.module_symbol unit))
      final_typing_env
  in
  Flambda_unit.with_body unit body, all_code, slot_offsets, final_typing_env
