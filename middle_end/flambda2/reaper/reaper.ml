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
      { deps : Global_flow_graph.graph;
        slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
        code_deps : Traverse_acc.code_dep Code_id.Map.t;
        code_references : Traverse_acc.code_reference list;
        le_monde_exterieur : Symbol.t;
        applications : Rebuild_queries.Applications.t;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
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
  end

  module Solution = struct
    type t =
      { solved_dep : Analysis.result;
        code_changes : Unboxing_analysis.code_changes;
        queries : Rebuild_queries.t;
        slot_offsets : Slot_offsets.result
      }
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
            le_monde_exterieur;
            applications;
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
          le_monde_exterieur;
          applications;
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

  let solve ~analysis_scope
      ({ deps;
         slot_offsets_inputs;
         code_deps;
         code_references;
         le_monde_exterieur;
         applications;
         all_sets_of_closures = _
       } :
        Solve_inputs.t) =
    Cross_unit_calls.link deps ~analysis_scope ~code_deps ~le_monde_exterieur
      code_references;
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
          (Types_rewriter.For_solve.rewrite_kind_with_subkind ~db:solved_dep.db)
        ~code_deps
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~analysis_scope
        ~code_changes solved_dep
    in
    let queries = Rebuild_queries.create solved_dep.db ~applications in
    Solution.{ solved_dep; code_changes; queries; slot_offsets }

  let rebuild ~unit ~rebuild_inputs ~(solution : Rebuild_solution.t)
      ~types_rewrite_context ~code_deps ~final_typing_env ~machine_width
      ~cmx_loader ~all_code =
    let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
    let Rebuild_inputs.
          { toplevel_expr;
            code;
            ordered_code_ids;
            fixed_arity_continuations;
            continuation_info
          } =
      rebuild_inputs
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
    Flambda_unit.with_body unit body, all_code, final_typing_env
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let solve_inputs, rebuild_inputs =
    Staged.traverse ~free_names ~cmx_loader ~all_code
      ~top_level_return_escapes:true unit
  in
  let Staged.Solution.{ solved_dep; code_changes; queries; slot_offsets } =
    Staged.solve ~analysis_scope:Current_unit solve_inputs
  in
  let types_rewrite_context =
    Types_rewriter.prepare_rewrite_context solved_dep
      solve_inputs.Staged.Solve_inputs.all_sets_of_closures
  in
  let solution =
    Rebuild_solution.create ~analysis_scope:Current_unit ~queries
      ~unboxing:solved_dep ~code_changes
  in
  let flambda, all_code, final_typing_env =
    Staged.rebuild ~unit ~rebuild_inputs ~solution ~types_rewrite_context
      ~code_deps:solve_inputs.Staged.Solve_inputs.code_deps ~final_typing_env
      ~machine_width ~cmx_loader ~all_code
  in
  flambda, all_code, slot_offsets, final_typing_env
