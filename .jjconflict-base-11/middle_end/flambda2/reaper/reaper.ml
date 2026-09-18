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
        applications : Traverse_acc.Applications.t
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
      { analysis : Analysis.result;
        code_changes : Unboxing_analysis.code_changes;
        slot_offsets : Slot_offsets.result
      }
  end

  let traverse ~free_names ~cmx_loader ~all_code unit =
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
            all_sets_of_closures = _;
            closure_function_decls
          } =
      Traverse.run unit
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

  let solve
      ({ deps;
         slot_offsets_inputs;
         code_deps;
         code_references;
         le_monde_exterieur;
         applications
       } :
        Solve_inputs.t) =
    Cross_unit_calls.link deps ~code_deps ~le_monde_exterieur code_references;
    let solved_dep, analysis =
      Profile.record_call ~accumulate:true "solver" (fun () ->
          Analysis.fixpoint deps ~applications)
    in
    let () =
      if Flambda_features.debug_reaper "print-solved"
      then (
        Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
        Dot_printer.print_solved_dep solved_dep deps)
    in
    let code_changes =
      Unboxing_analysis.compute_code_changes solved_dep
        ~rewrite_kind_with_subkind:(fun _name kind ->
          Types_rewriter.erase_subkind kind)
        ~rewrite_result_types:(fun ~my_closure:_ ~params:_ ~results:_ _types ->
          Or_unknown_or_bottom.Unknown)
        ~code_deps
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~code_changes
        solved_dep
    in
    Solution.{ analysis; code_changes; slot_offsets }

  let rebuild ~unit ~rebuild_inputs ~solution ~machine_width ~cmx_loader
      ~all_code =
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
    let Solution.{ analysis; code_changes; slot_offsets } = solution in
    let Rebuild.{ body; all_code; code_ids_to_remember } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids
        ~fixed_arity_continuations ~continuation_info ~final_typing_env:None
        ~rewrite_kind_with_subkind:(fun _ kind ->
          Types_rewriter.erase_subkind kind)
        ~code_changes analysis get_code_metadata toplevel_expr code
    in
    let all_code =
      Exported_code.add_code
        ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
        all_code
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
    in
    Flambda_unit.with_body unit body, all_code, slot_offsets
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
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
    Traverse.run unit
  in
  Cross_unit_calls.link deps ~code_deps ~le_monde_exterieur code_references;
  let solved_dep, uses =
    Profile.record_call ~accumulate:true "solver" (fun () ->
        Analysis.fixpoint deps ~applications)
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
  let slot_offsets_inputs =
    Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
      ~code_deps ~get_code_metadata
  in
  let slot_offsets =
    Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~code_changes
      solved_dep
  in
  let Rebuild.{ body; all_code; code_ids_to_remember } =
    Rebuild.rebuild ~machine_width ~ordered_code_ids ~fixed_arity_continuations
      ~continuation_info ~final_typing_env
      ~rewrite_kind_with_subkind:
        (Types_rewriter.rewrite_kind_with_subkind types_rewrite_context)
      ~code_changes uses get_code_metadata toplevel_expr code
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
