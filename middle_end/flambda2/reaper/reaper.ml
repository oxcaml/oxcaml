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
  let traverse = Traverse.run

  let solve Traverse.{ deps; _ } =
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

  let rebuild ~unit_metadata ~traverse_result ~solved_dep ~machine_width
      ~cmx_loader ~all_code ~final_typing_env =
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
            deps = _;
            kinds;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            all_sets_of_closures
          } =
      traverse_result
    in
    let types_rewrite_context =
      Types_rewriter.prepare_rewrite_context solved_dep all_sets_of_closures
    in
    let { Rebuild.body;
          free_names;
          all_code;
          code_ids_to_remember;
          slot_offsets
        } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids ~code_deps
        ~fixed_arity_continuations ~continuation_info ~final_typing_env
        ~types_rewrite_context kinds solved_dep get_code_metadata toplevel_expr
        code
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
    ( Flambda_unit.of_metadata_and_body unit_metadata body,
      free_names,
      all_code,
      slot_offsets,
      final_typing_env )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env
    (unit : Flambda_unit.t) =
  let traverse_result = Staged.traverse unit in
  let solved_dep = Staged.solve traverse_result in
  let unit_metadata = Flambda_unit.metadata unit in
  Staged.rebuild ~unit_metadata ~traverse_result ~solved_dep ~machine_width
    ~cmx_loader ~all_code ~final_typing_env
