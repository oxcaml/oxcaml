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

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env
    (unit : Flambda_unit.t) =
  let debug_print = Flambda_features.dump_reaper () in
  let load_code = Flambda_cmx.get_imported_code cmx_loader in
  let get_code_metadata code_id =
    Code_or_metadata.code_metadata
      (match Exported_code.find all_code code_id with
      | Some code -> code
      | None -> Exported_code.find_exn (load_code ()) code_id)
  in
  let Traverse.
        { holed;
          deps;
          kinds;
          fixed_arity_continuations;
          continuation_info;
          code_deps
        } =
    Traverse.run unit
  in
  let solved_dep = Dep_solver.fixpoint deps in
  let () =
    if debug_print
    then (
      Format.printf "RESULT@ %a@." Dep_solver.pp_result solved_dep;
      Dot_printer.print_solved_dep solved_dep deps)
  in
  let Rebuild.{ body; free_names; all_code; slot_offsets } =
    Rebuild.rebuild ~machine_width ~code_deps ~fixed_arity_continuations
      ~continuation_info ~final_typing_env kinds solved_dep get_code_metadata
      holed
  in
  (* Is this what we really want? This keeps all the code that has not been
     deleted by this pass to be exported in the cmx. It looks like this does the
     same thing as [Simplify], but on the other hand, we might not want to
     export un-inlinable functions. *)
  let all_code =
    Exported_code.add_code
      ~keep_code:(fun _ -> true)
      all_code
      (Exported_code.mark_as_imported
         (Flambda_cmx.get_imported_code cmx_loader ()))
  in
  let final_typing_env =
    Option.map
      (Dep_solver.rewrite_typing_env solved_dep
         ~unit_symbol:(Flambda_unit.module_symbol unit))
      final_typing_env
  in
  ( Flambda_unit.with_body unit body,
    free_names,
    all_code,
    slot_offsets,
    final_typing_env )
