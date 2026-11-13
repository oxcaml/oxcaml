(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
open! Simplify_import

type simplify_result =
  { free_names : Name_occurrences.t;
    final_typing_env : Typing_env.t;
    all_code : Exported_code.t;
    slot_offsets : Slot_offsets.t;
    unit : Flambda_unit.t
  }

(* The code of an instantiation of a template is simplified at the template's
   definition. If the definition was never reached, because it is in dead code,
   the code can still be referenced (for example by the code of another
   instantiation), but is never called: an instantiation only ever receives
   environments created by the definition. *)
let bind_code_of_unreached_instantiations dacc body name_occurrences all_code =
  let code_ids =
    Code_id.Set.inter
      (DA.instantiations_awaiting_simplification dacc)
      (NO.code_ids name_occurrences)
  in
  if Code_id.Set.is_empty code_ids
  then body, name_occurrences, all_code
  else
    let unreachable_code code_id =
      let code = DA.awaiting_instantiation_code dacc code_id in
      let params_and_body =
        Function_params_and_body.pattern_match (Code.params_and_body code)
          ~f:(fun
              ~return_continuation
              ~exn_continuation
              params
              ~body:_
              ~my_closure
              ~is_my_closure_used:_
              ~my_alloc_mode
              ~my_depth
              ~free_names_of_body:_
            ->
            Function_params_and_body.create ~return_continuation
              ~exn_continuation params
              ~body:
                (Expr.create_invalid
                   (Message
                      "The definition of this instantiation's template is \
                       never reached"))
              ~free_names_of_body:(Known NO.empty) ~my_closure ~my_alloc_mode
              ~my_depth)
      in
      Code.with_params_and_body ~params_and_body
        ~free_names_of_params_and_body:NO.empty ~cost_metrics:Cost_metrics.zero
        code
    in
    let code = Code_id.Map.of_set unreachable_code code_ids in
    let body =
      Let.create
        (Bound_pattern.static
           (Bound_static.create
              (List.map Bound_static.Pattern.code
                 (Code_id.Set.elements code_ids))))
        (Named.create_static_consts
           (Static_const_group.create
              (List.map Static_const_or_code.create_code (Code_id.Map.data code))))
        ~body ~free_names_of_body:(Known name_occurrences)
      |> Expr.create_let
    in
    let name_occurrences =
      Code_id.Set.fold
        (fun code_id name_occurrences ->
          NO.remove_code_id_or_symbol name_occurrences
            ~code_id_or_symbol:(Code_id_or_symbol.create_code_id code_id))
        code_ids name_occurrences
    in
    ( body,
      name_occurrences,
      Exported_code.add_code ~keep_code:(fun _ -> true) code all_code )

let run ~cmx_loader ~machine_width ~round ~code_slot_offsets unit =
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let toplevel_my_alloc_region = FU.toplevel_my_alloc_region unit in
  let module_symbol = FU.module_symbol unit in
  let resolver = Flambda_cmx.load_cmx_file_contents cmx_loader in
  let get_imported_code = Flambda_cmx.get_imported_code cmx_loader in
  let denv =
    DE.create ~round ~machine_width ~resolver ~get_imported_code
      ~propagating_float_consts:(Flambda_features.float_const_prop ())
      ~unit_toplevel_return_continuation:return_continuation
      ~unit_toplevel_exn_continuation:exn_continuation ~toplevel_my_alloc_region
  in
  (* CR gbury: only compute closure offsets if this is the last round. (same
     remark for the cmx contents) *)
  let dacc = DA.create denv code_slot_offsets Continuation_uses_env.empty in
  let body, uacc =
    Simplify_expr.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:(Flambda_arity.create_singletons [K.With_subkind.any_value])
      ~exn_continuation
  in
  let body = Rebuilt_expr.to_expr body (UA.are_rebuilding_terms uacc) in
  let name_occurrences = UA.name_occurrences uacc in
  NO.fold_names name_occurrences ~init:() ~f:(fun () name ->
      Name.pattern_match name
        ~var:(fun var ->
          if not (Variable.equal var toplevel_my_alloc_region)
          then
            Misc.fatal_errorf
              "Variable %a not expected to be free in whole-compilation-unit \
               term:@ %a"
              Variable.print var Expr.print body)
        ~symbol:(fun _symbol -> ()));
  let body, name_occurrences, all_code =
    bind_code_of_unreached_instantiations (UA.creation_dacc uacc) body
      name_occurrences (UA.all_code uacc)
  in
  if not (LCS.is_empty (UA.lifted_constants uacc))
  then
    Misc.fatal_errorf "Lifted constants accumulator should be empty:@ %a"
      UA.print uacc;
  let final_typing_env =
    let cont_uses_env = DA.continuation_uses_env (UA.creation_dacc uacc) in
    match
      Continuation_uses_env.get_typing_env_no_more_than_one_use cont_uses_env
        return_continuation
    with
    | Some final_typing_env -> final_typing_env
    | None ->
      (* Create an unknown typing_env in case the module never returns *)
      TE.add_symbol_definition
        (TE.create ~machine_width ~resolver)
        module_symbol
  in
  let all_code =
    Exported_code.merge all_code
      (Exported_code.mark_as_imported (get_imported_code ()))
  in
  let slot_offsets =
    match UA.slot_offsets uacc with
    | Unknown ->
      Misc.fatal_error "Slot offsets must be computed and cannot be unknown"
    | Known slot_offsets -> slot_offsets
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~toplevel_my_alloc_region
      ~module_symbol ~body
  in
  { unit;
    free_names = name_occurrences;
    final_typing_env;
    all_code;
    slot_offsets
  }
