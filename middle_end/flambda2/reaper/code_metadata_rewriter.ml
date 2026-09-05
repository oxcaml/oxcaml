(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module PTA = Points_to_analysis

let keep_or_delete_of_param_decisions vars decisions =
  List.map2
    (fun var (decision : Unboxing_analysis.param_decision) ->
      match decision with
      | Keep _ | Unbox _ -> var, PTA.Keep
      | Delete -> var, PTA.Delete)
    vars decisions

let rewrite_is_my_closure_used (uses : Unboxing_analysis.result) ~my_closure
    code_metadata =
  let is_my_closure_used =
    PTA.has_use uses.db (Code_id_or_name.var my_closure)
  in
  if
    Bool.equal is_my_closure_used
      (Code_metadata.is_my_closure_used code_metadata)
  then code_metadata
  else if is_my_closure_used
  then
    Misc.fatal_errorf
      "The analysis found a use of %a, the closure of %a, but the code \
       metadata says the closure is unused"
      Variable.print my_closure Code_id.print
      (Code_metadata.code_id code_metadata)
  else Code_metadata.with_is_my_closure_used false code_metadata

let rewrite_result_types (uses : Unboxing_analysis.result) ~final_typing_env
    ~types_rewrite_context ~calling_convention_changes
    ~(code_dep : Traverse_acc.code_dep) ~my_closure code_metadata =
  match Code_metadata.result_types code_metadata with
  | Unknown | Bottom -> code_metadata
  | Ok result_types ->
    let code_id = Code_metadata.code_id code_metadata in
    let result_types =
      match final_typing_env with
      | None ->
        (* This can happen if the result continuation of the compilation unit is
           never used. If this happens, this compilation unit either always
           raises an exception or diverges. In any case, it will not be possible
           to use this compilation unit in another compilation unit, so keeping
           the result types is useless. *)
        Or_unknown_or_bottom.Unknown
      | Some old_typing_env ->
        if Sys.getenv_opt "FORGETALL" <> None && true
        then Or_unknown_or_bottom.Unknown
        else
          let params, results =
            if Unboxing_analysis.cannot_change_calling_convention uses code_id
            then
              ( List.map (fun p -> p, PTA.Keep) code_dep.params,
                List.map (fun p -> p, PTA.Keep) code_dep.return )
            else
              let params_decision =
                match
                  Unboxing_analysis.function_params_to_keep
                    calling_convention_changes code_id
                with
                | None ->
                  Misc.fatal_errorf
                    "No parameter decisions found for code id %a in \
                     [rewrite_result_types]"
                    Code_id.print code_id
                | Some p -> p
              in
              let return_decisions =
                match
                  Unboxing_analysis.function_return_decision
                    calling_convention_changes code_id
                with
                | None ->
                  Misc.fatal_errorf
                    "No return decisions found for code id %a in \
                     [rewrite_result_types]"
                    Code_id.print code_id
                | Some p -> p
              in
              ( keep_or_delete_of_param_decisions code_dep.params params_decision,
                keep_or_delete_of_param_decisions code_dep.return
                  return_decisions )
          in
          Or_unknown_or_bottom.Ok
            (Types_rewriter.rewrite_result_types types_rewrite_context
               ~old_typing_env ~my_closure ~params ~results result_types)
    in
    Code_metadata.with_result_types result_types code_metadata

let rewrite_calling_convention ~calling_convention_changes code_metadata =
  let code_id = Code_metadata.code_id code_metadata in
  let params_decision =
    match
      Unboxing_analysis.function_params_to_keep calling_convention_changes
        code_id
    with
    | None ->
      Misc.fatal_errorf
        "No parameter decisions found for code id %a in \
         [rewrite_calling_convention]"
        Code_id.print code_id
    | Some p -> p
  in
  let return_decisions =
    match
      Unboxing_analysis.function_return_decision calling_convention_changes
        code_id
    with
    | None ->
      Misc.fatal_errorf
        "No return decisions found for code id %a in \
         [rewrite_calling_convention]"
        Code_id.print code_id
    | Some p -> p
  in
  let (my_closure_decision : Unboxing_analysis.param_decision), code_metadata =
    match
      Unboxing_analysis.my_closure_decision calling_convention_changes code_id
    with
    | None ->
      Misc.fatal_errorf
        "No my_closure decision found for code id %a in \
         [rewrite_calling_convention]"
        Code_id.print code_id
    | Some Keep_my_closure ->
      (* The closure is not unboxed, so the extra decision prepended below
         contributes no parameter. *)
      Delete, code_metadata
    | Some (Unbox_my_closure fields) ->
      Unbox fields, Code_metadata.with_is_my_closure_used false code_metadata
  in
  let result_arity =
    Flambda_arity.unarize_t
      (Unboxing_analysis.arity_of_param_decisions return_decisions)
  in
  let params_decision_and_modes =
    Flambda_arity.group_by_parameter
      (Code_metadata.params_arity code_metadata)
      (List.combine params_decision (Code_metadata.param_modes code_metadata))
  in
  let params_decision_and_modes =
    match params_decision_and_modes with
    | [] ->
      Misc.fatal_errorf
        "Empty parameter groups when changing calling convention for code id %a"
        Code_id.print code_id
    | first :: rest ->
      ((my_closure_decision, Alloc_mode.For_types.unknown ()) :: first) :: rest
  in
  let params_arity =
    Flambda_arity.create
      (List.map
         (fun group ->
           Flambda_arity.Component_for_creation.Unboxed_product
             (List.map
                (fun kind ->
                  Flambda_arity.Component_for_creation.Singleton kind)
                (Unboxing_analysis.unarized_kinds_of_param_decisions
                   (List.map fst group))))
         params_decision_and_modes)
  in
  let param_modes =
    List.concat_map
      (fun group ->
        List.concat_map
          (fun (decision, mode) ->
            List.map
              (fun _kind -> mode)
              (Unboxing_analysis.unarized_kinds_of_param_decisions [decision]))
          group)
      params_decision_and_modes
  in
  (* We only change the calling convention if the analysis has shown there are
     no partial applications. *)
  code_metadata
  |> Code_metadata.with_result_arity result_arity
  |> Code_metadata.with_is_tupled false
  |> Code_metadata.with_params_arity params_arity
  |> Code_metadata.with_param_modes param_modes
  |> Code_metadata.with_first_complex_local_param
       First_complex_local_param.Never_partially_applied

let rewrite ~final_typing_env ~get_code_metadata ~types_rewrite_context
    ~calling_convention_changes ~code_deps (uses : Unboxing_analysis.result) =
  Code_id.Map.mapi
    (fun code_id (code_dep : Traverse_acc.code_dep) ->
      let code_metadata = get_code_metadata code_id in
      let my_closure =
        match PTA.my_closure_of_code_id uses.db code_id with
        | Some my_closure -> my_closure
        | None ->
          Misc.fatal_errorf "No my_closure variable found for code id %a"
            Code_id.print code_id
      in
      let code_metadata =
        rewrite_is_my_closure_used uses ~my_closure code_metadata
      in
      let code_metadata =
        rewrite_result_types uses ~final_typing_env ~types_rewrite_context
          ~calling_convention_changes ~code_dep ~my_closure code_metadata
      in
      if Unboxing_analysis.cannot_change_calling_convention uses code_id
      then code_metadata
      else rewrite_calling_convention ~calling_convention_changes code_metadata)
    code_deps
