(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

open! Flambda.Import

let map_named ~code (named : Named.t) : Named.t =
  match named with
  | Static_consts group ->
    Named.create_static_consts
      (Static_const_group.map group ~f:(fun const ->
           match Static_const_or_code.to_code const with
           | Some c -> Static_const_or_code.create_code (code c)
           | None -> const))
  | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ -> named

let rec map_expr ~apply ~switch ~code (expr : Expr.t) : Expr.t =
  match Expr.descr expr with
  | Let let_expr ->
    Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
        let defining_expr = map_named ~code (Let.defining_expr let_expr) in
        let body = map_expr ~apply ~switch ~code body in
        Expr.create_let
          (Let.create bound_pattern defining_expr ~body
             ~free_names_of_body:Unknown))
  | Let_cont
      (Non_recursive
         { handler; num_free_occurrences; is_applied_with_traps; can_be_lifted })
    ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
        let body = map_expr ~apply ~switch ~code body in
        let handler =
          map_handler ~apply ~switch ~code
            (Non_recursive_let_cont_handler.handler handler)
        in
        Let_cont.create_non_recursive' ~can_be_lifted ~cont handler ~body
          ~num_free_occurrences_of_cont_in_body:num_free_occurrences
          ~is_applied_with_traps)
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body handlers ->
        let body = map_expr ~apply ~switch ~code body in
        let handlers =
          Continuation.Lmap.map
            (map_handler ~apply ~switch ~code)
            (Continuation_handlers.to_map handlers)
        in
        Let_cont.create_recursive ~invariant_params handlers ~body)
  | Apply apply_expr -> Expr.create_apply (apply apply_expr)
  | Switch switch_expr -> Expr.create_switch (switch switch_expr)
  | Apply_cont _ | Invalid _ -> expr

and map_handler ~apply ~switch ~code handler =
  Continuation_handler.pattern_match handler ~f:(fun params ~handler:body ->
      Continuation_handler.create params
        ~handler:(map_expr ~apply ~switch ~code body)
        ~free_names_of_handler:Unknown
        ~is_exn_handler:(Continuation_handler.is_exn_handler handler)
        ~is_cold:(Continuation_handler.is_cold handler))

let map_body ~f params_and_body =
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun
        ~return_continuation
        ~exn_continuation
        params
        ~body
        ~my_closure
        ~is_my_closure_used:_
        ~my_alloc_mode
        ~my_depth
        ~free_names_of_body:_
      ->
      Function_params_and_body.create ~return_continuation ~exn_continuation
        params ~body:(f body) ~free_names_of_body:Unknown ~my_closure
        ~my_alloc_mode ~my_depth)

let map_counters_of_apply ~f apply =
  Apply_expr.with_callsite_counter apply
    (Option.map f (Apply_expr.callsite_counter apply))

let map_counters_of_switch ~f switch =
  let arms =
    Target_ocaml_int.Map.map
      (Switch_arm.map_annotations (fun annotations ->
           Branch_annotations.with_fdo_annotation annotations
             (Fdo_annotation.map f
                (Branch_annotations.fdo_annotation annotations))))
      (Switch_expr.arms switch)
  in
  Switch_expr.create
    ~condition_dbg:(Switch_expr.condition_dbg switch)
    ~scrutinee:(Switch_expr.scrutinee switch)
    ~arms

let rec map_counters_of_code ~f code =
  let entry_annotation = Code.fdo_entry_annotation code in
  let code_metadata =
    Code_metadata.with_fdo_entry_annotation
      (Fdo_annotation.With_body_hash.with_annotation entry_annotation
         (Fdo_annotation.map f
            (Fdo_annotation.With_body_hash.annotation entry_annotation)))
      (Code.code_metadata code)
  in
  let params_and_body =
    Code.params_and_body code
    |> map_body
         ~f:
           (map_expr ~apply:(map_counters_of_apply ~f)
              ~switch:(map_counters_of_switch ~f)
              ~code:(map_counters_of_code ~f))
  in
  Code.create_with_metadata ~params_and_body
    ~free_names_of_params_and_body:(Code.free_names_of_params_and_body code)
    ~code_metadata

let of_inlined_call ~callsite_counter code =
  let entry_counters =
    Fdo_annotation.With_body_hash.annotation (Code.fdo_entry_annotation code)
  in
  match callsite_counter with
  | None -> entry_counters
  | Some at ->
    Fdo_annotation.map
      (fun counter -> Fdo_counter.inline counter ~at)
      entry_counters

let of_switch_arms arms =
  Target_ocaml_int.Map.fold
    (fun _ arm counters ->
      Fdo_annotation.combine counters
        (Branch_annotations.fdo_annotation (Switch_arm.annotations arm)))
    arms Fdo_annotation.empty
