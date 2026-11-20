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

let rec fold_applies_of_expr ~f ~init expr =
  match Expr.descr expr with
  | Let let_expr ->
    Let.pattern_match let_expr ~f:(fun _ ~body ->
        fold_applies_of_expr ~f ~init body)
  | Let_cont (Non_recursive { handler; _ }) ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun _ ~body ->
        let init =
          fold_applies_of_handler ~f ~init
            (Non_recursive_let_cont_handler.handler handler)
        in
        fold_applies_of_expr ~f ~init body)
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params:_ ~body handlers ->
        let init =
          Continuation.Lmap.fold
            (fun _ handler init -> fold_applies_of_handler ~f ~init handler)
            (Continuation_handlers.to_map handlers)
            init
        in
        fold_applies_of_expr ~f ~init body)
  | Apply apply -> f init apply
  | Switch _ | Apply_cont _ | Invalid _ -> init

and fold_applies_of_handler ~f ~init handler =
  Continuation_handler.pattern_match handler ~f:(fun _ ~handler:body ->
      fold_applies_of_expr ~f ~init body)

let calls_without_counter code =
  Function_params_and_body.pattern_match (Code.params_and_body code)
    ~f:(fun
        ~return_continuation:_
        ~exn_continuation:_
        _
        ~body
        ~my_closure:_
        ~is_my_closure_used:_
        ~my_alloc_mode:_
        ~my_depth:_
        ~free_names_of_body:_
      ->
      fold_applies_of_expr body ~init:0 ~f:(fun calls apply ->
          match Apply.callsite_counter apply, Apply.call_kind apply with
          | None, (Function _ | Method _) -> calls + 1
          | Some _, _ | None, (C_call _ | Effect _) -> calls))

let of_switch_arms arms =
  Target_ocaml_int.Map.fold
    (fun _ arm counters ->
      Fdo_annotation.combine counters
        (Branch_annotations.fdo_annotation (Switch_arm.annotations arm)))
    arms Fdo_annotation.empty
