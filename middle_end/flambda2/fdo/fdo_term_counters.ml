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

let map_counters_of_code ~f code =
  let entry_annotation = Code.fdo_entry_annotation code in
  let code_metadata =
    Code_metadata.with_fdo_entry_annotation
      (Fdo_annotation.With_body_hash.with_annotation entry_annotation
         (Fdo_annotation.map f
            (Fdo_annotation.With_body_hash.annotation entry_annotation)))
      (Code.code_metadata code)
  in
  let params_and_body =
    Function_params_and_body.pattern_match (Code.params_and_body code)
      ~f:(fun
          ~return_continuation
          ~exn_continuation
          params
          ~body
          ~my_closure
          ~is_my_closure_used:_
          ~my_alloc_mode
          ~my_depth
          ~free_names_of_body
        ->
        Function_params_and_body.create ~return_continuation ~exn_continuation
          params
          ~body:
            (Flambda.map_fdo_counters body ~callsite:(Option.map f)
               ~arm:(fun _ -> Fdo_annotation.map f))
          ~free_names_of_body ~my_closure ~my_alloc_mode ~my_depth)
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
