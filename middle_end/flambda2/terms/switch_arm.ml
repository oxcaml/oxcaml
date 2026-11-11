(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

type t =
  { action : Apply_cont_expr.t;
    annotations : Branch_annotations.t
  }

let create ~annotations action = { action; annotations }

let from_apply_cont action =
  create ~annotations:Branch_annotations.default action

let action { action; _ } = action

let annotations { annotations; _ } = annotations

let likelihood arm = Branch_annotations.likelihood (annotations arm)

let is_cold arm = Likelihood.is_cold (likelihood arm)

let map_action f ({ action; _ } as arm) =
  let action' = f action in
  if action == action' then arm else { arm with action = action' }

let free_names { action; annotations = _ } = Apply_cont_expr.free_names action

let apply_renaming ({ action; annotations } as arm) renaming =
  let action' = Apply_cont_expr.apply_renaming action renaming in
  if action == action' then arm else { action = action'; annotations }

let ids_for_export { action; annotations = _ } =
  Apply_cont_expr.ids_for_export action
