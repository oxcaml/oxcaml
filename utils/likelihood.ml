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

(* Likelihoods are represented in log space, with "cold" (probability 0) being
   -infty. *)

type t = { logit : float } [@@unboxed]

let from_logit logit = { logit }

let to_logit { logit } = logit

let cold = from_logit neg_infinity

let is_cold { logit } = Float.equal logit neg_infinity

let default = from_logit 0.

let from_weight weight = from_logit (log weight)

type classification =
  | Cold
  | Weight of float

let classify { logit } =
  if Float.equal logit neg_infinity
  then Cold
  else Weight (exp logit)

let rescale ~total:{ logit = sum_logit } { logit } =
  if Float.is_infinite logit then { logit }
  else from_logit (logit -. sum_logit)

let max_list l =
  List.fold_left
    (fun max_logit { logit } -> Float.max max_logit logit)
    neg_infinity l
  |> from_logit

let scale n t =
  if n = 1 then t else
  (* log (n * exp logit) = log n + logit *)
  from_logit (to_logit t+. log (float n))

let sum_list l =
  let max_t = max_list l in
  if Float.is_infinite max_t.logit then max_t
  else
    let max_logit = to_logit max_t in
    max_logit +. log (
      List.fold_left
        (fun sumexp { logit } -> sumexp +. exp (logit -. max_logit))
        0. l
    )
    |> from_logit

let is_uniform l =
  let max_logit = max_list l |> to_logit in
  List.for_all
    (fun { logit } -> Float.is_infinite logit || Float.equal logit max_logit)
    l
