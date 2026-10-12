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
  { likelihood : Likelihood.t;
    fdo_annotation : Fdo_annotation.t
  }

let create ~likelihood ~fdo_annotation = { likelihood; fdo_annotation }

let from_likelihood likelihood =
  { likelihood; fdo_annotation = Fdo_annotation.empty }

let default = from_likelihood Likelihood.default

let cold = from_likelihood Likelihood.cold

let likelihood { likelihood; _ } = likelihood

let with_likelihood t likelihood = { t with likelihood }

let fdo_annotation { fdo_annotation; _ } = fdo_annotation

let with_fdo_annotation t fdo_annotation = { t with fdo_annotation }

let sum_list annots =
  let likelihood = Likelihood.sum_list (List.map likelihood annots) in
  let fdo_annotation =
    List.fold_left
      (fun acc annot -> Fdo_annotation.combine acc annot.fdo_annotation)
      Fdo_annotation.empty annots
  in
  create ~likelihood ~fdo_annotation

let add t1 t2 = sum_list [t1; t2]
