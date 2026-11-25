(******************************************************************************
 *                                   OxCaml                                   *
 *                          Basile Clément, OCamlPro                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

type t = Bound_parameter.t

let create bp = bp

let var = Bound_parameter.var

let simple = Bound_parameter.simple

let kind = Bound_parameter.kind

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 = Variable.compare (var t1) (var t2)

  let equal t1 t2 = Variable.equal (var t1) (var t2)

  let hash t = Variable.hash (var t)

  let print = Bound_parameter.print
end)

let rename_list_as_bound_parameters ts =
  let renaming, bound_parameters =
    List.fold_left_map
      (fun renaming t ->
        let bp = Bound_parameter.rename t in
        Renaming.add_variable renaming (var t) (Bound_parameter.var bp), bp)
      Renaming.empty ts
  in
  renaming, Bound_parameters.create bound_parameters

let place_lifted ~is_exn_handler ~lifted l =
  if is_exn_handler then l @ lifted else lifted @ l
