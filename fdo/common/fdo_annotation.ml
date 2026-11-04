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

(* Every counter in the set is counted each time the point is reached, so the
   count of a point is not the sum of the counts of its counters. A point
   carries several counters when points are merged into one, e.g. identical
   switch arms that are shared. A point that is only passed through on the way
   to others, such as a node of a decision tree that dispatches to switch arms,
   carries none of their counters: they are counted below it. *)
type t = Fdo_counter.t list

let empty = []

let is_empty t = List.is_empty t

let of_list counters = Fdo_counter.add_all [] counters

let to_list t = t

let combine t1 t2 = Fdo_counter.add_all t1 t2

let union ts = Fdo_counter.add_all [] (List.concat ts)

let equal t1 t2 =
  List.equal Fdo_counter.equal
    (List.sort Fdo_counter.compare t1)
    (List.sort Fdo_counter.compare t2)

let print ppf t =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf counter ->
         Format.pp_print_string ppf (Fdo_counter.to_string counter)))
    t
