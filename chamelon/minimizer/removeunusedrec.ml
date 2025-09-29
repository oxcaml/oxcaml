(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

(* rem-rec : remove unused rec keywords *)

open Utils
open Typedtree
open Tast_mapper
open Stdlib
open Asttypes
open Compat

let minimize should_remove map cur_name =
  let suppress_rec =
    {
      Tast_mapper.default with
      structure_item =
        (fun mapper str_it ->
          {
            str_it with
            str_desc =
              (match str_it.str_desc with
              | Tstr_value (r, vb_l) ->
                  if
                    r = Recursive
                    && (not
                          (List.exists
                             (fun vb ->
                               match view_tpat vb.vb_pat.pat_desc with
                               | Tpat_var (id, _, _) ->
                                   let is_used = ref false in
                                   let mapper_used =
                                     Removedeadcode.search_in_str is_used id
                                   in
                                   ignore
                                     (mapper_used.structure_item mapper_used
                                        str_it);
                                   !is_used
                               | _ -> false)
                             vb_l))
                    && should_remove ()
                  then Tstr_value (Nonrecursive, vb_l)
                  else
                    (Tast_mapper.default.structure_item mapper str_it).str_desc
              | _ -> (Tast_mapper.default.structure_item mapper str_it).str_desc);
          });
      expr =
        (fun mapper e ->
          {
            e with
            exp_desc =
              (match e.exp_desc with
              | Texp_let (rf, vb_l, e_in) ->
                  if
                    rf = Recursive
                    && (not
                          (List.exists
                             (fun vb ->
                               match view_tpat vb.vb_pat.pat_desc with
                               | Tpat_var (id, _, _) ->
                                   let is_used = ref false in
                                   let mapper_used =
                                     Removedeadcode.search_in_str is_used id
                                   in
                                   ignore
                                     (mapper_used.expr mapper_used vb.vb_expr);
                                   !is_used
                               | _ -> false)
                             vb_l))
                    && should_remove ()
                  then Texp_let (Nonrecursive, vb_l, e_in)
                  else Texp_let (rf, vb_l, Tast_mapper.default.expr mapper e_in)
              | _ -> (Tast_mapper.default.expr mapper e).exp_desc);
          });
    }
  in
  let nstr = suppress_rec.structure suppress_rec (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "remove-unused-rec"; minimizer_func = minimize }
