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

(* Minimizer red-expr: reduce any sub-expression *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Dummy
open Compat

exception Not_implemented

let is_simplified e =
  match view_texp e.exp_desc with
  | Texp_tuple ([], _)
  | O (Texp_constant (Const_int 0))
  | O (Texp_constant (Const_char '0'))
  | O (Texp_constant (Const_string ("", _, None))) ->
      true
  | Texp_ident (_, name, _, _) ->
      Longident.last name.txt = "__dummy2__"
      || Longident.last name.txt = "__dummy1__"
      || Longident.last name.txt = "__ignore__"
  | Texp_construct (name, _, _, _) -> Longident.last name.txt = ""
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) ->
          Longident.last name.txt = "__dummy2__"
          || Longident.last name.txt = "__dummy1__"
          || Longident.last name.txt = "__ignore__"
      | _ -> false)
  | _ -> false

let simplify apply_dummy e =
  match get_desc e.exp_type with
  | Tconstr (path, [], _) -> (
      match path with
      | Path.Pident id -> (
          match name id with
          | "int" -> { e with exp_desc = Texp_constant (Const_int 0) }
          | "char" -> { e with exp_desc = Texp_constant (Const_char '0') }
          | "string" ->
              {
                e with
                exp_desc =
                  Texp_constant (Const_string ("", Location.none, None));
              }
          | "unit" -> { e with exp_desc = mkTexp_tuple [] }
          | _ -> apply_dummy)
      | _ -> apply_dummy)
  | _ -> apply_dummy

let minimize apply_dummy should_remove map cur_name =
  let reduce_def_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          if (not (is_simplified e)) && should_remove () then
            simplify apply_dummy e
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    reduce_def_mapper.structure reduce_def_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "reduce-expr"; minimizer_func = minimize apply_dummy2 }

let minimizer_dummy1 =
  { minimizer_name = "reduce-expr-2"; minimizer_func = minimize apply_dummy1 }
