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

(* Minimizer rem-unit: remove unit-typed expressions *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Compat

exception Not_implemented

let eunit = mkTexp_tuple []

let is_unit e =
  match view_texp e.exp_desc with
  | Texp_construct ({ txt = Lident "()"; _ }, _, _, _) | Texp_tuple ([], _) ->
      true
  | _ -> false

let is_unit_typ (typ : type_expr) =
  match get_desc typ with
  | Ttuple [] -> true
  | Tconstr (path, [], _) -> (
      match path with Path.Pident id -> name id = "unit" | _ -> false)
  | _ -> false

let minimize should_remove map cur_name =
  let remove_unit_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          if is_unit_typ e.exp_type && (not (is_unit e)) && should_remove ()
          then { e with exp_desc = eunit }
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    remove_unit_mapper.structure remove_unit_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "remove-unit"; minimizer_func = minimize }
