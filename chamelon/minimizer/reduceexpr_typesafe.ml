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

(* Minimizer red-expr: reduce any sub-expression without using %opaque *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Dummy_expr
open Env

let rec is_simplified env e =
  match e.exp_desc with
  | Texp_construct (_, cons, el) -> (
      match get_desc cons.cstr_res with
      | Tconstr (path, _, _) -> (
          let td = find_type path env in
          match td.type_kind with
          | Type_variant (cstr_list, _) ->
              List.for_all (is_simplified env) el
              && cons.cstr_name
                 = Ident.name (find_simpler_cons path cstr_list).cd_id
          | _ -> false)
      | _ -> false)
  | Texp_tuple exp_list | Texp_array exp_list ->
      List.for_all (is_simplified env) exp_list
  | Texp_record { fields; _ } ->
      Array.for_all
        (fun (_, rl_def) ->
          match rl_def with
          | Overridden (_, expr) -> is_simplified env expr
          | Kept _ -> failwith "kept in record")
        fields
  | Texp_ident _ -> (
      match get_desc e.exp_type with Tvar _ -> true | _ -> false)
  | Texp_function { cases; _ } ->
      List.for_all (fun vc -> is_simplified env vc.c_rhs) cases
  | Texp_constant (Const_int 0)
  | Texp_constant (Const_char '0')
  | Texp_constant (Const_string ("", _, None)) ->
      true
  | _ -> false

let minimize to_remove map cur_name =
  let nth_expr = ref (-1) in
  let is_removed = ref false in
  let rec reduce_def_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          incr nth_expr;
          if to_remove = !nth_expr && not (is_simplified e.exp_env e) then (
            is_removed := true;
            generate_dummy_expr e.exp_env e.exp_type)
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    reduce_def_mapper.structure reduce_def_mapper (Smap.find cur_name map)
  in
  (Smap.add cur_name nstr map, !is_removed)

(*let minimizer = {
  minimizer_name = "reduce-expr-typesafe" ;
  minimizer_func = minimize
  }*)
