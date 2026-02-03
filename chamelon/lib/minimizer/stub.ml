(******************************************************************************
 *                                 Chamelon                                   *
 *                         Basile Clément, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

(* Minimizer stub: replace value bindings in structures with dummy values,
   remove top-level expressions.

   This minimizer is useful to quickly remove irrelevant definitions without
   breaking signatures.

   When a value binding is stubbed out:

   - Function arguments are preserved.

      `let f x = e` -> `let f x = __dummy__ ()`
      `let f x = fun y -> e` -> `let f x = fun y -> __dummy__ ()`

   - Recursive bindings are stubbed out at once and made non-recursive, since it
     is fairly likely that stubbing out a single component of a recursive
     definition would cause type inference issues:

      `let rec f x = e1 and g y = e2` ->
        `let f x = __dummy__ () and g y = __dummy__ ()`

   - Non-recursive binding groups are stubbed out individually and preserve
     their shape:

      `let f x = e1 and g y = e2` ->
        `let f x = __dummy__ () and g y = e2`

  - Default values for optional arguments are removed.

      `let f ?(x = something ()) y = e` -> `let f ?x y = __dummy__ ()`
 *)

open Utils
open Typedtree
open Dummy
open Compat

let rec is_dummy e =
  match view_texp e.exp_desc with
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__dummy2__"
      | _ -> false)
  | Texp_function ({ body = Function_body body_expr; params }, _id) ->
      List.for_all (fun param -> Option.is_none param.optional_default) params
      && is_dummy body_expr
  | _ -> false

let rec stub_function_body expr =
  match view_texp expr.exp_desc with
  | Texp_function ({ params; body }, id) ->
      let new_body =
        match body with
        | Function_body body_expr ->
            Function_body (stub_function_body body_expr)
        | Function_cases _ -> Function_body apply_dummy2
      in
      let new_params =
        List.map (fun param -> { param with optional_default = None }) params
      in
      let new_func = { params = new_params; body = new_body } in
      { expr with exp_desc = mkTexp_function ~id new_func }
  | _ -> apply_dummy2

let minimize should_remove map cur_name =
  let mapper =
    {
      Tast_mapper.default with
      structure =
        (fun sub str ->
          (* We need to process bindings in reverse order, because
             stubbing out one binding can make later bindings fail to typecheck
             due to type inference. *)
          let new_items =
            List.filter_map
              (fun item ->
                match item.str_desc with
                | Tstr_value (Recursive, bindings) when should_remove () ->
                    let bindings =
                      List.map
                        (fun vb ->
                          { vb with vb_expr = stub_function_body vb.vb_expr })
                        bindings
                    in
                    Some
                      {
                        item with
                        str_desc = Tstr_value (Nonrecursive, bindings);
                      }
                | Tstr_value (Nonrecursive, bindings) ->
                    let bindings =
                      List.map
                        (fun vb ->
                          if is_dummy vb.vb_expr then vb
                          else if should_remove () then
                            { vb with vb_expr = stub_function_body vb.vb_expr }
                          else sub.value_binding sub vb)
                        bindings
                    in
                    Some
                      {
                        item with
                        str_desc = Tstr_value (Nonrecursive, bindings);
                      }
                | Tstr_eval _ when should_remove () -> None
                | _ -> Some (Tast_mapper.default.structure_item sub item))
              (List.rev str.str_items)
          in
          { str with str_items = List.rev new_items });
    }
  in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "stub"; minimizer_func = minimize }
