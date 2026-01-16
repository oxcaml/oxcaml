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

(* simp-match : simplify pattern matchings with a unique pattern *)

open Utils
open Typedtree
open Tast_mapper
open Compat

(** [replace_mapper id to_replace] is a mapper replacing every occurence of [id]
    by the expression [to_replace]*)
let replace_mapper id to_replace =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then
              { e with exp_desc = to_replace }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
  }

let ignore_guard ~c_guard e =
  match c_guard with None -> e | Some guard -> E.list [ E.ignore guard; e ]

let minimize should_remove map cur_name =
  let simplify_match_mapper =
    (* match e1 with x -> e2 => e2[x->e1] *)
    (* match e1 with p -> e2 => let p = e1 in e2 *)
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match view_texp e.exp_desc with
            | O (Texp_try (e_try, [ { c_lhs; c_guard; c_rhs } ])) ->
                (* try e1 with p -> e2 => e1 *)
                (* try e1 with p -> e2 => let p = __dummy2__ () in e2 *)
                if should_remove () then e_try
                else if should_remove () then
                  E.let_
                    [ E.bind c_lhs Dummy.apply_dummy2 ]
                    (ignore_guard ~c_guard c_rhs)
                else e
            | Texp_match
                ( e_match,
                  [
                    {
                      c_lhs = { pat_desc = Tpat_value tva; _ } as c_lhs;
                      c_guard;
                      c_rhs;
                    };
                  ],
                  _partial,
                  id ) -> (
                match (view_tpat (tva :> pattern).pat_desc, c_guard) with
                | Tpat_var (id, _, _), None when should_remove () ->
                    let rep_map = replace_mapper id e_match.exp_desc in
                    rep_map.expr rep_map c_rhs
                | _ when should_remove () ->
                    let pat_desc = (tva :> pattern) in
                    let pat_desc =
                      {
                        pat_desc with
                        pat_extra = c_lhs.pat_extra @ pat_desc.pat_extra;
                      }
                    in
                    let id =
                      value_binding_identifier_from_texp_match_identifier id
                    in
                    E.let_
                      [
                        E.bind ~attrs:c_lhs.pat_attributes ~id
                          (pat_desc :> pattern)
                          e_match;
                      ]
                      (ignore_guard ~c_guard c_rhs)
                | _ -> e)
            | _ -> e));
    }
  in
  let nstr =
    simplify_match_mapper.structure simplify_match_mapper
      (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "simplify-match"; minimizer_func = minimize }
