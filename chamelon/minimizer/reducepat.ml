(******************************************************************************
 *                                 Chamelon                                   *
 *                         Basile Clément, OCamlPro                           *
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

open Utils
open Typedtree
open Tast_mapper
open Compat

(* reducepat: remove match arms *)

let minimize should_remove map cur_name =
  let reduce_pat_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match view_texp e.exp_desc with
            | Texp_match (e_match, cc_l, partial, id) -> (
                let cc_l =
                  List.filter_map
                    (fun case ->
                      let c_lhs = case.c_lhs in
                      let c_lhs =
                        match c_lhs.pat_desc with
                        | Tpat_or (case1, case2, _) ->
                            if should_remove () then Some case1
                            else if should_remove () then Some case2
                            else if should_remove () then None
                            else Some c_lhs
                        | _ -> if should_remove () then None else Some c_lhs
                      in
                      match c_lhs with
                      | None -> None
                      | Some c_lhs -> Some { case with c_lhs })
                    cc_l
                in
                match cc_l with
                | [] -> Dummy.apply_dummy2
                | _ ->
                    {
                      e with
                      exp_desc = mkTexp_match ~id (e_match, cc_l, partial);
                    })
            | O (Texp_ifthenelse (e_if, e_then, e_else_opt)) ->
                if should_remove () then
                  (* if e1 then e2 [else e3] -> __ignore__ e1; e2 *)
                  E.list [ E.ignore e_if; e_then ]
                else if should_remove () then
                  match e_else_opt with
                  | None ->
                      (* if e1 then e2 -> __ignore__ e1 *)
                      E.ignore e_if
                  | Some e_else ->
                      (* if e1 then e2 else e3 -> __ignore__ e1; e3 *)
                      E.list [ E.ignore e_if; e_else ]
                else e
            | _ -> e));
    }
  in
  let nstr =
    reduce_pat_mapper.structure reduce_pat_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "reduce-pat"; minimizer_func = minimize }
