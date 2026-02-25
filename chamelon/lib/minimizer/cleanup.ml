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

(* Cleanup minimizer: extremely aggressive minimizer, intended to run as a last
   resort to remove "structural artefacts" from an already minimized file.

   This minimizer assumes that we are in a "mostly untyped" context, where
   everything that's not relevant to the minimized bug has been replaced with
   `__dummy2__` calls. It then tries to:

   - Replace type annotations with a wildcard `_` ;

   - Replace value bindings with a wildcard `_` ;

   - Remove elements from sequences ;

   - Replace function calls by either the function itself or either of its
   arguments ;

   - Remove labels from function parameters and arguments ;

   - Remove function parameters and arguments ; *)

module E = Utils.E

let cleanup_mapper should_remove =
  { Tast_mapper.default with
    typ =
      (fun sub typ ->
        Tast_mapper.default.typ sub
          (match typ.ctyp_desc with
          | Ttyp_var (None, _) -> typ
          | _ ->
            if should_remove ()
            then { typ with ctyp_desc = Compat.mkTtyp_any }
            else typ));
    value_binding =
      (fun sub vb ->
        Tast_mapper.default.value_binding sub
          (match Compat.view_tpat vb.vb_pat.pat_desc with
          | O Tpat_any -> vb
          | _ ->
            if should_remove ()
            then { vb with vb_pat = { vb.vb_pat with pat_desc = Tpat_any } }
            else vb));
    expr =
      (fun sub expr ->
        let default = Tast_mapper.default in
        match Compat.view_texp expr.exp_desc with
        | Texp_sequence (_, e2, _id) when should_remove () -> sub.expr sub e2
        | Texp_sequence (e1, _, _id) when should_remove () -> sub.expr sub e1
        | Texp_function ({ params; body }, id) -> (
          let lam ?id params body =
            let exp_desc = Compat.mkTexp_function ?id { params; body } in
            default.expr sub { expr with exp_desc }
          in
          let params = List.filter (fun _ -> not (should_remove ())) params in
          match params with
          | [] -> (
            match body with
            | Function_body body -> sub.expr sub body
            | Function_cases fc -> (
              let cases =
                List.filter (fun _ -> not (should_remove ())) fc.cases
              in
              match cases with
              | [] -> Utils.E.unit
              | [case] when should_remove () -> sub.expr sub case.c_rhs
              | _ -> lam ~id params (Function_cases { fc with cases })))
          | _ :: _ -> lam ~id params body)
        | O (Texp_ifthenelse (_, _, Some e3)) when should_remove () ->
          sub.expr sub e3
        | O (Texp_ifthenelse (_, e2, _)) when should_remove () ->
          sub.expr sub e2
        | O (Texp_let (_, _, body)) when should_remove () -> sub.expr sub body
        | O (Texp_let (_, value_bindings, _body)) -> (
          match
            List.fold_left
              (fun expr_opt (vb : Typedtree.value_binding) ->
                match expr_opt with
                | None when should_remove () -> Some vb.vb_expr
                | None | Some _ -> None)
              None value_bindings
          with
          | None -> default.expr sub expr
          | Some expr -> sub.expr sub expr)
        | Texp_tuple (exprs, id) -> (
          match List.filter (fun _ -> not (should_remove ())) exprs with
          | [] -> E.unit
          | [expr] -> sub.expr sub expr
          | exprs -> default.expr sub (E.tuple ~id exprs))
        | Texp_apply (fn, args, id) -> (
          if should_remove ()
          then sub.expr sub fn
          else
            match
              List.fold_left
                (fun expr_opt (_, apply_arg) ->
                  Compat.fold_arg_or_omitted
                    (fun expr_opt (arg, _) ->
                      match expr_opt with
                      | None when should_remove () -> Some arg
                      | None | Some _ -> expr_opt)
                    expr_opt apply_arg)
                None args
            with
            | Some expr -> sub.expr sub expr
            | None ->
              let args =
                List.filter_map
                  (fun (arg_label, apply_arg) ->
                    if should_remove ()
                    then None
                    else
                      match arg_label with
                      | Asttypes.Nolabel -> Some (arg_label, apply_arg)
                      | _ -> Some (Asttypes.Nolabel, apply_arg))
                  args
              in
              default.expr sub
                { expr with exp_desc = Compat.mkTexp_apply ~id (fn, args) })
        | _ -> expr);
    attributes =
      (fun sub attributes ->
        Tast_mapper.default.attributes sub
          (List.filter
             (fun attribute ->
               (* Preserve these attributes because we have another minimizer
                  that tries to add [@inline never] and [@local never]
                  annotations on functions. *)
               (Builtin_attributes.attr_equals_builtin attribute "inline"
               || Builtin_attributes.attr_equals_builtin attribute "local")
               || not (should_remove ()))
             attributes))
  }

let minimizer = Utils.tast_mapper_minimizer "cleanup" cleanup_mapper
