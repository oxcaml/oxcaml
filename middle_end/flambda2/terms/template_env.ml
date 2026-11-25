(******************************************************************************
 *                                  OxCaml                                    *
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

type t =
  { template_key : Symbol.t;
    instantiations : Code_id.t list;
    env : Simple.t
  }

let create ~template_key ~instantiations ~env =
  { template_key; instantiations; env }

let template_key t = t.template_key

let instantiations t = t.instantiations

let env t = t.env

let print ppf { template_key; instantiations; env } =
  Format.fprintf ppf
    "@[<hov 1>(Template_env@ (template_key %a)@ (instantiations %a)@ (env \
     %a))@]"
    Symbol.print template_key
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Code_id.print)
    instantiations Simple.print env

let free_names { template_key = _; instantiations; env } =
  (* [template_key] has no value: it only names the template. *)
  List.fold_left
    (fun free_names code_id ->
      Name_occurrences.add_code_id free_names code_id Name_mode.normal)
    (Simple.free_names env) instantiations

let apply_renaming ({ template_key; instantiations; env } as t) renaming =
  let template_key' = Renaming.apply_symbol renaming template_key in
  let instantiations' =
    Misc.Stdlib.List.map_sharing
      (Renaming.apply_code_id renaming)
      instantiations
  in
  let env' = Simple.apply_renaming env renaming in
  if
    template_key == template_key'
    && instantiations == instantiations'
    && env == env'
  then t
  else
    { template_key = template_key';
      instantiations = instantiations';
      env = env'
    }

let ids_for_export { template_key; instantiations; env } =
  List.fold_left Ids_for_export.add_code_id
    (Ids_for_export.add_symbol (Ids_for_export.from_simple env) template_key)
    instantiations
