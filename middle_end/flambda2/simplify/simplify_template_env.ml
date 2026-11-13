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

open! Flambda.Import
open! Simplify_import

let simplify_instantiation ~simplify_function_body ~env_type dacc code_id =
  let code = DA.awaiting_instantiation_code dacc code_id in
  let code_const, dacc =
    Simplify_set_of_closures.simplify_code_without_closure dacc code
      ~all_code:(Code_id.Map.singleton code_id code)
      ~simplify_function_body ~template_env_type:(Some env_type)
  in
  (* The code is lifted rather than bound here, since it has no free
     variables. *)
  let dacc =
    DA.add_to_lifted_constant_accumulator ~also_add_to_env:() dacc
      (LCS.singleton (LC.create_code code_id code_const))
  in
  DA.instantiation_simplified dacc code_id

let simplify_template_env dacc bound_pattern named ~template_key ~instantiations
    ~arg ~simplify_function_body =
  if not (Current_unit.is_current (Symbol.compilation_unit template_key))
  then
    Misc.fatal_errorf "Template %a is defined outside its compilation unit"
      Symbol.print template_key;
  let bound_var = Bound_pattern.must_be_singleton bound_pattern in
  let arg_ty, arg =
    S.simplify_simple dacc arg ~min_name_mode:(Bound_var.name_mode bound_var)
  in
  let env_type =
    let typing_env = DA.typing_env dacc in
    (* Every variable is erased, so that the type describes the environments
       created by every execution of the definition. [template_key] must not
       become an alias of the environment, since only the latter has a value. *)
    T.make_suitable_for_environment typing_env
      (All_variables_except Variable.Set.empty)
      [Name.symbol template_key, T.remove_outermost_alias typing_env arg_ty]
  in
  let dacc = DA.define_template dacc template_key env_type in
  let dacc =
    List.fold_left
      (simplify_instantiation ~simplify_function_body ~env_type)
      dacc instantiations
  in
  let dacc =
    (* Specialising a continuation copies its handler, which would make a copy
       of this definition. *)
    DA.map_denv dacc
      ~f:
        (DE.map_specialization_cost
           ~f:Specialization_cost.add_template_definition)
  in
  let dacc = DA.add_variable dacc bound_var arg_ty in
  let machine_width = DE.machine_width (DA.denv dacc) in
  Simplify_named_result.create dacc
    (Keep_binding
       { let_bound = bound_pattern;
         simplified_defining_expr =
           Simplified_named.create ~machine_width (Named.create_simple arg);
         original_defining_expr = Some named
       })
