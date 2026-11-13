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

(* [seen] holds the code already visited, so that each piece of code is only
   visited once. *)
let rec in_expr ~find_code ~seen expr =
  match Expr.descr expr with
  | Let let_expr ->
    Let.pattern_match let_expr ~f:(fun _ ~body ->
        in_named ~find_code ~seen (Let.defining_expr let_expr)
        || in_expr ~find_code ~seen body)
  | Let_cont
      (Non_recursive
         { handler;
           num_free_occurrences = _;
           is_applied_with_traps = _;
           can_be_lifted = _
         }) ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun _ ~body ->
        in_continuation_handler ~find_code ~seen
          (Non_recursive_let_cont_handler.handler handler)
        || in_expr ~find_code ~seen body)
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params:_ ~body handlers ->
        Continuation.Lmap.exists
          (fun _ handler -> in_continuation_handler ~find_code ~seen handler)
          (Continuation_handlers.to_map handlers)
        || in_expr ~find_code ~seen body)
  | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> false

and in_continuation_handler ~find_code ~seen handler =
  Continuation_handler.pattern_match handler ~f:(fun _ ~handler ->
      in_expr ~find_code ~seen handler)

and in_named ~find_code ~seen (named : Named.t) =
  match[@warning "-fragile-match"] named with
  | Prim (Unary (Template_env _, _), _) -> true
  | Prim _ | Simple _ | Rec_info _ -> false
  | Set_of_closures (set_of_closures, _alloc_mode) ->
    Function_slot.Map.exists
      (fun _ (code_id : Function_declarations.code_id_in_function_declaration)
         ->
        match code_id with
        | Deleted _ -> false
        | Code_id { code_id; only_full_applications = _ } -> (
          match find_code code_id with
          | None -> false
          | Some code -> in_code ~find_code ~seen code))
      (Function_declarations.funs
         (Set_of_closures.function_decls set_of_closures))
  | Static_consts static_consts ->
    List.exists (in_code ~find_code ~seen)
      (Static_const_group.pieces_of_code' static_consts)

and in_code ~find_code ~seen code =
  (not (Code_id.Set.mem (Code.code_id code) !seen))
  && begin
    seen := Code_id.Set.add (Code.code_id code) !seen;
    Function_params_and_body.pattern_match (Code.params_and_body code)
      ~f:(fun
          ~return_continuation:_
          ~exn_continuation:_
          _
          ~body
          ~my_closure:_
          ~is_my_closure_used:_
          ~my_alloc_mode:_
          ~my_depth:_
          ~free_names_of_body:_
        -> in_expr ~find_code ~seen body)
  end

let find_code denv code_id =
  match DE.find_code_exn denv code_id with
  | exception Not_found -> None
  | code_or_metadata ->
    if Code_or_metadata.code_present code_or_metadata
    then Some (Code_or_metadata.get_code code_or_metadata)
    else None

let in_code denv code =
  in_code ~find_code:(find_code denv) ~seen:(ref Code_id.Set.empty) code

let in_handlers denv (handlers : Original_handlers.t) =
  let in_expr =
    in_expr ~find_code:(find_code denv) ~seen:(ref Code_id.Set.empty)
  in
  match handlers with
  | Non_recursive { handler; _ } -> in_expr handler
  | Recursive { continuation_handlers; _ } ->
    Continuation.Lmap.exists
      (fun _cont one_rec_handler ->
        in_expr one_rec_handler.One_recursive_handler.handler)
      continuation_handlers
