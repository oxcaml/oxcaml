(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
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

module VA = Type_shape.Variable_availability

let observed_id_of_duid (duid : Flambda_debug_uid.t) : VA.observed_id =
  match duid with
  | Uid uid -> Source_uid uid
  | Proj { uid; unboxed_field } ->
    Projected_source_uid { source_uid = uid; field = unboxed_field }

let record_uid ~checkpoint duid =
  VA.record_observation ~checkpoint (observed_id_of_duid duid)

let record_bound_var ~checkpoint bv =
  record_uid ~checkpoint (Bound_var.debug_uid bv)

let record_bound_params ~checkpoint ps =
  List.iter
    (fun p ->
      let _, duid = Bound_parameter.var_and_uid p in
      record_uid ~checkpoint duid)
    (Bound_parameters.to_list ps)

let rec walk_expr ~(checkpoint : Variable_availability.Checkpoint.t) e =
  match Flambda.Expr.descr e with
  | Let l -> walk_let ~checkpoint l
  | Let_cont lc -> walk_let_cont ~checkpoint lc
  | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> ()

and walk_let ~checkpoint l =
  Flambda.Let_expr.pattern_match l ~f:(fun bound ~body ->
      Bound_pattern.fold_all_bound_vars bound ~init:() ~f:(fun () bv ->
          record_bound_var ~checkpoint bv);
      walk_named ~checkpoint (Flambda.Let_expr.defining_expr l);
      walk_expr ~checkpoint body)

and walk_named ~checkpoint (n : Flambda.Named.t) =
  match n with
  | Simple _ | Prim _ | Rec_info _ | Set_of_closures _ -> ()
  | Static_consts scg ->
    List.iter (walk_code ~checkpoint)
      (Flambda.Static_const_group.pieces_of_code' scg)

and walk_let_cont ~checkpoint (lc : Flambda.Let_cont_expr.t) =
  match lc with
  | Non_recursive { handler; _ } ->
    Flambda.Non_recursive_let_cont_handler.pattern_match handler
      ~f:(fun _cont ~body ->
        walk_expr ~checkpoint body;
        walk_cont_handler ~checkpoint
          (Flambda.Non_recursive_let_cont_handler.handler handler))
  | Recursive handlers ->
    Flambda.Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body cont_handlers ->
        record_bound_params ~checkpoint invariant_params;
        walk_expr ~checkpoint body;
        Continuation.Lmap.iter
          (fun _ h -> walk_cont_handler ~checkpoint h)
          (Flambda.Continuation_handlers.to_map cont_handlers))

and walk_cont_handler ~checkpoint h =
  Flambda.Continuation_handler.pattern_match h ~f:(fun params ~handler ->
      record_bound_params ~checkpoint params;
      walk_expr ~checkpoint handler)

and walk_code ~checkpoint code0 =
  Flambda.Function_params_and_body.pattern_match (Code0.params_and_body code0)
    ~f:(fun
        ~return_continuation:_
        ~exn_continuation:_
        params
        ~body
        ~my_closure:_
        ~is_my_closure_used:_
        ~my_alloc_mode:_
        ~my_depth:_
        ~free_names_of_body:_
      ->
      record_bound_params ~checkpoint params;
      walk_expr ~checkpoint body)

let observe ~checkpoint ?all_code unit =
  if !Clflags.dump_variable_availability
  then begin
    walk_expr ~checkpoint (Flambda_unit.body unit);
    Option.iter (Exported_code.iter_code ~f:(walk_code ~checkpoint)) all_code
  end
