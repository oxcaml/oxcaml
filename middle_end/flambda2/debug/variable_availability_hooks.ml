(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Simon Spies, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

let observe_flambda_unit ~(checkpoint : Variable_availability.Checkpoint.t)
    ?all_code unit =
  if !Clflags.dump_variable_availability
  then begin
    let rec walk_expr e =
      match Flambda.Expr.descr e with
      | Let l -> walk_let l
      | Let_cont lc -> walk_let_cont lc
      | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> ()
    and walk_let l =
      Flambda.Let_expr.pattern_match l ~f:(fun bound ~body ->
          Bound_pattern.fold_all_bound_vars bound ~init:() ~f:(fun () bv ->
              record_bound_var ~checkpoint bv);
          walk_named (Flambda.Let_expr.defining_expr l);
          walk_expr body)
    and walk_named (n : Flambda.Named.t) =
      match n with
      | Simple _ | Prim _ | Rec_info _ | Set_of_closures _ -> ()
      | Static_consts scg ->
        List.iter walk_code (Flambda.Static_const_group.pieces_of_code' scg)
    and walk_let_cont (lc : Flambda.Let_cont_expr.t) =
      match lc with
      | Non_recursive { handler; _ } ->
        Flambda.Non_recursive_let_cont_handler.pattern_match handler
          ~f:(fun _cont ~body ->
            walk_expr body;
            walk_cont_handler
              (Flambda.Non_recursive_let_cont_handler.handler handler))
      | Recursive handlers ->
        Flambda.Recursive_let_cont_handlers.pattern_match handlers
          ~f:(fun ~invariant_params ~body cont_handlers ->
            record_bound_params ~checkpoint invariant_params;
            walk_expr body;
            Continuation.Lmap.iter
              (fun _ h -> walk_cont_handler h)
              (Flambda.Continuation_handlers.to_map cont_handlers))
    and walk_cont_handler h =
      Flambda.Continuation_handler.pattern_match h ~f:(fun params ~handler ->
          record_bound_params ~checkpoint params;
          walk_expr handler)
    and walk_code code0 =
      Flambda.Function_params_and_body.pattern_match
        (Code0.params_and_body code0)
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
          walk_expr body)
    in
    walk_expr (Flambda_unit.body unit);
    Option.iter (Exported_code.iter_code ~f:walk_code) all_code
  end

let hooks_registered = ref false

let register_hooks () =
  if not !hooks_registered
  then begin
    hooks_registered := true;
    let module CH = Compiler_hooks in
    CH.register CH.Raw_flambda2 (fun u ->
        observe_flambda_unit ~checkpoint:Flambda2_input u);
    CH.register CH.Flambda2 (fun u ->
        observe_flambda_unit ~checkpoint:Flambda2_simplified u);
    CH.register CH.Reaped_flambda2 (fun u ->
        observe_flambda_unit ~checkpoint:Flambda2_output u)
  end
