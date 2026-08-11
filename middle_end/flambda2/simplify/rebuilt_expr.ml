(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module ART = Are_rebuilding_terms

type t =
  { expr : Expr.t;
    contents_hash : int Or_null.t
        (* If not null, this is a structural hash of the contents of this
           rebuilt expression (always null when not rebuilding terms).

           The hash should not depend on the name of variables within the
           expression, because we use it to de-duplicate continuation handlers
           that might bind variables with different names (see
           [Unique_continuation_map]). *)
  }

let create ?contents_hash expr =
  let contents_hash = Or_null.of_option contents_hash in
  { expr; contents_hash }

type rebuilt_expr = t

let to_expr t are_rebuilding =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    Misc.fatal_error
      "Cannot ask [Rebuilt_expr] for the built expression when \
       [UA.do_not_rebuild_terms] is set"
  else t.expr

let descr t = Expr.descr t.expr

let to_apply_cont t =
  match descr t with
  | Apply_cont apply_cont -> Some apply_cont
  | Let _ | Let_cont _ | Apply _ | Switch _ | Invalid _ -> None

let can_be_removed_as_invalid t are_rebuilding =
  if ART.do_not_rebuild_terms are_rebuilding
  then false
  else
    match descr t with
    | Invalid _ ->
      if Flambda_features.Debug.keep_invalid_handlers () then false else true
    | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ -> false

let [@ocamlformat "disable"] print are_rebuilding ppf t =
  if ART.do_not_rebuild_terms are_rebuilding then
    Format.fprintf ppf "<unavailable, terms not being rebuilt>"
  else
    Expr.print ppf t.expr

let term_not_rebuilt = create (Expr.create_invalid Code_not_rebuilt)

let contents_hash_simple simple =
  (* We want a "structural" hash that doesn't depend on names bound by
     continuations, so we ignore the names of variables, including inside
     coercions (but we still record whether we had a coercion or not). *)
  Simple.pattern_match' simple
    ~const:(fun const -> Hashtbl.hash (0, Reg_width_const.hash const))
    ~symbol:(fun symbol ~coercion ->
      let has_coercion = not (Coercion.is_id coercion) in
      Hashtbl.hash (1, Symbol.hash symbol, has_coercion))
    ~var:(fun _ ~coercion ->
      let has_coercion = not (Coercion.is_id coercion) in
      Hashtbl.hash (2, has_coercion))

let create_let are_rebuilding bound_vars defining_expr ~body ~free_names_of_body
    =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    let contents_hash =
      match body.contents_hash with
      | Null -> None
      | This body_hash -> (
        let[@local] simple_expr named_hash =
          Some (Hashtbl.hash (1, named_hash, body_hash))
        in
        match (defining_expr : Named.t) with
        | Simple simple ->
          simple_expr (Hashtbl.hash (0, contents_hash_simple simple))
        | Prim (prim, _dbg) ->
          let args_hash =
            Flambda_primitive.args prim |> List.map contents_hash_simple
          in
          let prim = Flambda_primitive.without_args prim in
          (* We use [Hashtbl.hash] directly on [prim] here, which should be
             fine. Worst case scenario, we have two primitives that should later
             compare equal but have different hashes (e.g. because they contain
             different regions), and we miss out on an optimisation. This should
             be rare and mostly inconsequential, though. *)
          simple_expr (Hashtbl.hash (1, prim, args_hash))
        | Set_of_closures _ | Static_consts _ | Rec_info _ -> None)
    in
    Let.create bound_vars defining_expr ~body:body.expr
      ~free_names_of_body:(Known free_names_of_body)
    |> Expr.create_let |> create ?contents_hash

let create_apply are_rebuilding apply =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else Expr.create_apply apply |> create

let create_apply_cont apply_cont =
  let contents_hash =
    match Apply_cont.trap_action apply_cont with
    | Some _ -> None
    | None ->
      Some
        (Hashtbl.hash
           ( 0,
             Continuation.hash (Apply_cont.continuation apply_cont),
             List.map contents_hash_simple (Apply_cont.args apply_cont) ))
  in
  Expr.create_apply_cont apply_cont |> create ?contents_hash

module Function_params_and_body = struct
  type t = Function_params_and_body.t

  let create ~return_continuation ~exn_continuation params ~body
      ~free_names_of_body ~my_closure ~my_alloc_mode ~my_depth =
    Function_params_and_body.create ~return_continuation ~exn_continuation
      params ~body:body.expr ~free_names_of_body:(Known free_names_of_body)
      ~my_closure ~my_alloc_mode ~my_depth

  let to_function_params_and_body t are_rebuilding =
    if ART.do_not_rebuild_terms are_rebuilding
    then
      Misc.fatal_error
        "Cannot ask for function params and body when not rebuilding terms"
    else t

  let is_my_closure_used t = Function_params_and_body.is_my_closure_used t
end

module Continuation_handler = struct
  type t = Continuation_handler.t

  let print ~cont ~recursive ppf ch =
    Continuation_handler.print ~cont ~recursive ppf ch

  let dummy =
    Continuation_handler.create Bound_parameters.empty
      ~handler:term_not_rebuilt.expr ~free_names_of_handler:Unknown
      ~is_exn_handler:false ~is_cold:false

  let create are_rebuilding params ~handler ~free_names_of_handler
      ~is_exn_handler ~is_cold =
    if ART.do_not_rebuild_terms are_rebuilding
    then dummy
    else
      Continuation_handler.create params ~handler:handler.expr
        ~free_names_of_handler:(Known free_names_of_handler) ~is_exn_handler
        ~is_cold
end

let create_non_recursive_let_cont are_rebuilding cont handler ~body
    ~free_names_of_body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive cont handler ~body:body.expr
      ~free_names_of_body:(Known free_names_of_body)
    |> create

let create_non_recursive_let_cont' are_rebuilding cont handler ~body
    ~num_free_occurrences_of_cont_in_body ~is_applied_with_traps =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive' ~cont handler ~body:body.expr
      ~num_free_occurrences_of_cont_in_body:
        (Known num_free_occurrences_of_cont_in_body) ~is_applied_with_traps
    |> create

let create_non_recursive_let_cont_without_free_names are_rebuilding cont handler
    ~body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive cont handler ~body:body.expr
      ~free_names_of_body:Unknown
    |> create

let create_recursive_let_cont are_rebuilding ~invariant_params handlers ~body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_recursive ~invariant_params handlers ~body:body.expr
    |> create

let create_switch are_rebuilding switch =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else Expr.create_switch switch |> create

let create_invalid reason = Expr.create_invalid reason |> create

let bind_no_simplification are_rebuilding ~bindings ~body ~cost_metrics_of_body
    ~free_names_of_body =
  ListLabels.fold_left (List.rev bindings)
    ~init:(body, cost_metrics_of_body, free_names_of_body)
    ~f:(fun
        (expr, cost_metrics, free_names)
        (var, size_of_defining_expr, defining_expr)
      ->
      let expr =
        create_let are_rebuilding
          (Bound_pattern.singleton var)
          defining_expr ~body:expr ~free_names_of_body:free_names
      in
      let free_names =
        Name_occurrences.union
          (Named.free_names defining_expr)
          (Name_occurrences.remove_var free_names ~var:(Bound_var.var var))
      in
      let is_phantom = Name_mode.is_phantom (Bound_var.name_mode var) in
      let cost_metrics_of_defining_expr =
        Cost_metrics.from_size size_of_defining_expr
      in
      let cost_metrics =
        Cost_metrics.( + ) cost_metrics
          (Cost_metrics.increase_due_to_let_expr ~is_phantom
             ~cost_metrics_of_defining_expr)
      in
      expr, cost_metrics, free_names)

module Equal_for_unique_handler = struct
  (* Computes an approximate equality between terms. Terms that are equal in
     this way can be substituted for each other, and in particular two
     continuation handlers with equal content can be merged. *)

  let named (named1 : Named.t) (named2 : Named.t) =
    match named1, named2 with
    | Simple simple1, Simple simple2 -> Simple.equal simple1 simple2
    | Prim (prim1, _dbg1), Prim (prim2, _dbg2) ->
      Flambda_primitive.equal prim1 prim2
    | (Simple _ | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _), _
      ->
      false

  let rec expr t1 t2 =
    match Expr.descr t1, Expr.descr t2 with
    | Let let_expr1, Let let_expr2 -> let_expr let_expr1 let_expr2
    | Apply_cont apply_cont1, Apply_cont apply_cont2 ->
      apply_cont apply_cont1 apply_cont2
    | (Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _), _ ->
      false

  and let_expr let_expr1 let_expr2 =
    named (Let.defining_expr let_expr1) (Let.defining_expr let_expr2)
    && Let.pattern_match let_expr1 ~f:(fun bound_pattern1 ~body:body1 ->
        Let.pattern_match let_expr2 ~f:(fun bound_pattern2 ~body:body2 ->
            match bound_pattern1, bound_pattern2 with
            | Singleton bound_var1, Singleton bound_var2
              when Flambda_kind.equal
                     (Variable.kind (Bound_var.var bound_var1))
                     (Variable.kind (Bound_var.var bound_var2)) ->
              let body2 =
                Expr.apply_renaming body2
                  (Renaming.add_variable Renaming.empty
                     (Bound_var.var bound_var1) (Bound_var.var bound_var2))
              in
              expr body1 body2
            | (Singleton _ | Set_of_closures _ | Static _), _ -> false))

  and apply_cont apply_cont1 apply_cont2 =
    match
      Apply_cont.trap_action apply_cont1, Apply_cont.trap_action apply_cont2
    with
    | None, None ->
      Continuation.equal
        (Apply_cont.continuation apply_cont1)
        (Apply_cont.continuation apply_cont2)
      && List.equal Simple.equal
           (Apply_cont.args apply_cont1)
           (Apply_cont.args apply_cont2)
    | _ ->
      (* CR-someday bclement: consider trap actions *)
      false

  let continuation_handler params1 handler1 params2 handler2 =
    Flambda_arity.equal_exact
      (Bound_parameters.arity params1)
      (Bound_parameters.arity params2)
    &&
    let renaming =
      List.fold_left2
        (fun renaming param1 param2 ->
          Renaming.add_variable renaming
            (Bound_parameter.var param1)
            (Bound_parameter.var param2))
        Renaming.empty
        (Bound_parameters.to_list params1)
        (Bound_parameters.to_list params2)
    in
    expr handler1 (Expr.apply_renaming handler2 renaming)
end

module Unique_continuation_handlers = struct
  type 'a t =
    (Bound_parameters.t * Expr.t * is_exn_handler:bool * 'a) list
    Numeric_types.Int.Map.t

  let empty = Numeric_types.Int.Map.empty

  let contents_hash are_rebuilding handler ~is_exn_handler
      ~free_names_without_params =
    if ART.do_not_rebuild_terms are_rebuilding
    then Or_null.null
    else
      match handler.contents_hash with
      | Null -> Or_null.null
      | This hash ->
        (* The [contents_hash] does not include variable names, so we include
           the free names of variables in the hash.

           This is an approximation, since we can't distinguish e.g. [x + y]
           from [y + x] in this way, but should be discriminating enough. *)
        Name_occurrences.fold_variables free_names_without_params
          ~init:(Hashtbl.hash (hash, Bool.hash is_exn_handler))
          ~f:(fun hash var -> Hashtbl.hash (hash, Variable.hash var))
        |> Or_null.this

  let add are_rebuilding params handler ~is_exn_handler
      ~free_names_without_params value t =
    match
      contents_hash are_rebuilding handler ~is_exn_handler
        ~free_names_without_params
    with
    | Null -> t
    | This hash ->
      let entries =
        match Numeric_types.Int.Map.find_or_null hash t with
        | Null -> []
        | This entries -> entries
      in
      Numeric_types.Int.Map.add hash
        ((params, handler.expr, ~is_exn_handler, value) :: entries)
        t

  let find are_rebuilding params handler ~is_exn_handler
      ~free_names_without_params t =
    match
      contents_hash are_rebuilding handler ~is_exn_handler
        ~free_names_without_params
    with
    | Null -> raise Not_found
    | This hash ->
      let _, _, ~is_exn_handler:_, value =
        List.find
          (fun ( other_params,
                 other_handler,
                 ~is_exn_handler:other_is_exn_handler,
                 _ ) ->
            Bool.equal is_exn_handler other_is_exn_handler
            && Equal_for_unique_handler.continuation_handler params handler.expr
                 other_params other_handler)
          (Numeric_types.Int.Map.find hash t)
      in
      value
end
