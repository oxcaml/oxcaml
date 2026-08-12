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

module Unify_for_unique_handler = struct
  (* Computes an approximate equality between terms. Terms that are equal in
     this way can be substituted for each other, and in particular two
     continuation handlers with equal content can be merged.

     As an extension, we support detecting continuations that have identical
     handlers up to permutation of their parameters. *)

  exception Cannot_unify

  let cannot_unify () = raise Cannot_unify

  let must_hold b = if not b then raise Cannot_unify

  let must_equal f x y = must_hold (f x y)

  module HV = Hashtbl.Make (Variable)

  type parameter_in_right_env =
    { parameter_in_right_env : Bound_parameter.t;
      mutable name_in_left_env : Variable.t option
    }

  type name_in_right_env =
    | Not_yet_renamed of parameter_in_right_env HV.t
    | Name_in_right_env of Variable.t

  type parameter_in_left_env =
    { parameter_in_left_env : Bound_parameter.t;
      mutable name_in_right_env : name_in_right_env
    }

  let empty = Variable.Map.empty

  (* Allows any bijection between [params1] in the left environment and
     [params2] in the right environment. *)
  let bind_permutable_parameters env params1 params2 =
    (* Must have the same number of parameters for both handlers, but we allow
       permutations -- kinds are checked in [unify_variable]. *)
    if not (Bound_parameters.same_number params1 params2) then cannot_unify ();
    (* The same hash table is shared across all the parameters for the same
       bijection, but not across other bijections. This ensures that we can only
       bind the parameters in [params1] to a parameter in [params2], not to
       arbitrary variables. *)
    let name_in_right_env = HV.create 16 in
    let args2 =
      List.map
        (fun param2 ->
          let binding2 =
            { parameter_in_right_env = param2; name_in_left_env = None }
          in
          HV.replace name_in_right_env (Bound_parameter.var param2) binding2;
          binding2)
        (Bound_parameters.to_list params2)
    in
    let name_in_right_env = Not_yet_renamed name_in_right_env in
    let env =
      List.fold_left
        (fun params1 param1 ->
          let binding1 =
            { parameter_in_left_env = param1; name_in_right_env }
          in
          Variable.Map.add (Bound_parameter.var param1) binding1 params1)
        env
        (Bound_parameters.to_list params1)
    in
    env, args2

  let unify_variable env var1 var2 =
    match Variable.Map.find_or_null var1 env with
    | Null -> must_equal Variable.equal var1 var2
    | This { name_in_right_env = Name_in_right_env var2'; _ } ->
      must_equal Variable.equal var2' var2
    | This
        ({ name_in_right_env = Not_yet_renamed renamed2;
           parameter_in_left_env = param1
         } as binding1) -> (
      match HV.find renamed2 var2 with
      | (exception Not_found) | { name_in_left_env = Some _; _ } ->
        raise Cannot_unify
      | { name_in_left_env = None; parameter_in_right_env = param2 } as binding2
        ->
        must_equal Flambda_kind.With_subkind.equal
          (Bound_parameter.kind param1)
          (Bound_parameter.kind param2);
        HV.remove renamed2 var2;
        binding1.name_in_right_env <- Name_in_right_env var2;
        binding2.name_in_left_env <- Some var1)

  let to_args_exn args =
    List.map
      (fun binding ->
        match binding.name_in_left_env with
        | None -> raise Cannot_unify
        | Some var -> Simple.var var)
      args

  module type Equal_and_free_names = sig
    type t

    val equal : t -> t -> bool

    val free_names : t -> Name_occurrences.t
  end

  let unify_equal_and_free_names (type t)
      (module T : Equal_and_free_names with type t = t) env t1 t2 =
    (* Make sure that the variables contained within are equal on both sides. *)
    must_equal T.equal t1 t2;
    Name_occurrences.fold_variables (T.free_names t1) ~init:() ~f:(fun () var ->
        unify_variable env var var)

  let unify_name env name1 name2 =
    Name.pattern_match name1
      ~var:(fun var1 ->
        Name.pattern_match name2
          ~var:(fun var2 -> unify_variable env var1 var2)
          ~symbol:(fun _ -> cannot_unify ()))
      ~symbol:(fun symbol1 -> must_equal Name.equal name2 (Name.symbol symbol1))

  let unify_simple env simple1 simple2 =
    Simple.pattern_match simple1
      ~const:(fun const1 ->
        Simple.pattern_match simple2
          ~const:(fun const2 -> must_equal Reg_width_const.equal const1 const2)
          ~name:(fun _ ~coercion:_ -> cannot_unify ()))
      ~name:(fun name1 ~coercion:coercion1 ->
        Simple.pattern_match simple2
          ~const:(fun _ -> cannot_unify ())
          ~name:(fun name2 ~coercion:coercion2 ->
            unify_equal_and_free_names (module Coercion) env coercion1 coercion2;
            unify_name env name1 name2))

  let rec unify_list unify env xs ys =
    match xs, ys with
    | [], [] -> ()
    | [], _ | _, [] -> cannot_unify ()
    | x :: xs, y :: ys ->
      unify env x y;
      unify_list unify env xs ys

  let unify_simples env simples1 simples2 =
    unify_list unify_simple env simples1 simples2

  let unify_primitive env prim1 prim2 =
    let module P = Flambda_primitive in
    unify_equal_and_free_names
      (module P.Without_args)
      env (P.without_args prim1) (P.without_args prim2);
    unify_simples env (P.args prim1) (P.args prim2)

  let unify_named env (named1 : Named.t) (named2 : Named.t) =
    match named1, named2 with
    | Simple simple1, Simple simple2 -> unify_simple env simple1 simple2
    | Prim (prim1, _dbg1), Prim (prim2, _dbg2) ->
      unify_primitive env prim1 prim2
    | (Simple _ | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _), _
      ->
      cannot_unify ()

  let rec unify_expr env t1 t2 =
    (* CR-someday bclement: consider sharing more expressions, e.g. apply
       switches, and maybe let conts -- [bind_permutable_parameters] should
       allow to do this up to permutation of their parameters, but make sure
       it's not too expensive. *)
    match Expr.descr t1, Expr.descr t2 with
    | Let let_expr1, Let let_expr2 -> unify_let_expr env let_expr1 let_expr2
    | Apply_cont apply_cont1, Apply_cont apply_cont2 ->
      unify_apply_cont env apply_cont1 apply_cont2
    | (Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _), _ ->
      cannot_unify ()

  and unify_let_expr env let_expr1 let_expr2 =
    (* This call to [unify_named] ensures that the kinds for the bound patterns
       below match. *)
    unify_named env (Let.defining_expr let_expr1) (Let.defining_expr let_expr2);
    Let.pattern_match let_expr1 ~f:(fun bound_pattern1 ~body:body1 ->
        Let.pattern_match let_expr2 ~f:(fun bound_pattern2 ~body:body2 ->
            match bound_pattern1, bound_pattern2 with
            | Singleton bound_var1, Singleton bound_var2 ->
              let body2 =
                Expr.apply_renaming body2
                  (Renaming.add_variable Renaming.empty
                     (Bound_var.var bound_var2) (Bound_var.var bound_var1))
              in
              unify_expr env body1 body2
            | (Singleton _ | Set_of_closures _ | Static _), _ -> cannot_unify ()))

  and unify_apply_cont env apply_cont1 apply_cont2 =
    match
      Apply_cont.trap_action apply_cont1, Apply_cont.trap_action apply_cont2
    with
    | None, None ->
      must_equal Continuation.equal
        (Apply_cont.continuation apply_cont1)
        (Apply_cont.continuation apply_cont2);
      unify_simples env
        (Apply_cont.args apply_cont1)
        (Apply_cont.args apply_cont2)
    | _ ->
      (* CR-someday bclement: consider trap actions *)
      cannot_unify ()

  let unify_permutable_continuation_handler env params1 handler1 params2
      handler2 =
    let env, args = bind_permutable_parameters env params1 params2 in
    unify_expr env handler1 handler2;
    (* We are unifying continuation handlers after rebuilding/dataflow, so we
       expect that all parameters are used and we can reconstruct a suitable
       bijection, so if we get there, [to_args_exn] should never raise. *)
    to_args_exn args

  let unify_non_recursive_continuation_handler env ~is_exn_handler params1
      handler1 params2 handler2 =
    if is_exn_handler
    then (
      (* If we are trying to share exception handlers, their first (exception)
         argument must match. *)
      match
        Bound_parameters.to_list params1, Bound_parameters.to_list params2
      with
      | [], _ | _, [] -> cannot_unify ()
      | exn1 :: params1, exn2 :: params2 ->
        must_equal Flambda_kind.With_subkind.equal
          (Bound_parameter.kind exn1)
          (Bound_parameter.kind exn2);
        let handler2 =
          Expr.apply_renaming handler2
            (Renaming.add_variable Renaming.empty (Bound_parameter.var exn2)
               (Bound_parameter.var exn1))
        in
        Bound_parameter.simple exn1
        :: unify_permutable_continuation_handler env
             (Bound_parameters.create params1)
             handler1
             (Bound_parameters.create params2)
             handler2)
    else
      unify_permutable_continuation_handler env params1 handler1 params2
        handler2
end

let unify_continuation_handler ~is_exn_handler params1 handler1 params2 handler2
    =
  let open Unify_for_unique_handler in
  match
    unify_non_recursive_continuation_handler ~is_exn_handler empty params1
      handler1 params2 handler2
  with
  | exception Cannot_unify -> None
  | args -> Some args

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

  let find_opt are_rebuilding params handler ~is_exn_handler
      ~free_names_without_params t =
    match
      contents_hash are_rebuilding handler ~is_exn_handler
        ~free_names_without_params
    with
    | Null -> None
    | This hash ->
      List.find_map
        (fun
          ( other_params,
            other_handler,
            ~is_exn_handler:other_is_exn_handler,
            value )
        ->
          if Bool.equal is_exn_handler other_is_exn_handler
          then
            Option.map
              (fun args -> value, args)
              (unify_continuation_handler ~is_exn_handler params handler.expr
                 other_params other_handler)
          else None)
      |> Option.bind (Numeric_types.Int.Map.find_opt hash t)
end
