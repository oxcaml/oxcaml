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

type contents_hash =
  { depth : int;
    structural_hash : int
  }

type t =
  { expr : Expr.t;
    contents_hash : contents_hash Or_null.t
        (* If not null, this is a structural hash of the contents of this
           rebuilt expression (always null when not rebuilding terms).

           The hash should not depend on the name of variables within the
           expression, because we use it to de-duplicate continuation handlers
           that might bind variables with different names (see
           [Unique_continuation_map]). *)
  }

(* Rebuilt terms with no [contents_hash] cannot be deduplicated (e.g. because
   they contain not-shareable subterms such as sets of closures). We also want
   to clear it for terms that are "too big": sharing those is unlikely to
   succeed, and might cause hash collisions.

   This limit is added out of abundance of caution; it is likely fine to
   increase it or even remove it completely if needed (e.g. we end up having
   programs where we do want to share very large terms). Note that unlike the
   sharing of match branches in [lambda.ml] (which computes a sharing key with
   the same size as the term), there is no prohibitive memory cost to increasing
   this limit.

   The value of [32] matches [max_raw] in [lambda.ml], which is at the time of
   writing the limit for the size of sub-expressions that can be shared across
   switch branches. *)
let max_hash_depth = 32

let create ?contents_hash expr =
  (* Since we are building terms from the bottom-up, we don't know initially at
     which depth they will end up. Instead, we eagerly compute hashes as we
     rebuild expressions (so that computing the hash does not need another
     traversal), and zero it out when we reach the maximum depth. *)
  let contents_hash =
    match contents_hash with
    | Some { depth; _ } when depth >= max_hash_depth -> Or_null.null
    | _ -> Or_null.of_option contents_hash
  in
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
      | This { depth; structural_hash = body_hash } -> (
        let[@local] simple_expr named_hash =
          Some
            { depth = depth + 1;
              structural_hash = Hashtbl.hash (1, named_hash, body_hash)
            }
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
        { depth = 0;
          structural_hash =
            Hashtbl.hash
              ( 0,
                Continuation.hash (Apply_cont.continuation apply_cont),
                List.map contents_hash_simple (Apply_cont.args apply_cont) )
        }
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

module Matching_for_unique_handler = struct
  (* Computes an approximate equality between terms. Terms that are equal in
     this way can be substituted for each other, and in particular two
     continuation handlers with equal content can be merged.

     As an extension, we support detecting continuations that have identical
     handlers up to permutation of their parameters.

     This is done by a matching algorithm that enforce a bipartite matching
     between the parameters of the two continuations when they occur at the same
     position in the term. *)

  exception Match_failure

  let fail () = raise Match_failure

  let fail_if_not b = if not b then raise Match_failure

  let fail_if_not_equal f x y = fail_if_not (f x y)

  module Match_permutable_parameters : sig
    (** Bipartite matcher between two lists of parameters.

        This is an imperative data structure that gets updated by calling
        [match_variable]. *)
    type t

    (** Create a new matcher.

        Fails if the two parameter lists have distinct number of parameters. *)
    val create : params1:Bound_parameters.t -> params2:Bound_parameters.t -> t

    (** [match_variable t var1 var2] records that [var1] and [var2] must match
        in the respective parameter lists.

        Fails if either [var1] or [var2] are not part of this matching, if
        either has already been matched to another variable, or if [var1] and
        [var2] are bound with incompatible kinds in their respective parameter
        lists. *)
    val match_variable : t -> Variable.t -> Variable.t -> unit

    (** Returns an argument that can be used to bind the provided parameter from
        the second list from a parameter in the first list.

        Fails if the parameter has not been matched. *)
    val matching_variable_for_param2 :
      t -> param2:Bound_parameter.t -> Variable.t
  end = struct
    module HV = Hashtbl.Make (Variable)

    (** Represents a parameter in one of the two parameter lists, and maps it to
        the corresponding parameter (as a variable) in the other list. *)
    type parameter_on_one_side =
      { kind : Flambda_kind.With_subkind.t;
        mutable matching_variable_on_other_side : Variable.t option
      }

    type t =
      { params1 : parameter_on_one_side HV.t;
            (** Keys are parameters from the first side (identified by their
                variable). *)
        params2 : parameter_on_one_side HV.t
            (** Keys are parameters from the second side (identified by their
                variable). *)
      }

    let matching_variable_for_param2 t ~param2 =
      match HV.find t.params2 (Bound_parameter.var param2) with
      | exception Not_found ->
        Misc.fatal_errorf "Parameter %a is not bound on the second side"
          Bound_parameter.print param2
      | { matching_variable_on_other_side = None; _ } -> fail ()
      | { matching_variable_on_other_side = Some var; _ } -> var

    let create ~params1 ~params2 =
      (* Must have the same number of parameters in both cases, but we allow
         permutations -- kinds are checked in [match_variable]. *)
      if not (Bound_parameters.same_number params1 params2) then fail ();
      let create_mapping params =
        let table = HV.create 16 in
        List.iter
          (fun param ->
            HV.replace table
              (Bound_parameter.var param)
              { kind = Bound_parameter.kind param;
                matching_variable_on_other_side = None
              })
          (Bound_parameters.to_list params);
        table
      in
      { params1 = create_mapping params1; params2 = create_mapping params2 }

    let match_variable t var1 var2 =
      match HV.find t.params1 var1 with
      | exception Not_found ->
        (* [var1] is not a parameter for this permutation *) fail ()
      | { matching_variable_on_other_side = Some var2'; _ } ->
        (* [var1] is a parameter and is already matched *)
        fail_if_not_equal Variable.equal var2' var2
      | { matching_variable_on_other_side = None; kind = kind1 } as binding1
        -> (
        (* [var1] is a parameter, but not yet matched: try to match it. *)
        match HV.find t.params2 var2 with
        | (exception Not_found)
        | { matching_variable_on_other_side = Some _; _ } ->
          (* [var2] is either not a parameter of the same continuation, or
             already matched to another parameter in the first list. *)
          fail ()
        | { matching_variable_on_other_side = None; kind = kind2 } as
          (* [var2] is a parameter of the same continuation and is not yet
             matched: we can match [var1] and [var2] if the kinds agree. *)
          binding2 ->
          fail_if_not_equal Flambda_kind.With_subkind.equal kind1 kind2;
          binding1.matching_variable_on_other_side <- Some var2;
          binding2.matching_variable_on_other_side <- Some var1)
  end

  (** Maps each variable that is a parameter to the corresponding matching
      between two [Bound_parameters.t].

      {b Note}: Matching information is recorded by mutably modifying the
      [Match_permutable_parameters.t] instance(s), so environments should only
      be passed down the call stack, not returned. *)
  type matching_env =
    { permutable_params1 : Match_permutable_parameters.t Variable.Map.t;
      permutable_params2 : Match_permutable_parameters.t Variable.Map.t
    }

  let match_variable env var1 var2 =
    match Variable.Map.find_or_null var1 env.permutable_params1 with
    | Null ->
      (* [var1] is not a parameter: both variables must be equal, and [var2]
         must not be a parameter either. *)
      if Variable.Map.mem var2 env.permutable_params2 then fail ();
      fail_if_not_equal Variable.equal var1 var2
    | This permutation ->
      Match_permutable_parameters.match_variable permutation var1 var2

  module type Equal_and_free_names = sig
    type t

    val equal : t -> t -> bool

    val free_names : t -> Name_occurrences.t
  end

  let match_equal_and_free_names (type t)
      (module T : Equal_and_free_names with type t = t) env t1 t2 =
    (* Make sure that the variables contained within are equal on both sides. *)
    fail_if_not_equal T.equal t1 t2;
    Name_occurrences.fold_variables (T.free_names t1) ~init:() ~f:(fun () var ->
        match_variable env var var)

  let match_name env name1 name2 =
    Name.pattern_match name1
      ~var:(fun var1 ->
        Name.pattern_match name2
          ~var:(fun var2 -> match_variable env var1 var2)
          ~symbol:(fun _ -> fail ()))
      ~symbol:(fun symbol1 ->
        fail_if_not_equal Name.equal name2 (Name.symbol symbol1))

  let match_simple env simple1 simple2 =
    Simple.pattern_match simple1
      ~const:(fun const1 ->
        Simple.pattern_match simple2
          ~const:(fun const2 ->
            fail_if_not_equal Reg_width_const.equal const1 const2)
          ~name:(fun _ ~coercion:_ -> fail ()))
      ~name:(fun name1 ~coercion:coercion1 ->
        Simple.pattern_match simple2
          ~const:(fun _ -> fail ())
          ~name:(fun name2 ~coercion:coercion2 ->
            match_equal_and_free_names (module Coercion) env coercion1 coercion2;
            match_name env name1 name2))

  (* Like [iter2], but raises [Match_failure] if the lengths are not equal *)
  let rec match_list match_elt env xs ys =
    match xs, ys with
    | [], [] -> ()
    | [], _ | _, [] -> fail ()
    | x :: xs, y :: ys ->
      match_elt env x y;
      match_list match_elt env xs ys

  let match_simples env simples1 simples2 =
    match_list match_simple env simples1 simples2

  let match_primitive env prim1 prim2 =
    let module P = Flambda_primitive in
    match_equal_and_free_names
      (module P.Without_args)
      env (P.without_args prim1) (P.without_args prim2);
    match_simples env (P.args prim1) (P.args prim2)

  let match_named env (named1 : Named.t) (named2 : Named.t) =
    match named1, named2 with
    | Simple simple1, Simple simple2 -> match_simple env simple1 simple2
    | Prim (prim1, _dbg1), Prim (prim2, _dbg2) ->
      match_primitive env prim1 prim2
    | (Simple _ | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _), _
      ->
      fail ()

  let rec match_expr env t1 t2 =
    (* CR-someday bclement: consider sharing more expressions, e.g. apply
       switches, and maybe let conts -- [bind_permutable_parameters] should
       allow to do this up to permutation of their parameters, but make sure
       it's not too expensive. *)
    match Expr.descr t1, Expr.descr t2 with
    | Let let_expr1, Let let_expr2 -> match_let_expr env let_expr1 let_expr2
    | Apply_cont apply_cont1, Apply_cont apply_cont2 ->
      match_apply_cont env apply_cont1 apply_cont2
    | (Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _), _ ->
      fail ()

  and match_let_expr env let_expr1 let_expr2 =
    (* This call to [match_named] ensures that the kinds for the bound patterns
       below match. *)
    match_named env (Let.defining_expr let_expr1) (Let.defining_expr let_expr2);
    Let.pattern_match let_expr1 ~f:(fun bound_pattern1 ~body:body1 ->
        Let.pattern_match let_expr2 ~f:(fun bound_pattern2 ~body:body2 ->
            match bound_pattern1, bound_pattern2 with
            | Singleton bound_var1, Singleton bound_var2 ->
              let body2 =
                Expr.apply_renaming body2
                  (Renaming.add_variable Renaming.empty
                     (Bound_var.var bound_var2) (Bound_var.var bound_var1))
              in
              match_expr env body1 body2
            | (Singleton _ | Set_of_closures _ | Static _), _ -> fail ()))

  and match_apply_cont env apply_cont1 apply_cont2 =
    match
      Apply_cont.trap_action apply_cont1, Apply_cont.trap_action apply_cont2
    with
    | None, None ->
      fail_if_not_equal Continuation.equal
        (Apply_cont.continuation apply_cont1)
        (Apply_cont.continuation apply_cont2);
      match_simples env
        (Apply_cont.args apply_cont1)
        (Apply_cont.args apply_cont2)
    | _ ->
      (* CR-someday bclement: consider trap actions *)
      fail ()

  let match_permutable_continuation_handler params1 handler1 params2 handler2 =
    let permutation = Match_permutable_parameters.create ~params1 ~params2 in
    let create_permutable_params params =
      List.fold_left
        (fun env param ->
          Variable.Map.add (Bound_parameter.var param) permutation env)
        Variable.Map.empty
        (Bound_parameters.to_list params)
    in
    let permutable_params1 = create_permutable_params params1 in
    let permutable_params2 = create_permutable_params params2 in
    match_expr { permutable_params1; permutable_params2 } handler1 handler2;
    (* We are matching continuation handlers after rebuilding/dataflow, so we
       expect that all parameters are used and we can reconstruct a suitable
       bijection, so if we get there, this should never raise (but it is also
       harmless if it does). *)
    List.map
      (fun param2 ->
        Simple.var
          (Match_permutable_parameters.matching_variable_for_param2 permutation
             ~param2))
      (Bound_parameters.to_list params2)

  let match_non_recursive_continuation_handler ~is_exn_handler params1 handler1
      params2 handler2 =
    if is_exn_handler
    then (
      (* If we are trying to share exception handlers, their first (exception)
         argument must match. *)
      match
        Bound_parameters.to_list params1, Bound_parameters.to_list params2
      with
      | [], _ | _, [] -> fail ()
      | exn1 :: params1, exn2 :: params2 ->
        fail_if_not_equal Flambda_kind.With_subkind.equal
          (Bound_parameter.kind exn1)
          (Bound_parameter.kind exn2);
        let handler2 =
          Expr.apply_renaming handler2
            (Renaming.add_variable Renaming.empty (Bound_parameter.var exn2)
               (Bound_parameter.var exn1))
        in
        Bound_parameter.simple exn1
        :: match_permutable_continuation_handler
             (Bound_parameters.create params1)
             handler1
             (Bound_parameters.create params2)
             handler2)
    else match_permutable_continuation_handler params1 handler1 params2 handler2
end

let match_continuation_handler ~is_exn_handler params1 handler1 params2 handler2
    =
  let open Matching_for_unique_handler in
  match
    match_non_recursive_continuation_handler ~is_exn_handler params1 handler1
      params2 handler2
  with
  | exception Match_failure -> None
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
      | This { depth; structural_hash } ->
        (* The [contents_hash] does not include variable names (it is not clear
           how to do this up to alpha-equivalence since we are computing the
           hashes from the bottom up), so we include the free names of variables
           in the hash.

           This is an approximation, since we can't distinguish e.g. [x + y]
           from [y + x] in this way, but should be discriminating enough.

           CR-someday bclement: consider occurrence count. *)
        Name_occurrences.fold_variables free_names_without_params
          ~init:
            (Hashtbl.hash (depth, structural_hash, Bool.hash is_exn_handler))
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
              (match_continuation_handler ~is_exn_handler params handler.expr
                 other_params other_handler)
          else None)
      |> Option.bind (Numeric_types.Int.Map.find_opt hash t)
end
