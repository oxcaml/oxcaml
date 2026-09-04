open! Flambda

type after_rebuild =
  | After_rebuild_id
  | After_rebuild_let : 'simplify_named_result after_rebuild_let_data *
      ('simplify_named_result after_rebuild_let_data -> Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t) -> after_rebuild
  | After_rebuild_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)
  | After_rebuild_single_non_recursive_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)
  | After_rebuild_single_recursive_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)

and 'simplify_named_result after_rebuild_let_data = {
  simplify_named_result : 'simplify_named_result;
  (* Simplify_named_result.t but hidden to avoid introducing dependency *)
  removed_operations : Removed_operations.t;
  lifted_constants_from_defining_expr : Lifted_constant_state.t;
  at_unit_toplevel : bool;
  closure_info : Closure_info.t;
  after_rebuild : after_rebuild;
  rewrite_id : Named_rewrite_id.t;
}

type 'a rebuild = Rebuild of (Upwards_acc.t -> after_rebuild:after_rebuild -> 'a)

type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

type 'a expr_simplifier =
  Downwards_acc.t ->
  'a ->
  down_to_up:
    (Rebuilt_expr.t * Upwards_acc.t, Rebuilt_expr.t * Upwards_acc.t) down_to_up ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_expr = Flambda.Expr.t expr_simplifier

let apply_after_rebuild (after_rebuild : after_rebuild) expr uacc =
  match after_rebuild with
  | After_rebuild_id ->
      expr, uacc
  | After_rebuild_let (data, after_rebuild) ->
      after_rebuild data expr uacc
  | After_rebuild_let_cont after_rebuild ->
      after_rebuild expr uacc
  | After_rebuild_single_non_recursive_let_cont after_rebuild ->
      after_rebuild expr uacc
  | After_rebuild_single_recursive_let_cont after_rebuild ->
      after_rebuild expr uacc

let apply_rebuild rebuild uacc ~after_rebuild =
  match rebuild with
  | Rebuild rebuild ->
      rebuild uacc ~after_rebuild
