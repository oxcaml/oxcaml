open! Flambda

type after_rebuild =
  | After_rebuild_id
  | After_rebuild_let of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)
  | After_rebuild_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)
  | After_rebuild_single_non_recursive_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)
  | After_rebuild_single_recursive_let_cont of
      (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t)

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
  | After_rebuild_let after_rebuild ->
      after_rebuild expr uacc
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
