open! Flambda

type 'a after_rebuild = After_rebuild of (Rebuilt_expr.t -> Upwards_acc.t -> 'a)

type 'a rebuild = Upwards_acc.t -> after_rebuild:'a after_rebuild -> 'a

type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

type 'a expr_simplifier =
  Downwards_acc.t ->
  'a ->
  down_to_up:
    (Rebuilt_expr.t * Upwards_acc.t, Rebuilt_expr.t * Upwards_acc.t) down_to_up ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_expr = Flambda.Expr.t expr_simplifier

let apply_after_rebuild (after_rebuild : 'a after_rebuild) expr uacc =
  match after_rebuild with
  | After_rebuild after_rebuild ->
      after_rebuild expr uacc
