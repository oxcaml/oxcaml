open NaturalLoop
open SsaModel

val is_loop_invariant : op_def -> block -> Ssa.finished Ssa.Value.t -> bool

type biv_step =
  | Step_const of int
  | Step_var of Ssa.finished Ssa.Value.t

type biv =
  { loop : natural_loop;
    param_index : int;
    update : Ssa.finished Ssa.Value.t list;
    step : biv_step;
    sign : [`Add | `Sub]
  }

val step_same : biv_step -> biv_step -> bool

val sign_same : [`Add | `Sub] -> [`Add | `Sub] -> bool

val signed_step : biv -> int option

val classify_update :
  (Ssa.finished Ssa.Value.t -> bool) ->
  op_def ->
  block ->
  Ssa.finished Ssa.Value.t ->
  (biv_step * [`Add | `Sub]) option

val back_edge_set : natural_loop -> blockset

val classify_param :
  natural_loop ->
  op_def ->
  (block * Ssa.finished Ssa.Terminator.arg list) list ->
  int ->
  biv option

val params_bivs :
  natural_loop ->
  op_def ->
  (block * Ssa.finished Ssa.Terminator.arg list) list ->
  int ->
  Ssa.finished Ssa.Value.t list ->
  biv list

val analyze_loop : op_def -> natural_loop -> biv list

val analyze : op_def -> block list -> (natural_loop * biv list) list
