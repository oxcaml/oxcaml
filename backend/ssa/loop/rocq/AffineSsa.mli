open AffineExpr
open FourierMotzkin
open LoopComparisons
open SsaModel

type ctx =
  { atoms : (int * Ssa.finished Ssa.Value.t) list;
    next : int;
    budget : int
  }

val new_ctx : ctx

val refill_budget : ctx -> ctx

val spend_budget : ctx -> int -> ctx

val intern : ctx -> Ssa.finished Ssa.Value.t -> int * ctx

val atomize : ctx -> Ssa.finished Ssa.Value.t -> (t * Range.t option) * ctx

val keep_if_range_known :
  ctx ->
  Ssa.finished Ssa.Value.t ->
  t ->
  Range.t option ->
  (t * Range.t option) * ctx

val add_optional_ranges : Range.t option -> Range.t option -> Range.t option

val sub_optional_ranges : Range.t option -> Range.t option -> Range.t option

val scale_optional_range : int -> Range.t option -> Range.t option

val expr_of_value :
  ctx -> Ssa.finished Ssa.Value.t -> (t * Range.t option) * ctx

val push_sides : Affine.t list -> Affine.t list -> Affine.t list

val linearize :
  ctx ->
  Affine.t list ->
  Ssa.finished Ssa.Value.t ->
  (Affine.t * Affine.t list) * ctx

val comparison_facts :
  ctx ->
  Affine.t list ->
  bool ->
  Ssa.finished Ssa.Value.t ->
  (Affine.t list * Affine.t list) * ctx

val guard_test : block -> block -> (Ssa.finished Ssa.Value.t * bool) option

val guard_facts :
  block ->
  (Affine.t list * Affine.t list) * ctx ->
  block ->
  (Affine.t list * Affine.t list) * ctx

val guards_at :
  ctx -> Affine.t list -> block -> (Affine.t list * Affine.t list) * ctx
