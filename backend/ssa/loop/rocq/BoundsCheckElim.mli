open AffineSsa
open FourierMotzkin
open InductionRange
open SsaModel

type limit =
  | Value_limit of Ssa.finished Ssa.Value.t
  | Const_limit of int

val unsigned_check :
  Ssa.finished Ssa.Value.t ->
  ((Cmm.integer_comparison * Ssa.finished Ssa.Value.t) * limit) option

val check_goals :
  Cmm.integer_comparison -> Affine.t -> Affine.t -> Affine.t list option

val limit_form :
  ctx -> Affine.t list -> limit -> (Affine.t * Affine.t list) * ctx

val check_passes :
  range_certificate list -> block -> Ssa.finished Ssa.Value.t -> bool

val eliminate_check : range_certificate list -> block -> (block * block) option

val find_eliminations : op_def -> block -> block list -> (block * block) list
