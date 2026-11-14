open AffineSsa
open FourierMotzkin
open InductionVar
open NaturalLoop
open SsaModel

type range_certificate =
  { biv : biv;
    lower : int;
    upper : Ssa.finished Ssa.Value.t option
  }

val literal_argument : block -> int -> block -> int option

val entry_predecessors : natural_loop -> block list

val least_literal_argument : natural_loop -> int -> int option

val argument_at_most : Ssa.finished Ssa.Value.t -> block -> int -> block -> bool

val compared_with :
  block list ->
  (Ssa.finished Ssa.Value.t -> bool) ->
  Ssa.finished Ssa.Value.t list

val upper_candidates :
  block list ->
  block ->
  int ->
  Ssa.finished Ssa.Value.t list ->
  Ssa.finished Ssa.Value.t list

val defined_above : op_def -> block -> Ssa.finished Ssa.Value.t -> bool

val verify_upper :
  op_def -> natural_loop -> int -> Ssa.finished Ssa.Value.t -> bool

val find_upper : op_def -> block list -> biv -> Ssa.finished Ssa.Value.t option

val lower_facts : int -> Affine.t -> Affine.t list

val bound_facts :
  ctx ->
  Affine.t list ->
  int ->
  Ssa.finished Ssa.Value.t option ->
  Affine.t ->
  (Affine.t list * Affine.t list) * ctx

val no_wrap_at :
  int -> int -> Ssa.finished Ssa.Value.t option -> block -> block -> int -> bool

val verify_certificate :
  block -> biv -> int -> Ssa.finished Ssa.Value.t option -> bool

val find_certificates : op_def -> block -> block list -> range_certificate list

val certificate_facts : ctx -> range_certificate -> Affine.t list * ctx

val add_range_fact :
  block -> Affine.t list * ctx -> range_certificate -> Affine.t list * ctx

val range_facts : range_certificate list -> block -> ctx -> Affine.t list * ctx
