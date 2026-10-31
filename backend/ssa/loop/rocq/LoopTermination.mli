open AffineSsa
open FourierMotzkin
open InductionVar
open LoopComparisons
open NaturalLoop
open SsaModel

type t =
  | Terminates
  | Unknown

type bound =
  | Value_bound of Ssa.finished Ssa.Value.t
  | Const_bound of int

val bound_in_range : int -> block -> bound -> bool

val extract_comparison :
  (Ssa.finished Ssa.Value.t -> bool) ->
  (Ssa.finished Ssa.Value.t -> bool) ->
  Ssa.finished Ssa.Value.t ->
  ((Cmm.integer_comparison * bool) * bound) option

val biv_implies_termination : op_def -> biv -> bool

val analyze : op_def -> biv list -> t
