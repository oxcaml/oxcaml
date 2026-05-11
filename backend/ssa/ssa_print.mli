module Make (S : Ssa.Finished_graph) : sig
  val print_block : Format.formatter -> S.Block.t -> unit

  val print : Format.formatter -> unit
end

val print : Format.formatter -> (module Ssa.Finished_graph) -> unit
