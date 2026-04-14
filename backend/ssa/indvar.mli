[@@@ocaml.warning "+a-40-41-42"]

(** Simple induction-variable analysis over the SSA representation.

    Detects natural loops via dominators, then classifies each loop header's
    block parameters as basic induction variables if, on every back edge, the
    incoming value is [param + c] or [param - c] for some loop-invariant [c]. *)

type loop =
  { header : Ssa.block;
    body : Ssa.Block.Set.t;
    back_edges : Ssa.block list
  }

type step =
  | Step_const of int
  | Step_var of Ssa.instruction

type biv =
  { loop : loop;
    param_index : int;
    init : Ssa.instruction list;
    step : step;
    sign : [ `Add | `Sub ]
  }

val analyze : Ssa.t -> biv list

val print : Format.formatter -> biv list -> unit
