[@@@ocaml.warning "+a-40-41-42"]

(** Natural-loop and basic induction variable detection over the SSA
    representation.

    A loop's header is a block to which a CFG back edge points (i.e. an edge
    [u -> v] where [v] dominates [u]). A basic induction variable (BIV) is a
    block parameter [i] of the header such that, on every back edge, the
    incoming value is [i + c], [c + i] or [i - c] for some loop-invariant
    [c]. *)

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
    update : Ssa.instruction list;
    step : step;
    sign : [ `Add | `Sub ]
  }

val analyze : Ssa.t -> (loop * biv list) list

val print : Format.formatter -> (loop * biv list) list -> unit

(** SSA value equality used throughout the loop analyses: two [Op]s are equal
    iff their ids match, two [Block_param]s are equal iff they share block
    and index, and all other comparisons return [false]. We do not recurse
    into [Proj]. *)
val instr_same : Ssa.instruction -> Ssa.instruction -> bool
