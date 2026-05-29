[@@@ocaml.warning "+a-40-41-42"]

(** Natural-loop and basic induction variable detection over the SSA
    representation.

    A loop's header is a block to which a CFG back edge points (i.e. an edge
    [u -> v] where [v] dominates [u]). A basic induction variable (BIV) is a
    block parameter [i] of the header such that, on every back edge, the
    incoming value is [i + c], [c + i] or [i - c] for some loop-invariant [c].
*)

module Make (S : Ssa.Finished_graph) : sig
  type loop =
    { header : S.Block.t;
      body : S.Block.Set.t;
      back_edges : S.Block.t list
    }

  type step =
    | Step_const of int
    | Step_var of S.Instruction.t

  type biv =
    { loop : loop;
      param_index : int;
      init : S.Instruction.t list;
      update : S.Instruction.t list;
      step : step;
      sign : [`Add | `Sub]
    }

  val analyze : unit -> (loop * biv list) list

  val print : Format.formatter -> (loop * biv list) list -> unit

  (** SSA value equality used throughout the loop analyses: two [Op]s are equal
      iff their ids match, two [Block_param]s are equal iff they share block and
      index, and all other comparisons return [false]. We do not recurse into
      [Proj]. *)
  val instr_same : S.Instruction.t -> S.Instruction.t -> bool

  (** [is_header_param block index v] is [true] iff [v] is the [Block_param]
      referring to parameter [index] of [block]. Used in place of building a
      [Block_param] value, which a finished graph does not let us construct. *)
  val is_header_param : S.Block.t -> int -> S.Instruction.t -> bool
end
