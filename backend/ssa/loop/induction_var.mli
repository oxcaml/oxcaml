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

  (** Map each [Op]'s id to the block in which it is defined. Computed once per
      graph and memoized. *)
  val op_def : unit -> S.Block.t S.Instruction.Id.Tbl.t

  (** Whether [v] is a compile-time constant operation. *)
  val is_const : S.Instruction.t -> bool

  (** The value of a [Const_int] that fits in an OCaml [int]. *)
  val const_int : S.Instruction.t -> int option

  (** [available_at preheader v] is [true] iff [v] can be referenced by a value
      built in [preheader]: it is a constant (rematerialisable anywhere), or its
      defining block dominates [preheader] (which also makes it invariant in any
      loop [preheader] is the preheader of). *)
  val available_at : S.Block.t -> S.Instruction.t -> bool

  (** The signed per-iteration step of a constant-step BIV ([+c] steps give [c],
      [-c] steps give [-c]); [None] for variable steps. *)
  val signed_step : biv -> int option

  (** [loop.back_edges] as a set. *)
  val back_edge_set : loop -> S.Block.Set.t

  (** The predecessors of [loop]'s header that are not back edges, i.e. the
      edges by which the loop is entered. *)
  val entry_predecessors : loop -> S.Block.t list

  (** The single entry predecessor of [loop]'s header, when there is exactly one
      (the loop's preheader). *)
  val preheader_of : loop -> S.Block.t option

  (** See {!Natural_loop.Make.edge_dominates}: a sufficient condition for the
      CFG edge [src -> succ] to dominate [target]. *)
  val edge_dominates :
    src:S.Block.t -> succ:S.Block.t -> target:S.Block.t -> bool

  (** [is_loop_invariant op_def loop_body v] is [true] iff [v] cannot change
      across iterations of a loop whose body is [loop_body]: it is a
      compile-time constant, or its defining block (for [op_def]) / owning block
      (for a [Block_param]) lies outside [loop_body]. [op_def] should come from
      {!op_def}. *)
  val is_loop_invariant :
    S.Block.t S.Instruction.Id.Tbl.t -> S.Block.Set.t -> S.Instruction.t -> bool
end
