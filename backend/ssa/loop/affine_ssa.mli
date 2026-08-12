[@@@ocaml.warning "+a-40-41-42"]

(** Linearization of SSA integer values into {!Fourier_motzkin.Affine} forms,
    and collection of the affine facts implied by dominating branch guards.

    This is the SSA-facing layer shared by the range-based loop analyses (bounds
    check elimination, induction-variable overflow reasoning): it turns SSA
    values and control-flow guards into affine inequalities, which
    {!Fourier_motzkin} then reasons about. *)

module Affine = Fourier_motzkin.Affine

module Make (S : Ssa.Finished_graph) : sig
  (** Interns SSA values as affine atoms, so equal values share a coefficient. A
      fresh [ctx] should be used per query. *)
  type ctx

  val new_ctx : unit -> ctx

  (** The atom id assigned to [instr] (allocating one on first sight). *)
  val intern : ctx -> S.Instruction.t -> int

  (** The SSA value an atom id stands for. *)
  val atom_instr : ctx -> int -> S.Instruction.t

  (** The atom, if any, standing for parameter [index] of [block]. *)
  val find_header_param_atom : ctx -> S.Block.t -> int -> int option

  (** [linearize ctx side v] is the affine form of [v]'s machine-integer value.
      Steps that are only soundly bounded (right shifts) intern an atom and push
      the bounding inequalities onto [side], which the caller must include among
      its facts. Anything not decomposed becomes an atom.

      Soundness: the returned form models the machine value {e exactly} (as an
      unbounded-integer expression over the atoms' machine values, together with
      the [side] facts) for {e all} runtime values: decompositions whose integer
      evaluation could escape signed 64-bit range — where machine wrap-around
      would break the model — are certified away via {!Affine_expr.Range} and
      atomized instead. Facts built from these forms (see {!facts} in
      {!Loop_comparisons}, {!guards_at}) are therefore true of the machine
      values whenever the corresponding tests were taken. *)
  val linearize : ctx -> Affine.t list ref -> S.Instruction.t -> Affine.t

  (** As {!linearize}, but for {e proof goals} only: composites are decomposed
      even when their integer evaluation is not statically certified to fit
      signed 64-bit. Sound only when the entailment being attempted itself pins
      the form's value into machine range (as the bounds-check goals do: the
      machine value always equals the form modulo 2^64, so proving
      [0 <= form <= bound] with [bound] within machine range identifies the
      two); never use the result to build facts. *)
  val linearize_goal : ctx -> Affine.t list ref -> S.Instruction.t -> Affine.t

  (** How {!coeff_of_target} should treat an SSA value it encounters, before any
      decomposition through the value's operation is attempted (constant
      integers are always decomposed as constants). *)
  type leaf_class =
    | Target  (** the value whose coefficient is being computed *)
    | Invariant  (** an opaque leaf the caller knows is invariant *)
    | Reject  (** reject the whole expression *)
    | Decompose  (** not a leaf: decompose through the operation *)

  (** [coeff_of_target ~classify v] is the coefficient of the (unique)
      [Target]-classified value in [v], seen as an affine function of it —
      [None] if [v] does not decompose into such a function (an unrecognised
      operation, a [Reject]-classified leaf, or [Target] occurring
      non-affinely). Recognises the same shapes as {!linearize}, plus constant
      multiplies; used by {!Strength_reduction} to compute derived induction
      variables' scale. The coefficient is computed in wrapping (mod 2^64)
      [Int64] arithmetic, matching machine semantics exactly (see
      {!Affine_expr.coeff_of_atom}). *)
  val coeff_of_target :
    classify:(S.Instruction.t -> leaf_class) -> S.Instruction.t -> int64 option

  (** [guards_at ctx side target] collects the affine facts that hold on entry
      to [target], from the signed comparisons on its immediate-dominator chain
      whose taken edge dominates [target]. Side conditions from [linearize] are
      pushed onto [side]. *)
  val guards_at : ctx -> Affine.t list ref -> S.Block.t -> Affine.t list

  (** [bounding_guards_at ~target ~matches] finds, on [target]'s
      immediate-dominator chain, the branch conditions of the form
      [value cmp other] (or [other cmp value]) with [matches value], whose taken
      edge dominates [target]. Each is returned as the comparison oriented with
      the matched value on the left, together with the other operand. Unlike
      {!guards_at}, the comparison is reported against the {e SSA value} rather
      than its affine form, so callers can reason directly about the tested
      machine value (used by the induction-variable range facts and the
      termination analysis's no-overflow obligations). *)
  val bounding_guards_at :
    target:S.Block.t ->
    matches:(S.Instruction.t -> bool) ->
    (Cmm.integer_comparison * [`Value of S.Instruction.t | `Const of int]) list
end
