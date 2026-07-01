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
      Steps that are only soundly bounded (e.g. right shifts) intern an atom and
      push the bounding inequalities onto [side], which the caller must include
      among its facts. Anything not decomposed becomes an atom. *)
  val linearize : ctx -> Affine.t list ref -> S.Instruction.t -> Affine.t

  (** [guards_at ctx side target] collects the affine facts that hold on entry
      to [target], from the signed comparisons on its immediate-dominator chain
      whose taken edge dominates [target]. Side conditions from [linearize] are
      pushed onto [side]. *)
  val guards_at : ctx -> Affine.t list ref -> S.Block.t -> Affine.t list
end
