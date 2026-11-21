import Oxiclean.Model
open Oxiclean.Model

namespace Oxiclean.Reduction

inductive Reduce (Step : Expr → Expr → Prop) : Expr → Value → Prop where
  | alreadyReduced {v} : Reduce Step (.val v) v
  | step
    {e e' : Expr}
    (step : Step e e')
    {v : Value}
    (reduce : Reduce Step e' v)
    : Reduce Step e v

class StepRelation (Step : Expr → Expr → Prop) where
  /-
  We can't require that steps are deterministic, because they really aren't:
  for example, function arguments can be evaluated in any order, and the
  function itself may be evaluated before, after, or during its arguments'
  evaluation. However, we can say that the overall result is deterministic, so
  we can rule out e.g. "`42` steps to `false`" in proofs without requiring a
  full elimination rule.
  -/
  deterministic : ∀ e v, Reduce Step e v → ∀ v', Reduce Step e v' → v = v'

inductive referenceStepRelation : Expr → Expr → Prop where
  -- empty

instance referenceStepRelationIsValid : StepRelation referenceStepRelation where
  deterministic e v r v' r' := by
    induction r generalizing v' with
    | alreadyReduced =>
      cases r' with
      | alreadyReduced => rfl
      | step step reduce => contradiction
    | step step reduce => contradiction

/-
-- This is what we'd like to say, but this requires an elimination rule:
theorem valuesCan'tStep : ∀ Step, StepRelation Step → ∀ v e, ¬ Step (.val v) e
-/
theorem valuesCan'tStep
    : ¬ ∀ Step, StepRelation Step → ∃ v e, Step (.val v) e := by
  intro impossible
  specialize impossible referenceStepRelation referenceStepRelationIsValid
  obtain ⟨v, e, impossible⟩ := impossible
  cases impossible

end Oxiclean.Reduction
