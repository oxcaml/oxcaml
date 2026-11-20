import Oxiclean.Model
open Oxiclean.Model

namespace Oxiclean.Reduction

class StepRelation (Step : Expr → Expr → Prop) where
  -- empty

inductive referenceStepRelation : Expr → Expr → Prop where
  -- empty

instance referenceStepRelationIsValid : StepRelation referenceStepRelation where
  -- empty

/-
-- This is what we'd like to say, but this requires an elimination rule:
theorem valuesCan'tStep : ∀ Step, StepRelation Step → ∀ v e, ¬ Step (.val v) e
-/
theorem valuesCan'tStep
    : ¬ ∀ Step, StepRelation Step → ∃ v e, Step (.val v) e := by
  intro contradiction
  specialize contradiction referenceStepRelation referenceStepRelationIsValid
  obtain ⟨v, e, contradiction⟩ := contradiction
  cases contradiction

inductive Reduce {Step} [S : StepRelation Step] : Expr → Value → Prop where
  | alreadyReduced {v} : Reduce (.val v) v
  | step {e e' v}
    (step : Step e e')
    (reduce : @Reduce Step S e' v)
    : Reduce e v

end Oxiclean.Reduction
