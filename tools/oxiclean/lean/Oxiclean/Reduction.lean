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

inductive _root_.List.allValues : List Expr → List Value → Prop where
  | nil : [].allValues []
  | cons {tail : List Expr} {tailValues}
      (tailAllValues : tail.allValues tailValues) head
      : (.val head :: tail).allValues (head :: tailValues)

/-- An incomplete axiomatic semantics without an elimination rule. -/
class StepRelation (Step : Expr → Expr → Prop) where
  /--
  We can't require that steps are deterministic, because they really aren't:
  for example, function arguments can be evaluated in any order, and the
  function itself may be evaluated before, after, or during its arguments'
  evaluation. However, we can say that the overall result is deterministic, so
  we can rule out e.g. "`42` steps to `false`" in proofs without requiring a
  full elimination rule.
  -/
  deterministic {e v}
    (reduction : Reduce Step e v) {v'}
    (reduction' : Reduce Step e v')
    : v = v'
  blockValues {tag}
    (kind : BlockKind.ofTag tag = .values) {args : List Expr} {values}
    (allValues : args.allValues values)
    : Step
        (.block tag args)
        (.val
          (.indirection
            (.noMutableFields ({ tag, contents := kind ▸ .values values }))))
  function {body}
    : Step
        (.function body)
        (.val
          (.indirection
            (.noMutableFields { tag := 247, contents := .closure body })))
  letBinding {bound bound'}
    (step : Step bound bound') {body}
    : Step (.letBinding bound body) (.letBinding bound' body)
  letBindingValue {bound body}
    : Step (.letBinding (.val bound) body) (body.substitute bound)

inductive Step' : Expr → Expr → Prop where
  | blockValues {tag}
      (kind : BlockKind.ofTag tag = .values) {args : List Expr} {values}
      (allValues : args.allValues values)
      : Step'
          (.block tag args)
          (.val
            (.indirection
              (.noMutableFields ({ tag, contents := kind ▸ .values values }))))
  | function {body}
      : Step'
          (.function body)
          (.val
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure body })))
  | letBinding {bound bound'}
      (step : Step' bound bound') {body}
      : Step' (.letBinding bound body) (.letBinding bound' body)
  | letBindingValue {bound body}
      : Step' (.letBinding (.val bound) body) (body.substitute bound)

@[grind ., simp]
theorem Step'.valuesCan'tStep {v e}
    : ¬ Step' (.val v) e :=
  nofun

instance referenceStepRelationIsValid : StepRelation Step' where
  deterministic {e v} r {v'} r' := by
    induction r generalizing v' with
    | alreadyReduced =>
      cases r' with
      | alreadyReduced => rfl
      | step step reduce => contradiction
    | @step e e' step v reduce ih =>
      cases step with
      | @blockValues tag kind args values allValues => sorry
      | @function body => sorry
      | @letBinding bound bound' step body => sorry
      | @letBindingValue bound body => sorry
  blockValues := .blockValues
  function := .function
  letBinding := .letBinding
  letBindingValue := .letBindingValue

end Oxiclean.Reduction
