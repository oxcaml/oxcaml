import Oxbow.Model
open Oxbow.Model

namespace Oxbow.Reduction

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
  /-
  Then, each inference rule, phrased as an axiom about `Step`:
  -/
  applyFunc {func func' : Expr}
    (step : Step func func') {arg}
    : Step (.apply func arg) (.apply func' arg)
  applyArg {arg arg' : Expr}
    (step : Step arg arg') {func}
    : Step (.apply func arg) (.apply func arg')
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
  physicalEqualityLhs {lhs lhs'}
    (step : Step lhs lhs') {rhs}
    : Step (.physicalEquality lhs rhs) (.physicalEquality lhs' rhs)
  physicalEqualityRhs {rhs rhs'}
    (step : Step rhs rhs') {lhs}
    : Step (.physicalEquality lhs rhs) (.physicalEquality lhs rhs')
  physicalEqualityImmediates {lhs rhs : BitVec 63}
    : Step
        (.physicalEquality
          (.val (.immediate { bitsAboveLsb := lhs }))
          (.val (.immediate { bitsAboveLsb := rhs })))
        (.val (.immediate { bitsAboveLsb := if lhs = rhs then 1 else 0 }))
  modulo {lhs rhs : BitVec 63}
    (nonzero : rhs ≠ 0)
    : Step
        (.scalarOperation
          (.binary (.integral .modulo)
            (.val (.immediate { bitsAboveLsb := lhs }))
            (.val (.immediate { bitsAboveLsb := rhs }))))
        (.val (.immediate { bitsAboveLsb := lhs.srem rhs }))
  switchScrutinee {scrutinee scrutinee'}
    (step : Step scrutinee scrutinee')
    {nImmediates nBlocks immediates blocks failAction}
    : Step
        (.switch scrutinee nImmediates nBlocks immediates blocks failAction)
        (.switch scrutinee' nImmediates nBlocks immediates blocks failAction)
  switchImmediate {scrutinee : BitVec 63} {immediates case}
    (found :
      immediates.find? (·.immediate.bitsAboveLsb = scrutinee) = .some case)
    {nImmediates nBlocks blocks failAction}
    : Step
        (.switch
          (.val (.immediate { bitsAboveLsb := scrutinee }))
          nImmediates nBlocks immediates blocks failAction)
        case.body

/--
A copy of `StepRelation` without assuming determinism, with an elimination rule.
-/
inductive Step' : Expr → Expr → Prop where
  | applyFunc {func func' : Expr}
      (step : Step' func func') {arg}
      : Step' (.apply func arg) (.apply func' arg)
  | applyArg {arg arg' : Expr}
      (step : Step' arg arg') {func}
      : Step' (.apply func arg) (.apply func arg')
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
  | physicalEqualityLhs {lhs lhs'}
      (step : Step' lhs lhs') {rhs}
      : Step' (.physicalEquality lhs rhs) (.physicalEquality lhs' rhs)
  | physicalEqualityRhs {rhs rhs'}
      (step : Step' rhs rhs') {lhs}
      : Step' (.physicalEquality lhs rhs) (.physicalEquality lhs rhs')
  | physicalEqualityImmediates {lhs rhs : BitVec 63}
      : Step'
          (.physicalEquality
            (.val (.immediate { bitsAboveLsb := lhs }))
            (.val (.immediate { bitsAboveLsb := rhs })))
          (.val (.immediate { bitsAboveLsb := if lhs = rhs then 1 else 0 }))
  | modulo {lhs rhs : BitVec 63}
      (nonzero : rhs ≠ 0)
      : Step'
          (.scalarOperation
            (.binary (.integral .modulo)
              (.val (.immediate { bitsAboveLsb := lhs }))
              (.val (.immediate { bitsAboveLsb := rhs }))))
          (.val (.immediate { bitsAboveLsb := lhs.srem rhs }))
  | switchScrutinee {scrutinee scrutinee'}
      (step : Step' scrutinee scrutinee')
      {nImmediates nBlocks immediates blocks failAction}
      : Step'
          (.switch scrutinee nImmediates nBlocks immediates blocks failAction)
          (.switch scrutinee' nImmediates nBlocks immediates blocks failAction)
  | switchImmediate {scrutinee : BitVec 63} {immediates case}
      (found :
        immediates.find? (·.immediate.bitsAboveLsb = scrutinee) = .some case)
      {nImmediates nBlocks blocks failAction}
      : Step'
          (.switch
            (.val (.immediate { bitsAboveLsb := scrutinee }))
            nImmediates nBlocks immediates blocks failAction)
          case.body

@[grind ., simp]
theorem Step'.valuesCan'tStep {v e}
    : ¬ Step' (.val v) e :=
  nofun

theorem _root_.List.allValues.functional {args : List Expr} {values}
    (allValues : args.allValues values) {values'}
    (allValues' : args.allValues values')
    : values = values' := by
  induction allValues generalizing values' with
  | nil => cases allValues'; rfl
  | cons _ head ih =>
    cases allValues' with
    | cons tail' _ => rw [ih tail']

theorem Step'.diamond {e e₁ e₂} (step₁ : Step' e e₁) (step₂ : Step' e e₂)
    : e₁ = e₂ ∨ ∃ e₃, Step' e₁ e₃ ∧ Step' e₂ e₃ := by
  induction step₁ generalizing e₂ with
  | applyFunc step₁ ih =>
    cases step₂ with
    | applyFunc step₂ =>
      specialize ih step₂
      match ih with
      | .inl ih => subst ih; left; rfl
      | .inr ⟨joined, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .applyFunc joined₁, .applyFunc joined₂⟩
    | applyArg step₂ =>
      exact .inr ⟨_, .applyArg step₂, .applyFunc step₁⟩
  | applyArg step₁ ih =>
    cases step₂ with
    | applyArg step₂ =>
      specialize ih step₂
      match ih with
      | .inl ih => subst ih; left; rfl
      | .inr ⟨joined, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .applyArg joined₁, .applyArg joined₂⟩
    | applyFunc step₂ =>
      exact .inr ⟨_, .applyFunc step₂, .applyArg step₁⟩
  | blockValues _ allValues =>
    cases step₂ with
    | blockValues _ allValues' =>
      cases allValues.functional allValues'; exact .inl rfl
  | function => cases step₂; exact .inl rfl
  | letBinding step₁ ih =>
    cases step₂ with
    | letBinding step₂ =>
      match ih step₂ with
      | .inl same => exact .inl (by rw [same])
      | .inr ⟨joined, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .letBinding joined₁, .letBinding joined₂⟩
    | letBindingValue => contradiction
  | letBindingValue =>
    cases step₂ with
    | letBinding step₂ => contradiction
    | letBindingValue => exact .inl rfl
  | physicalEqualityLhs step₁ ih =>
    cases step₂ with
    | physicalEqualityLhs step₂ =>
      match ih step₂ with
      | .inl same => exact .inl (by rw [same])
      | .inr ⟨joined, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .physicalEqualityLhs joined₁, .physicalEqualityLhs joined₂⟩
    | physicalEqualityRhs step₂ =>
      exact .inr ⟨_, .physicalEqualityRhs step₂, .physicalEqualityLhs step₁⟩
    | physicalEqualityImmediates => contradiction
  | physicalEqualityRhs step₁ ih =>
    cases step₂ with
    | physicalEqualityLhs step₂ =>
      exact .inr ⟨_, .physicalEqualityLhs step₂, .physicalEqualityRhs step₁⟩
    | physicalEqualityRhs step₂ =>
      match ih step₂ with
      | .inl same => exact .inl (by rw [same])
      | .inr ⟨_, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .physicalEqualityRhs joined₁, .physicalEqualityRhs joined₂⟩
    | physicalEqualityImmediates => contradiction
  | physicalEqualityImmediates =>
    cases step₂ with
    | physicalEqualityLhs step₂ => contradiction
    | physicalEqualityRhs step₂ => contradiction
    | physicalEqualityImmediates => exact .inl rfl
  | modulo => cases step₂; exact .inl rfl
  | switchScrutinee step₁ ih =>
    cases step₂ with
    | switchScrutinee step₂ =>
      match ih step₂ with
      | .inl same => exact .inl (by rw [same])
      | .inr ⟨_, joined₁, joined₂⟩ =>
        exact .inr ⟨_, .switchScrutinee joined₁, .switchScrutinee joined₂⟩
    | switchImmediate => contradiction
  | switchImmediate found =>
    cases step₂ with
    | switchScrutinee step₂ => contradiction
    | switchImmediate found' =>
      rw [found] at found'; cases found'; exact .inl rfl

theorem Reduce.strip {e v} (reduction : Reduce Step' e v) {e'} (step : Step' e e')
    : Reduce Step' e' v := by
  induction reduction generalizing e' with
  | alreadyReduced => contradiction
  | step step₁ reduction ih =>
    match step₁.diamond step with
    | .inl same => exact same ▸ reduction
    | .inr ⟨_, joined₁, joined₂⟩ => exact .step joined₂ (ih joined₁)

instance referenceStepRelationIsValid : StepRelation Step' where
  deterministic {e v} r {v'} r' := by
    induction r generalizing v' with
    | alreadyReduced =>
      cases r' with
      | alreadyReduced => rfl
      | step step reduce => contradiction
    | step step _ ih => exact ih (r'.strip step)
  applyFunc := .applyFunc
  applyArg := .applyArg
  blockValues := .blockValues
  function := .function
  letBinding := .letBinding
  letBindingValue := .letBindingValue
  physicalEqualityLhs := .physicalEqualityLhs
  physicalEqualityRhs := .physicalEqualityRhs
  physicalEqualityImmediates := .physicalEqualityImmediates
  modulo := .modulo
  switchScrutinee := .switchScrutinee
  switchImmediate := .switchImmediate

end Oxbow.Reduction
