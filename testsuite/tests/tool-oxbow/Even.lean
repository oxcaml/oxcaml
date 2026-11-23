import extracted_oxcaml_even
import Oxbow.SemanticTypes
open Oxbow.SemanticTypes
open Oxbow.Model
open Oxbow.Reduction

abbrev expectedInterface : ModuleType Parametric :=
  let int : ValType Parametric := .constr (.predef .int) []
  let t : ValType Parametric := .constr (.field (.var 0) 0) []
  let t_option : ValType Parametric := .constr (.predef .option) [t]
  let create : ValType Parametric := .arrow int t_option
  let get : ValType Parametric := .arrow t int

  { absent := [{ manifest := .abstract .parametric, arity := 0 }]
  , present := [.val create, .val get]
  }

abbrev expectedImplementation : Expr :=
  let get : Expr := .function (Expr.var 0)
  let intMod : BinaryScalarOperation := .integral .modulo
  let immZero : Immediate := { bitsAboveLsb := 0#63 }
  let immOne : Immediate := { bitsAboveLsb := 1#63 }
  let immTwo : Immediate := { bitsAboveLsb := 2#63 }
  let zero : Expr := .val (.immediate immZero)
  let two : Expr := .val (.immediate immTwo)
  let argModTwo : Expr := .scalarOperation (.binary intMod (.var 0) two)
  let argIsEven : Expr := .physicalEquality argModTwo zero
  let bodyIfFalse : ImmediateSwitchCase :=
    { immediate := immZero, body := zero }
  let bodyIfTrue : ImmediateSwitchCase :=
    { immediate := immOne, body := .block 0 [.var 0] }
  let create : Expr :=
    .function (.switch argIsEven 2 0 [bodyIfFalse, bodyIfTrue] [] none)

  .letBinding get (.letBinding create (.block 0 [.var 0, .var 1]))

abbrev expectedDependencies : List (String × ModuleType Parametric) := []

example : Oxbow.Extracted.Even.dependencies = expectedDependencies := by rfl
example : Oxbow.Extracted.Even.interface = expectedInterface := by rfl
example : Oxbow.Extracted.Even.implementation = expectedImplementation := by rfl

def evenImmediates : PER Value :=
  PER.refinedImmediate (fun bits : BitVec 63 => bits[0] = false)

abbrev refinedInterface : ModuleType SemanticTypeConstructor :=
  let int : ValType _ := .constr (.predef .int) []
  let t : ValType _ := .constr (.field (.var 0) 0) []
  let t_option : ValType _ := .constr (.predef .option) [t]
  let create : ValType _ := .arrow int t_option
  let get : ValType _ := .arrow t int

  { absent := [{ manifest := .abstract evenImmediates, arity := 0 }]
  , present := [.val create, .val get]
  }

example
    : refinedInterface.validWrtExtraction
        Oxbow.Extracted.Even.interface := by simp

@[simp]
abbrev expectedRelation {Step} [S : StepRelation Step] : PER Value :=
  PER.moduleRuntimeBlock
    (PER.cons
      (@PER.closure Step S PER.immediate (PER.option evenImmediates))
      (PER.cons
        (@PER.closure Step S evenImmediates PER.immediate)
        PER.emptyList))

abbrev actualRelation {Step} [S : StepRelation Step] : PER Value :=
  @refinedInterface.structuralRelation! Step S []

example {Step} [S : StepRelation Step]
    : @actualRelation Step S = @expectedRelation Step S := by simp; rfl

@[simp]
abbrev expectedValue : Value :=
  Value.indirection
    (Indirection.noMutableFields
      { tag := 0,
        contents :=
          BlockContents.values
            [Value.indirection
                (Indirection.noMutableFields
                  { tag := 247,
                    contents :=
                      BlockContents.closure
                        (Expr.switch
                          (Expr.physicalEquality
                            (Expr.scalarOperation
                              (ScalarOperation.binary
                                (BinaryScalarOperation.integral
                                  IntegralBinaryScalarOperation.modulo)
                                (Expr.var 0)
                                (Expr.val
                                  (Value.immediate { bitsAboveLsb := 2#63 }))))
                            (Expr.val
                              (Value.immediate { bitsAboveLsb := 0#63 })))
                          2 0
                          [{ immediate := { bitsAboveLsb := 0#63 },
                              body :=
                                Expr.val
                                  (Value.immediate { bitsAboveLsb := 0#63 }) },
                            { immediate := { bitsAboveLsb := 1#63 },
                              body := Expr.block 0 [Expr.var 0] }]
                          [] none) }),
              Value.indirection
                (Indirection.noMutableFields
                  { tag := 247,
                    contents := BlockContents.closure (Expr.var 0) })] })

theorem implementationReducesToValue
    : Oxbow.Extracted.Even.implementation.mustReduceTo
        expectedValue
        Oxbow.Extracted.Even.dependencies := by
  intro
    Step S refinedSignatures signaturesRefineDependencies context
    dependencies dependenciesInhabitTheirSignatures implementation
  subst context implementation
  obtain ⟨⟩ := signaturesRefineDependencies
  obtain ⟨⟩ := dependenciesInhabitTheirSignatures
  simp
  apply Reduce.step; apply S.letBinding; apply S.function
  apply Reduce.step; apply S.letBindingValue; simp
  apply Reduce.step; apply S.letBinding; apply S.function
  apply Reduce.step; apply S.letBindingValue; simp
  apply Reduce.step; apply S.blockValues
  · simp
  · constructor; constructor; constructor
  apply Reduce.alreadyReduced

theorem lsbZeroIfModTwoZero {bits : BitVec 63} (remainderZero : bits.srem 2 = 0)
    : bits[0] = false := by
  have remainderZero := congrArg BitVec.toNat remainderZero
  simp [BitVec.srem, BitVec.msb_eq_decide] at remainderZero
  have := bits.isLt
  split at remainderZero <;> simp_all [BitVec.toNat_umod, BitVec.toNat_neg,
    BitVec.getElem_eq_testBit_toNat, Nat.testBit_zero] <;> omega

theorem ifInputRelationIsEqualityThenDiagonalSufficesForExtensionality {Step}
    {domain codomain : PER Value} {body : Expr}
    (domainIsEquality : ∀ {lhs rhs}, domain.eq lhs rhs → lhs = rhs)
    (resultsRelated :
      ∀ {input output}, domain.eq input input →
      Reduce Step (body.substitute input) output →
      codomain.eq output output)
    : (PER.extensionality domain codomain).eq
        (fun input output => Reduce Step (body.substitute input) output)
        (fun input output => Reduce Step (body.substitute input) output) := by
  have atLeastAsDefined
      : PER.atLeastAsDefinedAs domain codomain
          (fun input output => Reduce Step (body.substitute input) output)
          (fun input output => Reduce Step (body.substitute input) output) := by
    intro lhs rhs inputsRelated output reduction
    cases domainIsEquality inputsRelated
    exact ⟨output, reduction, resultsRelated inputsRelated reduction⟩
  exact .extensionality atLeastAsDefined atLeastAsDefined

example
    : Oxbow.Extracted.Even.implementation.wellTyped
        refinedInterface
        Oxbow.Extracted.Even.dependencies := by
  intro
    Step S refinedSignatures signaturesRefineDependencies context dependencies₁
    dependencies₂ dependenciesInhabitTheirSignatures implementation₁
    implementation₂ v₁ reduction₁ v₂ reduction₂
  subst context implementation₁ implementation₂
  obtain ⟨⟩ := signaturesRefineDependencies
  obtain ⟨⟩ := dependenciesInhabitTheirSignatures
  obtain ⟨⟩ :=
    S.deterministic reduction₁ (implementationReducesToValue .nil .nil)
  obtain ⟨⟩ :=
    S.deterministic reduction₂ (implementationReducesToValue .nil .nil)
  simp
  constructor
  constructor
  · constructor
    apply ifInputRelationIsEqualityThenDiagonalSufficesForExtensionality
    · rintro _ _ ⟨⟩; rfl
    · rintro _ output ⟨⟩ reduction
      rename_i bits
      by_cases even : bits.srem 2#63 = 0#63
      · obtain ⟨⟩ := S.deterministic reduction
          (v' :=
            .indirection
              (.noMutableFields
                { tag := 0,
                  contents := .values [.immediate { bitsAboveLsb := bits }] }))
          (by
            simp
            apply Reduce.step; apply S.switchScrutinee
            apply S.physicalEqualityLhs; apply S.modulo; decide
            apply Reduce.step; apply S.switchScrutinee
            apply S.physicalEqualityImmediates
            simp only [even, if_true]
            apply Reduce.step; apply S.switchImmediate; rfl
            apply Reduce.step; apply S.blockValues
            · simp
            · constructor; constructor
            apply Reduce.alreadyReduced)
        exact
          PER.OxCamlOption.some
            (PER.RefinedImmediate.refinedImm (lsbZeroIfModTwoZero even))
      · obtain ⟨⟩ := S.deterministic reduction
          (v' := .immediate { bitsAboveLsb := 0 })
          (by
            simp
            apply Reduce.step; apply S.switchScrutinee
            apply S.physicalEqualityLhs; apply S.modulo; decide
            apply Reduce.step; apply S.switchScrutinee
            apply S.physicalEqualityImmediates
            simp only [even, if_false]
            apply Reduce.step; apply S.switchImmediate; rfl
            apply Reduce.alreadyReduced)
        exact PER.OxCamlOption.none
  · constructor
    · constructor
      apply ifInputRelationIsEqualityThenDiagonalSufficesForExtensionality
      · rintro _ _ ⟨⟩; rfl
      · rintro _ output ⟨⟩ reduction
        simp at reduction
        cases S.deterministic reduction .alreadyReduced
        exact PER.Immediate.imm
    · constructor
