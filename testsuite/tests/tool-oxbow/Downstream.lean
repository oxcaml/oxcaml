import extracted_oxcaml_downstream
import Oxbow.SemanticTypes
open Oxbow.SemanticTypes
open Oxbow.Model
open Oxbow.Reduction

abbrev expectedInterface : ModuleType Parametric :=
  let even : ModuleStatics := .compilationUnit "Even"
  let t : ValType _ := .constr (.field even 0) []
  let t_option : ValType _ := .constr (.predef .option) [t]

  { absent := [], present := [.val t_option] }

abbrev expectedImplementation : Expr :=
  let even : Expr := .compilationUnit "Even"
  let create : Expr := .field even 0
  let three : Expr := .val (.immediate { bitsAboveLsb := 3#63 })

  .letBinding (.apply create three) (.block 0 [.var 0])

abbrev expectedDependencies : List (String × ModuleType Parametric) :=
  [("Even", Oxbow.Extracted.Even.interface)]

example : Oxbow.Extracted.Downstream.dependencies = expectedDependencies := by rfl
example : Oxbow.Extracted.Downstream.interface = expectedInterface := by rfl
example : Oxbow.Extracted.Downstream.implementation = expectedImplementation := by rfl

abbrev refinedInterface : ModuleType SemanticTypeConstructor :=
  { absent := [],
    present := [
      .val
        (.constr
          (.predef .option)
          [.constr (.field (.compilationUnit "Even") 0) []]),
    ],
  }

example
    : refinedInterface.validWrtExtraction
        Oxbow.Extracted.Downstream.interface := by simp

@[simp]
abbrev expectedRelation : PER Value :=
  Oxbow.Extracted.Downstream.dependencies.enforceModularityAndPartialEquivalence
    fun refinedSignatures =>
      let context := refinedSignatures.toContext!
      let even := context.getDependencyCompilationUnit! "Even"
      let ⟨_even_t_arity, even_t⟩ := even.getTypeConstructor! 0
      PER.moduleRuntimeBlock
        (PER.cons
          (PER.option (even_t.instantiatedWithArguments! []))
          PER.emptyList)

abbrev actualRelation {Step} [S : StepRelation Step] : PER Value :=
  Oxbow.Extracted.Downstream.dependencies.enforceModularityAndPartialEquivalence
    (fun refinedSignatures =>
      @refinedInterface.structuralRelation! Step S refinedSignatures.toContext!)

example {Step} [S : StepRelation Step]
    : @actualRelation Step S = @expectedRelation := by simp

example
    : Oxbow.Extracted.Downstream.implementation.wellTyped
        refinedInterface
        Oxbow.Extracted.Downstream.dependencies := by
  intro
    Step S refinedSignatures signaturesRefineDependencies context dependencies₁
    dependencies₂ dependenciesInhabitTheirSignatures implementation₁
    implementation₂ v₁ reduction₁ v₂ reduction₂
  subst context implementation₁ implementation₂
  cases signaturesRefineDependencies with | @cons evenSig _ evenValid tailSig tailParametric signaturesRefineDependencies =>
  obtain ⟨⟩ := signaturesRefineDependencies
  cases dependenciesInhabitTheirSignatures with | @cons evenRelation even₁ even₂ evenWellTyped tailRelations tailValues₁ tailValues₂ dependenciesInhabitTheirSignatures =>
  obtain ⟨⟩ := dependenciesInhabitTheirSignatures
  obtain ⟨evenSigName, ⟨evenAbsent, evenPresent⟩⟩ := evenSig
  obtain ⟨evenName₁, even₁⟩ := even₁; obtain ⟨evenName₂, even₂⟩ := even₂
  simp at evenValid; obtain ⟨⟨⟩, evenValid⟩ := evenValid
  obtain ⟨evenAbsentValid, evenPresentValid⟩ := evenValid
  obtain ⟨evenT, evenTValid⟩ := evenAbsentValid
  obtain ⟨⟨⟩, evenTValid⟩ := evenTValid
  obtain ⟨evenTArity, evenT⟩ := evenT
  cases evenTArity with simp at evenTValid | zero =>
  cases evenT with simp at evenTValid | abstract evenT =>
  obtain ⟨⟩ := evenTValid
  cases evenPresent with simp at evenPresentValid | cons evenCreate evenPresent =>
  cases evenPresent with simp at evenPresentValid | cons evenGet evenPresent =>
  cases evenPresent with simp at evenPresentValid | nil =>
  obtain ⟨evenCreateValid, evenGetValid⟩ := evenPresentValid
  cases evenCreate with simp at evenCreateValid | val evenCreate =>
  cases evenCreate with simp at evenCreateValid | arrow evenCreateDomain evenCreateCodomain =>
  cases evenCreateDomain with simp at evenCreateValid | constr evenCreateDomain evenCreateDomainArgs =>
  cases evenCreateDomain with simp at evenCreateValid | predef evenCreateDomain =>
  cases evenCreateDomain with simp at evenCreateValid | int =>
  cases evenCreateDomainArgs with simp at evenCreateValid | nil =>
  cases evenCreateCodomain with simp at evenCreateValid | constr evenCreateCodomain evenCreateCodomainArgs =>
  cases evenCreateCodomain with simp at evenCreateValid | predef evenCreateCodomain =>
  cases evenCreateCodomain with simp at evenCreateValid | option =>
  cases evenCreateCodomainArgs with simp at evenCreateValid | cons evenCreateCodomainArg evenCreateCodomainArgs =>
  cases evenCreateCodomainArgs with simp at evenCreateValid | nil =>
  cases evenCreateCodomainArg with simp at evenCreateValid | constr evenCreateCodomainArg evenCreateCodomainArgArgs =>
  cases evenCreateCodomainArgArgs with simp at evenCreateValid | nil =>
  cases evenCreateCodomainArg with simp at evenCreateValid | field within index =>
  obtain ⟨⟨⟩, ⟨⟩⟩ := evenCreateValid
  simp at evenWellTyped; obtain ⟨⟨⟩, ⟨⟨⟩, evenWellTyped⟩⟩ := evenWellTyped
  simp
  sorry
