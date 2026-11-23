import extracted_oxcaml_downstream
import Oxbow.SemanticTypes
open Oxbow.SemanticTypes
open Oxbow.Model
open Oxbow.Reduction

abbrev expectedInterface : ModuleType Parametric :=
  let even : ModuleStatics := .compilationUnit 0
  let t : ValType _ := .constr (.field even 0) []
  let t_option : ValType _ := .constr (.predef .option) [t]

  { absent := [], present := [.val t_option] }

abbrev expectedImplementation : Expr :=
  let even : Expr := .compilationUnit 0
  let create : Expr := .field even 0
  let three : Expr := .val (.immediate { bitsAboveLsb := 3#63 })

  .letBinding (.apply create three) (.block 0 [.var 0])

abbrev expectedDependencies : List (ModuleType Parametric) :=
  [Oxbow.Extracted.Even.interface]

example : Oxbow.Extracted.Downstream.dependencies = expectedDependencies := by rfl
example : Oxbow.Extracted.Downstream.interface = expectedInterface := by rfl
example : Oxbow.Extracted.Downstream.implementation = expectedImplementation := by rfl

abbrev refinedInterface : ModuleType SemanticTypeConstructor :=
  { absent := [],
    present := [
      .val
        (.constr
          (.predef .option)
          [.constr (.field (.compilationUnit 0) 0) []]),
    ],
  }

example
    : refinedInterface.validWrtExtraction
        Oxbow.Extracted.Downstream.interface := by simp

@[simp]
abbrev expectedRelation {Step} [S : StepRelation Step] : PER Value :=
  PER.forAllWhere
    (List.forAllPairs
      SemanticModuleInContext.refines
      Oxbow.Extracted.Downstream.dependencies)
    fun dependencies =>
      let even := dependencies.getDependencyCompilationUnit! 0
      let ⟨_even_t_arity, even_t⟩ := even.getTypeConstructor! 0
      PER.moduleRuntimeBlock
        (PER.cons
          (PER.option (even_t.instantiatedWithArguments! []))
          PER.emptyList)

abbrev actualRelation {Step} [S : StepRelation Step] : PER Value :=
  @refinedInterface.structuralRelation! Step S Oxbow.Extracted.Downstream.dependencies

example {Step} [S : StepRelation Step]
    : @actualRelation Step S = @expectedRelation Step S := by simp

@[simp]
abbrev expectedValue : Value :=
  sorry

theorem implementationReducesToValue
    {refinedSignatures : List (ModuleType SemanticTypeConstructor)}
    (signaturesRefineDependencies
      : refinedSignatures.forAllPairs
          ModuleType.validWrtExtraction
          Oxbow.Extracted.Downstream.dependencies)
    {dependencies : List Value}
    (dependenciesInhabitTheirSignatures
      : refinedSignatures.map
          (fun sig Step S => @ModuleType.structuralRelation! Step S sorry sig)
        |>.forAllPairs Value.wellTyped dependencies)
    {Step} [S : StepRelation Step]
    : let implementation :=
        Oxbow.Extracted.Downstream.implementation.substituteCompilationUnits!
          dependencies
      Reduce Step implementation expectedValue := by
  cases signaturesRefineDependencies with | @cons evenSig _ evenValid tailSig tailParametric signaturesRefineDependencies =>
  obtain ⟨⟩ := signaturesRefineDependencies
  cases dependenciesInhabitTheirSignatures with | @cons evenRelation even evenWellTyped tailRelations tailValues dependenciesInhabitTheirSignatures =>
  obtain ⟨⟩ := dependenciesInhabitTheirSignatures
  obtain ⟨evenAbsent, evenPresent⟩ := evenSig; simp at evenValid
  obtain ⟨evenAbsentValid, evenPresentValid⟩ := evenValid
  cases evenAbsent with simp at evenAbsentValid | cons evenT evenAbsent =>
  cases evenAbsent with simp at evenAbsentValid | nil =>
  obtain ⟨evenTArity, evenT⟩ := evenT
  cases evenTArity with simp at evenAbsentValid | zero =>
  cases evenT with simp at evenAbsentValid | abstract evenT =>
  obtain ⟨⟩ := evenAbsentValid
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
  simp
  apply Reduce.step; apply S.letBinding; apply S.applyFunc; fail

example
    : Oxbow.Extracted.Downstream.implementation.wellTyped @expectedRelation := by
  intro Step S v reduction
  obtain ⟨⟩ := @S.deterministic _ _ reduction _ implementationReducesToValue
  sorry
