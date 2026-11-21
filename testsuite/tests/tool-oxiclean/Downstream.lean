import extracted_oxcaml_downstream
import Oxiclean.Leoization
open Oxiclean.Leoization
open Oxiclean.Model
open Oxiclean.Reduction

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
  [Oxiclean.Extracted.Even.interface]

example : Oxiclean.Extracted.Downstream.dependencies = expectedDependencies := by rfl
example : Oxiclean.Extracted.Downstream.interface = expectedInterface := by rfl
example : Oxiclean.Extracted.Downstream.implementation = expectedImplementation := by rfl

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
        Oxiclean.Extracted.Downstream.interface := by simp

@[simp]
abbrev expectedRelation {Step} [S : StepRelation Step] : PER Value :=
  PER.forAllWhere
    (List.forAllPairs
      SemanticModuleInContext.refines
      Oxiclean.Extracted.Downstream.dependencies)
    fun dependencies =>
      let even := dependencies.getDependencyCompilationUnit! 0
      let ⟨_even_t_arity, even_t⟩ := even.getTypeConstructor! 0
      PER.moduleRuntimeBlock
        (PER.cons
          (PER.option (even_t.instantiatedWithArguments! []))
          PER.emptyList)

abbrev actualRelation {Step} [S : StepRelation Step] : PER Value :=
  @refinedInterface.structuralRelation! Step S Oxiclean.Extracted.Downstream.dependencies

example {Step} [S : StepRelation Step]
    : @actualRelation Step S = @expectedRelation Step S := by simp

@[simp]
abbrev expectedValue : Value :=
  sorry

theorem implementationReducesToValue {Step} [S : StepRelation Step]
    : Reduce Step Oxiclean.Extracted.Downstream.implementation expectedValue := by
  simp
  sorry

example
    : Oxiclean.Extracted.Downstream.implementation.wellTyped @expectedRelation := by
  intro Step S v reduction
  obtain ⟨⟩ := @S.deterministic _ _ reduction _ implementationReducesToValue
  sorry
