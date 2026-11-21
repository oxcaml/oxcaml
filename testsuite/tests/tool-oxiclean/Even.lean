import extracted_oxcaml_even
import Oxiclean.Leoization
open Oxiclean.Leoization
open Oxiclean.Model
open Oxiclean.Reduction

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

abbrev expectedDependencies : List (ModuleType Parametric) := []

example : Oxiclean.Extracted.Even.dependencies = expectedDependencies := by rfl
example : Oxiclean.Extracted.Even.interface = expectedInterface := by rfl
example : Oxiclean.Extracted.Even.implementation = expectedImplementation := by rfl

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
        Oxiclean.Extracted.Even.interface := by simp

@[simp]
abbrev expectedRelation {Step} [S : StepRelation Step] : PER Value :=
  PER.moduleRuntimeBlock
    (PER.cons
      (@PER.closure Step S PER.immediate (PER.option evenImmediates))
      (PER.cons
        (@PER.closure Step S evenImmediates PER.immediate)
        PER.emptyList))

abbrev actualRelation {Step} [S : StepRelation Step] :=
  @refinedInterface.structuralRelation? Step S

example {Step} [S : StepRelation Step]
    : @actualRelation Step S = pure (@expectedRelation Step S) := by
  simp

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

theorem evenModuleReducesToValue {Step} [S : StepRelation Step]
    : Reduce Step Oxiclean.Extracted.Even.implementation expectedValue := by
  simp
  apply Reduce.step; apply S.letBinding; apply S.function
  apply Reduce.step; apply S.letBindingValue; simp
  apply Reduce.step; apply S.letBinding; apply S.function
  apply Reduce.step; apply S.letBindingValue; simp
  apply Reduce.step; apply S.blockValues
  · simp
  · constructor; constructor; constructor
  apply Reduce.alreadyReduced

example
    : Oxiclean.Extracted.Even.implementation.wellTyped @expectedRelation := by
  intro Step S v reduction
  obtain ⟨⟩ := @S.deterministic _ _ reduction _ evenModuleReducesToValue
  constructor
  constructor
  · constructor
    sorry
  · constructor
    · constructor
      sorry
    · constructor
