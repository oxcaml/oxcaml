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

abbrev expectedRelation {Step} [StepRelation Step] : PER Value :=
  PER.always

example {Step} [S : StepRelation Step]
    : @refinedInterface.structuralRelation? Step S =
      pure (@expectedRelation Step S) := by
  simp
