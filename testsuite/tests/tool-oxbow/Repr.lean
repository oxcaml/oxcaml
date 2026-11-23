import extracted_oxcaml_repr
open Oxbow.Model

abbrev interface : ModuleType :=
  let int : ExprType := .constr (.predef .int) []
  let t : ExprType := .constr (.var 0) []
  let t_option : ExprType := .constr (.predef .option) [t]
  let create : ExprType := .arrow int t_option
  let get : ExprType := .arrow t int

  .ty .abstract (.val create (.val get .empty))

abbrev implementation : Expr :=
  let get : Expr := .function (Expr.var 0)
  let intMod : BinaryScalarOperation := .integral .modulo
  let immZero : Immediate := { bitsAboveLsb := 0#63 }
  let immOne : Immediate := { bitsAboveLsb := 1#63 }
  let zero : Expr := .val (.immediate immZero)
  let two : Expr := .val (.immediate { bitsAboveLsb := 2#63 })
  let argModTwo : Expr := .scalarOperation (.binary intMod (.var 0) two)
  let argIsEven : Expr := .physicalEquality argModTwo zero
  let bodyIfFalse : ImmediateSwitchCase :=
    { immediate := immZero, body := zero }
  let bodyIfTrue : ImmediateSwitchCase :=
    { immediate := immOne, body := .block 0 [.var 0] }
  let create : Expr :=
    .function (.switch argIsEven 2 0 [bodyIfFalse, bodyIfTrue] [] none)

  .letBinding get (.letBinding create (.block 0 [.var 0, .var 1]))

example : Oxbow.Extracted.Repr.interface = interface := by rfl
example : Oxbow.Extracted.Repr.implementation = implementation := by rfl
