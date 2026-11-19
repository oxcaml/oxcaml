import extracted_oxcaml_even
open Oxiclean.Model

abbrev interface : ModuleType :=
  let int : ValType := .constr (.predef .int) []
  let t : ValType := .constr (.field (.var 0) 0) []
  let t_option : ValType := .constr (.predef .option) [t]
  let create : ValType := .arrow int t_option
  let get : ValType := .arrow t int

  .signature [.valType { manifest := none, arity := 0 }] [.val create, .val get]

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

example : Oxiclean.Extracted.Even.interface = interface := by rfl
example : Oxiclean.Extracted.Even.implementation = implementation := by rfl
