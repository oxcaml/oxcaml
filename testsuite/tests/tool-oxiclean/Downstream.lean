import extracted_oxcaml_downstream
open Oxiclean.Model

abbrev interface : ModuleType :=
  let even : ModuleStatics := .compilationUnit 0
  let t : ValType := .constr (.field even 0) []
  let t_option : ValType := .constr (.predef .option) [t]

  .signature [] [.val t_option]

abbrev implementation : Expr :=
  let even : Expr := .compilationUnit 0
  let create : Expr := .field even 0
  let three : Expr := .val (.immediate { bitsAboveLsb := 3#63 })

  .letBinding (.apply create three) (.block 0 [.var 0])

abbrev dependencies : List ModuleType := [Oxiclean.Extracted.Even.interface]

example : Oxiclean.Extracted.Downstream.dependencies = dependencies := by rfl
example : Oxiclean.Extracted.Downstream.interface = interface := by rfl
example : Oxiclean.Extracted.Downstream.implementation = implementation := by rfl
