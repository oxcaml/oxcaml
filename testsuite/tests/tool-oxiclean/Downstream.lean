import extracted_oxcaml_downstream
open Oxiclean.Model

abbrev interface : ModuleType Parametric :=
  let even : ModuleStatics := .compilationUnit 0
  let t : ValType _ := .constr (.field even 0) []
  let t_option : ValType _ := .constr (.predef .option) [t]

  { absent := [], present := [.val t_option] }

abbrev implementation : Expr :=
  let even : Expr := .compilationUnit 0
  let create : Expr := .field even 0
  let three : Expr := .val (.immediate { bitsAboveLsb := 3#63 })

  .letBinding (.apply create three) (.block 0 [.var 0])

abbrev dependencies : List (ModuleType Parametric) :=
  [Oxiclean.Extracted.Even.interface]

example : Oxiclean.Extracted.Downstream.dependencies = dependencies := by rfl
example : Oxiclean.Extracted.Downstream.interface = interface := by rfl
example : Oxiclean.Extracted.Downstream.implementation = implementation := by rfl
