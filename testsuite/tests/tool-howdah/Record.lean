import Howdah.Import
import Howdah.Eval

@[simp]
def imported : JsonImport := howdah% "record.json"

#eval imported |> repr |> Lean.logInfo

#eval imported.implementation.pureEval! |> repr |> Lean.logInfo

@[simp]
def expectedReducedRecordModule : Value :=
  .makeBlock 0
    [ .const (.block 0 [.base (.string "hello"), .base (.string "world")])
    , .const (.block 0 [.base (.string "formal"), .base (.string "methods")])
    ]

example : imported.implementation ⇓ expectedReducedRecordModule := by simp
