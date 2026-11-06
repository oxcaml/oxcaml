import Howdah.Import
import Howdah.Eval

@[simp]
def imported : JsonImport := howdah% "record.json"

#eval imported

@[simp]
def expectedReducedRecordModule : Value :=
  let string (s : String) : Value := .block 252 <| .bytes s.toUTF8.toList
  let block (contents : List Value) : Value := .block 0 <| .values contents
  block
    [ block [string "hello", string "world"]
    , block [string "formal", string "methods"]
    ]

example : imported.implementation ⇓ expectedReducedRecordModule := by
  simp
  decide +kernel
