import Howdah.Import
import Howdah.Eval

@[simp]
def imported : JsonImport := howdah% "record.json"

#eval imported

@[simp]
def expectedReducedRecordModule : Value :=
  -- let string (s : String) : Value := .block 252 <| .bytes s.toUTF8.toList
  let block contents : Value := .block 0 <| .values contents
  let closure f : Value := .block 247 <| .closure .nonrecursive f
  block [
    /- block [
     -   string "hello",
     -   string "world",
     - ],
     - block [
     -   string "formal",
     -   string "methods",
     - ], -/
    block [
      closure {
        headParam := ⟨"i/326"⟩,
        tailParams := [],
        body := .const <| .block 0 [.base <| .int 42#63],
      },
      closure {
        headParam := ⟨"i/329"⟩,
        tailParams := [],
        body := .var ⟨"i/329"⟩,
      },
    ],
  ]

example : imported.implementation ⇓ expectedReducedRecordModule := by simp
