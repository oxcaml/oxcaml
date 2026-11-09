import Howdah.Import
import Howdah.Eval

@[simp]
def imported : JsonImport := howdah% "record.json"

#eval imported

@[simp]
def expectedReducedRecordModule : Value :=
  -- let string (s : String) : Value := .block 252 <| .bytes s.toUTF8.toList
  let block contents : Value :=
    .indirection <| .immutable { tag := 0, contents := .values contents }
  let closure f : Value :=
    let contents := .closure .nonrecursive f
    .indirection <| .immutable { tag := 247, contents }
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
    -- block [.immediate 42#63],
  ]

example : imported.implementation ⇓ expectedReducedRecordModule := by
  simp
  constructor
  · constructor; constructor; simp; rfl
  · sorry
  constructor
  · constructor
  · sorry
  simp
  constructor
  · constructor; constructor; simp; rfl
  · sorry
  constructor
  · constructor
  · sorry
  simp
  constructor
  · constructor; constructor; constructor
  · sorry
  constructor
  · constructor; constructor
  · sorry
  simp
  constructor
  · constructor; constructor; constructor
  · sorry
  constructor
  · constructor; constructor
  · sorry
  simp
  constructor
  · constructor
    · constructor
      · constructor; constructor; constructor
      · constructor; simp; rfl
  · sorry
  constructor
  · constructor
  · sorry
  simp
  constructor
  · constructor
    · constructor; constructor
    · constructor; simp; rfl
  · sorry
  constructor
