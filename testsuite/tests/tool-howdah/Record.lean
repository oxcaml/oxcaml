import Howdah.Import
import Howdah.Eval

@[simp]
def imported := howdah% "record.json"

#print imported

@[simp]
def expectedReducedRecordModule : Val := fun {Ident} =>
  -- let string (s : String) : Val := .block 252 <| .bytes s.toUTF8.toList
  let block contents : Value (Ident := Ident) :=
    .indirection <| .immutable { tag := 0, contents := .values contents }
  let closure f : Value (Ident := Ident) :=
    .indirection <| .immutable {
      tag := 247,
      contents := .closure .nonrecursive f,
    }
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
        additionalParams := 0,
        body := fun _ => .value (block [.immediate { bitsAboveLsb := 42#63 }]),
      },
      closure {
        additionalParams := 0,
        body := fun args => .var args[0],
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
