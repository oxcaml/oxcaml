import Howdah.Import
import Howdah.Determinism

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
  simp [imported]
  howdah_step Deterministic.letBindingBody
  howdah_step Deterministic.letBindingBody
  howdah_step Deterministic.letBindingBound (Deterministic.letBindingBound Deterministic.function)
  howdah_step Deterministic.letBindingBound Deterministic.letBindingBody
  howdah_step Deterministic.letBindingBound (Deterministic.letBindingBound Deterministic.function)
  howdah_step Deterministic.letBindingBound Deterministic.letBindingBody
  howdah_step Deterministic.letBindingBound
    (Deterministic.primWithArgumentsReady (.cons (.cons .nil)) (.makeBlock rfl))
  howdah_step Deterministic.letBindingBody
  howdah_step Deterministic.primWithArgumentsReady (.cons .nil) (.makeBlock rfl)
  exact .alreadyValue
