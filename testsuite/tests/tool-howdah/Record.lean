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

attribute [local simp]
  Expression.join Function.join Value.join Indirection.join Block.join
  BlockContents.join List.joinExprs List.joinValues Vector.get

example : imported.implementation ⇓ expectedReducedRecordModule := by
  simp [imported]
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBody
  · sorry
  unfold Hole.substitute; simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBody
  · sorry
  unfold Hole.substitute; simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound (.letBindingBound .function)
  · sorry
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound .letBindingBody
  · sorry
  unfold Hole.substitute; simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound (.letBindingBound .function)
  · sorry
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound .letBindingBody
  · sorry
  unfold Hole.substitute; simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound
      (.primWithArgumentsReady (.cons (.cons .nil)) (.makeBlock rfl))
  · sorry
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBody
  · sorry
  unfold Hole.substitute; simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .primWithArgumentsReady (.cons .nil) (.makeBlock rfl)
  · sorry
  exact .alreadyValue
