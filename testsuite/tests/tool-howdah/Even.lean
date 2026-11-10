import Howdah.Import
import Howdah.Eval

@[simp]
def imported := howdah% "even.json"

#print imported

@[simp]
def expectedReducedEvenModule : Val := fun {Ident} =>
  let block contents : Value (Ident := Ident) :=
    .indirection <| .immutable { tag := 0, contents := .values contents }
  let closure f : Value (Ident := Ident) :=
    .indirection <| .immutable {
      tag := 247,
      contents := .closure .nonrecursive f,
    }
  let int (bits : BitVec 63) : Expression (Ident := Ident) :=
    .value (.immediate { bitsAboveLsb := bits })
  block [
    closure {
      additionalParams := 0,
      body := fun args =>
        .ifThenElse
          (.prim .physEqual [.prim .intMod [.var args[0], int 2], int 0])
          (.prim (.makeBlock 0) [.var args[0]])
          (int 0),
    },
    closure {
      additionalParams := 0,
      body := fun args => .var args[0],
    },
  ]

example : imported.implementation ⇓ expectedReducedEvenModule := by
  simp [imported]
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound .function
  · sorry
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBody
  · sorry
  simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBound .function
  · sorry
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .letBindingBody
  · sorry
  simp
  apply Expr.purelyEvaluatesTo.deterministicStep
  · exact .primWithArgumentsReady (.cons (.cons .nil)) (.makeBlock rfl)
  · sorry
  exact .alreadyValue
