import Howdah.Syntax

inductive PureEvalError : Type where
  | unboundVariable (id : Ident)

instance : ToString PureEvalError where
  toString
    | .unboundVariable id => s!"unbound variable `{id}`"

inductive PureEvalResult (T : Type) : Type where
  | halted (result : T)
  | maybeNonterminating
  | error (error : PureEvalError)

instance : Monad PureEvalResult where
  pure := .halted
  bind m f :=
    match m with
    | .maybeNonterminating => .maybeNonterminating
    | .error e => .error e
    | .halted result => f result

instance : LawfulMonad PureEvalResult :=
  LawfulMonad.mk'
    (id_map := @fun _ x ↦ by cases x <;> rfl)
    (pure_bind := @fun _ _ _ _ => by rfl)
    (bind_assoc := @fun _ _ _ x _ _ => by cases x <;> rfl)

@[simp]
theorem PureEvalResult.bind_halted {α β} {f : α → PureEvalResult β} {x} :
  (PureEvalResult.halted x >>= f) = f x := rfl

@[simp]
theorem PureEvalResult.map_halted {α β} {f : α → β} {x} :
  (f <$> PureEvalResult.halted x) = .halted (f x) := rfl

attribute [simp] dbgTrace

@[simp]
def Lambda.substitute (idToReplace : Ident) (value : Value) : Lambda → Lambda
  | .var id => if id = idToReplace then .value value else .var id
  | .const c => .const c
  | .makeBlock tag fields =>
    .makeBlock tag <|
      fields.attach.map fun ⟨field, _⟩ ↦ field.substitute idToReplace value
  | .bind id subst body =>
    let substSubstituted := substitute idToReplace value subst
    let bodySubstituted :=
      if id = idToReplace then
        dbgTrace s!"Unique identifiers like `{id}` should not be shadowed!" fun () ↦
        body
      else
        substitute idToReplace value body
    .bind id substSubstituted bodySubstituted
  | .value v => .value v
decreasing_by
  · decreasing_with exact Lambda.size_lt_of_mem <| by assumption
  · decreasing_tactic
  · decreasing_tactic

@[simp]
theorem Lambda.size_substitute (lambda : Lambda) idToReplace value :
    (lambda.substitute idToReplace value).size = lambda.size := by
  induction lambda with simp
  | var v => split <;> simp
  | bind id subst body ihSubst ihBody => split <;> simp_all
  | makeBlock tag fields ih => induction ih with simp_all

@[simp]
def Lambda.pureEval : Lambda → PureEvalResult Value
  | .var id => .unboundVariable id |> .error
  | .const c => .const c |> .halted
  | .makeBlock tag fields =>
    .makeBlock tag <$> fields.attach.mapM fun ⟨field, _⟩ ↦ field.pureEval
  | .bind id subst body => do
    let value : Value ← subst.pureEval
    body.substitute id value |>.pureEval
  | .value v => .halted v
decreasing_by
  · decreasing_with exact Lambda.size_lt_of_mem <| by assumption
  · decreasing_tactic
  · decreasing_tactic

def Lambda.pureEval! (lambda : Lambda) : Value :=
  match lambda.pureEval with
  | .halted result => result
  | .maybeNonterminating => panic! "maybe nonterminating"
  | .error e => panic! toString e

@[simp]
def PureEval (lambda : Lambda) (value : Value) : Prop :=
  lambda.pureEval = .halted value

notation:50 e " ⇓ " v => PureEval e v
