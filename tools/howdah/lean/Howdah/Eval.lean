import Howdah.Syntax

inductive PureEvalError : Type where
  | blockTagOutOfRange (tag : Nat)
  | unboundVariable (id : Ident)

instance : ToString PureEvalError where
  toString
    | .blockTagOutOfRange tag => s!"block tag `{tag}` out of range"
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
def Lambda.substitute {V} (idToReplace : Ident) (value : V) :
    Lambda V → Lambda V
  | .var id => if id = idToReplace then .value value else .var id
  | .const c => .const c
  | .prim p args =>
    .prim p <| args.attach.map (fun ⟨arg, _⟩ ↦ arg.substitute idToReplace value)
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
theorem Lambda.size_substitute {V} (lambda : Lambda V) idToReplace value :
    (lambda.substitute idToReplace value).size = lambda.size := by
  induction lambda with simp
  | var v => split <;> simp
  | bind id subst body ihSubst ihBody => split <;> simp_all
  | prim _ args ih => induction ih with simp_all

@[simp]
def Constant.pureEval : Constant → Value
  | .string bytes => .bytes bytes |> .block 252

@[simp]
def BlockKind.bindValues (tag : Nat) (contents : List Value) :
    PureEvalResult Value := do
  let tag8 ←
    if lt : tag < UInt8.size
    then .halted <| UInt8.ofNatLT tag lt
    else .error <| .blockTagOutOfRange tag
  match kind : BlockKind.ofTag tag8 with
  | .values => .halted <| .block tag8 <| kind ▸ .values contents
  | _ => .error <| .blockTagOutOfRange tag

@[simp]
def StructuredConstant.pureEval : StructuredConstant → PureEvalResult Value
  | .base c => c.pureEval |> .halted
  | .block tag fields => do
    let values ← fields.attach.mapM fun ⟨field, _⟩ ↦ field.pureEval
    BlockKind.bindValues tag values

@[simp]
def Primitive.pureEval (args : List Value) : Primitive → PureEvalResult Value
  | .makeBlock tag => BlockKind.bindValues tag args

@[simp]
def Lambda.pureEval : Expr → PureEvalResult Value
  | .var id => .unboundVariable id |> .error
  | .const c => c.pureEval
  | .prim p args => do
    let args ← args.attach.mapM fun ⟨arg, _⟩ => arg.pureEval
    p.pureEval args
  | .bind id subst body => do
    let value : Value ← subst.pureEval
    body.substitute id value |>.pureEval
  | .value v => .halted v
decreasing_by
  · decreasing_with exact Lambda.size_lt_of_mem <| by assumption
  · decreasing_tactic
  · decreasing_tactic

@[simp]
def Lambda.pureEval! (lambda : Expr) : Value :=
  match lambda.pureEval with
  | .halted result => result
  | .maybeNonterminating => panic! "maybe nonterminating"
  | .error e => panic! toString e

@[simp]
def PureEval (lambda : Expr) (value : Value) : Prop :=
  lambda.pureEval = .halted value

notation:50 e " ⇓ " v => PureEval e v
