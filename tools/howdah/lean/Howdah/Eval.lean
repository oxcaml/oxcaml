import Howdah.Syntax

inductive PureEvalError : Type where
  | blockTagOutOfRange (tag : Nat)
  | primFieldArguments (args : List Value)
  | primFieldOutOfRange (index : Nat) (values : List Value)
  | unboundVariable (id : Ident)

instance : ToString PureEvalError where
  toString
    | .blockTagOutOfRange tag =>
      s!"block tag `{tag}` out of range"
    | .primFieldArguments args =>
      s!"field primitive requires a single block but received `{repr args}`"
    | .primFieldOutOfRange index values =>
      s!"field primitive index `{index}` out of range of `{repr values}`"
    | .unboundVariable id =>
      s!"unbound variable `{id}`"

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
  | .function f =>
    .function { f with body := substitute idToReplace value f.body }
  | .apply func args =>
    .apply (substitute idToReplace value func) <|
      args.attach.map (fun ⟨arg, _⟩ ↦ arg.substitute idToReplace value)
  | .bind id bound body =>
    let substSubstituted := substitute idToReplace value bound
    let bodySubstituted :=
      if id = idToReplace then
        dbgTrace s!"Unique identifiers like `{id}` should not be shadowed!" fun () ↦
        body
      else
        substitute idToReplace value body
    .bind id substSubstituted bodySubstituted
  | .prim p args =>
    .prim p <| args.attach.map (fun ⟨arg, _⟩ ↦ arg.substitute idToReplace value)
  | .value v => .value v
decreasing_by
  all_goals first
    | decreasing_tactic
    | decreasing_with have := Lambda.size_lt_of_mem ‹_›; omega

@[simp]
theorem Lambda.size_substitute {V} (lambda : Lambda V) idToReplace value :
    (lambda.substitute idToReplace value).size = lambda.size := by
  induction lambda with simp
  | var v => split <;> simp
  | function f ih => exact ih
  | apply func args ihFunc ihArgs =>
    simp [ihFunc]
    (induction ihArgs with simp_all)
  | bind id bound body ihBound ihBody => split <;> simp_all
  | prim _ args ih => induction ih with simp_all

@[simp]
def Constant.toValue : Constant → Value
  | .int bits =>
    .immediate { bitsAboveLsb := bits }
  | .string bytes =>
    .indirection <| .immutable { tag := 252, contents := .bytes bytes }

@[simp]
def BlockKind.bindValues (natTag : Nat) (contents : List Value) :
    PureEvalResult Value := do
  let tag ←
    if tagIsNotTooBig : _
    then .halted <| UInt8.ofNatLT natTag tagIsNotTooBig
    else .error <| .blockTagOutOfRange natTag
  match kind : BlockKind.ofTag tag with
  | .values =>
    .halted <| .indirection <|
    .immutable { tag, contents := kind ▸ .values contents }
  | _ => .error <| .blockTagOutOfRange natTag

@[simp]
def StructuredConstant.pureEval : StructuredConstant → PureEvalResult Value
  | .base c => .halted c.toValue
  | .block tag fields => do
    let values ← fields.attach.mapM fun ⟨field, _⟩ ↦ field.pureEval
    BlockKind.bindValues tag values

@[simp]
def Primitive.pureEval (args : List Value) : Primitive → PureEvalResult Value
  | .field index =>
    match args with
    | [.indirection <| .immutable { tag := 0, contents := .values values }] =>
      match values[index]? with
      | .some value => .halted value
      | .none => .error <| .primFieldOutOfRange index values
    | _ => .error <| .primFieldArguments args
  | .makeBlock tag => BlockKind.bindValues tag args

@[simp]
def Lambda.pureEval : Expr → PureEvalResult Value
  | .var id => .error <| .unboundVariable id
  | .const c => c.pureEval
  | .function f =>
    .halted <| .indirection <|
    .immutable { tag := 247, contents := .closure .nonrecursive f }
  | .apply _ _ => .maybeNonterminating
  | .bind id bound body => do
    let value : Value ← bound.pureEval
    body.substitute id value |>.pureEval
  | .prim p args => do
    let args ← args.attach.mapM fun ⟨arg, _⟩ => arg.pureEval
    p.pureEval args
  | .value v => .halted v
decreasing_by
  all_goals first
    | decreasing_tactic
    | decreasing_with have := Lambda.size_lt_of_mem ‹_›; omega

@[simp]
def Lambda.pureEval! (lambda : Expr) : Value :=
  match lambda.pureEval with
  | .halted result => result
  | .maybeNonterminating => panic! "maybe nonterminating"
  | .error e => panic! toString e

@[simp]
def PureEval (lambda : Expr) (value : Value) : Prop :=
  lambda.pureEval = .halted value

inductive List.allValues : List Expr → List Value → Prop where
  | nil
      : [].allValues []
  | cons
      {tailExprs tailValues}
      (tailAllValues : tailExprs.allValues tailValues)
      {headValue}
      : (.value headValue :: tailExprs).allValues (headValue :: tailValues)

inductive Primitive.stepsTo : Primitive → List Value → Value → Prop where
  | makeBlock
      {tag args v}
      (thisIsABlockOfValues : BlockKind.bindValues tag args = .halted v)
      : (Primitive.makeBlock tag).stepsTo args v

mutual
  inductive Lambda.stepsTo : Expr → Expr → Prop where
    | const
        {c : StructuredConstant} {v}
        (step : c.pureEval = .halted v)
        : (Lambda.const c).stepsTo (Lambda.value v)
    | function {headParam tailParams body}
        : Lambda.function { headParam, tailParams, body } |>.stepsTo <|
            Lambda.value <| .indirection <| .immutable {
              tag := 247,
              contents :=
                .closure .nonrecursive { headParam, tailParams, body },
            }
    | bindBody
        {id v} {body : Expr}
        : (Lambda.bind id (.value v) body).stepsTo (body.substitute id v)
    | bindBound
        {bound bound' : Expr}
        (step : bound.stepsTo bound')
        {id body}
        : (Lambda.bind id bound body).stepsTo (Lambda.bind id bound' body)
    | primWithArgumentsReady
        {args : List Expr} {values : List Value}
        (argsReady : args.allValues values)
        {p : Primitive} {v}
        (step : p.stepsTo values v)
        : (Lambda.prim p args).stepsTo (.value v)
    | primArgs
        {args args' : List Expr}
        (step : args.oneArgumentStepsTo args')
        {p : Primitive}
        : (Lambda.prim p args).stepsTo (Lambda.prim p args')

  -- Note that argument evaluation order is unspecified.
  inductive List.oneArgumentStepsTo : List Expr → List Expr → Prop where
    | head
        {head head' : Expr}
        (step : head.stepsTo head')
        {tail}
        : (head :: tail).oneArgumentStepsTo (head' :: tail)
    | tail
        {tail tail' : List Expr}
        (step : tail.oneArgumentStepsTo tail')
        {head} -- no requirement that `head` cannot step
        : (head :: tail).oneArgumentStepsTo (head :: tail')
end

@[simp]
theorem valuesCannotStep {v e} : ¬(Lambda.value v).stepsTo e := nofun

inductive Lambda.purelyEvaluatesTo : Expr → Value → Prop where
  | alreadyValue {v} :
    (Lambda.value v).purelyEvaluatesTo v
  | deterministicStep {e e' : Expr} {v}
    (step : e.stepsTo e')
    (deterministic : forall e'', e.stepsTo e'' → e' = e'')
    (transitive : e'.purelyEvaluatesTo v) :
    e.purelyEvaluatesTo v

notation:50 e:51 " ⇓ " v:51 => Lambda.purelyEvaluatesTo e v
