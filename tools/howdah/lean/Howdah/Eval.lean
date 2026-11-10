import Howdah.Expr

mutual
  @[simp]
  def Expression.join {Ident} :
      Expression (Ident := Expression (Ident := Ident)) →
      Expression (Ident := Ident)
    | .var e => e
    | .apply f args => .apply f.join args.joinExprs
    | .function f => .function (Function.join f)
    | .letBinding bound body =>
      .letBinding bound.join (fun x => (body (.var x)).join)
    | .prim p args => .prim p args.joinExprs
    | .ifThenElse condition bodyIfTrue bodyIfFalse =>
      .ifThenElse condition.join bodyIfTrue.join bodyIfFalse.join
    | .value v => .value v.join

  def Value.join {Ident} :
      Value (Ident := Expression (Ident := Ident)) →
      Value (Ident := Ident)
    | .immediate i => .immediate i
    | .indirection i => .indirection i.join

  def Indirection.join {Ident} :
      Indirection (Ident := Expression (Ident := Ident)) →
      Indirection (Ident := Ident)
    | .immutable b => .immutable b.join
    | .mutable loc => .mutable loc

  def Block.join {Ident} :
      Block (Ident := Expression (Ident := Ident)) →
      Block (Ident := Ident)
    | { tag, contents } => { tag, contents := contents.join }

  def BlockContents.join {Ident kind} :
      BlockContents (Ident := Expression (Ident := Ident)) kind →
      BlockContents (Ident := Ident) kind
    | .values values => .values values.joinValues
    | .bytes bytes => .bytes bytes
    | .closure .nonrecursive f => .closure .nonrecursive (Function.join f)

  def Function.join {Ident} :
      Function (Ident := Expression (Ident := Ident)) →
      Function (Ident := Ident)
    | { additionalParams, body } =>
      { additionalParams, body := fun args => (body (args.map .var)).join }

  def List.joinExprs {Ident} :
      List (Expression (Ident := Expression (Ident := Ident))) →
      List (Expression (Ident := Ident))
    | [] => []
    | e :: es => e.join :: es.joinExprs

  def List.joinValues {Ident} :
      List (Value (Ident := Expression (Ident := Ident))) →
      List (Value (Ident := Ident))
    | [] => []
    | v :: vs => v.join :: vs.joinValues
end

@[simp]
def Hole.substitute (value : Val) (hole : Hole) : Expr := fun {Ident} =>
  (hole (Ident := Expression (Ident := Ident)) (.value value)).join

inductive List.allValues :
    List Expr →
    List Val →
    Prop where
  | nil
      : [].allValues []
  | cons
      {tailExprs : List Expr} {tailValues : List Val}
      (tailAllValues : tailExprs.allValues tailValues)
      {headValue : Val}
      : (@Expr.value headValue :: tailExprs).allValues (headValue :: tailValues)

inductive Primitive.stepsTo : Primitive → List Val → Val → Prop where
  | makeBlock
      {tag args block}
      (thisIsABlockOfValues : Blk.values? tag args = .some block)
      : (Primitive.makeBlock tag).stepsTo args
          (Val.indirection (Indirect.immutable block))

mutual
  inductive Expr.stepsTo : Expr → Expr → Prop where
    | function {f : Fun}
        : (@Expr.function f).stepsTo <|
            Expr.value <| Val.indirection <| Indirect.immutable {
              tag := 247,
              contents := .closure .nonrecursive f,
            }
    | letBindingBody {v body}
        : (@Expr.letBinding (Expr.value v) body).stepsTo (body.substitute v)
    | letBindingBound {bound bound' : Expr}
        (step : bound.stepsTo bound') {body}
        : (@Expr.letBinding bound body).stepsTo (Expr.letBinding bound' body)
    | primWithArgumentsReady {args values}
        (argsReady : args.allValues values) {p v}
        (step : p.stepsTo values v)
        : (@Expr.prim p args).stepsTo (Expr.value v)
    | primArgs {args args' : List Expr}
        (step : args.oneArgumentStepsTo args') {p}
        : (@Expr.prim p args).stepsTo (Expr.prim p args')

  -- Note that argument evaluation order is unspecified.
  inductive List.oneArgumentStepsTo : List Expr → List Expr → Prop where
    | head {head head' : Expr}
        (step : head.stepsTo head') {tail}
        : (@head :: tail).oneArgumentStepsTo (head' :: tail)
    | tail {tail tail' : List Expr}
        (step : tail.oneArgumentStepsTo tail')
        {head} -- no requirement that `head` cannot step
        : (head :: tail).oneArgumentStepsTo (head :: tail')
end

@[simp]
theorem valuesCannotStep {v e} : ¬(@Expr.value v).stepsTo e := by
  intro impossibleStep
  generalize sourceEq : @Expr.value v = source at impossibleStep
  cases impossibleStep <;>
    cases congrArg (fun (source : Expr) => source (Ident := Unit)) sourceEq

inductive Expr.purelyEvaluatesTo : Expr → Val → Prop where
  | alreadyValue {v} :
    (@Expr.value v).purelyEvaluatesTo v
  | deterministicStep {e e' : Expr}
    (step : e.stepsTo e')
    (deterministic : forall (e'' : Expr), e.stepsTo e'' → @e' = @e'') {v : Val}
    (transitive : e'.purelyEvaluatesTo v) :
    e.purelyEvaluatesTo v

notation:50 e:51 " ⇓ " v:51 => Expr.purelyEvaluatesTo e v
