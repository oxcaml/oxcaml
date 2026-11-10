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

  @[simp]
  def Value.join {Ident} :
      Value (Ident := Expression (Ident := Ident)) →
      Value (Ident := Ident)
    | .immediate i => .immediate i
    | .indirection i => .indirection i.join

  @[simp]
  def Indirection.join {Ident} :
      Indirection (Ident := Expression (Ident := Ident)) →
      Indirection (Ident := Ident)
    | .immutable b => .immutable b.join
    | .mutable loc => .mutable loc

  @[simp]
  def Block.join {Ident} :
      Block (Ident := Expression (Ident := Ident)) →
      Block (Ident := Ident)
    | { tag, contents } => { tag, contents := contents.join }

  @[simp]
  def BlockContents.join {Ident kind} :
      BlockContents (Ident := Expression (Ident := Ident)) kind →
      BlockContents (Ident := Ident) kind
    | .values values => .values values.joinValues
    | .bytes bytes => .bytes bytes
    | .closure .nonrecursive f => .closure .nonrecursive (Function.join f)

  @[simp]
  def Function.join {Ident} :
      Function (Ident := Expression (Ident := Ident)) →
      Function (Ident := Ident)
    | { additionalParams, body } =>
      { additionalParams, body := fun args => (body (args.map .var)).join }

  @[simp]
  def List.joinExprs {Ident} :
      List (Expression (Ident := Expression (Ident := Ident))) →
      List (Expression (Ident := Ident))
    | [] => []
    | e :: es => e.join :: es.joinExprs

  @[simp]
  def List.joinValues {Ident} :
      List (Value (Ident := Expression (Ident := Ident))) →
      List (Value (Ident := Ident))
    | [] => []
    | v :: vs => v.join :: vs.joinValues
end

@[simp]
def Hole.substitute (value : Val) (hole : Hole) : Expr := fun {Ident} =>
  (hole (Ident := Expression (Ident := Ident)) (.value value)).join

attribute [simp] Hole.substitute.eq_unfold

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
  | field
      {index} {fields : List Val} {field block : Val}
      (isABlockOfValues : @block = @Val.values fields)
      (inRange : fields[index]? = .some @field)
      : (Primitive.field index).stepsTo [@block] @field
  | physEqualImmediates
      {lhs rhs : BitVec 63}
      : Primitive.physEqual.stepsTo [Val.immediate lhs, Val.immediate rhs]
          (Val.immediate (if lhs = rhs then 1 else 0))
  | intModNonzero
      {dividend divisor : BitVec 63}
      (nonzero : divisor ≠ 0)
      : Primitive.intMod.stepsTo [Val.immediate dividend, Val.immediate divisor]
          (Val.immediate (dividend.srem divisor))

@[simp]
def Body.substitute {arity} (values : List Val)
    (exactArity : values.length = arity.succ) (body : Body arity) : Expr :=
  fun {Ident} =>
    let args := values.map fun v => Expression.value (v (Ident := Ident))
    (body (Ident := Expression (Ident := Ident))
      ⟨args.toArray, by simp [args, exactArity]⟩).join

attribute [simp] Body.substitute.eq_unfold

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
    | applyArgs {args args' : List Expr}
        (step : args.oneArgumentStepsTo args') {func}
        : (@Expr.apply func args).stepsTo (Expr.apply func args')
    | applyFunc {func func' : Expr}
        (step : func.stepsTo func') {args}
        : (@Expr.apply func args).stepsTo (Expr.apply func' args)
    | applyClosure {args values}
        (argsReady : args.allValues values) {arity body}
        (exactArity : values.length = arity.succ)
        : (@Expr.apply (Expr.value (Val.closure arity body)) args).stepsTo
            (Body.substitute values exactArity body)
    | ifThenElseCondition {condition condition' : Expr}
        (step : condition.stepsTo condition') {bodyIfTrue bodyIfFalse}
        : (@Expr.ifThenElse condition bodyIfTrue bodyIfFalse).stepsTo
            (Expr.ifThenElse condition' bodyIfTrue bodyIfFalse)
    | ifThenElseFalse {bodyIfTrue bodyIfFalse}
        : (@Expr.ifThenElse (Expr.value (Val.immediate 0))
            bodyIfTrue bodyIfFalse).stepsTo bodyIfFalse
    | ifThenElseTrue {bits} (nonzero : bits ≠ 0) {bodyIfTrue bodyIfFalse}
        : (@Expr.ifThenElse (Expr.value (Val.immediate bits))
            bodyIfTrue bodyIfFalse).stepsTo bodyIfTrue

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
