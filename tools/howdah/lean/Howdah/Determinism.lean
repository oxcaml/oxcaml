import Howdah.Eval

theorem Expr.ext {lhs rhs : Expr}
    (h : ∀ {Ident}, lhs (Ident := Ident) = rhs (Ident := Ident)) :
    @lhs = @rhs :=
  funext fun _ => h

theorem Val.ext {lhs rhs : Val}
    (h : ∀ {Ident}, lhs (Ident := Ident) = rhs (Ident := Ident)) :
    @lhs = @rhs :=
  funext fun _ => h

theorem Expr.list_ext : ∀ {lhs rhs : List Expr},
    (∀ {Ident}, lhs.map (fun e => e (Ident := Ident)) =
      rhs.map (fun e => e (Ident := Ident))) →
    lhs = rhs
  | [], [], _ => rfl
  | [], _ :: _, h => by have := @h Unit; simp at this
  | _ :: _, [], h => by have := @h Unit; simp at this
  | _ :: _, _ :: _, h => by
    have heads := Expr.ext fun {Ident} => (List.cons.inj (@h Ident)).1
    have tails := Expr.list_ext fun {Ident} => (List.cons.inj (@h Ident)).2
    simp only [List.cons.injEq]
    exact ⟨heads, tails⟩

theorem Val.list_ext : ∀ {lhs rhs : List Val},
    (∀ {Ident}, lhs.map (fun v => v (Ident := Ident)) =
      rhs.map (fun v => v (Ident := Ident))) →
    lhs = rhs
  | [], [], _ => rfl
  | [], _ :: _, h => by have := @h Unit; simp at this
  | _ :: _, [], h => by have := @h Unit; simp at this
  | _ :: _, _ :: _, h => by
    have heads := Val.ext fun {Ident} => (List.cons.inj (@h Ident)).1
    have tails := Val.list_ext fun {Ident} => (List.cons.inj (@h Ident)).2
    simp only [List.cons.injEq]
    exact ⟨heads, tails⟩

macro "phoas_disjoint " h:ident : tactic =>
  `(tactic| (have := congrArg (fun e : Expr => e (Ident := Unit)) $h;
             simp at this))

theorem Expr.value_inj {v w : Val} (h : @Expr.value v = @Expr.value w) :
    @v = @w :=
  Val.ext fun {Ident} => by
    have := congrArg (fun e : Expr => e (Ident := Ident)) h
    simpa using this

theorem Val.immediate_inj {a b : BitVec 63}
    (h : @Val.immediate a = @Val.immediate b) : a = b := by
  have := congrArg (fun v : Val => v (Ident := Unit)) h
  simpa using this

theorem Val.values_inj {vs ws : List Val}
    (h : @Val.values vs = @Val.values ws) : vs = ws :=
  Val.list_ext fun {Ident} => by
    have := congrArg (fun v : Val => v (Ident := Ident)) h
    simpa using this

theorem Val.closure_inj {n m : Nat} {f : Body n} {g : Body m}
    (h : @Val.closure n @f = @Val.closure m @g) :
    n = m ∧ HEq (@f : Body n) (@g : Body m) := by
  have contents {Ident} := by
    have := congrArg (fun v : Val => v (Ident := Ident)) h
    simp only [Val.closure, Value.indirection.injEq,
      Indirection.immutable.injEq, Block.mk.injEq, true_and] at this
    exact eq_of_heq this
  have arity : n = m := by
    have := @contents Unit
    injection this with _ same
    injection same
  subst arity
  refine ⟨rfl, heq_of_eq (funext fun Ident => ?_)⟩
  have := @contents Ident
  injection this with _ same
  injection same with _ same

theorem Expr.letBinding_inj {b b' : Expr} {body body' : Hole}
    (h : @Expr.letBinding b body = @Expr.letBinding b' body') :
    @b = @b' ∧ @body = @body' := by
  constructor
  · exact Expr.ext fun {Ident} => by
      have := congrArg (fun e : Expr => e (Ident := Ident)) h
      simp at this
      exact this.1
  · exact funext fun Ident => by
      have := congrArg (fun e : Expr => e (Ident := Ident)) h
      simp at this
      exact this.2

theorem Expr.prim_inj {p p' : Primitive} {args args' : List Expr}
    (h : @Expr.prim p args = @Expr.prim p' args') :
    p = p' ∧ args = args' := by
  constructor
  · have := congrArg (fun e : Expr => e (Ident := Unit)) h
    simp at this
    exact this.1
  · exact Expr.list_ext fun {Ident} => by
      have := congrArg (fun e : Expr => e (Ident := Ident)) h
      simp at this
      exact this.2

theorem Expr.apply_inj {f f' : Expr} {args args' : List Expr}
    (h : @Expr.apply f args = @Expr.apply f' args') :
    @f = @f' ∧ args = args' := by
  constructor
  · exact Expr.ext fun {Ident} => by
      have := congrArg (fun e : Expr => e (Ident := Ident)) h
      simp at this
      exact this.1
  · exact Expr.list_ext fun {Ident} => by
      have := congrArg (fun e : Expr => e (Ident := Ident)) h
      simp at this
      exact this.2

theorem Expr.ifThenElse_inj {c c' t t' e e' : Expr}
    (h : @Expr.ifThenElse c t e = @Expr.ifThenElse c' t' e') :
    @c = @c' ∧ @t = @t' ∧ @e = @e' := by
  refine ⟨?_, ?_, ?_⟩ <;> refine Expr.ext fun {Ident} => ?_ <;>
    have := congrArg (fun e : Expr => e (Ident := Ident)) h <;>
    simp at this
  · exact this.1
  · exact this.2.1
  · exact this.2.2

theorem Expr.function_inj {f f' : Fun}
    (h : @Expr.function f = @Expr.function f') : @f = @f' :=
  funext fun Ident => by
    have := congrArg (fun e : Expr => e (Ident := Ident)) h
    simpa using this

theorem List.allValues.cannotStep :
    ∀ {args values args'}, List.allValues args values →
      ¬ List.oneArgumentStepsTo args args'
  | _, _, _, .nil, step => by cases step
  | _, _, _, .cons tailAllValues, step => by
    cases step with
    | head step => exact valuesCannotStep step
    | tail step => exact tailAllValues.cannotStep step

theorem List.allValues.unique :
    ∀ {args values values'}, List.allValues args values →
      List.allValues args values' → values = values'
  | _, _, _, .nil, other => by cases other; rfl
  | _, _, _, @List.allValues.cons _ _ tailAllValues v, other => by
    generalize sameArgs : (@Expr.value @v :: _) = args at other
    cases other with
    | nil => cases sameArgs
    | @cons _ _ otherTail w =>
      obtain ⟨heads, tails⟩ := List.cons.inj sameArgs
      subst tails
      simp only [List.cons.injEq]
      exact ⟨Expr.value_inj heads, tailAllValues.unique otherTail⟩

theorem Primitive.stepsTo.deterministic {p values} {v w : Val}
    (lhs : Primitive.stepsTo p values @v) (rhs : Primitive.stepsTo p values @w) :
    @v = @w := by
  generalize sameValues : values = values' at rhs
  generalize sameResult : @w = result at rhs
  cases lhs <;> cases rhs <;> subst sameResult
  case makeBlock.makeBlock lhsBlock lhsIsBlock rhsBlock rhsIsBlock =>
    subst sameValues
    rw [lhsIsBlock] at rhsIsBlock
    cases rhsIsBlock
    rfl
  case field.field lhsFields lhsBlock lhsIsBlock lhsInRange
      rhsFields rhsBlock rhsIsBlock rhsInRange =>
    have sameBlock : @lhsBlock = @rhsBlock := (List.cons.inj sameValues).1
    rw [lhsIsBlock, rhsIsBlock] at sameBlock
    cases Val.values_inj sameBlock
    rw [lhsInRange] at rhsInRange
    cases rhsInRange
    rfl
  case physEqualImmediates.physEqualImmediates =>
    simp only [List.cons.injEq, and_true] at sameValues
    obtain ⟨sameLhs, sameRhs⟩ := sameValues
    cases Val.immediate_inj sameLhs
    cases Val.immediate_inj sameRhs
    rfl
  case intModNonzero.intModNonzero =>
    simp only [List.cons.injEq, and_true] at sameValues
    obtain ⟨sameLhs, sameRhs⟩ := sameValues
    cases Val.immediate_inj sameLhs
    cases Val.immediate_inj sameRhs
    rfl

local macro:50 e:term:51 " ⟶! " e':term:51 : term =>
  `(Expr.stepsTo $e $e' ∧ ∀ other : Expr, Expr.stepsTo $e @other → $e' = @other)

local macro:50 args:term:51 " ⟶ₗ! " args':term:51 : term =>
  `(List.oneArgumentStepsTo $args $args' ∧
      ∀ other, List.oneArgumentStepsTo $args other → $args' = other)

namespace Deterministic

theorem function {f : Fun} :
    (@Expr.function f) ⟶! (@Expr.value <| Val.indirection <| Indirect.immutable {
      tag := 247, contents := .closure .nonrecursive f }) := by
  refine ⟨.function, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.function f) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  cases Expr.function_inj sameSource
  rfl

theorem letBindingBody {v : Val} {body : Hole} :
    (@Expr.letBinding (Expr.value @v) body) ⟶! (@Hole.substitute @v body) := by
  refine ⟨.letBindingBody, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.letBinding (Expr.value @v) body) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case letBindingBody =>
    obtain ⟨sameBound, sameBody⟩ := Expr.letBinding_inj sameSource
    cases Expr.value_inj sameBound
    cases sameBody
    rfl
  case letBindingBound step _ =>
    cases (Expr.letBinding_inj sameSource).1
    exact absurd step valuesCannotStep

theorem letBindingBound {bound bound' : Expr} {body : Hole}
    (inner : @bound ⟶! @bound') :
    (@Expr.letBinding bound body) ⟶! (@Expr.letBinding bound' body) := by
  refine ⟨.letBindingBound inner.1, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.letBinding bound body) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case letBindingBody =>
    cases (Expr.letBinding_inj sameSource).1.symm
    exact absurd inner.1 valuesCannotStep
  case letBindingBound step _ =>
    obtain ⟨sameBound, sameBody⟩ := Expr.letBinding_inj sameSource
    cases sameBound
    cases sameBody
    rw [inner.2 _ step]

theorem primWithArgumentsReady {args : List Expr} {values : List Val}
    (argsReady : args.allValues values) {p : Primitive} {v : Val}
    (step : p.stepsTo values @v) :
    (@Expr.prim p args) ⟶! (@Expr.value @v) := by
  refine ⟨.primWithArgumentsReady argsReady step, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.prim p args) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case primWithArgumentsReady argsReady' _ _ step' =>
    obtain ⟨samePrim, sameArgs⟩ := Expr.prim_inj sameSource
    cases samePrim
    cases sameArgs
    cases argsReady.unique argsReady'
    exact congrArg (fun v : Val => (Expr.value v : Expr))
      (Primitive.stepsTo.deterministic step step')
  case primArgs step' _ =>
    cases (Expr.prim_inj sameSource).2
    exact absurd step' argsReady.cannotStep

theorem primArgs {args args' : List Expr} {p : Primitive}
    (inner : args ⟶ₗ! args') :
    (@Expr.prim p args) ⟶! (@Expr.prim p args') := by
  refine ⟨.primArgs inner.1, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.prim p args) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case primWithArgumentsReady argsReady _ _ _ =>
    cases (Expr.prim_inj sameSource).2.symm
    exact absurd inner.1 argsReady.cannotStep
  case primArgs step _ =>
    obtain ⟨samePrim, sameArgs⟩ := Expr.prim_inj sameSource
    cases samePrim
    cases sameArgs
    rw [inner.2 _ step]

theorem applyArgs {args args' : List Expr} {func : Val}
    (inner : args ⟶ₗ! args') :
    (@Expr.apply (Expr.value @func) args) ⟶!
      (@Expr.apply (Expr.value @func) args') := by
  refine ⟨.applyArgs inner.1, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.apply (Expr.value @func) args) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case applyArgs step _ =>
    obtain ⟨sameFunc, sameArgs⟩ := Expr.apply_inj sameSource
    cases sameFunc
    cases sameArgs
    rw [inner.2 _ step]
  case applyFunc step _ =>
    cases (Expr.apply_inj sameSource).1
    exact absurd step valuesCannotStep
  case applyClosure argsReady _ _ _ =>
    cases (Expr.apply_inj sameSource).2.symm
    exact absurd inner.1 argsReady.cannotStep

theorem applyFunc {args : List Expr} {values : List Val}
    (argsReady : args.allValues values) {func func' : Expr}
    (inner : @func ⟶! @func') :
    (@Expr.apply func args) ⟶! (@Expr.apply func' args) := by
  refine ⟨.applyFunc inner.1, fun other rhs => ?_⟩
  generalize sameSource : (@Expr.apply func args) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case applyArgs step _ =>
    cases (Expr.apply_inj sameSource).2
    exact absurd step argsReady.cannotStep
  case applyFunc step _ =>
    obtain ⟨sameFunc, sameArgs⟩ := Expr.apply_inj sameSource
    cases sameFunc
    cases sameArgs
    rw [inner.2 _ step]
  case applyClosure =>
    cases (Expr.apply_inj sameSource).1.symm
    exact absurd inner.1 valuesCannotStep

theorem applyClosure {args : List Expr} {values : List Val}
    (argsReady : args.allValues values) {arity : Nat} {body : Body arity}
    (exactArity : values.length = arity.succ) :
    (@Expr.apply (Expr.value (Val.closure arity @body)) args) ⟶!
      (@Body.substitute arity values exactArity @body) := by
  refine ⟨.applyClosure argsReady exactArity, fun other rhs => ?_⟩
  generalize sameSource :
    (@Expr.apply (Expr.value (Val.closure arity @body)) args) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case applyArgs step _ =>
    cases (Expr.apply_inj sameSource).2
    exact absurd step argsReady.cannotStep
  case applyFunc step _ =>
    cases (Expr.apply_inj sameSource).1
    exact absurd step valuesCannotStep
  case applyClosure argsReady' _ _ _ =>
    obtain ⟨sameFunc, sameArgs⟩ := Expr.apply_inj sameSource
    obtain ⟨sameArity, sameBody⟩ := Val.closure_inj (Expr.value_inj sameFunc)
    cases sameArity
    cases sameBody
    cases sameArgs
    cases argsReady.unique argsReady'
    rfl

theorem ifThenElseCondition {condition condition' bodyIfTrue bodyIfFalse : Expr}
    (inner : @condition ⟶! @condition') :
    (@Expr.ifThenElse condition bodyIfTrue bodyIfFalse) ⟶!
      (@Expr.ifThenElse condition' bodyIfTrue bodyIfFalse) := by
  refine ⟨.ifThenElseCondition inner.1, fun other rhs => ?_⟩
  generalize sameSource :
    (@Expr.ifThenElse condition bodyIfTrue bodyIfFalse) = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case ifThenElseCondition step _ _ =>
    obtain ⟨sameCondition, sameTrue, sameFalse⟩ := Expr.ifThenElse_inj sameSource
    cases sameCondition
    cases sameTrue
    cases sameFalse
    rw [inner.2 _ step]
  case ifThenElseFalse =>
    cases (Expr.ifThenElse_inj sameSource).1.symm
    exact absurd inner.1 valuesCannotStep
  case ifThenElseTrue =>
    cases (Expr.ifThenElse_inj sameSource).1.symm
    exact absurd inner.1 valuesCannotStep

theorem ifThenElseFalse {bodyIfTrue bodyIfFalse : Expr} :
    (@Expr.ifThenElse (Expr.value (Val.immediate 0)) bodyIfTrue bodyIfFalse) ⟶!
      @bodyIfFalse := by
  refine ⟨.ifThenElseFalse, fun other rhs => ?_⟩
  generalize sameSource :
    (@Expr.ifThenElse (Expr.value (Val.immediate 0)) bodyIfTrue bodyIfFalse)
      = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case ifThenElseCondition step _ _ =>
    cases (Expr.ifThenElse_inj sameSource).1
    exact absurd step valuesCannotStep
  case ifThenElseFalse =>
    exact (Expr.ifThenElse_inj sameSource).2.2
  case ifThenElseTrue _ nonzero _ _ =>
    have := (Expr.ifThenElse_inj sameSource).1
    exact absurd (Val.immediate_inj (Expr.value_inj this)).symm nonzero

theorem ifThenElseTrue {bits : BitVec 63} (nonzero : bits ≠ 0)
    {bodyIfTrue bodyIfFalse : Expr} :
    (@Expr.ifThenElse (Expr.value (Val.immediate bits)) bodyIfTrue bodyIfFalse) ⟶!
      @bodyIfTrue := by
  refine ⟨.ifThenElseTrue nonzero, fun other rhs => ?_⟩
  generalize sameSource :
    (@Expr.ifThenElse (Expr.value (Val.immediate bits)) bodyIfTrue bodyIfFalse)
      = source at rhs
  cases rhs
  all_goals first | (phoas_disjoint sameSource; done) | skip
  case ifThenElseCondition step _ _ =>
    cases (Expr.ifThenElse_inj sameSource).1
    exact absurd step valuesCannotStep
  case ifThenElseFalse =>
    have := (Expr.ifThenElse_inj sameSource).1
    exact absurd (Val.immediate_inj (Expr.value_inj this)) nonzero
  case ifThenElseTrue =>
    exact (Expr.ifThenElse_inj sameSource).2.1

theorem head {head head' : Expr} {tail : List Expr} {values : List Val}
    (inner : @head ⟶! @head') (tailReady : tail.allValues values) :
    (@head :: tail) ⟶ₗ! (@head' :: tail) := by
  refine ⟨.head inner.1, fun other rhs => ?_⟩
  cases rhs
  case head step => rw [inner.2 _ step]
  case tail step => exact absurd step tailReady.cannotStep

theorem tail {v : Val} {tail tail' : List Expr}
    (inner : tail ⟶ₗ! tail') :
    (@Expr.value @v :: tail) ⟶ₗ! (@Expr.value @v :: tail') := by
  refine ⟨.tail inner.1, fun other rhs => ?_⟩
  cases rhs
  case head step => exact absurd step valuesCannotStep
  case tail step => rw [inner.2 _ step]

end Deterministic

theorem Expr.purelyEvaluatesTo.unique {e : Expr} {v w : Val}
    (lhs : Expr.purelyEvaluatesTo @e @v) (rhs : Expr.purelyEvaluatesTo @e @w) :
    @v = @w := by
  generalize sameSource : @e = source at rhs
  induction lhs generalizing source with
  | alreadyValue =>
    cases rhs with
    | alreadyValue => exact Expr.value_inj sameSource
    | deterministicStep step =>
      subst sameSource
      exact absurd step valuesCannotStep
  | deterministicStep step deterministic _ ih =>
    cases rhs with
    | alreadyValue =>
      subst sameSource
      exact absurd step valuesCannotStep
    | deterministicStep step' _ rest' =>
      subst sameSource
      exact ih _ (deterministic _ step') rest'

macro "howdah_step " deterministicStep:term : tactic =>
  `(tactic| (
    apply Expr.purelyEvaluatesTo.deterministicStep
      ($deterministicStep).1 ($deterministicStep).2
    try simp))
