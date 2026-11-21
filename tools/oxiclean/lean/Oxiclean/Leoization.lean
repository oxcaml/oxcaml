import Oxiclean.Reduction
open Oxiclean.Model
open Oxiclean.Reduction

namespace Oxiclean.Leoization

structure PER (α : Type u) where
  eq : α → α → Prop
  symmetric {a b} : eq a b → eq b a
  transitive {a b c} : eq a b → eq b c → eq a c

namespace PER
  def always {α : Type u} : PER α where
    eq _ _ := True
    symmetric _ := True.intro
    transitive _ _ := True.intro

  def never {α : Type u} : PER α where
    eq _ _ := False
    symmetric := nofun
    transitive := nofun

  def diagonal {α : Type u} : PER α where
    eq := Eq
    symmetric := Eq.symm
    transitive := Eq.trans

  def refine {α : Type u} (condition : α → Prop) (per : PER α) : PER α where
    eq lhs rhs := per.eq lhs rhs ∧ condition lhs ∧ condition rhs
    symmetric
      | ⟨e, pl, pr⟩ => ⟨per.symmetric e, pr, pl⟩
    transitive ab bc := match ab, bc with
      | ⟨ab, pa, _⟩, ⟨bc, _, pc⟩ => ⟨per.transitive ab bc, pa, pc⟩

  /--
  Wherever the first function is defined (even if nondeterministic),
  the second function can produce the same result.
  -/
  @[simp, pp_nodot]
  def atLeastAsDefinedAs {α : Type a} {β : Type b}
      (domain : PER α) (codomain : PER β)
      (reference maybeMoreDefined : α → β → Prop)
      : Prop :=
    ∀ lhs rhs : α, domain.eq lhs rhs →
    ∀ lhs', reference lhs lhs' →
    ∃ rhs', maybeMoreDefined rhs rhs' ∧ codomain.eq lhs' rhs'

  inductive Extensionality {α : Type a} {β : Type b}
      (domain : PER α)
      (codomain : PER β)
      : (α → β → Prop) → (α → β → Prop) → Prop where
    | extensionality {f g : α → β → Prop}
        (fAtLeastAsDefinedAsG : atLeastAsDefinedAs domain codomain f g)
        (gAtLeastAsDefinedAsF : atLeastAsDefinedAs domain codomain g f)
        : Extensionality domain codomain f g

  @[pp_nodot]
  def extensionality {α : Type a} {β : Type b}
      (domain : PER α)
      (codomain : PER β)
      : PER (α → β → Prop) where
    eq := Extensionality domain codomain
    symmetric
      | .extensionality hf hg => .extensionality hg hf
    transitive {a b c}
      | .extensionality ab ba, .extensionality bc cb => by
        simp at *; constructor <;> intro lhs rhs e lhs' al
        · obtain ⟨tmp', bTmp, eTmp⟩ := ab lhs rhs e lhs' al
          obtain ⟨rhs', br, e'⟩ :=
            bc rhs rhs (domain.transitive (domain.symmetric e) e) tmp' bTmp
          exact ⟨rhs', ⟨br, codomain.transitive eTmp e'⟩⟩
        · obtain ⟨tmp', bTmp, eTmp⟩ := cb lhs rhs e lhs' al
          obtain ⟨rhs', br, e'⟩ :=
            ba rhs rhs (domain.transitive (domain.symmetric e) e) tmp' bTmp
          exact ⟨rhs', ⟨br, codomain.transitive eTmp e'⟩⟩

  inductive Closure {Step} [S : StepRelation Step]
      (domain : PER Value)
      (codomain : PER Value)
      : Value → Value → Prop where
    | closure {f g : Expr}
        (extensional :
          (extensionality domain codomain).eq
            (fun input output => Reduce Step (f.substitute input) output)
            (fun input output => Reduce Step (g.substitute input) output))
        : Closure domain codomain
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure f }))
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure g }))

  @[pp_nodot]
  def closure {Step} [S : StepRelation Step]
      (domain : PER Value)
      (codomain : PER Value)
      : PER Value where
    eq := @Closure Step S domain codomain
    symmetric
      | .closure extensional =>
        .closure ((extensionality domain codomain).symmetric extensional)
    transitive
      | .closure ab, .closure bc =>
        .closure ((extensionality domain codomain).transitive ab bc)

  inductive EmptyList {α} : List α → List α → Prop where
    | emptyList : EmptyList [] []

  @[pp_nodot]
  def emptyList : PER (List α) where
    eq := EmptyList
    symmetric
      | .emptyList => .emptyList
    transitive
      | .emptyList, .emptyList => .emptyList

  inductive Immediate : Value → Value → Prop where
    | imm {bitsAboveLsb : BitVec 63}
        : Immediate (.immediate { bitsAboveLsb }) (.immediate { bitsAboveLsb })

  @[pp_nodot]
  def immediate : PER Value where
    eq := Immediate
    symmetric
      | .imm => .imm
    transitive
      | .imm, .imm => .imm

  inductive RefinedImmediate (condition : BitVec 63 → Prop)
      : Value → Value → Prop where
    | refinedImm {bitsAboveLsb : BitVec 63} (refined : condition bitsAboveLsb)
        : RefinedImmediate condition
            (.immediate { bitsAboveLsb })
            (.immediate { bitsAboveLsb })

  @[pp_nodot]
  def refinedImmediate (condition : BitVec 63 → Prop) : PER Value where
    eq := RefinedImmediate condition
    symmetric
      | .refinedImm r => .refinedImm r
    transitive
      | .refinedImm r, .refinedImm _ => .refinedImm r

  inductive Boolean : Value → Value → Prop where
    | false
        : Boolean
            (.immediate { bitsAboveLsb := 0 })
            (.immediate { bitsAboveLsb := 0 })
    | true
        : Boolean
            (.immediate { bitsAboveLsb := 1 })
            (.immediate { bitsAboveLsb := 1 })

  @[pp_nodot]
  def boolean : PER Value where
    eq := Boolean
    symmetric
      | .false => .false
      | .true => .true
    transitive
      | .false, .false => .false
      | .true, .true => .true

  inductive OxCamlOption (ifSome : PER Value) : Value → Value → Prop where
    | none
        : OxCamlOption ifSome
            (.immediate { bitsAboveLsb := 0 })
            (.immediate { bitsAboveLsb := 0 })
    | some
        {lhs rhs : Value}
        (relatedValuesInSome : ifSome.eq lhs rhs)
        : OxCamlOption ifSome
            (.indirection
              (.noMutableFields { tag := 0, contents := .values [lhs] }))
            (.indirection
              (.noMutableFields { tag := 0, contents := .values [rhs] }))

  @[pp_nodot]
  def option (ifSome : PER Value) : PER Value where
    eq := OxCamlOption ifSome
    symmetric
      | .none => .none
      | .some s => .some (ifSome.symmetric s)
    transitive
      | .none, .none => .none
      | .some ab, .some bc => .some (ifSome.transitive ab bc)

  inductive ModuleRuntimeBlock (relationOnValuesInTheBlock : PER (List Value))
      : Value → Value → Prop where
    | moduleRuntimeBlock
        {lhs rhs}
        (valuesSatisfyRelation : relationOnValuesInTheBlock.eq lhs rhs)
        : ModuleRuntimeBlock relationOnValuesInTheBlock
            (.indirection (.noMutableFields
              { tag := 0, contents := .values lhs }))
            (.indirection (.noMutableFields
              { tag := 0, contents := .values rhs }))

  @[pp_nodot]
  def moduleRuntimeBlock
      (relationOnValuesInTheBlock : PER (List Value))
      : PER Value where
    eq := ModuleRuntimeBlock relationOnValuesInTheBlock
    symmetric
      | .moduleRuntimeBlock valuesSatisfyRelation =>
        .moduleRuntimeBlock
          (relationOnValuesInTheBlock.symmetric valuesSatisfyRelation)
    transitive
      | .moduleRuntimeBlock ab, .moduleRuntimeBlock bc =>
        .moduleRuntimeBlock (relationOnValuesInTheBlock.transitive ab bc)

  inductive Cons {α} (headRelation : PER α) (tailRelation : PER (List α))
      : List α → List α → Prop where
    | cons
        {lhsHead rhsHead}
        (headSatisfiesRelation : headRelation.eq lhsHead rhsHead)
        {lhsTail rhsTail}
        (tailSatisfiesRelation : tailRelation.eq lhsTail rhsTail)
        : Cons
            headRelation
            tailRelation
            (lhsHead :: lhsTail)
            (rhsHead :: rhsTail)

  @[pp_nodot]
  def cons {α} (headRelation : PER α) (tailRelation : PER (List α))
      : PER (List α) where
    eq := Cons headRelation tailRelation
    symmetric
      | .cons headSatisfiesRelation tailSatisfiesRelation =>
        .cons
          (headRelation.symmetric headSatisfiesRelation)
          (tailRelation.symmetric tailSatisfiesRelation)
    transitive
      | .cons abHead abTail, .cons bcHead bcTail =>
        .cons
          (headRelation.transitive abHead bcHead)
          (tailRelation.transitive abTail bcTail)
end PER

namespace Statics
  structure AbstractType : Type 0 where
    abstractTypeRelation : PER Value

  structure DefinedElsewhere : Type 0 where
    -- empty

  structure Nil : Type 0 where
    -- empty

  structure Cons (Head Tail : Type 0) : Type 0 where
    head : Head
    tail : Tail

  structure AbstractModuleType : Type 0 where
    -- empty

  structure ModuleType (Absent Present : Type 0) : Type 0 where
    absent : Absent
    present : Present

  structure ModuleTypeDecl (Decl : Type 0) : Type 0 where
    moduleTypeDecl : Decl

  structure ModuleDecl (Decl : Type 0) : Type 0 where
    moduleDecl : Decl

  structure ValDecl (Decl : Type 0) : Type 0 where
    valDecl : Decl

  structure TypeDeclaration (Decl : Type 0) : Type 0 where
    typeDeclaration : Decl

  structure Arrow (Domain Codomain : Type 0) : Type 0 where
    domain : Domain
    codomain : Codomain

  structure TypeConstr (Constr Args : Type 0) : Type 0 where
    constr : Constr
    args : Args

  structure FirstClassModule (Signature : Type 0) : Type 0 where
    signature : Signature

  structure Poly (Body : Type 0) : Type 0 where
    body : Body
end Statics

end Oxiclean.Leoization

open Oxiclean.Leoization

def SemanticTypeConstructor : (arity : Nat) → Type 0
  | .zero => PER Value
  | .succ pred => PER Value → SemanticTypeConstructor pred

@[simp]
def Oxiclean.Model.ModuleType.validWrtExtraction
    (userSuppliedRefinedSignature : ModuleType SemanticTypeConstructor)
    (extractedParametricSignature : ModuleType Parametric)
    : Prop :=
  userSuppliedRefinedSignature.parameterize = extractedParametricSignature

@[simp]
def Oxiclean.Model.PredefinedType.structuralRelation
    : PredefinedType → (arity : Nat) × SemanticTypeConstructor arity
  | .int => ⟨0, PER.immediate⟩
  | .bool => ⟨0, PER.boolean⟩
  /- CR-soon wsturgeon: `PER.option` needs a layout as well. -/
  | .option => ⟨1, PER.option⟩

structure SemanticModuleInContext : Type 0 where
  absent : List ((arity : Nat) × SemanticTypeConstructor arity)
  present : List (Option SemanticModuleInContext)

inductive StructuralRelationError : Type 0 where
  | arityMismatch
  | moduleIndexOutOfBounds
      (context : List SemanticModuleInContext)
      (deBruijn : Nat)
  | typeIndexOutOfBounds (within : ModuleStatics) (index : Nat)

@[simp]
def SemanticTypeConstructor.instantiatedWithArguments?
    : {arity : Nat} →
      SemanticTypeConstructor arity →
      List (PER Value) →
      Except StructuralRelationError (PER Value)
  | .zero, (relation : PER Value), args =>
    match args with
    | [] => pure relation
    | _ :: _ => throw .arityMismatch
  | .succ _, (relation : PER Value → SemanticTypeConstructor _), args =>
    match args with
    | [] => throw .arityMismatch
    | head :: tail => relation head |>.instantiatedWithArguments? tail

mutual
  @[simp]
  def Oxiclean.Model.ModuleType.semanticModuleInContext?
      (moduleType : ModuleType SemanticTypeConstructor)
      : Except StructuralRelationError SemanticModuleInContext := do
    let { absent, present } := moduleType
    let absent ← absent.mapM (·.semanticModuleInContext?)
    let present ← present.mapM (·.semanticModuleInContext?)
    pure { absent, present }

  @[simp]
  def Oxiclean.Model.TypeDecl.semanticModuleInContext?
      (typeDecl : TypeDecl SemanticTypeConstructor)
      : Except
          StructuralRelationError
          ((arity : Nat) × SemanticTypeConstructor arity) :=
    match typeDecl.manifest with
    | .abstract constructor => pure ⟨typeDecl.arity, constructor⟩
    | .manifest _ => sorry

  @[simp]
  def Oxiclean.Model.ModuleItemPresentAtRuntime.semanticModuleInContext?
      : ModuleItemPresentAtRuntime SemanticTypeConstructor →
        Except StructuralRelationError (Option SemanticModuleInContext)
    | .mod m => .some <$> m.semanticModuleInContext?
    | .val _ => pure .none
end

@[simp]
def Oxiclean.Model.ModuleStatics.resolve?
    (context : List SemanticModuleInContext)
    (statics : ModuleStatics)
    : Except StructuralRelationError SemanticModuleInContext := do
  match statics with
  | .var deBruijn =>
    let .some semanticModule := context[deBruijn]?
      | throw (.moduleIndexOutOfBounds context deBruijn)
    pure semanticModule
  | .field within index =>
    let semanticModule ← within.resolve? context
    let .some (.some submodule) := semanticModule.present[index]?
      | throw (.typeIndexOutOfBounds statics index)
    pure submodule
  | .compilationUnit _ => sorry

mutual
  @[simp]
  def Oxiclean.Model.ModuleType.structuralRelationInScope?
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      (userSuppliedRefinedSignature : ModuleType SemanticTypeConstructor)
      : Except StructuralRelationError (PER Value) := do
    let { absent, present } := userSuppliedRefinedSignature
    let semanticModuleInContext ←
      userSuppliedRefinedSignature.semanticModuleInContext?
    let relationOnValuesInTheBlock ←
      present.foldrM
        (fun item tail => do
          let head ←
            @item.structuralRelationInScope? Step S
              (semanticModuleInContext :: context)
          pure (PER.cons head tail))
        PER.emptyList
    pure (PER.moduleRuntimeBlock relationOnValuesInTheBlock)

  @[simp]
  def Oxiclean.Model.ModuleItemPresentAtRuntime.structuralRelationInScope?
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      : ModuleItemPresentAtRuntime SemanticTypeConstructor →
        Except StructuralRelationError (PER Value)
    | .mod m => @m.structuralRelationInScope? Step S context
    | .val v => @v.structuralRelationInScope? Step S context

  @[simp]
  def Oxiclean.Model.ValType.structuralRelationInScope?
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      : ValType SemanticTypeConstructor →
        Except StructuralRelationError (PER Value)
    | .arrow domain codomain => do
      let domain ← @domain.structuralRelationInScope? Step S context
      let codomain ← @codomain.structuralRelationInScope? Step S context
      pure (@PER.closure Step S domain codomain)
    | .constr c args => do
      let ⟨arity, ctor⟩ ←
        match c with
        | .field within index =>
          let semanticModule : SemanticModuleInContext ← within.resolve? context
          let .some typeConstructor := semanticModule.absent[index]?
            | throw (.typeIndexOutOfBounds within index)
          pure typeConstructor
        | .predef p => pure p.structuralRelation
      args.mapM (@·.structuralRelationInScope? Step S context)
      >>= ctor.instantiatedWithArguments?
    | .firstClassModule signature => sorry
    | .poly body => sorry
    | .var deBruijn => sorry
end

@[simp]
def Oxiclean.Model.ModuleType.structuralRelation?
      {Step} [S : StepRelation Step]
    : ModuleType SemanticTypeConstructor →
      Except StructuralRelationError (PER Value) :=
  (@·.structuralRelationInScope? Step S [])

@[simp]
def Oxiclean.Model.Value.wellTyped
    (relation : {Step : _} → [StepRelation Step] → PER Value)
    (implementation : Value)
    : Prop :=
  ∀ {Step} [S : StepRelation Step],
  (@relation Step S).eq implementation implementation

@[simp]
def Oxiclean.Model.Expr.wellTyped
    (relation : {Step : _} → [StepRelation Step] → PER Value)
    (implementation : Expr)
    : Prop :=
  ∀ {Step} [S : StepRelation Step],
  ∀ v : Value, (reduction : Reduce Step implementation v) →
  (@relation Step S).eq v v
