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
  @[simp]
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
            (fun input output => @Reduce Step S (f.substitute input) output)
            (fun input output => @Reduce Step S (g.substitute input) output))
        : Closure domain codomain
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure f }))
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure g }))

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

  def emptyList : PER (List α) where
    eq := EmptyList
    symmetric
      | .emptyList => .emptyList
    transitive
      | .emptyList, .emptyList => .emptyList

  inductive Immediate : Value → Value → Prop where
    | imm {bitsAboveLsb : BitVec 63}
        : Immediate (.immediate { bitsAboveLsb }) (.immediate { bitsAboveLsb })

  def immediate : PER Value where
    eq := Immediate
    symmetric
      | .imm => .imm
    transitive
      | .imm, .imm => .imm

  inductive RefinedImmediate (condition : BitVec 63 → Prop)
      : Value → Value → Prop where
    | refinedImm {bitsAboveLsb : BitVec 63} (refined : condition lhs)
        : RefinedImmediate condition
            (.immediate { bitsAboveLsb })
            (.immediate { bitsAboveLsb })

  def refinedImmediate (condition : BitVec 63 → Prop) : PER Value where
    eq := RefinedImmediate condition
    symmetric
      | .refinedImm r => .refinedImm r
    transitive
      | .refinedImm r, .refinedImm _ => .refinedImm r

  inductive ModuleRuntimeBlock (valuesRelation : PER (List Value))
      : Value → Value → Prop where
    | moduleRuntimeBlock
        {lhs rhs}
        (valuesSatisfyRelation : valuesRelation.eq lhs rhs)
        : ModuleRuntimeBlock valuesRelation
            (.indirection (.noMutableFields
              { tag := 0, contents := .values lhs }))
            (.indirection (.noMutableFields
              { tag := 0, contents := .values rhs }))

  def moduleRuntimeBlock (valuesRelation : PER (List Value)) : PER Value where
    eq := ModuleRuntimeBlock valuesRelation
    symmetric
      | .moduleRuntimeBlock valuesSatisfyRelation =>
        .moduleRuntimeBlock (valuesRelation.symmetric valuesSatisfyRelation)
    transitive
      | .moduleRuntimeBlock ab, .moduleRuntimeBlock bc =>
        .moduleRuntimeBlock (valuesRelation.transitive ab bc)

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

@[simp]
def Oxiclean.Model.ModuleType.validWrtExtraction
    (userSuppliedRefinedSignature : ModuleType (PER Value))
    (extractedParametricSignature : ModuleType Parametric)
    : Prop :=
  userSuppliedRefinedSignature.parameterize = extractedParametricSignature

mutual
  @[simp]
  def Oxiclean.Model.ModuleType.structuralRelationInScope
      {Step} [S : StepRelation Step]
      (context : List (ModuleType (PER Value)))
      (userSuppliedRefinedSignature : ModuleType (PER Value))
      : PER Value :=
    match userSuppliedRefinedSignature with
    | .signature absent present =>
      present.foldr
        (fun item : ModuleItemPresentAtRuntime (PER Value) =>
          PER.cons
            (@item.structuralRelationInScope Step S
              (userSuppliedRefinedSignature :: context)))
        PER.emptyList
      |> PER.moduleRuntimeBlock
    | _ => sorry

  @[simp]
  def Oxiclean.Model.ModuleItemPresentAtRuntime.structuralRelationInScope
      {Step} [S : StepRelation Step]
      (context : List (ModuleType (PER Value)))
      : ModuleItemPresentAtRuntime (PER Value) → PER Value
    | .mod m => @m.structuralRelationInScope Step S context
    | .val v => @v.structuralRelationInScope Step S context

  @[simp]
  def Oxiclean.Model.ValType.structuralRelationInScope
      {Step} [S : StepRelation Step]
      (context : List (ModuleType (PER Value)))
      : ValType (PER Value) → PER Value
    | .arrow domain codomain =>
      @PER.closure Step S
        (@domain.structuralRelationInScope Step S context)
        (@codomain.structuralRelationInScope Step S context)
    | _ => sorry
end

@[simp]
def Oxiclean.Model.ModuleType.structuralRelation
      {Step} [S : StepRelation Step]
    : ModuleType (PER Value) → PER Value :=
  (@·.structuralRelationInScope Step S [])
