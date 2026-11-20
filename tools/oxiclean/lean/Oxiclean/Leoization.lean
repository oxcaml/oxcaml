import Oxiclean.Model
open Oxiclean.Model

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

  inductive Immediate : Value → Value → Prop where
    | imm {bitsAboveLsb : BitVec 63}
        : Immediate (.immediate { bitsAboveLsb }) (.immediate { bitsAboveLsb })

  def immediate : PER Value where
    eq := Immediate
    symmetric
      | .imm => .imm
    transitive ab bc := match ab, bc with
      | .imm, .imm => .imm

  inductive RefinedImmediate (condition : BitVec 63 → Prop)
      : Value → Value → Prop where
    | refinedImm
        {lhs rhs : BitVec 63}
        (lhsRefined : condition lhs)
        (rhsRefined : condition rhs)
        : RefinedImmediate condition
            (.immediate { bitsAboveLsb := lhs })
            (.immediate { bitsAboveLsb := rhs })

  def refinedImmediate (condition : BitVec 63 → Prop) : PER Value where
    eq := RefinedImmediate condition
    symmetric
      | .refinedImm lhs rhs => .refinedImm rhs lhs
    transitive ab bc := match ab, bc with
      | .refinedImm a _, .refinedImm _ c => .refinedImm a c

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
    transitive ab bc := match ab, bc with
      | .moduleRuntimeBlock ab, .moduleRuntimeBlock bc =>
        .moduleRuntimeBlock (valuesRelation.transitive ab bc)

  inductive ValueInBlock
      (headRelation : PER Value)
      (tailRelation : PER (List Value))
      : List Value → List Value → Prop where
    | valueInBlock
        {lhsHead rhsHead}
        (headSatisfiesRelation : headRelation.eq lhsHead rhsHead)
        {lhsTail rhsTail}
        (tailSatisfiesRelation : tailRelation.eq lhsTail rhsTail)
        : ValueInBlock
            headRelation
            tailRelation
            (lhsHead :: lhsTail)
            (rhsHead :: rhsTail)

  def valueInBlock (headRelation : PER Value) (tailRelation : PER (List Value))
      : PER (List Value) where
    eq := ValueInBlock headRelation tailRelation
    symmetric
      | .valueInBlock headSatisfiesRelation tailSatisfiesRelation =>
        .valueInBlock
          (headRelation.symmetric headSatisfiesRelation)
          (tailRelation.symmetric tailSatisfiesRelation)
    transitive ab bc := match ab, bc with
      | .valueInBlock abHead abTail, .valueInBlock bcHead bcTail =>
        .valueInBlock
          (headRelation.transitive abHead bcHead)
          (tailRelation.transitive abTail bcTail)
end PER

namespace Statics
  structure AbstractType : Type 0 where
    abstractTypeRelation : PER Value

  structure DefinedElsewhere : Type 0 where
    -- empty

  structure ModuleEnd : Type 0 where
    -- empty

  structure ModuleItem (Item Tail : Type 0) : Type 0 where
    item : Item
    tail : Tail

  structure ModuleType : Type 0 where
    -- empty

  structure TypeDeclaration (Decl : Type 0) : Type 0 where
    typeDeclaration : Decl
end Statics

end Oxiclean.Leoization

open Oxiclean.Leoization

mutual
  @[simp]
  def Oxiclean.Model.ModuleType.statics : ModuleType → Type 0
    | .field _ _ => Statics.DefinedElsewhere
    | .signature absent _present => absent.statics

  @[simp]
  def _root_.List.statics : List ModuleItemAbsentAtRuntime → Type 0 :=
    List.foldr (Statics.ModuleItem ·.statics) Statics.ModuleEnd

  @[simp]
  def Oxiclean.Model.ModuleItemAbsentAtRuntime.statics
      : ModuleItemAbsentAtRuntime → Type 0
    | .moduleType _ => Statics.ModuleType
    | .valType t => Statics.TypeDeclaration t.statics

  @[simp]
  def Oxiclean.Model.TypeDecl.statics (t : TypeDecl) : Type 0 :=
    match t.manifest with
    | .none => Statics.AbstractType
    | .some _ => Statics.DefinedElsewhere
end

/-
namespace Oxiclean.Leoization

mutual
  def Oxiclean.Model.ModuleType.structuralRelation
      : ∀ moduleType : ModuleType, moduleType.statics → PER Value
    | .field within index => fun { } =>
      let within := sorry -- look up `within`
      let indexed : ModuleType := moduleTypeWithinAnotherModule within index
      indexed.knownStructuralRelation
    | .signature absent present =>
      PER.moduleRuntimeBlock ∘ absent.structuralRelation present

  def _root_.List.structuralRelation
      (absent : List ModuleItemAbsentAtRuntime)
      (present : List ModuleItemPresentAtRuntime)
      : absent.statics → PER (List Value) :=
    sorry

  def Oxiclean.Model.ModuleItemPresentAtRuntime.structuralRelation
      : ∀ item : ModuleItemPresentAtRuntime, sorry.statics → PER Value
    | _ => sorry
end

end Oxiclean.Leoization
-/
