import Oxiclean.Model
open Oxiclean.Model

namespace Oxiclean.Leoization

structure PER (α : Type u) where
  eq : α → α → Prop
  symmetric : ∀ a b, eq a b → eq b a
  transitive : ∀ a b c, eq a b → eq b c → eq a c

def PER.always {α : Type u} : PER α where
  eq _ _ := True
  symmetric _ _ _ := True.intro
  transitive _ _ _ _ _ := True.intro

def PER.never {α : Type u} : PER α where
  eq _ _ := False
  symmetric _ _ := nofun
  transitive _ _ _ := nofun

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
    | .signature absent _present =>
      absent.foldl
        (fun tail item => Statics.ModuleItem item.statics tail)
        Statics.ModuleEnd

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
