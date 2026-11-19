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

end Oxiclean.Leoization

mutual
  @[simp]
  def Oxiclean.Model.ModuleType.statics : ModuleType → Type 0
    | .field _ _ => PUnit -- already defined elsewhere
    | .signature absent _present => absent.foldl (· × ·.statics) PUnit

  @[simp]
  def Oxiclean.Model.ModuleItemAbsentAtRuntime.statics
      : ModuleItemAbsentAtRuntime → Type 0
    | .moduleType _ => PUnit
    | .valType t => t.statics

  @[simp]
  def Oxiclean.Model.TypeDecl.statics (t : TypeDecl) : Type 0 :=
    match t.manifest with
    | .none => Oxiclean.Leoization.PER Value
    | .some _ => PUnit
end
