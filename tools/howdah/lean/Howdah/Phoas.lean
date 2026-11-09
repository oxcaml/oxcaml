import Howdah.Syntax
import Howdah.Expr

inductive IllFormedConstant : Type where
  | tagDoesNotRepresentABlockOfValues (tag : Nat)

@[simp]
def Syntax.Constant.toValue :
    Syntax.Constant → ∀ {Ident}, Value (Ident := Ident)
  | .int bitsAboveLsb =>
    .immediate { bitsAboveLsb }
  | .string bytes =>
    .indirection <| .immutable { tag := 252, contents := .bytes bytes }

@[simp]
def Syntax.StructuredConstant.toValue? :
    Syntax.StructuredConstant →
    Except IllFormedConstant (∀ {Ident}, Value (Ident := Ident))
  | .base c => return c.toValue
  | .block natTag fields => do
    if tagIsNotTooBig : natTag < UInt8.size then
      let tag := UInt8.ofNatLT natTag tagIsNotTooBig
      match kind : BlockKind.ofTag tag with
      | .values =>
        let polymorphicValues ← fields.mapM (·.toValue?)
        return fun {Ident} =>
          let values := polymorphicValues.map (fun v => v (Ident := Ident))
          let contents := kind ▸ BlockContents.values values
          Value.indirection (.immutable { tag, contents })
      | _ => .error (.tagDoesNotRepresentABlockOfValues natTag)
    else .error (.tagDoesNotRepresentABlockOfValues natTag)

@[simp]
def Syntax.Lambda.toPhoas? :
    Syntax.Lambda →
    Except IllFormedConstant (∀ Ident, Expr (Ident := Ident))
  | .const c => do
    let v ← c.toValue?
    return fun {Ident} => .value (v (Ident := Ident))
  | .bind id bound body => do
    let bound ← bound.toPhoas?
    return fun {Ident} => .bind (bound (Ident := Ident)) sorry
  | _ => sorry

namespace LetIdentityExample
  @[simp] def x : Syntax.Ident := { name := "x", stamp := .stamped 42 }
  @[simp] def f : Syntax.Lambda := .bind x (.const (.base (.int 42))) (.var x)
  @[simp] def fortyTwo {Ident} : Expr (Ident := Ident) :=
    .value (.immediate { bitsAboveLsb := 42 })
  example : f.toPhoas? = .ok fun Ident => .bind fortyTwo (fun x => .var x) := by
    simp
    sorry
end IdentityFunctionExample
