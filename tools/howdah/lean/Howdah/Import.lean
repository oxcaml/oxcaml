import Lean
import Howdah.Syntax
import Howdah.Phoas

structure JsonSchema : Type where
  interface : Syntax.Signature
  implementation : Syntax.Lambda

structure InterfaceAndImplementation : Type 1 where
  interface : Syntax.Signature
  implementation : Expr

instance : Lean.FromJson UInt8 where
  fromJson? json := do
    let n : Nat ← Lean.fromJson? json
    if h : n < UInt8.size
    then return ⟨⟨n, h⟩⟩
    else throw s!"{n} does not fit in a byte"

def BitVec.ofInt? {n : Nat} (i : Int) : Option (BitVec n.succ) :=
  if -2^n ≤ i ∧ i < 2^n
  then .some <| BitVec.ofInt _ i
  else .none

instance : Lean.FromJson (BitVec 63) where
  fromJson? json := do
    let i : Int ← Lean.fromJson? json
    match BitVec.ofInt? i with
    | .some bits => return bits
    | .none => throw s!"{i} does not fit in a 63-bit immediate"

deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Constant
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.IdentStamp
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Ident
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Path
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.TypeExpr
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.StructuredConstant
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Primitive
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Function
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Lambda
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.RecordLabelDecl
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.RecordTypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.TypeDesc
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.TypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.SignatureItem
deriving instance Lean.FromJson, Lean.ToExpr for Syntax.Signature
deriving instance Lean.FromJson, Lean.ToExpr for JsonSchema

open Lean Elab Command

elab "howdah% " path:str : term => do
  let filename : String ← getFileName
  let dir : System.FilePath := System.FilePath.mk filename |>.parent.getD "."
  let fileContents : String ← dir / path.getString |> IO.FS.readFile
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  let imported : JsonSchema ← fromJson? json |> IO.ofExcept
  let interface ← Term.exprToSyntax (toExpr imported.interface)
  let phoas ← imported.implementation.toPhoas?
  let implementation ← Term.exprToSyntax phoas
  let r ← `({ interface := $interface, implementation := @$implementation })
  Term.elabTermEnsuringType r
    (some (mkConst ``InterfaceAndImplementation))
