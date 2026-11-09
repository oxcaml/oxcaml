import Lean
import Howdah.Syntax

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

deriving instance Lean.FromJson, Lean.ToExpr for Loc
deriving instance Lean.FromJson, Lean.ToExpr for Constant
deriving instance Lean.FromJson, Lean.ToExpr for Ident
deriving instance Lean.FromJson, Lean.ToExpr for Path
deriving instance Lean.FromJson, Lean.ToExpr for TypeExpr
deriving instance Lean.FromJson, Lean.ToExpr for StructuredConstant
deriving instance Lean.FromJson, Lean.ToExpr for BlockKind
deriving instance Lean.FromJson, Lean.ToExpr for Primitive
deriving instance Lean.FromJson, Lean.ToExpr for Function
deriving instance Lean.FromJson, Lean.ToExpr for Lambda
deriving instance Lean.FromJson, Lean.ToExpr for RecordLabelDecl
deriving instance Lean.FromJson, Lean.ToExpr for RecordTypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for TypeDesc
deriving instance Lean.FromJson, Lean.ToExpr for TypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for SignatureItem, ModuleType
deriving instance Lean.FromJson, Lean.ToExpr for Signature

instance : Lean.FromJson Value where
  fromJson? _ := throw s!"don't import `Value`s"
deriving instance Lean.ToExpr for Value

deriving instance Lean.FromJson, Lean.ToExpr for JsonImport

open Lean Elab Command

elab "howdah% " path:str : term => do
  let filename : String ← getFileName
  let dir : System.FilePath := System.FilePath.mk filename |>.parent.getD "."
  let fileContents : String ← dir / path.getString |> IO.FS.readFile
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  let imported : JsonImport ← fromJson? json |> IO.ofExcept
  return toExpr imported
