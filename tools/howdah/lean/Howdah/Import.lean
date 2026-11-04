import Lean
import Howdah.Syntax

deriving instance Lean.FromJson, Lean.ToExpr for Constant
deriving instance Lean.FromJson, Lean.ToExpr for Ident
deriving instance Lean.FromJson, Lean.ToExpr for Path
deriving instance Lean.FromJson, Lean.ToExpr for TypeExpr
deriving instance Lean.FromJson, Lean.ToExpr for StructuredConstant
deriving instance Lean.FromJson, Lean.ToExpr for Lambda
deriving instance Lean.FromJson, Lean.ToExpr for RecordLabelDecl
deriving instance Lean.FromJson, Lean.ToExpr for RecordTypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for TypeDeclDesc
deriving instance Lean.FromJson, Lean.ToExpr for TypeDecl
deriving instance Lean.FromJson, Lean.ToExpr for SignatureItem
deriving instance Lean.FromJson, Lean.ToExpr for Signature
deriving instance Lean.FromJson, Lean.ToExpr for JsonImport

open Lean Elab Command

elab "howdah% " path:str : term => do
  let filename : String ← getFileName
  let dir : System.FilePath := System.FilePath.mk filename |>.parent.getD "."
  let fileContents : String ← dir / path.getString |> IO.FS.readFile
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  let imported : JsonImport ← fromJson? json |> IO.ofExcept
  return toExpr imported
