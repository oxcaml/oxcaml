import Lean

set_option autoImplicit false

inductive Constant where
  | String (string : String)
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure Ident where
  unique_toplevel_name : String
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive Path where
  | Ident (id : Ident)
  | Dot (nested : Path) (field : String)
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive TypeExpr where
  | Constr (path : Path)
  | String
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive StructuredConstant where
  | Base (const : Constant)
  | Block (tag : Nat) (fields : List StructuredConstant)
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive Lambda where
  | Var (id : Ident)
  | Const (const : StructuredConstant)
  | Let (id : Ident) (value : Lambda) (body : Lambda)
  | MakeBlock (tag : Nat) (fields : List Lambda)
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure RecordTypeDecl where
  labels : List RecordLabelDecl
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive TypeDeclDesc where
  | Record (record : RecordTypeDecl)
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure TypeDecl where
  id : Ident
  desc : TypeDeclDesc
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive SignatureItem where
  | TypeDecl (decl : TypeDecl)
  | Value (id : Ident) (type : TypeExpr)
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure Signature where
  items : List SignatureItem
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure JsonImport : Type where
  interface : Signature
  implementation : Lambda
  deriving Lean.FromJson, Lean.ToExpr, Repr

open Lean Elab Command

elab "howdah% " path:str : term => do
  let filename : String ← getFileName
  let dir : System.FilePath := System.FilePath.mk filename |>.parent.getD "."
  let fileContents : String ← dir / path.getString |> IO.FS.readFile
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  let imported : JsonImport ← fromJson? json |> IO.ofExcept
  return toExpr imported
