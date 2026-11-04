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

mutual
  structure Expr where
    desc : ExprDesc
    type : TypeExpr
    deriving Lean.FromJson, Lean.ToExpr, Repr

  inductive ExprDesc where
    | Constant (const : Constant)
    | Record (labels : List RecordLabel)
    deriving Lean.FromJson, Lean.ToExpr, Repr

  structure RecordLabel where
    name : String
    value : Expr
    deriving Lean.FromJson, Lean.ToExpr, Repr

  inductive TypeExpr where
    | Constr (path : Path)
    | String
    deriving Lean.FromJson, Lean.ToExpr, Repr
end

structure ValueBinding where
  id : Ident -- TODO: pattern, but then handle `let () = go_modify_some_mutable_state ()`
  value : Expr
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure RecordLabelDecl where
  name : String
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
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive StructureItem where
  | TypeDeclarations (type_declarations : List TypeDecl)
  | ValueBindings (value_bindings : List ValueBinding)
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure Signature where
  items : List SignatureItem
  deriving Lean.FromJson, Lean.ToExpr, Repr

structure Structure where
  items : List StructureItem
  deriving Lean.FromJson, Lean.ToExpr, Repr

inductive JsonImport : Type where
  | Signature (sig : Signature)
  | Structure (str : Structure)
  deriving Lean.FromJson, Lean.ToExpr, Repr

open Lean Elab Command

elab "howdah% " path:str : term => do
  let filename : String ← getFileName
  let dir : System.FilePath := System.FilePath.mk filename |>.parent.getD "."
  let fileContents : String ← dir / path.getString |> IO.FS.readFile
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  let imported : JsonImport ← fromJson? json |> IO.ofExcept
  return toExpr imported
