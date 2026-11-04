inductive Constant where
  | String (string : String)
  deriving Repr

structure Ident where
  unique_toplevel_name : String
  deriving Repr

inductive Path where
  | Ident (id : Ident)
  | Dot (nested : Path) (field : String)
  deriving Repr

inductive TypeExpr where
  | Constr (path : Path)
  | String
  deriving Repr

inductive StructuredConstant where
  | Base (const : Constant)
  | Block (tag : Nat) (fields : List StructuredConstant)
  deriving Repr

inductive Lambda where
  | Var (id : Ident)
  | Const (const : StructuredConstant)
  | Let (id : Ident) (value : Lambda) (body : Lambda)
  | MakeBlock (tag : Nat) (fields : List Lambda)
  deriving Repr

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr
  deriving Repr

structure RecordTypeDecl where
  labels : List RecordLabelDecl
  deriving Repr

inductive TypeDeclDesc where
  | Record (record : RecordTypeDecl)
  deriving Repr

structure TypeDecl where
  id : Ident
  desc : TypeDeclDesc
  deriving Repr

inductive SignatureItem where
  | TypeDecl (decl : TypeDecl)
  | Value (id : Ident) (type : TypeExpr)
  deriving Repr

structure Signature where
  items : List SignatureItem
  deriving Repr

structure JsonImport : Type where
  interface : Signature
  implementation : Lambda
  deriving Repr
