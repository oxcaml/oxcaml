inductive Constant where
  | string (string : String)
  deriving Repr

structure Ident where
  unique_toplevel_name : String
  deriving Repr

inductive Path where
  | ident (id : Ident)
  | dot (nested : Path) (field : String)
  deriving Repr

inductive TypeExpr where
  | constr (path : Path)
  | string
  deriving Repr

inductive StructuredConstant where
  | base (const : Constant)
  | block (tag : Nat) (fields : List StructuredConstant)
  deriving Repr

inductive Lambda where
  | var (id : Ident)
  | const (const : StructuredConstant)
  | let (id : Ident) (value : Lambda) (body : Lambda)
  | makeBlock (tag : Nat) (fields : List Lambda)
  deriving Repr

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr
  deriving Repr

structure RecordTypeDecl where
  labels : List RecordLabelDecl
  deriving Repr

inductive TypeDeclDesc where
  | record (record : RecordTypeDecl)
  deriving Repr

structure TypeDecl where
  id : Ident
  desc : TypeDeclDesc
  deriving Repr

inductive SignatureItem where
  | typeDecl (decl : TypeDecl)
  | value (id : Ident) (type : TypeExpr)
  deriving Repr

structure Signature where
  items : List SignatureItem
  deriving Repr

structure JsonImport : Type where
  interface : Signature
  implementation : Lambda
  deriving Repr
