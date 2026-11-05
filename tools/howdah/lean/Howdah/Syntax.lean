inductive Constant where
  | string (string : String)
  deriving Repr

structure Ident where
  uniqueToplevelName : String
  deriving DecidableEq, Repr

instance : ToString Ident where
  toString := (·.uniqueToplevelName)

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

inductive Value where
  | const (const : StructuredConstant)
  | makeBlock (tag : Nat) (fields : List Value)
  deriving Repr

instance : Inhabited Value where
  default := .makeBlock USize.size []

inductive Lambda where
  | var (id : Ident)
  | const (const : StructuredConstant)
  | bind (id : Ident) (subst : Lambda) (body : Lambda)
  | makeBlock (tag : Nat) (fields : List Lambda)
  | value (value : Value) -- For call-by-value substitution.
  deriving Repr

inductive List.forAll {T} (P : T → Prop) : List T → Prop
  | Nil : [].forAll P
  | Cons {head tail} : P head → tail.forAll P → (head :: tail).forAll P

@[induction_eliminator]
def Lambda.induct' {motive : Lambda → Prop} :=
  @Lambda.rec motive (List.forAll motive)
    (nil := .Nil) (cons := fun _ _ ↦ .Cons)

@[simp]
def Lambda.size : Lambda → Nat
  | .var _ | .const _ | .value _ => 1
  | .bind _ subst body => subst.size + body.size |>.succ
  | .makeBlock _ fields => fields.map size |>.sum |>.succ

instance (priority := high) : SizeOf Lambda where
  sizeOf := Lambda.size

@[simp]
theorem unfoldSizeOfLambda l : sizeOf l = Lambda.size l := by rfl

@[simp]
theorem Lambda.size_lt_of_mem {fields : List Lambda} {field : Lambda} :
    field ∈ fields → field.size < (fields.map Lambda.size |>.sum |>.succ) := by
  intro inFields
  induction fields with
  | nil => contradiction
  | cons head tail ih =>
    cases inFields with simp
    | head => omega
    | tail _ inTail => specialize ih inTail; omega

/-
macro_rules
  | `(tactic| decreasing_trivial) =>
    `(tactic| exact Lambda.size_lt_of_mem (by assumption))
-/

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
  | exprDecl (id : Ident) (type : TypeExpr)
  deriving Repr

structure Signature where
  items : List SignatureItem
  deriving Repr

structure JsonImport : Type where
  interface : Signature
  implementation : Lambda
  deriving Repr
