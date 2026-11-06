inductive Constant where
  | string (bytes : List UInt8)
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

inductive BlockKind : Type where
  | values
  | bytes
  | closure
  | notYetSupported -- lazy, effect continuations, etc.
  deriving Repr

@[simp]
def BlockKind.ofTag (tag : UInt8) : BlockKind :=
  if tag = 247 then .closure else
  if tag < 244 then .values else
  if tag < 251 then .notYetSupported else
  .bytes

inductive StructuredConstant where
  | base (const : Constant)
  | block (tag : Nat) (fields : List StructuredConstant)
  deriving Repr

inductive Mutability : Type where
  | Mutable
  | Immutable
  deriving Repr

inductive Primitive : Type where
  | makeBlock (tag : Nat)
  deriving Repr

/-
This should be a character-for-character copy of the compiler's [Lambda.lambda]
to avoid misformalization via translation errors.
-/
inductive Lambda (V : Type) : Type where
  /- Lvar -/
  | var (id : Ident)
  /- TODO: Lmutvar -/
  /- Lconst -/
  | const (const : StructuredConstant)
  /- TODO: Lapply -/
  /- TODO: Lfunction -/
  /- Llet -/
  | bind (id : Ident) (subst : Lambda V) (body : Lambda V)
  /- TODO: Lmutlet -/
  /- TODO: Lletrec -/
  /- Lprim -/
  | prim (prim : Primitive) (args : List <| Lambda V)
  /- TODO: Lswitch -/
  /- TODO: Lstringswitch -/
  /- TODO: Lstaticraise -/
  /- TODO: Lstaticcatch -/
  /- TODO: Ltrywith -/
  /- TODO: Lifthenelse -/
  /- TODO: Lsequence -/
  /- TODO: Lwhile -/
  /- TODO: Lfor -/
  /- TODO: Lassign -/
  /- TODO: Lsend -/
  /- TODO: Levent -/
  /- TODO: Lifused -/
  /- TODO: Lregion -/
  /- TODO: Lexclave -/
  /- A fully-reduced value (for call-by-value substitution). -/
  | value (value : V)
  deriving Repr

mutual
  inductive Value : Type where
    | immediate (bits : BitVec 63)
    | block (tag : UInt8) (contents : BlockContents (.ofTag tag))
    deriving Repr

  -- GADT
  inductive BlockContents : BlockKind → Type where
    | values (values : List Value) : BlockContents .values
    | bytes (bytes : List UInt8) : BlockContents .bytes
    | closure (rec : Recursive) (closure : Closure) : BlockContents .closure
    -- nothing for `.notYetSupported`
    deriving Repr

  structure Closure : Type where
    self : Ident
    headParam : Ident -- to prevent empty lists
    tailParams : List Ident
    body : Lambda Value
    deriving Repr

  inductive Recursive : Type where
    | nonrecursive
    | recursive (mutuals : List Closure)
    deriving Repr
end

instance : Inhabited Value where
  default := .block 0 <| .values []

abbrev Expr := Lambda Value

@[simp]
def Lambda.size {V} : Lambda V → Nat
  | .var _ | .const _ | .value _ => 1
  | .bind _ subst body => subst.size + body.size |>.succ
  | .prim _ args => args.map size |>.sum |>.succ

instance (priority := high) {V} : SizeOf <| Lambda V where
  sizeOf := Lambda.size

@[simp]
theorem unfoldSizeOfLambda {V} (l : Lambda V) :
    sizeOf l = Lambda.size l := by rfl

@[simp]
theorem Lambda.size_lt_of_mem {V}
    {fields : List <| Lambda V} {field} :
    field ∈ fields →
    field.size < (fields.map Lambda.size |>.sum |>.succ) := by
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

inductive List.forAll {T} (P : T → Prop) : List T → Prop
  | Nil : [].forAll P
  | Cons {head tail} : P head → tail.forAll P → (head :: tail).forAll P

@[induction_eliminator]
def Lambda.induct' {V} {motive : Lambda V → Prop}
    (var : ∀ id, motive (.var id))
    (const : ∀ c, motive (.const c))
    (bind :
      ∀ id subst body,
      motive subst →
      motive body →
      motive (.bind id subst body))
    (prim : ∀ prim args, List.forAll motive args → motive (.prim prim args))
    (value : ∀ v, motive (.value v))
    : ∀ l, motive l
:=
  @Lambda.rec V
    motive
    (List.forAll motive)
    (var := var)
    (const := const)
    (bind := bind)
    (prim := prim)
    (value := value)
    (nil := .Nil)
    (cons := fun _ _ => .Cons)

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr
  deriving Repr

structure RecordTypeDecl where
  labels : List RecordLabelDecl
  deriving Repr

inductive TypeDesc where
  | record (record : RecordTypeDecl)
  deriving Repr

structure TypeDecl where
  id : Ident
  desc : TypeDesc
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
  implementation : Expr
  deriving Repr
