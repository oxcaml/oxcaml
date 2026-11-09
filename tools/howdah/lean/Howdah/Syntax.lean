inductive List.forAll {T} (P : T → Prop) : List T → Prop
  | Nil : [].forAll P
  | Cons {head tail} : P head → tail.forAll P → (head :: tail).forAll P

namespace Syntax

inductive Constant where
  | int (bits : BitVec 63)
  | string (bytes : List UInt8)
  deriving Repr

inductive IdentStamp where
  | predef
  | global
  | stamped (stamp : Nat)
  deriving DecidableEq, Repr

structure Ident where
  name : String
  stamp : IdentStamp
  deriving DecidableEq, Repr

instance : ToString Ident where
  toString := (·.name)

inductive Path where
  | ident (id : Ident)
  | dot (nested : Path) (field : String)
  deriving Repr

inductive TypeExpr where
  | constr (path : Path) (args : List TypeExpr)
  | arrow (lhs rhs : TypeExpr)
  | string
  deriving Repr

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
  | field (index : Nat)
  deriving Repr

structure Function (Body : Type) : Type where
  headParam : Ident -- to prevent empty lists
  tailParams : List Ident
  body : Body
  deriving Repr

/-
This should be a character-for-character copy of the compiler's [Lambda.lambda]
to avoid misformalization via translation errors.
-/
inductive Lambda : Type where
  /- Lvar -/
  | var (id : Ident)
  /- TODO: Lmutvar -/
  /- Lconst -/
  | const (const : StructuredConstant)
  /- Lapply -/
  | apply (func : Lambda) (args : List Lambda)
  /- Lfunction -/
  | function (function : Function Lambda)
  /- Llet -/
  | bind (id : Ident) (bound : Lambda) (body : Lambda)
  /- TODO: Lmutlet -/
  /- TODO: Lletrec -/
  /- Lprim -/
  | prim (prim : Primitive) (args : List Lambda)
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
  deriving Repr

@[simp]
def Lambda.size : Lambda → Nat
  | .var _ | .const _ => 1
  | .function f => f.body.size.succ
  | .apply func args => func.size + (args.map size).sum |>.succ
  | .bind _ bound body => bound.size + body.size |>.succ
  | .prim _ args => args.map size |>.sum |>.succ
decreasing_by
  all_goals first
    | decreasing_tactic
    | cases f with (simp; omega)

instance (priority := high) : SizeOf Lambda where
  sizeOf := Lambda.size

@[simp]
theorem unfoldSizeOfLambda (l : Lambda) :
    sizeOf l = Lambda.size l := by rfl

@[simp]
theorem Lambda.size_lt_of_mem
    {fields : List Lambda} {field} :
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

@[induction_eliminator]
def Lambda.induct' {motive : Lambda → Prop} :=
  let motiveList : List Lambda → Prop := List.forAll motive
  let motiveFunction : Function Lambda → Prop := (motive ·.body)
  @Lambda.rec
    motive
    motiveList
    motiveFunction
    (mk := fun _ _ _ h => h)
    (nil := .Nil)
    (cons := fun _ _ => .Cons)

/-
@[induction_eliminator]
def Lambda.induct' {V} {motive : Lambda V → Prop}
    (var : ∀ id, motive (.var id))
    (const : ∀ const, motive (.const const))
    (function :
      ∀ headParam tailParam body,
      motive body →
      motive (.function headParam tailParam body))
    (bind :
      ∀ id bound body,
      motive bound →
      motive body →
      motive (.bind id bound body))
    (prim : ∀ prim args, List.forAll motive args → motive (.prim prim args))
    (value : ∀ value, motive (.value value))
    : ∀ l, motive l
:=
  @Lambda.rec V
    motive
    (List.forAll motive)
    (var := var)
    (const := const)
    (function := function)
    (bind := bind)
    (prim := prim)
    (value := value)
    (nil := .Nil)
    (cons := fun _ _ => .Cons)
-/

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr
  deriving Repr

structure RecordTypeDecl where
  labels : List RecordLabelDecl
  deriving Repr

inductive TypeDesc where
  | abstract
  | record (record : RecordTypeDecl)
  deriving Repr

structure TypeDecl where
  id : Ident
  desc : TypeDesc
  deriving Repr

mutual
  inductive SignatureItem where
    | typeDecl (decl : TypeDecl)
    | exprDecl (id : Ident) (type : TypeExpr)
    | moduleDecl (id : Ident) (type : ModuleType)
    deriving Repr

  inductive ModuleType where
    | signature (items : List SignatureItem)
end

structure Signature where
  items : List SignatureItem
  deriving Repr

end Syntax
