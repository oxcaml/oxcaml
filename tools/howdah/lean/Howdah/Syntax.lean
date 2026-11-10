inductive List.forAll {T} (P : T → Prop) : List T → Prop
  | Nil : [].forAll P
  | Cons {head tail} : P head → tail.forAll P → (head :: tail).forAll P

namespace Syntax

inductive Constant where
  | int (bits : BitVec 63)
  | string (bytes : List UInt8)

inductive IdentStamp where
  | predef
  | global
  | stamped (stamp : Nat)
  deriving DecidableEq

structure Ident where
  name : String
  stamp : IdentStamp
  deriving DecidableEq

instance : ToString Ident where
  toString := (·.name)

inductive Path where
  | ident (id : Ident)
  | dot (nested : Path) (field : String)

inductive TypeExpr where
  | constr (path : Path) (args : List TypeExpr)
  | arrow (lhs rhs : TypeExpr)
  | string

inductive StructuredConstant where
  | base (const : Constant)
  | block (tag : Nat) (fields : List StructuredConstant)

inductive Mutability : Type where
  | Mutable
  | Immutable

inductive Primitive : Type where
  | makeBlock (tag : Nat)
  | field (index : Nat)
  | physEqual
  | intMod

structure Function (Body : Type) : Type where
  headParam : Ident -- to prevent empty lists
  tailParams : List Ident
  body : Body

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
  | letBinding (id : Ident) (bound : Lambda) (body : Lambda)
  /- TODO: Lmutlet -/
  /- TODO: Lletrec -/
  /- Lprim -/
  | prim (prim : Primitive) (args : List Lambda)
  /- TODO: Lswitch -/
  /- TODO: Lstringswitch -/
  /- TODO: Lstaticraise -/
  /- TODO: Lstaticcatch -/
  /- TODO: Ltrywith -/
  /- Lifthenelse -/
  | ifThenElse (condition bodyIfTrue bodyIfFalse : Lambda)
  /- TODO: Lsequence -/
  /- TODO: Lwhile -/
  /- TODO: Lfor -/
  /- TODO: Lassign -/
  /- TODO: Lsend -/
  /- TODO: Levent -/
  /- TODO: Lifused -/
  /- TODO: Lregion -/
  /- TODO: Lexclave -/

structure RecordLabelDecl where
  id : Ident
  type : TypeExpr

structure RecordTypeDecl where
  labels : List RecordLabelDecl

inductive TypeDesc where
  | abstract
  | record (record : RecordTypeDecl)

structure TypeDecl where
  id : Ident
  desc : TypeDesc

mutual
  inductive SignatureItem where
    | typeDecl (decl : TypeDecl)
    | exprDecl (id : Ident) (type : TypeExpr)
    | moduleDecl (id : Ident) (type : ModuleType)
    | law (id : Ident)

  inductive ModuleType where
    | signature (items : List SignatureItem)
end

structure Signature where
  items : List SignatureItem

structure AbstractTypeDefinition where
  id : Ident
  definition : TypeExpr

structure LawParam where
  id : Ident
  type : TypeExpr

structure Law where
  name : String
  params : List LawParam
  assumptions : List (Function Lambda)
  conclusion : Function Lambda

end Syntax
