inductive BlockKind : Type where
  | values
  | bytes
  | closure
  | notYetSupported -- lazy, effect continuations, etc.
  deriving Repr

inductive Loc : Type where
  -- TODO
  deriving Repr

@[simp]
def BlockKind.ofTag (tag : UInt8) : BlockKind :=
  if tag < 244 then .values else
  if tag > 250 then .bytes else
  match tag with
  | 247 => .closure
  | _ => .notYetSupported

structure Immediate : Type where
  bitsAboveLsb : BitVec 63
  deriving Repr

inductive Primitive : Type where
  | makeBlock (tag : Nat)
  | field (index : Nat)
  deriving Repr

mutual
  variable {Ident : Type}

  inductive Expr : Type where
    /- Lvar -/
    | var (id : Ident)
    /- TODO: Lmutvar -/
    /- Lconst is replaced by `.value` -/
    /- Lapply -/
    | apply (func : Expr) (args : List Expr)
    /- Lfunction -/
    | function (function : Function)
    /- Llet -/
    | bind (bound : Expr) (body : Ident → Expr)
    /- TODO: Lmutlet -/
    /- TODO: Lletrec -/
    /- Lprim -/
    | prim (prim : Primitive) (args : List Expr)
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
    | value (value : Value)

  inductive Value : Type where
    | immediate (immediate : Immediate)
    | indirection (indirection : Indirection)

  structure Function : Type where
    additionalParams : Nat -- `.succ` to prevent zero-argument functions
    body : Vector Ident additionalParams.succ → Expr

  inductive Indirection : Type where
    | immutable (block : Block) -- use Lean's ambient model of indirection
    | mutable (loc : Loc) -- block with a known, shared memory location

  structure Block : Type where
    tag : UInt8
    contents : BlockContents (BlockKind.ofTag tag) -- notably dependent on `tag`

  inductive BlockContents : BlockKind → Type where
    | values (values : List Value) : BlockContents .values
    | bytes (bytes : List UInt8) : BlockContents .bytes
    | closure (rec : Recursive) (closure : Function) : BlockContents .closure

  inductive Recursive : Type where
    | nonrecursive
    | recursive (self : Ident) (mutuals : List Function)
end

instance {Ident} : Inhabited (Value (Ident := Ident)) where
  default := .indirection (.immutable { tag := 0, contents := .values [] })
