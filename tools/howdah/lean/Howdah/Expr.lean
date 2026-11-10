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

  inductive Expression : Type where
    /- Lvar -/
    | var (id : Ident)
    /- TODO: Lmutvar -/
    /- Lconst is replaced by `.value` -/
    /- Lapply -/
    | apply (func : Expression) (args : List Expression)
    /- Lfunction -/
    | function (function : Function)
    /- Llet -/
    | letBinding (bound : Expression) (body : Ident → Expression)
    /- TODO: Lmutlet -/
    /- TODO: Lletrec -/
    /- Lprim -/
    | prim (prim : Primitive) (args : List Expression)
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
    body : Vector Ident additionalParams.succ → Expression

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
    -- | recursive (self : Ident) (mutuals : List Function)
end

instance {Ident} : Inhabited (Value (Ident := Ident)) where
  default := .indirection (.immutable { tag := 0, contents := .values [] })

@[simp]
def Block.values? {Ident} (natTag : Nat)
    (contents : List (Value (Ident := Ident))) :
    Option (Block (Ident := Ident)) :=
  if tagIsNotTooBig : natTag < UInt8.size then
    let tag := UInt8.ofNatLT natTag tagIsNotTooBig
    match kind : BlockKind.ofTag tag with
    | .values => .some { tag, contents := kind ▸ .values contents }
    | _ => .none
  else .none

@[simp] abbrev Expr := ∀ {Ident}, Expression (Ident := Ident)
@[simp] abbrev Hole := ∀ {Ident}, Ident → Expression (Ident := Ident)
@[simp] abbrev Fun := ∀ {Ident}, Function (Ident := Ident)
@[simp] abbrev Val := ∀ {Ident}, Value (Ident := Ident)
@[simp] abbrev Indirect := ∀ {Ident}, Indirection (Ident := Ident)
@[simp] abbrev Blk := ∀ {Ident}, Block (Ident := Ident)

@[simp]
def Blk.values? (natTag : Nat) (contents : List Val) : Option Blk :=
  if tagIsNotTooBig : natTag < UInt8.size then
    let tag := UInt8.ofNatLT natTag tagIsNotTooBig
    match kind : BlockKind.ofTag tag with
    | .values => .some fun {Ident} => {
        tag,
        contents := kind ▸ .values (contents.map (fun v => v (Ident := Ident))),
      }
    | _ => .none
  else .none

@[simp]
def Expr.value (v : Val) : Expr :=
  fun {Ident} => Expression.value (v (Ident := Ident))

@[simp]
def Expr.function (f : Fun) : Expr :=
  fun {Ident} => Expression.function (f (Ident := Ident))

@[simp]
def Expr.letBinding (bound : Expr) (body : Hole) : Expr :=
  fun {Ident} => Expression.letBinding (bound (Ident := Ident)) body

@[simp]
def Expr.prim (p : Primitive) (args : List Expr) : Expr :=
  fun {Ident} => Expression.prim p (args.map fun e => e (Ident := Ident))

@[simp]
def Val.indirection (indirect : Indirect) : Val :=
  fun {Ident} => Value.indirection (Ident := Ident) indirect

@[simp]
def Indirect.immutable (block : Blk) : Indirect :=
  fun {Ident} => Indirection.immutable (Ident := Ident) block
