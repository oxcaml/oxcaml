namespace Oxiclean.Model

inductive PredefinedType : Type where
  | bool
  | int
  | option

inductive TypeConstructor : Type where
  | predef (predef : PredefinedType)
  | var (deBruijn : Nat)

inductive TypeKind : Type where
  | abstract

mutual
  inductive ExprType : Type where
    | arrow (domain : ExprType) (codomain : ExprType)
    | constr (constructor : TypeConstructor) (args : List ExprType)
    | mod (moduleType : ModuleType)
    | poly (body : ExprType)
    | var (deBruijn : Nat)

  inductive ModuleType : Type where
    | empty
    | mod (moduleType : ModuleType) (etc : ModuleType)
    | ty (kind : TypeKind) (etc : ModuleType)
    | val (exprType : ExprType) (etc : ModuleType)
end

inductive IntegralBinaryScalarOperation : Type where
  | modulo

inductive BinaryScalarOperation : Type where
  | integral (integral : IntegralBinaryScalarOperation)

structure Immediate : Type where
  bitsAboveLsb : BitVec 63

-- Defined separately to allow custom import behavior.
structure UpstreamCompilationUnit : Type where
  signature : ModuleType

inductive BlockKind : Type where
  | values
  | bytes
  | closure
  | notYetSupported

def BlockKind.ofTag (tag : UInt8) : BlockKind :=
  if tag < 244 then .values else
  if tag > 250 then .bytes else
  if tag = 247 then .closure else
  .notYetSupported

mutual
  inductive Expr : Type where
    | apply (func : Expr) (arg : Expr)
    | block (tag : UInt8) (fields : List Expr)
    | field (indexInto : Expr) (index : Nat)
    | function (body : Expr)
    | letBinding (bound : Expr) (body : Expr)
    | physicalEquality (lhs : Expr) (rhs : Expr)
    | scalarOperation (operation : ScalarOperation)
    | switch
        (scrutinee : Expr)
        (nImmediates : Nat)
        (nBlocks : Nat)
        (immediates : List ImmediateSwitchCase)
        (blocks : List BlockSwitchCase)
        (failAction : Option Expr)
    | upstreamCompilationUnit (compilationUnit : UpstreamCompilationUnit)
    | val (value : Value)
    | var (deBruijn : Nat)

  inductive Value : Type where
    | immediate (imm : Immediate)
    | indirection (ptr : Indirection)

  inductive Indirection : Type where
    | noMutableFields (block : Block)

  structure Block : Type where
    tag : UInt8
    contents : BlockContents (BlockKind.ofTag tag)

  inductive BlockContents : BlockKind → Type where
    | values (fields : List Value) : BlockContents .values
    | bytes (data : List UInt8) : BlockContents .bytes
    | closure (body : Expr) : BlockContents .closure

  inductive ScalarOperation : Type where
    | binary (operation : BinaryScalarOperation) (lhs : Expr) (rhs : Expr)

  structure ImmediateSwitchCase : Type where
    immediate : Immediate
    body : Expr

  structure BlockSwitchCase : Type where
    tag : UInt8
    body : Expr
end

attribute [pp_nodot]
  -- ExprType
  ExprType.arrow ExprType.constr ExprType.mod ExprType.poly ExprType.var
  -- ModuleType
  ModuleType.empty ModuleType.mod ModuleType.ty ModuleType.val
  -- Expr
  Expr.apply Expr.block Expr.field Expr.function Expr.letBinding
  Expr.physicalEquality Expr.scalarOperation Expr.switch
  Expr.upstreamCompilationUnit Expr.val Expr.var
  -- Please keep this up to date when adding new constructors!

end Oxiclean.Model
