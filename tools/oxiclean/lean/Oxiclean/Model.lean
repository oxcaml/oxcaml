namespace Oxiclean.Model

inductive PredefinedType : Type where
  | bool
  | int
  | option

inductive TypeKind : Type where
  | abstract

mutual
  inductive ModuleStatics : Type where
    | upstreamCompilationUnit (signature : ModuleType)
    | var (deBruijn : Nat)
    | field (within : ModuleStatics) (index : Nat)

  inductive ModuleType : Type where
    | signature
        (absent : List ModuleItemAbsentAtRuntime)
        (present : List ModuleItemPresentAtRuntime)
    | field (within : ModuleStatics) (index : Nat)

  inductive ModuleItemAbsentAtRuntime : Type where
    | moduleType (moduleTypeDecl : Option ModuleType)
    | valType (typeDecl : TypeDecl)

  inductive ModuleItemPresentAtRuntime : Type where
    | mod (moduleType : ModuleType)
    | val (valType : ValType)

  structure TypeDecl : Type where
    manifest : Option ValType
    arity : Nat

  inductive ValType : Type where
    | arrow (domain : ValType) (codomain : ValType)
    | constr (constructor : TypeConstr) (args : List ValType)
    | firstClassModule (signature : ModuleType)
    | poly (body : ValType)
    | var (deBruijn : Nat)

  inductive TypeConstr : Type where
    | field (within : ModuleStatics) (index : Nat)
    | predef (predef : PredefinedType)
end

inductive IntegralBinaryScalarOperation : Type where
  | modulo

inductive BinaryScalarOperation : Type where
  | integral (integral : IntegralBinaryScalarOperation)

structure Immediate : Type where
  bitsAboveLsb : BitVec 63

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
    | upstreamCompilationUnit (signature : ModuleType)
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
  -- ValType
  ValType.arrow ValType.constr ValType.firstClassModule ValType.poly ValType.var
  -- Expr
  Expr.apply Expr.block Expr.field Expr.function Expr.letBinding
  Expr.physicalEquality Expr.scalarOperation Expr.switch
  Expr.upstreamCompilationUnit Expr.val Expr.var
  -- Please keep this up to date when adding new constructors!

end Oxiclean.Model
