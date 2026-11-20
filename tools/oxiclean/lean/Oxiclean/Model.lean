namespace Oxiclean.Model

inductive PredefinedType : Type where
  | bool
  | int
  | option

inductive ModuleStatics : Type where
  /--
  An imported dependency.
  Note that only its signature is accessible;
  its implementation is invisible for modularity.
  -/
  | compilationUnit (indexAmongDependencies : Nat)
  | var (deBruijn : Nat)
  | field (within : ModuleStatics) (index : Nat)

mutual
  universe u
  variable (AbstractType : Type u)

  inductive ModuleType : Type u where
    | signature
        (absent : List ModuleItemAbsentAtRuntime)
        (present : List ModuleItemPresentAtRuntime)
    | field (within : ModuleStatics) (index : Nat)

  inductive ModuleItemAbsentAtRuntime : Type u where
    | moduleType (moduleTypeDecl : Option ModuleType)
    | valType (typeDecl : TypeDecl)

  inductive ModuleItemPresentAtRuntime : Type u where
    | mod (moduleType : ModuleType)
    | val (valType : ValType)

  structure TypeDecl : Type u where
    arity : Nat
    manifest : TypeManifest

  inductive TypeManifest : Type u where
    | abstract (abstractType : AbstractType)
    | manifest (definition : ValType)

  inductive ValType : Type u where
    | arrow (domain : ValType) (codomain : ValType)
    | constr (constructor : TypeConstr) (args : List ValType)
    | firstClassModule (signature : ModuleType)
    | poly (body : ValType)
    | var (deBruijn : Nat)

  inductive TypeConstr : Type u where
    | field (within : ModuleStatics) (index : Nat)
    | predef (predef : PredefinedType)
end

/-- Sentinel used to denote that abstract types are left undefined. -/
inductive Parametric : Type where
  | parametric

mutual
  universe u
  variable (AbstractType : Type u)

  def ModuleType.parameterize
      : ModuleType AbstractType -> ModuleType Parametric
    | .signature absent present =>
      .signature
        (absent.map (·.parameterize))
        (present.map (·.parameterize))
    | .field within index => .field within index

  def ModuleItemAbsentAtRuntime.parameterize
      : ModuleItemAbsentAtRuntime AbstractType ->
        ModuleItemAbsentAtRuntime Parametric
    | .moduleType .none => .moduleType .none
    | .moduleType (.some m) => .moduleType (.some m.parameterize)
    | .valType typeDecl => .valType typeDecl.parameterize

  def ModuleItemPresentAtRuntime.parameterize
      : ModuleItemPresentAtRuntime AbstractType ->
      ModuleItemPresentAtRuntime Parametric
    | .mod moduleType => .mod moduleType.parameterize
    | .val valType => .val valType.parameterize

  def TypeDecl.parameterize
      : TypeDecl AbstractType -> TypeDecl Parametric
    | { arity, manifest } => { arity, manifest := manifest.parameterize }

  def TypeManifest.parameterize
      : TypeManifest AbstractType -> TypeManifest Parametric
    | .abstract _ => .abstract .parametric
    | .manifest definition => .manifest definition.parameterize

  def ValType.parameterize
      : ValType AbstractType -> ValType Parametric
    | .arrow domain codomain => .arrow domain.parameterize codomain.parameterize
    | .constr c args => .constr c.parameterize (args.map (·.parameterize))
    | .firstClassModule signature => .firstClassModule signature.parameterize
    | .poly body => .poly body.parameterize
    | .var deBruijn => .var deBruijn

  def TypeConstr.parameterize
      : TypeConstr AbstractType -> TypeConstr Parametric
    | .field within index => .field within index
    | .predef predef => .predef predef
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
    | compilationUnit (indexAmongDependencies : Nat)
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
  Expr.apply Expr.block Expr.compilationUnit Expr.field Expr.function
  Expr.letBinding Expr.physicalEquality Expr.scalarOperation Expr.switch
  Expr.val Expr.var
  -- Please keep this up to date when adding new constructors!

end Oxiclean.Model
