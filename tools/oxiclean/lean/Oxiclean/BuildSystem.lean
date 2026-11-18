import Lean
import Oxiclean.Model

open Oxiclean.Model

def BitVec.ofInt? {width : Nat} (i : Int)
    : Except String (BitVec width.succ) := do
  if -2^width ≤ i ∧ i < 2^width
  then return BitVec.ofInt width.succ i
  else throw s!"{i} (signed) does not fit into {width.succ} bits"

def BitVec.ofNat? {width : Nat} (n : Nat) : Except String (BitVec width) := do
  if n < 2^width
  then return BitVec.ofNat width n
  else throw s!"{n} (unsigned) does not fit into {width} bits"

instance {n : Nat} : Lean.FromJson (BitVec n.succ) where
  fromJson? json := Lean.FromJson.fromJson? json >>= BitVec.ofInt?

instance : Lean.FromJson UInt8 where
  fromJson? json := do
    let n : Nat ← Lean.FromJson.fromJson? json
    let b8 : BitVec 8 ← BitVec.ofNat? n
    return UInt8.ofBitVec b8

namespace Oxiclean.BuildSystem

deriving instance Lean.FromJson, Lean.ToExpr, Repr for PredefinedType
deriving instance Lean.FromJson, Lean.ToExpr, Repr for TypeConstructor
deriving instance Lean.FromJson, Lean.ToExpr, Repr for TypeKind

deriving instance Lean.ToExpr, Repr for ExprType, ModuleType

-- TODO: remove when https://github.com/leanprover/lean4/pull/14959 is merged
namespace WorkaroundForPr14959
  section ExprType
    variable (ModuleType : Type)
    inductive ExprType : Type where
      | arrow (domain : ExprType) (codomain : ExprType)
      | constr (constructor : TypeConstructor) (args : List ExprType)
      | mod (moduleType : ModuleType)
      | poly (body : ExprType)
      | var (deBruijn : Nat)
    deriving Lean.FromJson
  end ExprType

  def ExprType.toModel : ExprType Model.ModuleType → Model.ExprType
    | .arrow domain codomain => .arrow domain.toModel codomain.toModel
    | .constr c args => .constr c (args.map ExprType.toModel)
    | .mod moduleType => .mod moduleType
    | .poly body => .poly body.toModel
    | .var deBruijn => .var deBruijn

  section ModuleType
    variable (ExprType : Type)
    inductive ModuleType : Type where
      | empty
      | mod (moduleType : ModuleType) (etc : ModuleType)
      | ty (kind : TypeKind) (etc : ModuleType)
      | val (exprType : ExprType) (etc : ModuleType)
    deriving Lean.FromJson
  end ModuleType

  def ModuleType.toModel : ModuleType Model.ExprType → Model.ModuleType
    | .empty => .empty
    | .mod moduleType etc => .mod moduleType.toModel etc.toModel
    | .ty kind etc => .ty kind etc.toModel
    | .val exprType etc => .val exprType etc.toModel

  mutual
    partial def ModuleType.fromJson? (json : Lean.Json)
        : Except String Model.ModuleType := do
      @instFromJsonModuleType Model.ExprType ⟨ExprType.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def ExprType.fromJson? (json : Lean.Json)
        : Except String Model.ExprType := do
      @instFromJsonExprType Model.ModuleType ⟨ModuleType.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)
  end
end WorkaroundForPr14959

instance : Lean.FromJson ModuleType where
  fromJson? := WorkaroundForPr14959.ModuleType.fromJson?

instance : Lean.FromJson ExprType where
  fromJson? := WorkaroundForPr14959.ExprType.fromJson?

deriving instance Lean.FromJson, Lean.ToExpr, Repr for
  IntegralBinaryScalarOperation
deriving instance Lean.FromJson, Lean.ToExpr, Repr for BinaryScalarOperation
deriving instance Lean.FromJson, Lean.ToExpr, Repr for Immediate
deriving instance Lean.ToExpr, Repr for UpstreamCompilationUnit
deriving instance Lean.FromJson, Lean.ToExpr, Repr for BlockKind

structure UpstreamCompilationUnitPath : Type where
  path : List String
deriving Lean.FromJson

instance : Lean.FromJson UpstreamCompilationUnit where
  fromJson? json := do
    let { path := _ } : UpstreamCompilationUnitPath ← Lean.FromJson.fromJson? json
    throw "TODO"

deriving instance Lean.ToExpr, Repr for
  Expr, Value, Indirection, Block, BlockContents, ScalarOperation,
  ImmediateSwitchCase, BlockSwitchCase

-- TODO: remove when https://github.com/leanprover/lean4/pull/14959 is merged
namespace WorkaroundForPr14959
  section Expr
    variable (BlockSwitchCase ImmediateSwitchCase ScalarOperation Value : Type)
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
    deriving Lean.FromJson
  end Expr

  def Expr.toModel
      : Expr
          Model.BlockSwitchCase
          Model.ImmediateSwitchCase
          Model.ScalarOperation
          Model.Value →
        Model.Expr
    | .apply func arg => .apply func.toModel arg.toModel
    | .block tag fields => .block tag (fields.map Expr.toModel)
    | .field indexInto index => .field indexInto.toModel index
    | .function body => .function body.toModel
    | .letBinding bound body => .letBinding bound.toModel body.toModel
    | .physicalEquality lhs rhs => .physicalEquality lhs.toModel rhs.toModel
    | .scalarOperation operation => .scalarOperation operation
    | .switch s ni nb i b .none => .switch s.toModel ni nb i b .none
    | .switch s ni nb i b (.some f) =>
      .switch s.toModel ni nb i b (.some f.toModel)
    | .upstreamCompilationUnit compilationUnit =>
      .upstreamCompilationUnit compilationUnit
    | .val v => .val v
    | .var i => .var i

  section Value
    variable (Indirection : Type)
    inductive Value : Type where
      | immediate (imm : Immediate)
      | indirection (ptr : Indirection)
    deriving Lean.FromJson
  end Value

  def Value.toModel : Value Model.Indirection → Model.Value
    | .immediate imm => .immediate imm
    | .indirection ptr => .indirection ptr

  section Indirection
    variable (Block : Type)
    inductive Indirection : Type where
      | noMutableFields (block : Block)
    deriving Lean.FromJson
  end Indirection

  def Indirection.toModel : Indirection Model.Block → Model.Indirection
    | .noMutableFields block => .noMutableFields block

  section BlockContents
    variable (Expr Value : Type)
    inductive BlockContents : Type where
      | values (fields : List Value) : BlockContents
      | bytes (data : List UInt8) : BlockContents
      | closure (body : Expr) : BlockContents
    deriving Lean.FromJson, Repr
  end BlockContents

  def BlockContents.toModel? kind
      (contents : BlockContents Model.Expr Model.Value)
      : Except String (Model.BlockContents kind) :=
    match kind, contents with
    | .values, .values fields => return .values fields
    | .bytes, .bytes data => return .bytes data
    | .closure, .closure body => return .closure body
    | .notYetSupported, _ => throw "block kind not yet supported"
    | _, _ => throw s!"block kind mismatch: {repr kind} ≠ {repr contents}"

  section Block
    variable (Expr Value : Type)
    structure Block : Type where
      tag : UInt8
      contents : BlockContents Expr Value
    deriving Lean.FromJson
  end Block

  def Block.toModel? : Block Model.Expr Model.Value → Except String Model.Block
    | { tag, contents } => do
      let contents ← contents.toModel? (BlockKind.ofTag tag)
      return { tag, contents }

  section ScalarOperation
    variable (Expr : Type)
    inductive ScalarOperation : Type where
      | binary (operation : BinaryScalarOperation) (lhs : Expr) (rhs : Expr)
    deriving Lean.FromJson
  end ScalarOperation

  def ScalarOperation.toModel
      : ScalarOperation Model.Expr → Model.ScalarOperation
    | .binary operation lhs rhs => .binary operation lhs rhs

  section ImmediateSwitchCase
    variable (Expr : Type)
    structure ImmediateSwitchCase : Type where
      immediate : Immediate
      body : Expr
    deriving Lean.FromJson
  end ImmediateSwitchCase

  def ImmediateSwitchCase.toModel
      : ImmediateSwitchCase Model.Expr → Model.ImmediateSwitchCase
    | { immediate, body } => { immediate, body }

  section BlockSwitchCase
    variable (Expr : Type)
    structure BlockSwitchCase : Type where
      tag : UInt8
      body : Expr
    deriving Lean.FromJson
  end BlockSwitchCase

  def BlockSwitchCase.toModel
      : BlockSwitchCase Model.Expr → Model.BlockSwitchCase
    | { tag, body } => { tag, body }

  mutual
    partial def Expr.fromJson? (json : Lean.Json)
        : Except String Model.Expr := do
      @instFromJsonExpr
        Model.BlockSwitchCase
        Model.ImmediateSwitchCase
        Model.ScalarOperation
        Model.Value
        ⟨BlockSwitchCase.fromJson?⟩
        ⟨ImmediateSwitchCase.fromJson?⟩
        ⟨ScalarOperation.fromJson?⟩
        ⟨Value.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def Value.fromJson? (json : Lean.Json)
        : Except String Model.Value := do
      @instFromJsonValue
        Model.Indirection
        ⟨Indirection.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def Indirection.fromJson? (json : Lean.Json)
        : Except String Model.Indirection := do
      @instFromJsonIndirection
        Model.Block
        ⟨Block.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def Block.fromJson? (json : Lean.Json)
        : Except String Model.Block := do
      @instFromJsonBlock
        Model.Expr
        Model.Value
        ⟨Expr.fromJson?⟩
        ⟨Value.fromJson?⟩
      |>.fromJson? json
      >>= Block.toModel?

    partial def ScalarOperation.fromJson? (json : Lean.Json)
        : Except String Model.ScalarOperation := do
      @instFromJsonScalarOperation
        Model.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def ImmediateSwitchCase.fromJson? (json : Lean.Json)
        : Except String Model.ImmediateSwitchCase := do
      @instFromJsonImmediateSwitchCase
        Model.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)

    partial def BlockSwitchCase.fromJson? (json : Lean.Json)
        : Except String Model.BlockSwitchCase := do
      @instFromJsonBlockSwitchCase
        Model.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toModel)
  end
end WorkaroundForPr14959

instance : Lean.FromJson Expr where
  fromJson? := WorkaroundForPr14959.Expr.fromJson?

instance : Lean.FromJson Value where
  fromJson? := WorkaroundForPr14959.Value.fromJson?

instance : Lean.FromJson Indirection where
  fromJson? := WorkaroundForPr14959.Indirection.fromJson?

instance : Lean.FromJson Block where
  fromJson? := WorkaroundForPr14959.Block.fromJson?

instance : Lean.FromJson ScalarOperation where
  fromJson? := WorkaroundForPr14959.ScalarOperation.fromJson?

instance : Lean.FromJson ImmediateSwitchCase where
  fromJson? := WorkaroundForPr14959.ImmediateSwitchCase.fromJson?

instance : Lean.FromJson BlockSwitchCase where
  fromJson? := WorkaroundForPr14959.BlockSwitchCase.fromJson?

end Oxiclean.BuildSystem

open Lean Elab Command

def readJson {α : Type} [FromJson α] (path : System.FilePath) : IO α := do
  let fileContents : String ← IO.FS.readFile path
  let json : Json ← Json.parse fileContents |> IO.ofExcept
  fromJson? json |> IO.ofExcept

def addTopLevelDefinition (name : Name) (type : Lean.Expr) (value : Lean.Expr)
    : CoreM Unit :=
  addAndCompile <| .defnDecl
    { name, levelParams := [], type, value, hints := .abbrev, safety := .safe }

elab "_do_not_use_this_outside_automatically_generated_files%" : command => do
  let file : System.FilePath := ← getFileName
  let some unit := file.fileStem >>= (·.dropPrefix? "extracted_oxcaml_")
    | throwError "{file} is not named `extracted_oxcaml_<unit>.lean`"
  let unit : String := unit.toString
  let ns : Name := `Oxiclean.Extracted ++ .mkSimple unit.capitalize
  let interface : Oxiclean.Model.ModuleType ←
    file.withFileName s!"{unit}.intf.json" |> readJson
  let implementation : Oxiclean.Model.Expr ←
    file.withFileName s!"{unit}.impl.json" |> readJson
  liftCoreM do
    addTopLevelDefinition (ns ++ `interface)
      (mkConst ``Oxiclean.Model.ModuleType) (toExpr interface)
    addTopLevelDefinition (ns ++ `implementation)
      (mkConst ``Oxiclean.Model.Expr) (toExpr implementation)
