import Lean
import Oxiclean.Model

open Oxiclean.Model Lean Elab Command

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

def Lean.Expr.mkList (type : Lean.Expr) : List Lean.Expr → Lean.Expr
  | [] => mkApp (mkConst ``List.nil [.zero]) type
  | x :: xs => mkApp3 (mkConst ``List.cons [.zero]) type x (mkList type xs)

def Lean.Expr.mkOption (type : Lean.Expr) : Option Lean.Expr → Lean.Expr
  | .none => mkApp (mkConst ``Option.none [.zero]) type
  | .some x => mkApp2 (mkConst ``Option.some [.zero]) type x

namespace Oxiclean.BuildSystem

def upstreamInterface (compilationUnit : String) : Lean.Expr :=
  mkConst (`Oxiclean.Extracted ++ .mkSimple compilationUnit ++ `interface)

deriving instance Lean.FromJson, Lean.ToExpr, Repr for PredefinedType

deriving instance Repr for
  ModuleItemAbsentAtRuntime, ModuleItemPresentAtRuntime, ModuleStatics,
  ModuleType, TypeDecl, ValType, TypeConstr

-- TODO: remove when https://github.com/leanprover/lean4/pull/14959 is merged
namespace WorkaroundForPr14959
  inductive ModuleStatics : Type where
    | upstreamCompilationUnit (compilationUnit : String)
    | var (deBruijn : Nat)
    | field (within : ModuleStatics) (index : Nat)
  deriving FromJson

  def ModuleStatics.toTerm : ModuleStatics → Lean.Expr
    | .upstreamCompilationUnit compilationUnit =>
      mkApp (mkConst ``Model.ModuleStatics.upstreamCompilationUnit)
        (upstreamInterface compilationUnit)
    | .var deBruijn =>
      mkApp (mkConst ``Model.ModuleStatics.var) (toExpr deBruijn)
    | .field within index =>
      mkApp2 (mkConst ``Model.ModuleStatics.field) within.toTerm (toExpr index)

  inductive TypeConstr : Type where
    | field (within : ModuleStatics) (index : Nat)
    | predef (predef : PredefinedType)
  deriving FromJson

  def TypeConstr.toTerm : TypeConstr → Lean.Expr
    | .field within index =>
      mkApp2 (mkConst ``Model.TypeConstr.field) within.toTerm (toExpr index)
    | .predef predefined =>
      mkApp (mkConst ``Model.TypeConstr.predef) (toExpr predefined)

  section ModuleType
    variable
      (ModuleItemAbsentAtRuntime ModuleItemPresentAtRuntime TypeDecl : Type)
    inductive ModuleType : Type where
      | signature
          (absent : List ModuleItemAbsentAtRuntime)
          (present : List ModuleItemPresentAtRuntime)
      | field (within : ModuleStatics) (index : Nat)
    deriving FromJson
  end ModuleType

  def ModuleType.toTerm
      : ModuleType Lean.Expr Lean.Expr → Lean.Expr
    | .signature absent present =>
      mkApp2 (mkConst ``Model.ModuleType.signature)
        (Lean.Expr.mkList
          (mkConst ``Model.ModuleItemAbsentAtRuntime)
          absent)
        (Lean.Expr.mkList
          (mkConst ``Model.ModuleItemPresentAtRuntime)
          present)
    | .field within index =>
      mkApp2 (mkConst ``Model.ModuleType.field) within.toTerm (toExpr index)

  section ModuleItemAbsentAtRuntime
    variable (ModuleType TypeDecl : Type)
    inductive ModuleItemAbsentAtRuntime : Type where
      | moduleType (moduleType : Option ModuleType)
      | valType (typeDecl : TypeDecl)
    deriving FromJson
  end ModuleItemAbsentAtRuntime

  def ModuleItemAbsentAtRuntime.toTerm
      : ModuleItemAbsentAtRuntime Lean.Expr Lean.Expr → Lean.Expr
    | .moduleType moduleTypeDecl =>
      mkApp
        (mkConst ``Model.ModuleItemAbsentAtRuntime.moduleType)
        (Lean.Expr.mkOption (mkConst ``Model.ModuleType) moduleTypeDecl)
    | .valType typeDecl =>
      mkApp (mkConst ``Model.ModuleItemAbsentAtRuntime.valType) typeDecl

  section ModuleItemPresentAtRuntime
    variable (ModuleType ValType : Type)
    inductive ModuleItemPresentAtRuntime : Type where
      | mod (moduleType : ModuleType)
      | val (valType : ValType)
    deriving FromJson
  end ModuleItemPresentAtRuntime

  def ModuleItemPresentAtRuntime.toTerm
      : ModuleItemPresentAtRuntime Lean.Expr Lean.Expr → Lean.Expr
    | .mod moduleType =>
      mkApp (mkConst ``Model.ModuleItemPresentAtRuntime.mod) moduleType
    | .val valType =>
      mkApp (mkConst ``Model.ModuleItemPresentAtRuntime.val) valType

  section TypeDecl
    variable (ValType : Type)
    structure TypeDecl : Type where
      manifest : Option ValType
      arity : Nat
    deriving FromJson
  end TypeDecl

  def TypeDecl.toTerm : TypeDecl Lean.Expr → Lean.Expr
    | { manifest, arity } =>
      mkApp2 (mkConst ``Model.TypeDecl.mk)
        (Lean.Expr.mkOption (mkConst ``Model.ValType) manifest)
        (toExpr arity)

  section ValType
    variable (ModuleType : Type)
    inductive ValType : Type where
      | arrow (domain : ValType) (codomain : ValType)
      | constr (constructor : TypeConstr) (args : List ValType)
      | firstClassModule (signature : ModuleType)
      | poly (body : ValType)
      | var (deBruijn : Nat)
    deriving FromJson
  end ValType

  def ValType.toTerm : ValType Lean.Expr → Lean.Expr
    | .arrow domain codomain =>
      mkApp2 (mkConst ``Model.ValType.arrow) domain.toTerm codomain.toTerm
    | .constr constructor args =>
      mkApp2 (mkConst ``Model.ValType.constr) constructor.toTerm
        (Lean.Expr.mkList (mkConst ``Model.ValType) (args.map ValType.toTerm))
    | .firstClassModule signature =>
      mkApp (mkConst ``Model.ValType.firstClassModule) signature
    | .poly body => mkApp (mkConst ``Model.ValType.poly) body.toTerm
    | .var deBruijn => mkApp (mkConst ``Model.ValType.var) (toExpr deBruijn)

  mutual
    partial def ModuleType.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonModuleType
        Lean.Expr
        Lean.Expr
        ⟨ModuleItemAbsentAtRuntime.fromJson?⟩
        ⟨ModuleItemPresentAtRuntime.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ModuleItemAbsentAtRuntime.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonModuleItemAbsentAtRuntime
        Lean.Expr
        Lean.Expr
        ⟨ModuleType.fromJson?⟩
        ⟨TypeDecl.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ModuleItemPresentAtRuntime.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonModuleItemPresentAtRuntime
        Lean.Expr
        Lean.Expr
        ⟨ModuleType.fromJson?⟩
        ⟨ValType.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def TypeDecl.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonTypeDecl
        Lean.Expr
        ⟨ValType.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ValType.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonValType
        Lean.Expr
        ⟨ModuleType.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)
  end
end WorkaroundForPr14959

deriving instance Lean.FromJson, Lean.ToExpr, Repr for
  IntegralBinaryScalarOperation
deriving instance Lean.FromJson, Lean.ToExpr, Repr for BinaryScalarOperation
deriving instance Lean.FromJson, Lean.ToExpr, Repr for Immediate
deriving instance Lean.FromJson, Lean.ToExpr, Repr for BlockKind

deriving instance Repr for
  Model.Expr, Value, Indirection, Block, BlockContents, ScalarOperation,
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
      | upstreamCompilationUnit (compilationUnit : String)
      | val (value : Value)
      | var (deBruijn : Nat)
    deriving FromJson
  end Expr

  def Expr.toTerm
      : Expr Lean.Expr Lean.Expr Lean.Expr Lean.Expr → Lean.Expr
    | .apply func arg =>
      mkApp2 (mkConst ``Model.Expr.apply) func.toTerm arg.toTerm
    | .block tag fields =>
      mkApp2 (mkConst ``Model.Expr.block) (toExpr tag)
        (Lean.Expr.mkList (mkConst ``Model.Expr) (fields.map Expr.toTerm))
    | .field indexInto index =>
      mkApp2 (mkConst ``Model.Expr.field) indexInto.toTerm (toExpr index)
    | .function body => mkApp (mkConst ``Model.Expr.function) body.toTerm
    | .letBinding bound body =>
      mkApp2 (mkConst ``Model.Expr.letBinding) bound.toTerm body.toTerm
    | .physicalEquality lhs rhs =>
      mkApp2 (mkConst ``Model.Expr.physicalEquality) lhs.toTerm rhs.toTerm
    | .scalarOperation operation =>
      mkApp (mkConst ``Model.Expr.scalarOperation) operation
    | .switch scrutinee nImmediates nBlocks immediates blocks failAction =>
      mkAppN (mkConst ``Model.Expr.switch)
        #[ scrutinee.toTerm
         , toExpr nImmediates
         , toExpr nBlocks
         , Lean.Expr.mkList (mkConst ``Model.ImmediateSwitchCase) immediates
         , Lean.Expr.mkList (mkConst ``Model.BlockSwitchCase) blocks
         , Lean.Expr.mkOption (mkConst ``Model.Expr)
             (match failAction with
              | .none => .none
              | .some failAction => .some failAction.toTerm)
         ]
    | .upstreamCompilationUnit compilationUnit =>
      mkApp (mkConst ``Model.Expr.upstreamCompilationUnit)
        (upstreamInterface compilationUnit)
    | .val value => mkApp (mkConst ``Model.Expr.val) value
    | .var deBruijn => mkApp (mkConst ``Model.Expr.var) (toExpr deBruijn)

  section Value
    variable (Indirection : Type)
    inductive Value : Type where
      | immediate (imm : Immediate)
      | indirection (ptr : Indirection)
    deriving FromJson
  end Value

  def Value.toTerm : Value Lean.Expr → Lean.Expr
    | .immediate imm => mkApp (mkConst ``Model.Value.immediate) (toExpr imm)
    | .indirection ptr => mkApp (mkConst ``Model.Value.indirection) ptr

  section Indirection
    variable (Block : Type)
    inductive Indirection : Type where
      | noMutableFields (block : Block)
    deriving FromJson
  end Indirection

  def Indirection.toTerm : Indirection Lean.Expr → Lean.Expr
    | .noMutableFields block =>
      mkApp (mkConst ``Model.Indirection.noMutableFields) block

  section BlockContents
    variable (Expr Value : Type)
    inductive BlockContents : Type where
      | values (fields : List Value) : BlockContents
      | bytes (data : List UInt8) : BlockContents
      | closure (body : Expr) : BlockContents
    deriving FromJson, Repr
  end BlockContents

  def BlockContents.toTerm? (kind : BlockKind)
      (contents : BlockContents Lean.Expr Lean.Expr)
      : Except String Lean.Expr :=
    match kind, contents with
    | .values, .values fields =>
      return mkApp (mkConst ``Model.BlockContents.values)
        (Lean.Expr.mkList (mkConst ``Model.Value) fields)
    | .bytes, .bytes data =>
      return mkApp (mkConst ``Model.BlockContents.bytes) (toExpr data)
    | .closure, .closure body =>
      return mkApp (mkConst ``Model.BlockContents.closure) body
    | .notYetSupported, _ => throw "block kind not yet supported"
    | _, _ => throw s!"block kind mismatch: {repr kind} ≠ {repr contents}"

  section Block
    variable (Expr Value : Type)
    structure Block : Type where
      tag : UInt8
      contents : BlockContents Expr Value
    deriving FromJson
  end Block

  def Block.toTerm? : Block Lean.Expr Lean.Expr → Except String Lean.Expr
    | { tag, contents } => do
      let contents ← contents.toTerm? (BlockKind.ofTag tag)
      return mkApp2 (mkConst ``Model.Block.mk) (toExpr tag) contents

  section ScalarOperation
    variable (Expr : Type)
    inductive ScalarOperation : Type where
      | binary (operation : BinaryScalarOperation) (lhs : Expr) (rhs : Expr)
    deriving FromJson
  end ScalarOperation

  def ScalarOperation.toTerm : ScalarOperation Lean.Expr → Lean.Expr
    | .binary operation lhs rhs =>
      mkApp3 (mkConst ``Model.ScalarOperation.binary) (toExpr operation) lhs rhs

  section ImmediateSwitchCase
    variable (Expr : Type)
    structure ImmediateSwitchCase : Type where
      immediate : Immediate
      body : Expr
    deriving FromJson
  end ImmediateSwitchCase

  def ImmediateSwitchCase.toTerm : ImmediateSwitchCase Lean.Expr → Lean.Expr
    | { immediate, body } =>
      mkApp2 (mkConst ``Model.ImmediateSwitchCase.mk) (toExpr immediate) body

  section BlockSwitchCase
    variable (Expr : Type)
    structure BlockSwitchCase : Type where
      tag : UInt8
      body : Expr
    deriving FromJson
  end BlockSwitchCase

  def BlockSwitchCase.toTerm : BlockSwitchCase Lean.Expr → Lean.Expr
    | { tag, body } =>
      mkApp2 (mkConst ``Model.BlockSwitchCase.mk) (toExpr tag) body

  mutual
    partial def Expr.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonExpr
        Lean.Expr
        Lean.Expr
        Lean.Expr
        Lean.Expr
        ⟨BlockSwitchCase.fromJson?⟩
        ⟨ImmediateSwitchCase.fromJson?⟩
        ⟨ScalarOperation.fromJson?⟩
        ⟨Value.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def Value.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonValue
        Lean.Expr
        ⟨Indirection.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def Indirection.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonIndirection
        Lean.Expr
        ⟨Block.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def Block.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonBlock
        Lean.Expr
        Lean.Expr
        ⟨Expr.fromJson?⟩
        ⟨Value.fromJson?⟩
      |>.fromJson? json
      >>= Block.toTerm?

    partial def ScalarOperation.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonScalarOperation
        Lean.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ImmediateSwitchCase.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonImmediateSwitchCase
        Lean.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def BlockSwitchCase.fromJson? (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonBlockSwitchCase
        Lean.Expr
        ⟨Expr.fromJson?⟩
      |>.fromJson? json
      |>.map (·.toTerm)
  end
end WorkaroundForPr14959

end Oxiclean.BuildSystem

def readJson (path : System.FilePath) : IO Json := do
  let fileContents : String ← IO.FS.readFile path
  Json.parse fileContents |> IO.ofExcept

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
  let interface : Json ← file.withFileName s!"{unit}.intf.json" |> readJson
  let implementation : Json ← file.withFileName s!"{unit}.impl.json" |> readJson
  let interface : Lean.Expr ←
    Oxiclean.BuildSystem.WorkaroundForPr14959.ModuleType.fromJson? interface
    |> IO.ofExcept
  let implementation : Lean.Expr ←
    Oxiclean.BuildSystem.WorkaroundForPr14959.Expr.fromJson? implementation
    |> IO.ofExcept
  liftCoreM do
    addTopLevelDefinition (ns ++ `interface)
      (mkConst ``Oxiclean.Model.ModuleType) interface
    addTopLevelDefinition (ns ++ `implementation)
      (mkConst ``Oxiclean.Model.Expr) implementation
