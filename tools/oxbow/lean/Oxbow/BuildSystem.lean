import Lean
import Oxbow.Model

open Oxbow.Model Lean Elab Command

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

namespace Oxbow.BuildSystem

deriving instance Lean.FromJson, Lean.ToExpr, Repr for PredefinedType

deriving instance Repr for ModuleStatics

def parametric (name : Name) : Lean.Expr :=
  mkApp (mkConst name [.zero]) (mkConst ``Model.Parametric)

-- TODO: remove when https://github.com/leanprover/lean4/pull/14959 is merged
namespace WorkaroundForPr14959
  inductive ModuleStatics : Type where
    | compilationUnit (name : String)
    | var (deBruijn : Nat)
    | field (within : ModuleStatics) (index : Nat)
  deriving FromJson

  def ModuleStatics.toTerm? (dependencyNames : List String)
      : ModuleStatics → Except String Lean.Expr
    | .compilationUnit name => do
      let .some indexAmongDependencies := dependencyNames.idxOf? name
        | throw s!"couldn't find a dependency called `{name}`"
      mkApp
        (mkConst ``Model.ModuleStatics.compilationUnit)
        (toExpr indexAmongDependencies)
      |> pure
    | .var deBruijn =>
      pure (mkApp (mkConst ``Model.ModuleStatics.var) (toExpr deBruijn))
    | .field within index => do
      let within ← within.toTerm? dependencyNames
      pure (mkApp2 (mkConst ``Model.ModuleStatics.field) within (toExpr index))

  inductive TypeConstr : Type where
    | field (within : ModuleStatics) (index : Nat)
    | predef (predef : PredefinedType)
  deriving FromJson

  def TypeConstr.toTerm? (dependencyNames : List String)
      : TypeConstr → Except String Lean.Expr
    | .field within index => do
      let within ← within.toTerm? dependencyNames
      pure (mkApp2 (parametric ``Model.TypeConstr.field) within (toExpr index))
    | .predef predefined =>
      pure (mkApp (parametric ``Model.TypeConstr.predef) (toExpr predefined))

  section ModuleType
    variable
      (TypeDecl ModuleItemPresentAtRuntime : Type)
    structure ModuleType : Type where
      absent : List TypeDecl
      present : List ModuleItemPresentAtRuntime
    deriving FromJson
  end ModuleType

  def ModuleType.toTerm?
      : ModuleType Lean.Expr Lean.Expr → Except String Lean.Expr
    | { absent, present } =>
      mkApp2 (parametric ``Model.ModuleType.mk)
        (Lean.Expr.mkList (parametric ``Model.TypeDecl) absent)
        (Lean.Expr.mkList
          (parametric ``Model.ModuleItemPresentAtRuntime)
          present)
      |> pure

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
      mkApp (parametric ``Model.ModuleItemPresentAtRuntime.mod) moduleType
    | .val valType =>
      mkApp (parametric ``Model.ModuleItemPresentAtRuntime.val) valType

  section TypeDecl
    variable (ValType : Type)
    structure TypeDecl : Type where
      manifest : Option ValType
      arity : Nat
    deriving FromJson
  end TypeDecl

  def TypeDecl.toTerm : TypeDecl Lean.Expr → Lean.Expr
    | { manifest, arity } =>
      let arity := toExpr arity
      let manifest := match manifest with
        | .none =>
          mkApp2
            (parametric ``Model.TypeManifest.abstract)
            arity
            (mkApp (mkConst ``Model.Parametric.parametric) arity)
        | .some definition =>
          mkApp2 (parametric ``Model.TypeManifest.manifest) arity definition
      mkApp2 (parametric ``Model.TypeDecl.mk) arity manifest

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

  def ValType.toTerm? (dependencyNames : List String)
      : ValType Lean.Expr → Except String Lean.Expr
    | .arrow domain codomain => do
      let domain ← domain.toTerm? dependencyNames
      let codomain ← codomain.toTerm? dependencyNames
      pure (mkApp2 (parametric ``Model.ValType.arrow) domain codomain)
    | .constr constructor args => do
      let constructor ← constructor.toTerm? dependencyNames
      let args ← args.mapM (·.toTerm? dependencyNames)
      mkApp2 (parametric ``Model.ValType.constr) constructor
        (Lean.Expr.mkList (parametric ``Model.ValType) args)
      |> pure
    | .firstClassModule signature =>
      pure (mkApp (parametric ``Model.ValType.firstClassModule) signature)
    | .poly body => do
      let body ← body.toTerm? dependencyNames
      pure (mkApp (parametric ``Model.ValType.poly) body)
    | .var deBruijn =>
      pure (mkApp (parametric ``Model.ValType.var) (toExpr deBruijn))

  mutual
    partial def ModuleType.fromJson?
        (dependencyNames : List String)
        (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonModuleType
        Lean.Expr
        Lean.Expr
        ⟨TypeDecl.fromJson? dependencyNames⟩
        ⟨ModuleItemPresentAtRuntime.fromJson? dependencyNames⟩
      |>.fromJson? json
      >>= (·.toTerm?)

    partial def ModuleItemPresentAtRuntime.fromJson?
        (dependencyNames : List String)
        (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonModuleItemPresentAtRuntime
        Lean.Expr
        Lean.Expr
        ⟨ModuleType.fromJson? dependencyNames⟩
        ⟨ValType.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def TypeDecl.fromJson?
        (dependencyNames : List String)
        (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonTypeDecl
        Lean.Expr
        ⟨ValType.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ValType.fromJson?
        (dependencyNames : List String)
        (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonValType
        Lean.Expr
        ⟨ModuleType.fromJson? dependencyNames⟩
      |>.fromJson? json
      >>= (·.toTerm? dependencyNames)
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
      | compilationUnit (name : String)
      | val (value : Value)
      | var (deBruijn : Nat)
    deriving FromJson
  end Expr

  def Expr.toTerm? (dependencyNames : List String)
      : Expr Lean.Expr Lean.Expr Lean.Expr Lean.Expr → Except String Lean.Expr
    | .apply func arg => do
      let func ← func.toTerm? dependencyNames
      let arg ← arg.toTerm? dependencyNames
      pure (mkApp2 (mkConst ``Model.Expr.apply) func arg)
    | .block tag fields => do
      let fields ← fields.mapM (·.toTerm? dependencyNames)
      mkApp2 (mkConst ``Model.Expr.block) (toExpr tag)
        (Lean.Expr.mkList (mkConst ``Model.Expr) fields)
      |> pure
    | .field within index => do
      let within ← within.toTerm? dependencyNames
      pure (mkApp2 (mkConst ``Model.Expr.field) within (toExpr index))
    | .function body => do
      let body ← body.toTerm? dependencyNames
      pure (mkApp (mkConst ``Model.Expr.function) body)
    | .letBinding bound body => do
      let bound ← bound.toTerm? dependencyNames
      let body ← body.toTerm? dependencyNames
      pure (mkApp2 (mkConst ``Model.Expr.letBinding) bound body)
    | .physicalEquality lhs rhs => do
      let lhs ← lhs.toTerm? dependencyNames
      let rhs ← rhs.toTerm? dependencyNames
      pure (mkApp2 (mkConst ``Model.Expr.physicalEquality) lhs rhs)
    | .scalarOperation operation =>
      pure (mkApp (mkConst ``Model.Expr.scalarOperation) operation)
    | .switch scrutinee nImmediates nBlocks immediates blocks failAction => do
      let scrutinee ← scrutinee.toTerm? dependencyNames
      let failAction ← match failAction with
        | .none => pure .none
        | .some failAction => .some <$> failAction.toTerm? dependencyNames
      mkAppN (mkConst ``Model.Expr.switch)
        #[ scrutinee
         , toExpr nImmediates
         , toExpr nBlocks
         , Lean.Expr.mkList (mkConst ``Model.ImmediateSwitchCase) immediates
         , Lean.Expr.mkList (mkConst ``Model.BlockSwitchCase) blocks
         , Lean.Expr.mkOption (mkConst ``Model.Expr) failAction
         ]
      |> pure
    | .compilationUnit name => do
      let .some indexAmongDependencies := dependencyNames.idxOf? name
        | throw s!"couldn't find a dependency called `{name}`"
      mkApp
        (mkConst ``Model.Expr.compilationUnit)
        (toExpr indexAmongDependencies)
      |> pure
    | .val value => pure (mkApp (mkConst ``Model.Expr.val) value)
    | .var deBruijn => pure (mkApp (mkConst ``Model.Expr.var) (toExpr deBruijn))

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
    partial def Expr.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonExpr
        Lean.Expr
        Lean.Expr
        Lean.Expr
        Lean.Expr
        ⟨BlockSwitchCase.fromJson? dependencyNames⟩
        ⟨ImmediateSwitchCase.fromJson? dependencyNames⟩
        ⟨ScalarOperation.fromJson? dependencyNames⟩
        ⟨Value.fromJson? dependencyNames⟩
      |>.fromJson? json
      >>= (·.toTerm? dependencyNames)

    partial def Value.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonValue
        Lean.Expr
        ⟨Indirection.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def Indirection.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonIndirection
        Lean.Expr
        ⟨Block.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def Block.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonBlock
        Lean.Expr
        Lean.Expr
        ⟨Expr.fromJson? dependencyNames⟩
        ⟨Value.fromJson? dependencyNames⟩
      |>.fromJson? json
      >>= Block.toTerm?

    partial def ScalarOperation.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonScalarOperation
        Lean.Expr
        ⟨Expr.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def ImmediateSwitchCase.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonImmediateSwitchCase
        Lean.Expr
        ⟨Expr.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)

    partial def BlockSwitchCase.fromJson? (dependencyNames : List String) (json : Json)
        : Except String Lean.Expr := do
      @instFromJsonBlockSwitchCase
        Lean.Expr
        ⟨Expr.fromJson? dependencyNames⟩
      |>.fromJson? json
      |>.map (·.toTerm)
  end
end WorkaroundForPr14959

def readJson (path : System.FilePath) : IO Json := do
  let fileContents : String ← IO.FS.readFile path
  Json.parse fileContents |> IO.ofExcept

def addTopLevelDefinition (name : Name) (type : Lean.Expr) (value : Lean.Expr)
    : CoreM Unit := do
  addAndCompile <| .defnDecl
    { name, levelParams := [], type, value, hints := .abbrev, safety := .safe }
  enableRealizationsForConst name

elab "_do_not_use_this_outside_automatically_generated_files%" : command => do
  let file : System.FilePath := ← getFileName
  let some unit := file.fileStem >>= (·.dropPrefix? "extracted_oxcaml_")
    | throwError "{file} is not named `extracted_oxcaml_<unit>.lean`"
  let unit : String := unit.toString
  let prefixComponents : Name :=
    `Oxbow.Extracted ++ .mkSimple unit.capitalize
  let env : Lean.Environment ← getEnv
  let dependencyNames : List String :=
    env.header.imports.toList.filterMap fun imported =>
      imported.module.toString.dropPrefix? "extracted_oxcaml_"
      |>.map (·.toString.capitalize)
  let interface : Json ← file.withFileName s!"{unit}.intf.json" |> readJson
  let implementation : Json ← file.withFileName s!"{unit}.impl.json" |> readJson
  let interface : Lean.Expr ←
    WorkaroundForPr14959.ModuleType.fromJson?
      dependencyNames
      interface
    |> IO.ofExcept
  let implementation : Lean.Expr ←
    WorkaroundForPr14959.Expr.fromJson?
      dependencyNames
      implementation
    |> IO.ofExcept
  let dependencies : Lean.Expr :=
    (dependencyNames.map fun name =>
      mkConst (`Oxbow.Extracted ++ .mkSimple name ++ `interface))
    |> Lean.Expr.mkList (parametric ``Model.ModuleType)
  liftCoreM do
    addTopLevelDefinition (prefixComponents ++ `dependencies)
      (mkApp
        (mkConst ``List [Level.zero])
        (parametric ``Model.ModuleType))
      dependencies
    addTopLevelDefinition (prefixComponents ++ `interface)
      (parametric ``Model.ModuleType) interface
    addTopLevelDefinition (prefixComponents ++ `implementation)
      (mkConst ``Model.Expr) implementation
  let simpDeps ←
    `(attribute [simp] $(mkIdent (prefixComponents ++ `dependencies)))
  let simpIntf ←
    `(attribute [simp] $(mkIdent (prefixComponents ++ `interface)))
  let simpImpl ←
    `(attribute [simp] $(mkIdent (prefixComponents ++ `implementation)))
  elabCommand simpDeps
  elabCommand simpIntf
  elabCommand simpImpl

end Oxbow.BuildSystem
