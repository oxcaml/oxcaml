import Oxbow.Reduction
open Oxbow.Model
open Oxbow.Reduction

universe u a b c l r

namespace Oxbow.SemanticTypes

inductive _root_.List.forAll {α : Type u} (condition : α → Prop)
    : List α → Prop where
  | nil : [].forAll condition
  | cons {head : α}
      (headHolds : condition head) {tail : List α}
      (tailHolds : tail.forAll condition)
      : (head :: tail).forAll condition

inductive _root_.List.forAllPairs {L : Type l} {R : Type r}
    (condition : L → R → Prop)
    : List L → List R → Prop where
  | nil : [].forAllPairs condition []
  | cons {lhsHead rhsHead}
      (headsHold : condition lhsHead rhsHead) {lhsTail : List L} {rhsTail}
      (tailsHold : lhsTail.forAllPairs condition rhsTail)
      : (lhsHead :: lhsTail).forAllPairs condition (rhsHead :: rhsTail)

inductive _root_.List.forAllTriples {A : Type a} {B : Type b} {C : Type c}
    (condition : A → B → C → Prop)
    : List A → List B → List C → Prop where
  | nil : [].forAllTriples condition [] []
  | cons {aHead bHead cHead}
      (headsHold : condition aHead bHead cHead) {aTail : List A} {bTail} {cTail}
      (tailsHold : aTail.forAllTriples condition bTail cTail)
      : (aHead :: aTail).forAllTriples
          condition
          (bHead :: bTail)
          (cHead :: cTail)

structure PER (α : Type u) where
  eq : α → α → Prop
  symmetric {a b} : eq a b → eq b a
  transitive {a b c} : eq a b → eq b c → eq a c

namespace PER
  def always {α : Type u} : PER α where
    eq _ _ := True
    symmetric _ := True.intro
    transitive _ _ := True.intro

  def never {α : Type u} : PER α where
    eq _ _ := False
    symmetric := nofun
    transitive := nofun

  instance inhabited {α}: Inhabited (PER α) where
    default := never

  def diagonal {α : Type u} : PER α where
    eq := Eq
    symmetric := Eq.symm
    transitive := Eq.trans

  def refine {α : Type u} (condition : α → Prop) (per : PER α) : PER α where
    eq lhs rhs := per.eq lhs rhs ∧ condition lhs ∧ condition rhs
    symmetric
      | ⟨e, pl, pr⟩ => ⟨per.symmetric e, pr, pl⟩
    transitive ab bc := match ab, bc with
      | ⟨ab, pa, _⟩, ⟨bc, _, pc⟩ => ⟨per.transitive ab bc, pa, pc⟩

  def forAll {α : Type a} {β : Type b} (parameterized : α → PER β) : PER β where
    eq lhs rhs := ∀ a : α, parameterized a |>.eq lhs rhs
    symmetric h a := parameterized a |>.symmetric (h a)
    transitive xy yz a := parameterized a |>.transitive (xy a) (yz a)

  def forAllWhere {α : Type a} {β : Type b}
      (precondition : α → Prop)
      (parameterized : α → PER β)
      : PER β where
    eq lhs rhs := ∀ a : α, precondition a → (parameterized a).eq lhs rhs
    symmetric h a pre := parameterized a |>.symmetric (h a pre)
    transitive xy yz a pre :=
      parameterized a |>.transitive (xy a pre) (yz a pre)

  /--
  Wherever the first function is defined (even if nondeterministic),
  the second function can produce the same result.
  -/
  @[simp, pp_nodot]
  def atLeastAsDefinedAs {α : Type a} {β : Type b}
      (domain : PER α) (codomain : PER β)
      (reference maybeMoreDefined : α → β → Prop)
      : Prop :=
    ∀ lhs rhs : α, domain.eq lhs rhs →
    ∀ lhs', reference lhs lhs' →
    ∃ rhs', maybeMoreDefined rhs rhs' ∧ codomain.eq lhs' rhs'

  inductive Extensionality {α : Type a} {β : Type b}
      (domain : PER α)
      (codomain : PER β)
      : (α → β → Prop) → (α → β → Prop) → Prop where
    | extensionality {f g : α → β → Prop}
        (fAtLeastAsDefinedAsG : atLeastAsDefinedAs domain codomain f g)
        (gAtLeastAsDefinedAsF : atLeastAsDefinedAs domain codomain g f)
        : Extensionality domain codomain f g

  @[pp_nodot]
  def extensionality {α : Type a} {β : Type b}
      (domain : PER α)
      (codomain : PER β)
      : PER (α → β → Prop) where
    eq := Extensionality domain codomain
    symmetric
      | .extensionality hf hg => .extensionality hg hf
    transitive {a b c}
      | .extensionality ab ba, .extensionality bc cb => by
        simp at *; constructor <;> intro lhs rhs e lhs' al
        · obtain ⟨tmp', bTmp, eTmp⟩ := ab lhs rhs e lhs' al
          obtain ⟨rhs', br, e'⟩ :=
            bc rhs rhs (domain.transitive (domain.symmetric e) e) tmp' bTmp
          exact ⟨rhs', ⟨br, codomain.transitive eTmp e'⟩⟩
        · obtain ⟨tmp', bTmp, eTmp⟩ := cb lhs rhs e lhs' al
          obtain ⟨rhs', br, e'⟩ :=
            ba rhs rhs (domain.transitive (domain.symmetric e) e) tmp' bTmp
          exact ⟨rhs', ⟨br, codomain.transitive eTmp e'⟩⟩

  inductive Closure {Step} [S : StepRelation Step]
      (domain : PER Value)
      (codomain : PER Value)
      : Value → Value → Prop where
    | closure {f g : Expr}
        (extensional :
          (extensionality domain codomain).eq
            (fun input output => Reduce Step (f.substitute input) output)
            (fun input output => Reduce Step (g.substitute input) output))
        : Closure domain codomain
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure f }))
            (.indirection
              (.noMutableFields { tag := 247, contents := .closure g }))

  @[pp_nodot]
  def closure {Step} [S : StepRelation Step]
      (domain : PER Value)
      (codomain : PER Value)
      : PER Value where
    eq := @Closure Step S domain codomain
    symmetric
      | .closure extensional =>
        .closure ((extensionality domain codomain).symmetric extensional)
    transitive
      | .closure ab, .closure bc =>
        .closure ((extensionality domain codomain).transitive ab bc)

  inductive EmptyList {α} : List α → List α → Prop where
    | emptyList : EmptyList [] []

  @[pp_nodot]
  def emptyList {α : Type u} : PER (List α) where
    eq := EmptyList
    symmetric
      | .emptyList => .emptyList
    transitive
      | .emptyList, .emptyList => .emptyList

  inductive Immediate : Value → Value → Prop where
    | imm {bitsAboveLsb : BitVec 63}
        : Immediate (.immediate { bitsAboveLsb }) (.immediate { bitsAboveLsb })

  @[pp_nodot]
  def immediate : PER Value where
    eq := Immediate
    symmetric
      | .imm => .imm
    transitive
      | .imm, .imm => .imm

  inductive RefinedImmediate (condition : BitVec 63 → Prop)
      : Value → Value → Prop where
    | refinedImm {bitsAboveLsb : BitVec 63} (refined : condition bitsAboveLsb)
        : RefinedImmediate condition
            (.immediate { bitsAboveLsb })
            (.immediate { bitsAboveLsb })

  @[pp_nodot]
  def refinedImmediate (condition : BitVec 63 → Prop) : PER Value where
    eq := RefinedImmediate condition
    symmetric
      | .refinedImm r => .refinedImm r
    transitive
      | .refinedImm r, .refinedImm _ => .refinedImm r

  inductive Boolean : Value → Value → Prop where
    | false
        : Boolean
            (.immediate { bitsAboveLsb := 0 })
            (.immediate { bitsAboveLsb := 0 })
    | true
        : Boolean
            (.immediate { bitsAboveLsb := 1 })
            (.immediate { bitsAboveLsb := 1 })

  @[pp_nodot]
  def boolean : PER Value where
    eq := Boolean
    symmetric
      | .false => .false
      | .true => .true
    transitive
      | .false, .false => .false
      | .true, .true => .true

  inductive OxCamlOption (ifSome : PER Value) : Value → Value → Prop where
    | none
        : OxCamlOption ifSome
            (.immediate { bitsAboveLsb := 0 })
            (.immediate { bitsAboveLsb := 0 })
    | some
        {lhs rhs : Value}
        (relatedValuesInSome : ifSome.eq lhs rhs)
        : OxCamlOption ifSome
            (.indirection
              (.noMutableFields { tag := 0, contents := .values [lhs] }))
            (.indirection
              (.noMutableFields { tag := 0, contents := .values [rhs] }))

  @[pp_nodot]
  def option (ifSome : PER Value) : PER Value where
    eq := OxCamlOption ifSome
    symmetric
      | .none => .none
      | .some s => .some (ifSome.symmetric s)
    transitive
      | .none, .none => .none
      | .some ab, .some bc => .some (ifSome.transitive ab bc)

  inductive ModuleRuntimeBlock (relationOnValuesInTheBlock : PER (List Value))
      : Value → Value → Prop where
    | moduleRuntimeBlock
        {lhs rhs}
        (valuesSatisfyRelation : relationOnValuesInTheBlock.eq lhs rhs)
        : ModuleRuntimeBlock relationOnValuesInTheBlock
            (.indirection (.noMutableFields
              { tag := 0, contents := .values lhs }))
            (.indirection (.noMutableFields
              { tag := 0, contents := .values rhs }))

  @[pp_nodot]
  def moduleRuntimeBlock
      (relationOnValuesInTheBlock : PER (List Value))
      : PER Value where
    eq := ModuleRuntimeBlock relationOnValuesInTheBlock
    symmetric
      | .moduleRuntimeBlock valuesSatisfyRelation =>
        .moduleRuntimeBlock
          (relationOnValuesInTheBlock.symmetric valuesSatisfyRelation)
    transitive
      | .moduleRuntimeBlock ab, .moduleRuntimeBlock bc =>
        .moduleRuntimeBlock (relationOnValuesInTheBlock.transitive ab bc)

  inductive Cons {α} (headRelation : PER α) (tailRelation : PER (List α))
      : List α → List α → Prop where
    | cons
        {lhsHead rhsHead}
        (headSatisfiesRelation : headRelation.eq lhsHead rhsHead)
        {lhsTail rhsTail}
        (tailSatisfiesRelation : tailRelation.eq lhsTail rhsTail)
        : Cons
            headRelation
            tailRelation
            (lhsHead :: lhsTail)
            (rhsHead :: rhsTail)

  @[pp_nodot]
  def cons {α} (headRelation : PER α) (tailRelation : PER (List α))
      : PER (List α) where
    eq := Cons headRelation tailRelation
    symmetric
      | .cons headSatisfiesRelation tailSatisfiesRelation =>
        .cons
          (headRelation.symmetric headSatisfiesRelation)
          (tailRelation.symmetric tailSatisfiesRelation)
    transitive
      | .cons abHead abTail, .cons bcHead bcTail =>
        .cons
          (headRelation.transitive abHead bcHead)
          (tailRelation.transitive abTail bcTail)
end PER

namespace Statics
  structure AbstractType : Type 0 where
    abstractTypeRelation : PER Value

  structure DefinedElsewhere : Type 0 where
    -- empty

  structure Nil : Type 0 where
    -- empty

  structure Cons (Head Tail : Type 0) : Type 0 where
    head : Head
    tail : Tail

  structure AbstractModuleType : Type 0 where
    -- empty

  structure ModuleType (Absent Present : Type 0) : Type 0 where
    absent : Absent
    present : Present

  structure ModuleTypeDecl (Decl : Type 0) : Type 0 where
    moduleTypeDecl : Decl

  structure ModuleDecl (Decl : Type 0) : Type 0 where
    moduleDecl : Decl

  structure ValDecl (Decl : Type 0) : Type 0 where
    valDecl : Decl

  structure TypeDeclaration (Decl : Type 0) : Type 0 where
    typeDeclaration : Decl

  structure Arrow (Domain Codomain : Type 0) : Type 0 where
    domain : Domain
    codomain : Codomain

  structure TypeConstr (Constr Args : Type 0) : Type 0 where
    constr : Constr
    args : Args

  structure FirstClassModule (Signature : Type 0) : Type 0 where
    signature : Signature

  structure Poly (Body : Type 0) : Type 0 where
    body : Body
end Statics

end Oxbow.SemanticTypes

open Oxbow.SemanticTypes

@[simp]
def SemanticTypeConstructor : (arity : Nat) → Type 0
  | .zero => PER Value
  | .succ pred => PER Value → SemanticTypeConstructor pred

instance {arity} : Inhabited (SemanticTypeConstructor arity) where
  default := by
    induction arity with
    | zero => exact PER.inhabited.default
    | succ pred ih => exact fun _ => ih

instance : Inhabited ((arity : Nat) × SemanticTypeConstructor arity) where
  default := ⟨0, PER.inhabited.default⟩

@[simp]
def _root_.Oxbow.Model.ModuleType.validWrtExtraction
    (userSuppliedRefinedSignature : ModuleType SemanticTypeConstructor)
    (extractedParametricSignature : ModuleType Parametric)
    : Prop :=
  userSuppliedRefinedSignature.parameterize = extractedParametricSignature

@[simp]
def _root_.Oxbow.Model.PredefinedType.structuralRelation
    : PredefinedType → (arity : Nat) × SemanticTypeConstructor arity
  | .int => ⟨0, PER.immediate⟩
  | .bool => ⟨0, PER.boolean⟩
  /- CR-soon wsturgeon: `PER.option` needs a layout as well. -/
  | .option => ⟨1, PER.option⟩

structure SemanticModuleInContext : Type 0 where
  absent : List ((arity : Nat) × SemanticTypeConstructor arity)
  present : List (Option SemanticModuleInContext)
deriving Inhabited

@[simp]
def SemanticModuleInContext.getTypeConstructor!
    (index : Nat)
    (semanticModule : SemanticModuleInContext)
    : (arity : Nat) × SemanticTypeConstructor arity :=
  match semanticModule.absent[index]? with
  | .some typeConstructor => typeConstructor
  | .none => panic! s!"type constructor index out of bounds: #{index} of {semanticModule.absent.length}"

@[simp]
def SemanticTypeConstructor.instantiatedWithArguments!
    {arity : Nat}
    (relation : SemanticTypeConstructor arity)
    (args : List (PER Value))
    : PER Value :=
  match arity with
  | .zero =>
    match args with
    | [] => relation
    | _ :: _ => panic! s!"semantic type constructor arity mismatch: too many arguments ({arity} left over)"
  | .succ _ =>
    match args with
    | [] => panic! s!"semantic type constructor arity mismatch: not enough arguments (expected {arity} more)"
    | head :: tail => relation head |>.instantiatedWithArguments! tail

mutual
  @[simp]
  def _root_.Oxbow.Model.ModuleType.semanticModuleInContext!
      (moduleType : ModuleType SemanticTypeConstructor)
      : SemanticModuleInContext :=
    let { absent, present } := moduleType
    { absent := absent.map TypeDecl.semanticModuleInContext!,
      present :=
        present.map ModuleItemPresentAtRuntime.semanticModuleInContext! }

  @[simp]
  def _root_.Oxbow.Model.TypeDecl.semanticModuleInContext!
      (typeDecl : TypeDecl SemanticTypeConstructor)
      : (arity : Nat) × SemanticTypeConstructor arity :=
    match typeDecl.manifest with
    | .abstract constructor => ⟨typeDecl.arity, constructor⟩
    | .manifest _ => panic! "types with manifests not yet implemented"

  @[simp]
  def _root_.Oxbow.Model.ModuleItemPresentAtRuntime.semanticModuleInContext!
      : ModuleItemPresentAtRuntime SemanticTypeConstructor →
        Option SemanticModuleInContext
    | .mod m => .some m.semanticModuleInContext!
    | .val _ => .none
end

@[simp]
def _root_.List.toContext!
    : List (String × ModuleType SemanticTypeConstructor) →
      List (String × SemanticModuleInContext) :=
  List.map (fun ⟨name, modType⟩ => (name, modType.semanticModuleInContext!))
attribute [simp] _root_.List.lookup List.getElem?_cons_zero

@[simp]
def _root_.List.getDependencyCompilationUnit!
    (fullyQualifiedName : String)
    (dependencies : List (String × SemanticModuleInContext))
    : SemanticModuleInContext :=
  match dependencies.lookup fullyQualifiedName with
  | .none => panic s!"dependency compilation not found: #{fullyQualifiedName}"
  | .some dependency => dependency

@[simp]
def _root_.Oxbow.Model.ModuleStatics.resolve!
    (context : List SemanticModuleInContext)
    (dependencies : List (String × SemanticModuleInContext))
    (statics : ModuleStatics)
    : SemanticModuleInContext :=
  match statics with
  | .var deBruijn =>
    match context[deBruijn]? with
    | .none => panic s!"module de Bruijn index out of bounds: #{deBruijn} of {context.length}"
    | .some semanticModule => semanticModule
  | .field within index =>
    let semanticModule := within.resolve! context dependencies
    match semanticModule.present[index]? with
    | .none => panic s!"type index out of bounds: #{index} of {semanticModule.present.length}"
    | .some .none => panic s!"type index not populated: #{index} of {semanticModule.present.length}"
    | .some (.some submodule) => submodule
  | .compilationUnit fullyQualifiedName =>
    dependencies.getDependencyCompilationUnit! fullyQualifiedName

mutual
  @[simp]
  def _root_.Oxbow.Model.ModuleType.structuralRelationInScope!
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      (dependencies : List (String × SemanticModuleInContext))
      (userSuppliedRefinedSignature : ModuleType SemanticTypeConstructor)
      : PER Value :=
    let { present, .. } := userSuppliedRefinedSignature
    let semanticModuleInContext :=
      userSuppliedRefinedSignature.semanticModuleInContext!
    let relationOnValuesInTheBlock :=
      present.foldr
        (fun item tail =>
          let head :=
            @item.structuralRelationInScope! Step S
              (semanticModuleInContext :: context) dependencies
          PER.cons head tail)
        PER.emptyList
    PER.moduleRuntimeBlock relationOnValuesInTheBlock

  @[simp]
  def _root_.Oxbow.Model.ModuleItemPresentAtRuntime.structuralRelationInScope!
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      (dependencies : List (String × SemanticModuleInContext))
      : ModuleItemPresentAtRuntime SemanticTypeConstructor → PER Value
    | .mod m => @m.structuralRelationInScope! Step S context dependencies
    | .val v => @v.structuralRelationInScope! Step S context dependencies

  @[simp]
  def _root_.Oxbow.Model.ValType.structuralRelationInScope!
      {Step} [S : StepRelation Step]
      (context : List SemanticModuleInContext)
      (dependencies : List (String × SemanticModuleInContext))
      : ValType SemanticTypeConstructor → PER Value
    | .arrow domain codomain =>
      let domain :=
        @domain.structuralRelationInScope! Step S context dependencies
      let codomain :=
        @codomain.structuralRelationInScope! Step S context dependencies
      @PER.closure Step S domain codomain
    | .constr c args =>
      let ⟨arity, ctor⟩ :=
        match c with
        | .field within index =>
          within.resolve! context dependencies |>.getTypeConstructor! index
        | .predef p => p.structuralRelation
      args.map (@ValType.structuralRelationInScope! Step S context dependencies)
      |> ctor.instantiatedWithArguments!
    | .firstClassModule _ => panic! "first-class modules not yet implemented"
    | .poly _ => panic! "polymorphic types not yet implemented"
    | .var _ => panic! "type references not yet implemented"
end

def SemanticModuleInContext.refines
    (dependencySignature : ModuleType Parametric)
    (semanticModuleInContext : SemanticModuleInContext)
    : Prop :=
  ∃ refinedSignature : ModuleType SemanticTypeConstructor,
  refinedSignature.validWrtExtraction dependencySignature ∧
  refinedSignature.semanticModuleInContext! = semanticModuleInContext

@[simp]
def _root_.Oxbow.Model.ModuleType.structuralRelation!
    {Step} [S : StepRelation Step]
    (context : List (String × SemanticModuleInContext))
    (userSuppliedRefinedSignature : ModuleType SemanticTypeConstructor)
    : PER Value :=
  @userSuppliedRefinedSignature.structuralRelationInScope! Step S [] context

@[simp]
def _root_.List.enforceModularity
    (dependencySignatures : List (String × ModuleType Parametric))
    (withRefinedSignatures
      : (refinedSignatures
          : List (String × ModuleType SemanticTypeConstructor)) →
        Prop)
    : Prop :=
  {refinedSignatures : List (String × ModuleType SemanticTypeConstructor)} →
  (signaturesRefineDependencies
      : refinedSignatures.forAllPairs
          (fun ⟨name₁, semantic⟩ ⟨name₂, parametric⟩ =>
            name₁ = name₂ ∧
            ModuleType.validWrtExtraction semantic parametric)
          dependencySignatures) →
  withRefinedSignatures refinedSignatures

@[simp]
def _root_.List.enforceModularityAndPartialEquivalence {T : Type u}
    (dependencySignatures : List (String × ModuleType Parametric))
    (withRefinedSignatures
      : (refinedSignatures
          : List (String × ModuleType SemanticTypeConstructor)) →
        PER T)
    : PER T :=
  PER.forAllWhere
    (fun
        {refinedSignatures
          : List (String × ModuleType SemanticTypeConstructor)} =>
      refinedSignatures.forAllPairs
        (fun ⟨name₁, semantic⟩ ⟨name₂, parametric⟩ =>
          name₁ = name₂ ∧
          ModuleType.validWrtExtraction semantic parametric)
        dependencySignatures)
    withRefinedSignatures

@[simp]
def _root_.List.forAllDependencyValues
    {Step : Expr → Expr → Prop} [S : StepRelation Step]
    (dependencySignatures : List (String × ModuleType Parametric))
    (withDependencies : (dependencies : List (String × Value)) → Prop)
    : Prop :=
  dependencySignatures.enforceModularity fun refinedSignatures =>
  let context := refinedSignatures.toContext!
  {dependencies : List (String × Value)} →
  (dependenciesInhabitTheirSignatures
      : refinedSignatures.forAllPairs
          (fun ⟨name₁, sig⟩ ⟨name₂, v⟩ =>
            name₁ = name₂ ∧
            (@sig.structuralRelation! Step S context).eq v v)
          dependencies) →
  withDependencies dependencies

@[simp]
def _root_.List.enforceRepresentationInvariance
    {Step : Expr → Expr → Prop} [S : StepRelation Step]
    (dependencySignatures : List (String × ModuleType Parametric))
    (withDependencies
      : (context : List (String × SemanticModuleInContext)) →
        (dependencies₁ dependencies₂ : List (String × Value)) →
        Prop)
    : Prop :=
  dependencySignatures.enforceModularity fun refinedSignatures =>
  let context := refinedSignatures.toContext!
  {dependencies₁ dependencies₂ : List (String × Value)} →
  (dependenciesInhabitTheirSignatures
      : refinedSignatures.forAllTriples
          (fun ⟨name₀, sig⟩ ⟨name₁, v₁⟩ ⟨name₂, v₂⟩ =>
            name₀ = name₁ ∧
            name₁ = name₂ ∧
            (@sig.structuralRelation! Step S context).eq v₁ v₂)
          dependencies₁
          dependencies₂) →
  withDependencies context dependencies₁ dependencies₂

@[simp]
def _root_.Oxbow.Model.Expr.wellTyped
    (refinedInterface : ModuleType SemanticTypeConstructor)
    (dependencySignatures : List (String × ModuleType Parametric))
    (implementationWithoutDependencies : Expr)
    : Prop :=
  {Step : Expr → Expr → Prop} → [S : StepRelation Step] →
  @dependencySignatures.enforceRepresentationInvariance Step S
    (fun context dependencies₁ dependencies₂ =>
      let implementation₁ :=
        implementationWithoutDependencies.substituteCompilationUnits!
          dependencies₁
      let implementation₂ :=
        implementationWithoutDependencies.substituteCompilationUnits!
          dependencies₂
      (v₁ : Value) → (reduction₁ : Reduce Step implementation₁ v₁) →
      (v₂ : Value) → (reduction₂ : Reduce Step implementation₂ v₂) →
      (@refinedInterface.structuralRelation! Step S context).eq v₁ v₂)

@[simp]
def _root_.Oxbow.Model.Expr.mustReduceTo
    (expectedValue : Value)
    (dependencySignatures : List (String × ModuleType Parametric))
    (implementationWithoutDependencies : Expr)
    : Prop :=
  {Step : Expr → Expr → Prop} → [S : StepRelation Step] →
  @dependencySignatures.forAllDependencyValues Step S
    (fun dependencies =>
      let implementation :=
        implementationWithoutDependencies.substituteCompilationUnits!
          dependencies
      Reduce Step implementation expectedValue)
