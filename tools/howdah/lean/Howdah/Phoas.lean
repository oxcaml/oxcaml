-- Parametric higher-order abstract syntax.

import Lean
import Howdah.Syntax
import Howdah.Expr

open Lean Meta Elab Term

private def quoteBlockValue
    (identType : Lean.Expr) (tag : UInt8) (contents : Lean.Expr) :
    TermElabM Lean.Expr := do
  let tag ← exprToSyntax (toExpr tag)
  let contents ← exprToSyntax contents
  elabTermEnsuringType
    (← `(.indirection (.immutable { tag := $tag, contents := $contents })))
    (some (mkApp (mkConst ``Value) identType))

private partial def Syntax.StructuredConstant.quote
    (identType : Lean.Expr) : Syntax.StructuredConstant → TermElabM Lean.Expr
  | .base (.int bits) => do
    let bits ← toExpr bits |> exprToSyntax
    let imm ← `(.immediate { bitsAboveLsb := $bits })
    elabTermEnsuringType imm (some (mkApp (mkConst ``Value) identType))
  | .base (.string bytes) =>
    quoteBlockValue identType 252
      (mkApp2 (mkConst ``BlockContents.bytes) identType (toExpr bytes))
  | .block natTag fields => do
    let eachField ← fields.mapM (Syntax.StructuredConstant.quote identType)
    let fields ← mkListLit (mkApp (mkConst ``Value) identType) eachField
    let block ← withTransparency .all <| whnf <|
      mkApp3 (mkConst ``Block.values?) identType (toExpr natTag) fields
    unless block.isAppOfArity ``Option.some 2 do
      throwError "invalid constant block tag {natTag}"
    let block ← exprToSyntax (block.getArg! 1)
    elabTermEnsuringType
      (← `(.indirection (.immutable $block)))
      (some (mkApp (mkConst ``Value) identType))

private partial def Syntax.Lambda.quote
    (identType : Lean.Expr)
    (env : Syntax.Ident → Option Lean.Expr) :
    Syntax.Lambda → TermElabM Lean.Expr
  | .var id => do
    let some x := env id | throwError "unbound identifier {id.name}"
    return mkApp2 (mkConst ``_root_.Expression.var) identType x
  | .const c => do
    let value ← c.quote identType
    return mkApp2 (mkConst ``_root_.Expression.value) identType value
  | .letBinding id bound body => do
    let boundExpr ← bound.quote identType env
    let bodyExpr ← withLocalDeclD (Name.mkSimple id.name) identType fun x => do
      let env' := fun other => if other = id then some x else env other
      let bodyExpr ← body.quote identType env'
      mkLambdaFVars #[x] bodyExpr
    return mkApp3 (mkConst ``_root_.Expression.letBinding) identType boundExpr bodyExpr
  | .apply func args => do
    let func ← func.quote identType env
    let args ← args.mapM (Syntax.Lambda.quote identType env)
    let args ← mkListLit (mkApp (mkConst ``_root_.Expression) identType) args
    return mkApp3 (mkConst ``_root_.Expression.apply) identType func args
  | .function f => do
    let params := f.headParam :: f.tailParams
    let body ← withLocalDeclsDND
        (params.toArray.map fun id => (Name.mkSimple id.name, identType)) fun xs => do
      let env' := (params.zip xs.toList).foldl
        (fun env (id, x) => fun other => if other = id then some x else env other) env
      mkLambdaFVars xs (← f.body.quote identType env')
    let body ← withLocalDeclD `args
        (mkApp2 (mkConst ``Vector [.zero]) identType (toExpr params.length)) fun args => do
      let args' ← exprToSyntax args
      let xs ← (List.range params.length).mapM fun i => do
        elabTerm (← `($args'[$(Syntax.mkNumLit (toString i))])) none
      synthesizeSyntheticMVarsNoPostponing
      mkLambdaFVars #[args] (← instantiateMVars (mkAppN body xs.toArray))
    let body ← exprToSyntax body
    let additionalParams ← exprToSyntax (toExpr f.tailParams.length)
    elabTermEnsuringType
      (← `(.function { additionalParams := $additionalParams, body := $body }))
      (some (mkApp (mkConst ``_root_.Expression) identType))
  | .prim p args => do
    let p := match p with
      | .makeBlock tag => mkApp (mkConst ``_root_.Primitive.makeBlock) (toExpr tag)
      | .field index => mkApp (mkConst ``_root_.Primitive.field) (toExpr index)
      | .physEqual => mkConst ``_root_.Primitive.physEqual
      | .intMod => mkConst ``_root_.Primitive.intMod
    let args ← args.mapM (Syntax.Lambda.quote identType env)
    let args ← mkListLit (mkApp (mkConst ``_root_.Expression) identType) args
    return mkApp3 (mkConst ``_root_.Expression.prim) identType p args
  | .ifThenElse condition bodyIfTrue bodyIfFalse => do
    let condition ← condition.quote identType env
    let bodyIfTrue ← bodyIfTrue.quote identType env
    let bodyIfFalse ← bodyIfFalse.quote identType env
    return mkApp4 (mkConst ``_root_.Expression.ifThenElse)
      identType condition bodyIfTrue bodyIfFalse

def Syntax.Lambda.toPhoas? (source : Syntax.Lambda) : TermElabM Lean.Expr :=
  withLocalDeclD `Ident (mkSort (.succ .zero)) fun identType => do
    let body ← source.quote identType fun _ => none
    mkLambdaFVars #[identType] body

namespace IdentityFunctionExample
  @[simp] def x : Syntax.Ident := { name := "x", stamp := .stamped 42 }
  @[simp] def f : Syntax.Lambda := .letBinding x (.const (.base (.int 42))) (.var x)
  @[simp] def v {I : Type} : Expression (Ident := I) :=
    .value (.immediate ⟨42⟩)
  theorem asExpected (I : Type) :
      (by_elab f.toPhoas?) I = Expression.letBinding v .var := by
    simp
end IdentityFunctionExample
