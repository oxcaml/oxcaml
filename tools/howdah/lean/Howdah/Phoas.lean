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

private partial def quoteConstant
    (identType : Lean.Expr) : Syntax.StructuredConstant → TermElabM Lean.Expr
  | .base (.int bits) => do
    let bits ← exprToSyntax (toExpr bits)
    elabTermEnsuringType
      (← `(.immediate { bitsAboveLsb := $bits }))
      (some (mkApp (mkConst ``Value) identType))
  | .base (.string bytes) =>
    quoteBlockValue identType 252
      (mkApp2 (mkConst ``BlockContents.bytes) identType (toExpr bytes))
  | .block natTag fields => do
    if h : natTag < UInt8.size then
      let tag := UInt8.ofNatLT natTag h
      match BlockKind.ofTag tag with
      | .values =>
        let fields ← fields.mapM (quoteConstant identType)
        let fields ← mkListLit (mkApp (mkConst ``Value) identType) fields
        quoteBlockValue identType tag
          (mkApp2 (mkConst ``BlockContents.values) identType fields)
      | _ => throwError "invalid block tag {natTag}"
    else
      throwError "invalid block tag {natTag}"

private def quoteLambda
    (identType : Lean.Expr)
    (env : Syntax.Ident → Option Lean.Expr) :
    Syntax.Lambda → TermElabM Lean.Expr
  | .var id => do
    let some x := env id | throwError "unbound identifier {id.name}"
    return mkApp2 (mkConst ``_root_.Expr.var) identType x
  | .const c => do
    let value ← quoteConstant identType c
    return mkApp2 (mkConst ``_root_.Expr.value) identType value
  | .bind id bound body => do
    let boundExpr ← quoteLambda identType env bound
    let bodyExpr ← withLocalDeclD (Name.mkSimple id.name) identType fun x => do
      let env' := fun other => if other = id then some x else env other
      let bodyExpr ← quoteLambda identType env' body
      mkLambdaFVars #[x] bodyExpr
    return mkApp3 (mkConst ``_root_.Expr.bind) identType boundExpr bodyExpr
  | .apply _ _ | .function _ | .prim _ _ =>
    throwError "unsupported Lambda expression"

def Syntax.Lambda.toPhoas? (source : Syntax.Lambda) : TermElabM Lean.Expr :=
  withLocalDeclD `Ident (mkSort (.succ .zero)) fun identType => do
    let body ← quoteLambda identType (fun _ => none) source
    mkLambdaFVars #[identType] body

namespace IdentityFunctionExample
  @[simp] def x : Syntax.Ident := { name := "x", stamp := .stamped 42 }
  @[simp] def f : Syntax.Lambda := .bind x (.const (.base (.int 42))) (.var x)
  @[simp] def v {I} : @Expr I := .value (.immediate ⟨42⟩)
  theorem asExpected I : (by_elab f.toPhoas?) I = .bind v .var := by
    simp
end IdentityFunctionExample
