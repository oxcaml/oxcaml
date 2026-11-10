import Howdah.Import
import Howdah.Per

open Lean Elab Command Term Meta

private def abstractTypes (interface : Syntax.Signature) : List Syntax.Ident :=
  interface.items.filterMap fun
    | .typeDecl { id, desc := .abstract } => some id
    | _ => none

private partial def perOf (unit : Name) (types : Ident)
    (abstract : List Syntax.Ident) : Syntax.TypeExpr → CommandElabM Term
  | .constr (.ident ⟨"int", .predef⟩) [] => `(Per.int)
  | .constr (.ident ⟨"option", .predef⟩) [arg] => do
    `(Per.option $(← perOf unit types abstract arg))
  | .arrow lhs rhs => do
    `(Per.arrow $(← perOf unit types abstract lhs)
        $(← perOf unit types abstract rhs))
  | .constr (.ident id) [] =>
    if abstract.contains id then
      `($(mkIdent (unit ++ `Types ++ Name.mkSimple id.name)) $types)
    else throwError "type `{id.name}` is not yet supported"
  | .constr path _ => throwError "type at path `{repr path}` is not yet supported"
  | .string => throwError "type `string` is not yet supported"
where
  repr : Syntax.Path → String
    | .ident id => id.name
    | .dot nested field => s!"{repr nested}.{field}"

private def addDefinition (name : Name) (type : Lean.Expr)
    (value : TermElabM Lean.Expr) : CommandElabM Unit :=
  liftTermElabM do
    let value ← instantiateMVars (← value)
    addDecl <| .defnDecl {
      name, levelParams := [], type, value,
      hints := .abbrev, safety := .safe }

elab "howdah_import " path:str : command => do
  let schema ← JsonSchema.read (← getFileName) path.getString
  let unit := Name.mkSimple schema.unitName
  let abstract := abstractTypes schema.interface
  let types := mkIdent (← MonadQuotation.addMacroScope `types)
  let self := mkIdent (← MonadQuotation.addMacroScope `self)
  let perOf := perOf unit types abstract
  let typeField (id : Syntax.Ident) := mkIdent (Name.mkSimple id.name)
  let field (suffix : String) (id : Syntax.Ident) :=
    mkIdent (Name.mkSimple s!"{id.name}_{suffix}")

  addDefinition (unit ++ `implementation) (mkConst ``_root_.Expr)
    schema.implementation.toPhoas?

  let typeFields ← abstract.toArray.mapM fun id =>
    `(Lean.Parser.Command.structExplicitBinder| ($(typeField id) : Per))
  elabCommand (← `(
    structure $(mkIdent (unit ++ `Types)) : Type 1 where
      $[$typeFields:structExplicitBinder]*))

  let validFields ← abstract.toArray.flatMapM fun id => do
    let some definition := schema.abstractTypeDefinitions.find? (·.id == id)
      | throwError "abstract type `{id.name}` has no definition"
    let per ← `($(mkIdent (unit ++ `Types ++ Name.mkSimple id.name)) $types)
    return #[
      ← `(Lean.Parser.Command.structExplicitBinder|
          ($(field "isPer" id) : IsPer $per)),
      ← `(Lean.Parser.Command.structExplicitBinder|
          ($(field "refines" id) :
            Per.Refines $per $(← perOf definition.definition)))]
  elabCommand (← `(
    structure $(mkIdent (unit ++ `Types ++ `Valid))
        ($types : $(mkIdent (unit ++ `Types))) : Prop where
      $[$validFields:structExplicitBinder]*))

  let fieldPers ← schema.interface.items.toArray.filterMapM fun
    | .exprDecl _ type => some <$> perOf type
    | .moduleDecl id _ => throwError "submodule `{id.name}` is not yet supported"
    | .typeDecl _ | .law _ => pure none
  elabCommand (← `(
    def $(mkIdent (unit ++ `Signature)) ($types : $(mkIdent (unit ++ `Types))) :
        Per :=
      Per.module [$fieldPers,*]))

  let lawFields ← schema.laws.toArray.mapM fun law => do
    let lawName := unit ++ `Laws ++ Name.mkSimple law.name
    let arity := law.params.length
    let clause (name : Name) (body : Syntax.Function Syntax.Lambda) := do
      addDefinition (lawName ++ name)
        (mkApp (mkConst ``Body) (toExpr arity)) body.toBody?
      let args ← (self :: law.params.map fun param =>
          mkIdent (Name.mkSimple param.id.name)).toArray.mapM fun x =>
        `(Expr.value @$x)
      `(Expr.apply
          (Expr.value (Val.closure $(quote arity) $(mkIdent (lawName ++ name))))
          [$args,*]
        ⇓ Val.true)
    let assumptions ← law.assumptions.toArray.mapIdxM fun index body =>
      clause (Name.mkSimple s!"assumption{index}") body
    let conclusion ← clause `conclusion law.conclusion
    let statement ← assumptions.foldrM (fun a c => `($a → $c)) conclusion
    let statement ← law.params.foldrM (init := statement) fun param c => do
      let x := mkIdent (Name.mkSimple param.id.name)
      `(∀ $x:ident : Val, $(← perOf param.type) @$x @$x → $c)
    `(Lean.Parser.Command.structExplicitBinder|
        ($(mkIdent (Name.mkSimple law.name)) : $statement))
  elabCommand (← `(
    structure $(mkIdent (unit ++ `Laws))
        ($types : $(mkIdent (unit ++ `Types))) ($self : Val) : Prop where
      $[$lawFields:structExplicitBinder]*))

  let module := mkIdent (← MonadQuotation.addMacroScope `module)
  elabCommand (← `(
    structure $(mkIdent (unit ++ `Verified)) : Type 1 where
      (types : $(mkIdent (unit ++ `Types)))
      (valid : $(mkIdent (unit ++ `Types ++ `Valid)) types)
      (implementation : ∃ $module:ident : Val,
        $(mkIdent (unit ++ `implementation)) ⇓ @$module ∧
        $(mkIdent (unit ++ `Signature)) types @$module @$module ∧
        $(mkIdent (unit ++ `Laws)) types @$module)))

private def standardAxioms : List Name :=
  [``propext, ``Quot.sound, ``Classical.choice]

elab "#howdah_check " name:ident : command => do
  let name ← liftCoreM <| realizeGlobalConstNoOverloadWithInfo name
  let axioms ← liftCoreM <| collectAxioms name
  let extra := axioms.filter (!standardAxioms.contains ·)
  unless extra.isEmpty do
    throwError "`{name}` depends on nonstandard axioms: {extra}"
  logInfo m!"`{name}` depends only on standard axioms"
