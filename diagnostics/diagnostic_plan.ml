module Diagnostic = Structured_diagnostic

module Annotation = struct
  type t =
    | Code
    | Source of Location.t
    | Mention of
        { entity : Location.t;
          form : Diagnostic.Form.t
        }
    | Term of Diagnostic.Glossary.Entry.t
end

module Inline = struct
  type t =
    | Text of string
    | Annotated of
        { annotation : Annotation.t;
          content : t list
        }
end

type t =
  { kind : Diagnostic.Kind.t;
    content : Inline.t list;
    children : (Diagnostic.Relation.t * t) list
  }

let create ~kind ~content ~children = { kind; content; children }

let children t = t.children

let with_children t children = { t with children }

let equal_form (left : Diagnostic.Form.t) (right : Diagnostic.Form.t) =
  match left, right with
  | Name, Name | Pronoun, Pronoun -> true
  | Name, Pronoun | Pronoun, Name -> false

let equal_kind (left : Diagnostic.Kind.t) (right : Diagnostic.Kind.t) =
  match left, right with
  | Explanation, Explanation | Background, Background
  | Suggestion, Suggestion ->
    true
  | (Explanation | Background | Suggestion), _ -> false

let equal_relation (left : Diagnostic.Relation.t)
    (right : Diagnostic.Relation.t) =
  match left, right with
  | Claim, Claim | Elaboration, Elaboration -> true
  | (Claim | Elaboration), _ -> false

let equal_glossary_entry (left : Diagnostic.Glossary.Entry.t)
    (right : Diagnostic.Glossary.Entry.t) =
  String.equal left.term right.term
  && String.equal left.category right.category
  && String.equal left.description right.description
  && Option.equal String.equal left.url right.url

let equal_location left right =
  Diagnostic.Location_key.equal
    (Diagnostic.Location_key.of_location left)
    (Diagnostic.Location_key.of_location right)

let equal_annotation (left : Annotation.t) (right : Annotation.t) =
  match left, right with
  | Code, Code -> true
  | Source left, Source right -> equal_location left right
  | Mention left, Mention right ->
    equal_location left.entity right.entity && equal_form left.form right.form
  | Term left, Term right -> equal_glossary_entry left right
  | (Code | Source _ | Mention _ | Term _), _ -> false

let rec equal_inline (left : Inline.t) (right : Inline.t) =
  match left, right with
  | Text left, Text right -> String.equal left right
  | Annotated left, Annotated right ->
    equal_annotation left.annotation right.annotation
    && equal_inlines left.content right.content
  | (Text _ | Annotated _), _ -> false

and equal_inlines left right =
  List.length left = List.length right
  && List.for_all2 equal_inline left right

let rec equal left right =
  equal_kind left.kind right.kind
  && equal_inlines left.content right.content
  && equal_children left.children right.children

and equal_children left right =
  List.length left = List.length right
  && List.for_all2
       (fun (left_relation, left) (right_relation, right) ->
         equal_relation left_relation right_relation && equal left right)
       left right

type tables =
  { entities : Diagnostic.Entities.t;
    glossary : Diagnostic.Glossary.t
  }

let rec finalize_inline tables (inline : Inline.t) =
  match inline with
  | Text text -> tables, Diagnostic.Inline.Text text
  | Annotated { annotation; content } ->
    let tables, annotation = finalize_annotation tables annotation in
    let tables, content = List.fold_left_map finalize_inline tables content in
    ( tables,
      Diagnostic.Inline.Annotated { annotation; content } )

and finalize_annotation tables (annotation : Annotation.t) =
  match annotation with
  | Code -> tables, Diagnostic.Annotation.Code
  | Source loc -> tables, Diagnostic.Annotation.Source loc
  | Mention { entity; form } ->
    let entities, entity = Diagnostic.Entities.intern tables.entities entity in
    ( { tables with entities },
      Diagnostic.Annotation.Mention { entity; form } )
  | Term entry ->
    let glossary, term = Diagnostic.Glossary.intern tables.glossary entry in
    { tables with glossary }, Diagnostic.Annotation.Term term

let rec finalize tables t =
  let tables, content = List.fold_left_map finalize_inline tables t.content in
  let tables, children =
    List.fold_left_map
      (fun tables (relation, child) ->
        let tables, child = finalize tables child in
        tables, (relation, child))
      tables t.children
  in
  tables, { Diagnostic.Block.kind = t.kind; content; children }

let to_diagnostic ~loc plans =
  let initial =
    { entities = Diagnostic.Entities.empty;
      glossary = Diagnostic.Glossary.empty
    }
  in
  let tables, body = List.fold_left_map finalize initial plans in
  { Diagnostic.loc;
    entities = tables.entities;
    glossary = tables.glossary;
    body
  }
