module Location_key = struct
  type position_key =
    { line : int;
      column : int
    }

  type t =
    { file : string;
      start_position : position_key;
      end_position : position_key
    }

  let of_position (position : Lexing.position) =
    { line = position.pos_lnum; column = position.pos_cnum - position.pos_bol }

  let of_location (loc : Location.t) =
    { file = loc.loc_start.pos_fname;
      start_position = of_position loc.loc_start;
      end_position = of_position loc.loc_end
    }

  let equal_position p1 p2 =
    Int.equal p1.line p2.line && Int.equal p1.column p2.column

  let equal t1 t2 =
    String.equal t1.file t2.file
    && equal_position t1.start_position t2.start_position
    && equal_position t1.end_position t2.end_position
end

module Symbol_table (Item : sig
  type t

  val equal : t -> t -> bool
end) : sig
  module Id : sig
    type t

    val to_int : t -> int
  end

  type item = Item.t

  type t

  val empty : t

  val intern : t -> item -> t * Id.t

  val to_list : t -> (Id.t * item) list
end = struct
  module Id = struct
    type t = int

    let to_int t = t
  end

  type item = Item.t

  type t =
    { newest_first : item list;
      minted : int
    }

  let empty = { newest_first = []; minted = 0 }

  let intern t item =
    let rec minted_as from_newest = function
      | [] -> None
      | candidate :: older ->
        if Item.equal candidate item then Some (t.minted - 1 - from_newest)
        else minted_as (from_newest + 1) older
    in
    match minted_as 0 t.newest_first with
    | Some id -> t, id
    | None ->
      { newest_first = item :: t.newest_first; minted = t.minted + 1 }, t.minted

  let to_list t = List.mapi (fun id item -> id, item) (List.rev t.newest_first)
end

module Glossary_entry = struct
  type t =
    { term : string;
      category : string;
      description : string;
      url : string option
    }

  let equal t1 t2 =
    String.equal t1.term t2.term
    && String.equal t1.category t2.category
    && String.equal t1.description t2.description
    && Option.equal String.equal t1.url t2.url
end

module Form = struct
  type t =
    | Name
    | Pronoun
end

module Kind = struct
  type t =
    | Explanation
    | Background
    | Suggestion
end

module Relation = struct
  type t =
    | Claim
    | Elaboration
end

module Annotation = struct
  type t =
    | Code
    | Source of Location.t
    | Mention of
        { entity : Location.t;
          form : Form.t
        }
    | Term of Glossary_entry.t
end

module Inline = struct
  type t =
    | Text of string
    | Annotated of
        { annotation : Annotation.t;
          content : t list
        }
end

module Block = struct
  type t =
    { kind : Kind.t;
      content : Inline.t list;
      children : (Relation.t * t) list
    }

  let equal_location left right =
    Location_key.equal
      (Location_key.of_location left)
      (Location_key.of_location right)

  let equal_form (left : Form.t) (right : Form.t) =
    match left, right with
    | Name, Name | Pronoun, Pronoun -> true
    | Name, Pronoun | Pronoun, Name -> false

  let equal_kind (left : Kind.t) (right : Kind.t) =
    match left, right with
    | Explanation, Explanation | Background, Background
    | Suggestion, Suggestion ->
      true
    | (Explanation | Background | Suggestion), _ -> false

  let equal_relation (left : Relation.t) (right : Relation.t) =
    match left, right with
    | Claim, Claim | Elaboration, Elaboration -> true
    | (Claim | Elaboration), _ -> false

  let equal_annotation (left : Annotation.t) (right : Annotation.t) =
    match left, right with
    | Code, Code -> true
    | Source left, Source right -> equal_location left right
    | Mention left, Mention right ->
      equal_location left.entity right.entity && equal_form left.form right.form
    | Term left, Term right -> Glossary_entry.equal left right
    | (Code | Source _ | Mention _ | Term _), _ -> false

  let rec equal_inline (left : Inline.t) (right : Inline.t) =
    match left, right with
    | Text left, Text right -> String.equal left right
    | Annotated left, Annotated right ->
      equal_annotation left.annotation right.annotation
      && equal_inlines left.content right.content
    | (Text _ | Annotated _), _ -> false

  and equal_inlines left right = List.equal equal_inline left right

  let rec equal left right =
    equal_kind left.kind right.kind
    && equal_inlines left.content right.content
    && equal_children left.children right.children

  and equal_children left right =
    List.equal
      (fun (left_relation, left) (right_relation, right) ->
        equal_relation left_relation right_relation && equal left right)
      left right
end

type t =
  { loc : Location.t;
    body : Block.t list
  }

module Entities = Symbol_table (struct
  type t = Location.t

  let equal loc1 loc2 =
    Location_key.equal
      (Location_key.of_location loc1)
      (Location_key.of_location loc2)
end)

module Glossary = Symbol_table (Glossary_entry)

module Wire = struct
  module Annotation = struct
    type t =
      | Code
      | Source of Location.t
      | Mention of
          { entity : Entities.Id.t;
            form : Form.t
          }
      | Term of Glossary.Id.t
  end

  module Inline = struct
    type t =
      | Text of string
      | Annotated of
          { annotation : Annotation.t;
            content : t list
          }
  end

  module Block = struct
    type t =
      { kind : Kind.t;
        content : Inline.t list;
        children : (Relation.t * t) list
      }
  end
end

type tables =
  { entities : Entities.t;
    glossary : Glossary.t
  }

let rec intern_inline tables (inline : Inline.t) : tables * Wire.Inline.t =
  match inline with
  | Text text -> tables, Wire.Inline.Text text
  | Annotated { annotation; content } ->
    let tables, annotation = intern_annotation tables annotation in
    let tables, content = List.fold_left_map intern_inline tables content in
    tables, Wire.Inline.Annotated { annotation; content }

and intern_annotation tables (annotation : Annotation.t) =
  match annotation with
  | Code -> tables, Wire.Annotation.Code
  | Source loc -> tables, Wire.Annotation.Source loc
  | Mention { entity; form } ->
    let entities, entity = Entities.intern tables.entities entity in
    { tables with entities }, Wire.Annotation.Mention { entity; form }
  | Term entry ->
    let glossary, term = Glossary.intern tables.glossary entry in
    { tables with glossary }, Wire.Annotation.Term term

let rec intern_block tables (block : Block.t) : tables * Wire.Block.t =
  let tables, content = List.fold_left_map intern_inline tables block.content in
  let tables, children =
    List.fold_left_map
      (fun tables (relation, child) ->
        let tables, child = intern_block tables child in
        tables, (relation, child))
      tables block.children
  in
  tables, { Wire.Block.kind = block.kind; content; children }

let intern_body body =
  let initial = { entities = Entities.empty; glossary = Glossary.empty } in
  List.fold_left_map intern_block initial body

module Json = struct
  let form_to_string (form : Form.t) =
    match form with Name -> "name" | Pronoun -> "pronoun"

  let kind_to_string (kind : Kind.t) =
    match kind with
    | Explanation -> "explanation"
    | Background -> "background"
    | Suggestion -> "suggestion"

  let relation_to_string (relation : Relation.t) =
    match relation with Claim -> "claim" | Elaboration -> "elaboration"

  let to_value ~string ~int ~array ~object_ (diagnostic : t) =
    let kind_field kind = "kind", string kind in
    let position_to_value (position : Lexing.position) =
      object_
        ["line", int position.pos_lnum;
         "col", int (position.pos_cnum - position.pos_bol)]
    in
    let location_to_value (loc : Location.t) =
      object_
        ["file", string loc.loc_start.pos_fname;
         "start", position_to_value loc.loc_start;
         "end", position_to_value loc.loc_end]
    in
    let annotation_to_value (annotation : Wire.Annotation.t) =
      match annotation with
      | Code -> object_ [kind_field "code"]
      | Source loc ->
        object_ [kind_field "source"; "loc", location_to_value loc]
      | Mention { entity; form } ->
        object_
          [kind_field "mention";
           "entity", int (Entities.Id.to_int entity);
           "form", string (form_to_string form)]
      | Term term ->
        object_ [kind_field "term"; "term", int (Glossary.Id.to_int term)]
    in
    let rec inline_to_value (inline : Wire.Inline.t) =
      match inline with
      | Text text -> object_ [kind_field "text"; "text", string text]
      | Annotated { annotation; content } ->
        object_
          [kind_field "annotated";
           "annotation", annotation_to_value annotation;
           "content", inlines_to_value content]
    and inlines_to_value content = array (List.map inline_to_value content) in
    let rec block_to_value (block : Wire.Block.t) =
      object_
        [kind_field (kind_to_string block.kind);
         "content", inlines_to_value block.content;
         "children", array (List.map child_to_value block.children)]
    and child_to_value ((relation, block) : Relation.t * Wire.Block.t) =
      object_
        ["relation", string (relation_to_string relation);
         "block", block_to_value block]
    in
    let entity_to_value ((id, loc) : Entities.Id.t * Location.t) =
      object_ ["id", int (Entities.Id.to_int id); "loc", location_to_value loc]
    in
    let glossary_entry_to_value
        ((id, entry) : Glossary.Id.t * Glossary_entry.t) =
      let url =
        match entry.url with
        | None -> []
        | Some url -> ["url", string url]
      in
      object_
        (["id", int (Glossary.Id.to_int id);
          "term", string entry.term;
          "category", string entry.category;
          "description", string entry.description]
        @ url)
    in
    let tables, body = intern_body diagnostic.body in
    object_
      ["loc", location_to_value diagnostic.loc;
       "entities",
       array (List.map entity_to_value (Entities.to_list tables.entities));
       "glossary",
       array
         (List.map glossary_entry_to_value (Glossary.to_list tables.glossary));
       "body", array (List.map block_to_value body)]

  let from_value ~string ~int ~array ~field ~optional_field value =
    let position_of_value ~file json =
      let line = int (field "line" json) in
      let column = int (field "col" json) in
      { Lexing.pos_fname = file;
        pos_lnum = line;
        pos_bol = 0;
        pos_cnum = column
      }
    in
    let location_of_value json =
      let file = string (field "file" json) in
      { Location.loc_start = position_of_value ~file (field "start" json);
        loc_end = position_of_value ~file (field "end" json);
        loc_ghost = false
      }
    in
    let resolve items kind json =
      let serialized = int json in
      match List.assoc_opt serialized items with
      | Some item -> item
      | None -> Json.malformed "unknown %s id %d" kind serialized
    in
    let items_in_serialized_id_order kind item_of_value json =
      List.mapi
        (fun expected_id json ->
          let serialized_id = int (field "id" json) in
          if not (Int.equal serialized_id expected_id) then
            Json.malformed "invalid %s id %d" kind serialized_id;
          (expected_id, item_of_value json))
        (array json)
    in
    let entities_of_value json =
      items_in_serialized_id_order "entity"
        (fun json -> location_of_value (field "loc" json))
        json
    in
    let glossary_of_value json =
      items_in_serialized_id_order "glossary"
        (fun json ->
          let entry : Glossary_entry.t =
            { term = string (field "term" json);
              category = string (field "category" json);
              description = string (field "description" json);
              url = Option.map string (optional_field "url" json)
            }
          in
          entry)
        json
    in
    let form_of_value json =
      match string json with
      | "name" -> Form.Name
      | "pronoun" -> Form.Pronoun
      | form -> Json.malformed "unknown mention form %S" form
    in
    let annotation_of_value ~entities ~glossary json =
      match string (field "kind" json) with
      | "code" -> Annotation.Code
      | "source" -> Annotation.Source (location_of_value (field "loc" json))
      | "mention" ->
        Annotation.Mention
          { entity = resolve entities "entity" (field "entity" json);
            form = form_of_value (field "form" json)
          }
      | "term" ->
        Annotation.Term (resolve glossary "glossary" (field "term" json))
      | kind -> Json.malformed "unknown annotation kind %S" kind
    in
    let rec inline_of_value ~entities ~glossary json =
      match string (field "kind" json) with
      | "text" -> Inline.Text (string (field "text" json))
      | "annotated" ->
        Inline.Annotated
          { annotation =
              annotation_of_value ~entities ~glossary
                (field "annotation" json);
            content =
              List.map (inline_of_value ~entities ~glossary)
                (array (field "content" json))
          }
      | kind -> Json.malformed "unknown inline kind %S" kind
    in
    let kind_of_value json =
      match string json with
      | "explanation" -> Kind.Explanation
      | "background" -> Kind.Background
      | "suggestion" -> Kind.Suggestion
      | kind -> Json.malformed "unknown block kind %S" kind
    in
    let relation_of_value json =
      match string json with
      | "claim" -> Relation.Claim
      | "elaboration" -> Relation.Elaboration
      | relation -> Json.malformed "unknown block relation %S" relation
    in
    let rec block_of_value ~entities ~glossary json : Block.t =
      { kind = kind_of_value (field "kind" json);
        content =
          List.map (inline_of_value ~entities ~glossary)
            (array (field "content" json));
        children =
          List.map (child_of_value ~entities ~glossary)
            (array (field "children" json))
      }
    and child_of_value ~entities ~glossary json =
      relation_of_value (field "relation" json),
      block_of_value ~entities ~glossary (field "block" json)
    in
    let loc = location_of_value (field "loc" value) in
    let entities = entities_of_value (field "entities" value) in
    let glossary = glossary_of_value (field "glossary" value) in
    let body =
      List.map (block_of_value ~entities ~glossary)
        (array (field "body" value))
    in
    { loc; body }

  let of_json text =
    match
      from_value ~string:Json.string ~int:Json.int ~array:Json.array
        ~field:Json.field ~optional_field:Json.optional_field
        (Json.parse text)
    with
    | diagnostic -> Ok diagnostic
    | exception Json.Malformed message -> Error message
    | exception Invalid_argument message -> Error message

  let to_json diagnostic =
    Json.to_string
      (to_value
         ~string:(fun value -> Json.String value)
         ~int:(fun value -> Json.Number (string_of_int value))
         ~array:(fun items -> Json.Array items)
         ~object_:(fun fields -> Json.Object fields)
         diagnostic)
end

let to_json = Json.to_json

let of_json = Json.of_json
