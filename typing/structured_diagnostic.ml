module Location_key = struct
  type t =
    { file : string;
      start_offset : int;
      end_offset : int
    }

  let of_location (loc : Location.t) =
    { file = loc.loc_start.pos_fname;
      start_offset = loc.loc_start.pos_cnum;
      end_offset = loc.loc_end.pos_cnum
    }

  let equal t1 t2 =
    String.equal t1.file t2.file
    && Int.equal t1.start_offset t2.start_offset
    && Int.equal t1.end_offset t2.end_offset
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

  val find : t -> Id.t -> item option

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

  let find t id =
    let from_newest = t.minted - 1 - Id.to_int id in
    if from_newest < 0 then None else List.nth_opt t.newest_first from_newest

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

module Entities = Symbol_table (struct
  type t = Location.t

  let equal loc1 loc2 =
    Location_key.equal
      (Location_key.of_location loc1)
      (Location_key.of_location loc2)
end)

module Glossary = struct
  module Entry = struct
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

  include Symbol_table (Entry)
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

type t =
  { loc : Location.t;
    title : string;
    entities : Entities.t;
    glossary : Glossary.t;
    body : Block.t list
  }

let dedup_by_key locs =
  let rec loop seen = function
    | [] -> []
    | loc :: rest ->
      let key = Location_key.of_location loc in
      if List.exists (Location_key.equal key) seen then loop seen rest
      else loc :: loop (key :: seen) rest
  in
  loop [] locs

let locations t content =
  let rec collect (inline : Inline.t) =
    match inline with
    | Text _ -> []
    | Annotated { annotation; content } ->
      let here =
        match annotation with
        | Annotation.Code | Annotation.Term _ -> []
        | Annotation.Source loc -> [ loc ]
        | Annotation.Mention { entity; form = _ } ->
          Option.to_list (Entities.find t.entities entity)
      in
      here @ List.concat_map collect content
  in
  dedup_by_key (List.concat_map collect content)

type diagnostic = t

module Diagnostic = struct
  type t = diagnostic

  module Entities = Entities
  module Glossary = Glossary
  module Form = Form
  module Kind = Kind
  module Relation = Relation
  module Annotation = Annotation
  module Inline = Inline
  module Block = Block
end

let string_to_json value =
  let escaped = Buffer.create (String.length value + 2) in
  let width = String.length value in
  let rec add index =
    if index < width then
      match String.get value index with
      | '"' ->
          Buffer.add_string escaped "\\\"";
          add (index + 1)
      | '\\' ->
          Buffer.add_string escaped "\\\\";
          add (index + 1)
      | '\b' ->
          Buffer.add_string escaped "\\b";
          add (index + 1)
      | '\012' ->
          Buffer.add_string escaped "\\f";
          add (index + 1)
      | '\n' ->
          Buffer.add_string escaped "\\n";
          add (index + 1)
      | '\r' ->
          Buffer.add_string escaped "\\r";
          add (index + 1)
      | '\t' ->
          Buffer.add_string escaped "\\t";
          add (index + 1)
      | '\000' .. '\031' as control ->
          Buffer.add_string escaped
            (Printf.sprintf "\\u%04x" (Char.code control));
          add (index + 1)
      | ' ' .. '\127' as ascii ->
          Buffer.add_char escaped ascii;
          add (index + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar value index in
          let bytes = Uchar.utf_decode_length decoded in
          if Uchar.utf_decode_is_valid decoded then
            Buffer.add_substring escaped value index bytes
          else Buffer.add_utf_8_uchar escaped Uchar.rep;
          add (index + bytes)
  in
  Buffer.add_char escaped '"';
  add 0;
  Buffer.add_char escaped '"';
  Buffer.contents escaped

let kind_field kind = Misc.Json.field "kind" (string_to_json kind)

let position_to_json (position : Lexing.position) =
  Misc.Json.object_
    [
      Misc.Json.field "line" (Misc.Json.int position.pos_lnum);
      Misc.Json.field "col"
        (Misc.Json.int (position.pos_cnum - position.pos_bol));
    ]

let location_to_json (loc : Location.t) =
  Misc.Json.object_
    [
      Misc.Json.field "file" (string_to_json loc.loc_start.pos_fname);
      Misc.Json.field "start" (position_to_json loc.loc_start);
      Misc.Json.field "end" (position_to_json loc.loc_end);
    ]

let form_to_string (form : Diagnostic.Form.t) =
  match form with Name -> "name" | Pronoun -> "pronoun"

let kind_to_string (kind : Diagnostic.Kind.t) =
  match kind with
  | Explanation -> "explanation"
  | Background -> "background"
  | Suggestion -> "suggestion"

let relation_to_string (relation : Diagnostic.Relation.t) =
  match relation with Claim -> "claim" | Elaboration -> "elaboration"

let annotation_to_json (annotation : Diagnostic.Annotation.t) =
  match annotation with
  | Code -> Misc.Json.object_ [ kind_field "code" ]
  | Source loc ->
      Misc.Json.object_
        [ kind_field "source"; Misc.Json.field "loc" (location_to_json loc) ]
  | Mention { entity; form } ->
      Misc.Json.object_
        [
          kind_field "mention";
          Misc.Json.field "entity"
            (Misc.Json.int (Diagnostic.Entities.Id.to_int entity));
          Misc.Json.field "form" (string_to_json (form_to_string form));
        ]
  | Term term ->
      Misc.Json.object_
        [
          kind_field "term";
          Misc.Json.field "term"
            (Misc.Json.int (Diagnostic.Glossary.Id.to_int term));
        ]

let rec inline_to_json (inline : Diagnostic.Inline.t) =
  match inline with
  | Text text ->
      Misc.Json.object_
        [ kind_field "text"; Misc.Json.field "text" (string_to_json text) ]
  | Annotated { annotation; content } ->
      Misc.Json.object_
        [
          kind_field "annotated";
          Misc.Json.field "annotation" (annotation_to_json annotation);
          Misc.Json.field "content" (inlines_to_json content);
        ]

and inlines_to_json content = Misc.Json.array (List.map inline_to_json content)

let rec block_to_json (block : Diagnostic.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "kind" (string_to_json (kind_to_string block.kind));
      Misc.Json.field "content" (inlines_to_json block.content);
      Misc.Json.field "children"
        (Misc.Json.array (List.map child_to_json block.children));
    ]

and child_to_json
    ((relation, block) : Diagnostic.Relation.t * Diagnostic.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "relation" (string_to_json (relation_to_string relation));
      Misc.Json.field "block" (block_to_json block);
    ]

let entity_to_json ((id, loc) : Diagnostic.Entities.Id.t * Location.t) =
  Misc.Json.object_
    [
      Misc.Json.field "id" (Misc.Json.int (Diagnostic.Entities.Id.to_int id));
      Misc.Json.field "loc" (location_to_json loc);
    ]

let glossary_entry_to_json
    ((id, entry) : Diagnostic.Glossary.Id.t * Diagnostic.Glossary.Entry.t) =
  let url =
    match entry.url with
    | None -> []
    | Some url -> [ Misc.Json.field "url" (string_to_json url) ]
  in
  Misc.Json.object_
    ([
       Misc.Json.field "id" (Misc.Json.int (Diagnostic.Glossary.Id.to_int id));
       Misc.Json.field "term" (string_to_json entry.term);
       Misc.Json.field "category" (string_to_json entry.category);
       Misc.Json.field "description" (string_to_json entry.description);
     ]
    @ url)

let diagnostic_to_json (diagnostic : Diagnostic.t) =
  Misc.Json.object_
    [
      Misc.Json.field "loc" (location_to_json diagnostic.loc);
      Misc.Json.field "entities"
        (Misc.Json.array
           (List.map entity_to_json
              (Diagnostic.Entities.to_list diagnostic.entities)));
      Misc.Json.field "glossary"
        (Misc.Json.array
           (List.map glossary_entry_to_json
              (Diagnostic.Glossary.to_list diagnostic.glossary)));
      Misc.Json.field "body"
        (Misc.Json.array (List.map block_to_json diagnostic.body));
    ]

let to_json diagnostic =
  String.concat "" (String.split_on_char '\n' (diagnostic_to_json diagnostic))
