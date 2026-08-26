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

let form_to_string (form : Form.t) =
  match form with Name -> "name" | Pronoun -> "pronoun"

let kind_to_string (kind : Kind.t) =
  match kind with
  | Explanation -> "explanation"
  | Background -> "background"
  | Suggestion -> "suggestion"

let relation_to_string (relation : Relation.t) =
  match relation with Claim -> "claim" | Elaboration -> "elaboration"

let annotation_to_json (annotation : Wire.Annotation.t) =
  match annotation with
  | Code -> Misc.Json.object_ [ kind_field "code" ]
  | Source loc ->
      Misc.Json.object_
        [ kind_field "source"; Misc.Json.field "loc" (location_to_json loc) ]
  | Mention { entity; form } ->
      Misc.Json.object_
        [
          kind_field "mention";
          Misc.Json.field "entity" (Misc.Json.int (Entities.Id.to_int entity));
          Misc.Json.field "form" (string_to_json (form_to_string form));
        ]
  | Term term ->
      Misc.Json.object_
        [
          kind_field "term";
          Misc.Json.field "term" (Misc.Json.int (Glossary.Id.to_int term));
        ]

let rec inline_to_json (inline : Wire.Inline.t) =
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

let rec block_to_json (block : Wire.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "kind" (string_to_json (kind_to_string block.kind));
      Misc.Json.field "content" (inlines_to_json block.content);
      Misc.Json.field "children"
        (Misc.Json.array (List.map child_to_json block.children));
    ]

and child_to_json ((relation, block) : Relation.t * Wire.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "relation" (string_to_json (relation_to_string relation));
      Misc.Json.field "block" (block_to_json block);
    ]

let entity_to_json ((id, loc) : Entities.Id.t * Location.t) =
  Misc.Json.object_
    [
      Misc.Json.field "id" (Misc.Json.int (Entities.Id.to_int id));
      Misc.Json.field "loc" (location_to_json loc);
    ]

let glossary_entry_to_json ((id, entry) : Glossary.Id.t * Glossary_entry.t) =
  let url =
    match entry.url with
    | None -> []
    | Some url -> [ Misc.Json.field "url" (string_to_json url) ]
  in
  Misc.Json.object_
    ([
       Misc.Json.field "id" (Misc.Json.int (Glossary.Id.to_int id));
       Misc.Json.field "term" (string_to_json entry.term);
       Misc.Json.field "category" (string_to_json entry.category);
       Misc.Json.field "description" (string_to_json entry.description);
     ]
    @ url)

let diagnostic_to_json (diagnostic : t) =
  let tables, body = intern_body diagnostic.body in
  Misc.Json.object_
    [
      Misc.Json.field "loc" (location_to_json diagnostic.loc);
      Misc.Json.field "entities"
        (Misc.Json.array
           (List.map entity_to_json (Entities.to_list tables.entities)));
      Misc.Json.field "glossary"
        (Misc.Json.array
           (List.map glossary_entry_to_json
              (Glossary.to_list tables.glossary)));
      Misc.Json.field "body"
        (Misc.Json.array (List.map block_to_json body));
    ]

module Json = struct
  exception Malformed of string

  type t =
    | Null
    | Bool of bool
    | Number of string
    | String of string
    | Array of t list
    | Object of (string * t) list

  let parse text =
    let length = String.length text in
    let position = ref 0 in
    let fail message =
      raise
        (Malformed
           (Printf.sprintf "at byte %d: %s" !position message))
    in
    let peek () =
      if !position < length then Some text.[!position] else None
    in
    let peek_ahead offset =
      let position = !position + offset in
      if position < length then Some text.[position] else None
    in
    let advance () = incr position in
    let rec skip_whitespace () =
      match peek () with
      | Some (' ' | '\t' | '\r' | '\n') ->
        advance ();
        skip_whitespace ()
      | Some _ | None -> ()
    in
    let expect character =
      match peek () with
      | Some found when Char.equal found character -> advance ()
      | Some found ->
        fail (Printf.sprintf "expected %c, found %c" character found)
      | None ->
        fail (Printf.sprintf "expected %c, found end of input" character)
    in
    let hexadecimal character =
      match character with
      | '0' .. '9' -> Char.code character - Char.code '0'
      | 'a' .. 'f' -> Char.code character - Char.code 'a' + 10
      | 'A' .. 'F' -> Char.code character - Char.code 'A' + 10
      | _ -> fail "expected a hexadecimal digit"
    in
    let code_unit () =
      let code = ref 0 in
      for _ = 1 to 4 do
        match peek () with
        | None -> fail "unterminated escape"
        | Some digit ->
          code := (!code * 16) + hexadecimal digit;
          advance ()
      done;
      !code
    in
    let is_leading_surrogate code = code >= 0xd800 && code <= 0xdbff in
    let is_trailing_surrogate code = code >= 0xdc00 && code <= 0xdfff in
    let scalar_value () =
      let code = code_unit () in
      if is_trailing_surrogate code then fail "lone trailing surrogate";
      if not (is_leading_surrogate code) then code
      else begin
        (match peek (), peek_ahead 1 with
        | Some '\\', Some 'u' -> position := !position + 2
        | _ -> fail "lone leading surrogate");
        let trailing = code_unit () in
        if not (is_trailing_surrogate trailing) then
          fail "expected a trailing surrogate";
        0x10000 + ((code - 0xd800) * 0x400) + (trailing - 0xdc00)
      end
    in
    let parse_string () =
      expect '"';
      let buffer = Buffer.create 32 in
      let rec loop () =
        match peek () with
        | None -> fail "unterminated string"
        | Some ('\000' .. '\031') ->
          fail "unescaped control character in string"
        | Some '"' ->
          advance ();
          Buffer.contents buffer
        | Some '\\' ->
          advance ();
          (match peek () with
          | None -> fail "unterminated escape"
          | Some 'u' ->
            advance ();
            Buffer.add_utf_8_uchar buffer (Uchar.of_int (scalar_value ()))
          | Some escaped ->
            advance ();
            Buffer.add_char buffer
              (match escaped with
              | 'n' -> '\n'
              | 't' -> '\t'
              | 'r' -> '\r'
              | 'b' -> '\b'
              | 'f' -> '\012'
              | '"' -> '"'
              | '\\' -> '\\'
              | '/' -> '/'
              | _ -> fail "unknown escape"));
          loop ()
        | Some character ->
          advance ();
          Buffer.add_char buffer character;
          loop ()
      in
      loop ()
    in
    let parse_literal spelling value =
      let width = String.length spelling in
      let stop = !position + width in
      if stop > length
         || not (String.equal (String.sub text !position width) spelling)
      then fail (Printf.sprintf "expected %s" spelling);
      position := stop;
      value
    in
    let parse_number () =
      let start = !position in
      let rec loop () =
        match peek () with
        | Some ('-' | '+' | '.' | 'e' | 'E' | '0' .. '9') ->
          advance ();
          loop ()
        | Some _ | None -> ()
      in
      loop ();
      if Int.equal !position start then fail "expected a number";
      Number (String.sub text start (!position - start))
    in
    let rec parse_value () =
      skip_whitespace ();
      match peek () with
      | None -> fail "expected a value"
      | Some '"' -> String (parse_string ())
      | Some '{' -> parse_object ()
      | Some '[' -> parse_array ()
      | Some 't' -> parse_literal "true" (Bool true)
      | Some 'f' -> parse_literal "false" (Bool false)
      | Some 'n' -> parse_literal "null" Null
      | Some _ -> parse_number ()
    and parse_object () =
      expect '{';
      skip_whitespace ();
      match peek () with
      | Some '}' ->
        advance ();
        Object []
      | Some _ | None ->
        let rec loop fields =
          skip_whitespace ();
          let name = parse_string () in
          skip_whitespace ();
          expect ':';
          let value = parse_value () in
          let fields = (name, value) :: fields in
          skip_whitespace ();
          match peek () with
          | Some ',' ->
            advance ();
            loop fields
          | Some '}' ->
            advance ();
            Object (List.rev fields)
          | Some _ | None -> fail "expected , or } in object"
        in
        loop []
    and parse_array () =
      expect '[';
      skip_whitespace ();
      match peek () with
      | Some ']' ->
        advance ();
        Array []
      | Some _ | None ->
        let rec loop items =
          let items = parse_value () :: items in
          skip_whitespace ();
          match peek () with
          | Some ',' ->
            advance ();
            loop items
          | Some ']' ->
            advance ();
            Array (List.rev items)
          | Some _ | None -> fail "expected , or ] in array"
        in
        loop []
    in
    let value = parse_value () in
    skip_whitespace ();
    if not (Int.equal !position length) then fail "unexpected trailing input";
    value
end

let malformed format =
  Printf.ksprintf (fun message -> raise (Json.Malformed message)) format

let object_fields = function
  | Json.Object fields -> fields
  | _ -> malformed "expected an object"

let field name json =
  match List.assoc_opt name (object_fields json) with
  | Some value -> value
  | None -> malformed "missing field %S" name

let optional_field name json = List.assoc_opt name (object_fields json)

let string = function
  | Json.String string -> string
  | _ -> malformed "expected a string"

let int = function
  | Json.Number number -> (match int_of_string_opt number with
    | Some int -> int
    | None -> malformed "expected an integer, found %S" number)
  | _ -> malformed "expected an integer"

let array = function
  | Json.Array values -> values
  | _ -> malformed "expected an array"

let position_of_json ~file json =
  let line = int (field "line" json) in
  let column = int (field "col" json) in
  { Lexing.pos_fname = file;
    pos_lnum = line;
    pos_bol = 0;
    pos_cnum = column
  }

let location_of_json json =
  let file = string (field "file" json) in
  { Location.loc_start = position_of_json ~file (field "start" json);
    loc_end = position_of_json ~file (field "end" json);
    loc_ghost = false
  }

let resolve items kind json =
  let serialized = int json in
  match List.assoc_opt serialized items with
  | Some item -> item
  | None -> malformed "unknown %s id %d" kind serialized

let items_in_serialized_id_order kind item_of_json json =
  List.mapi
    (fun expected_id json ->
      let serialized_id = int (field "id" json) in
      if not (Int.equal serialized_id expected_id) then
        malformed "invalid %s id %d" kind serialized_id;
      (expected_id, item_of_json json))
    (array json)

let entities_of_json json =
  items_in_serialized_id_order "entity"
    (fun json -> location_of_json (field "loc" json))
    json

let glossary_of_json json =
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

let form_of_json json =
  match string json with
  | "name" -> Form.Name
  | "pronoun" -> Form.Pronoun
  | form -> malformed "unknown mention form %S" form

let annotation_of_json ~entities ~glossary json =
  match string (field "kind" json) with
  | "code" -> Annotation.Code
  | "source" -> Annotation.Source (location_of_json (field "loc" json))
  | "mention" ->
    Annotation.Mention
      { entity = resolve entities "entity" (field "entity" json);
        form = form_of_json (field "form" json)
      }
  | "term" -> Annotation.Term (resolve glossary "glossary" (field "term" json))
  | kind -> malformed "unknown annotation kind %S" kind

let rec inline_of_json ~entities ~glossary json =
  match string (field "kind" json) with
  | "text" -> Inline.Text (string (field "text" json))
  | "annotated" ->
    Inline.Annotated
      { annotation =
          annotation_of_json ~entities ~glossary (field "annotation" json);
        content =
          List.map (inline_of_json ~entities ~glossary)
            (array (field "content" json))
      }
  | kind -> malformed "unknown inline kind %S" kind

let kind_of_json json =
  match string json with
  | "explanation" -> Kind.Explanation
  | "background" -> Kind.Background
  | "suggestion" -> Kind.Suggestion
  | kind -> malformed "unknown block kind %S" kind

let relation_of_json json =
  match string json with
  | "claim" -> Relation.Claim
  | "elaboration" -> Relation.Elaboration
  | relation -> malformed "unknown block relation %S" relation

let rec block_of_json ~entities ~glossary json : Block.t =
  { kind = kind_of_json (field "kind" json);
    content =
      List.map (inline_of_json ~entities ~glossary)
        (array (field "content" json));
    children =
      List.map (child_of_json ~entities ~glossary)
        (array (field "children" json))
  }

and child_of_json ~entities ~glossary json =
  relation_of_json (field "relation" json),
  block_of_json ~entities ~glossary (field "block" json)

let of_json text =
  match
    let json = Json.parse text in
    let loc = location_of_json (field "loc" json) in
    let entities = entities_of_json (field "entities" json) in
    let glossary = glossary_of_json (field "glossary" json) in
    let body =
      List.map (block_of_json ~entities ~glossary)
        (array (field "body" json))
    in
    { loc; body }
  with
  | diagnostic -> Ok diagnostic
  | exception Json.Malformed message -> Error message
  | exception Invalid_argument message -> Error message

let to_json diagnostic =
  String.concat "" (String.split_on_char '\n' (diagnostic_to_json diagnostic))
