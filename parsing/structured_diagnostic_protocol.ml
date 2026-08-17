type json_string = string

let protocol_version = 1

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

module Form = struct
  type t =
    | Name
    | Pronoun

  let to_string = function
    | Name -> "name"
    | Pronoun -> "pronoun"
end

module Kind = struct
  type t =
    | Explanation
    | Background
    | Suggestion

  let to_string = function
    | Explanation -> "explanation"
    | Background -> "background"
    | Suggestion -> "suggestion"
end

module Relation = struct
  type t =
    | Claim
    | Elaboration

  let to_string = function
    | Claim -> "claim"
    | Elaboration -> "elaboration"
end

module Generic = struct
  type 'loc annotation =
    | Code
    | Source of 'loc
    | Mention of
        { entity : int;
          form : Form.t
        }
    | Term of int

  and 'loc inline =
    | Text of string
    | Annotated of
        { annotation : 'loc annotation;
          content : 'loc inline list
        }

  and 'loc child =
    { relation : Relation.t;
      block : 'loc block
    }

  and 'loc block =
    { kind : Kind.t;
      content : 'loc inline list;
      children : 'loc child list
    }

  type 'loc entity =
    { id : int;
      loc : 'loc
    }

  type glossary_entry =
    { id : int;
      term : string;
      category : string;
      description : string;
      url : string option
    }

  type 'loc diagnostic =
    { loc : 'loc;
      title : string;
      entities : 'loc entity list;
      glossary : glossary_entry list;
      body : 'loc block list
    }

  type 'loc response =
    { version : int;
      diagnostics : 'loc diagnostic list
    }

  let kind_field kind = Misc.Json.field "kind" (string_to_json kind)

  let rec annotation_to_json ~loc_to_json (annotation : 'loc annotation) =
    match annotation with
    | Code -> Misc.Json.object_ [ kind_field "code" ]
    | Source loc ->
      Misc.Json.object_
        [ kind_field "source"; Misc.Json.field "loc" (loc_to_json loc) ]
    | Mention { entity; form } ->
      Misc.Json.object_
        [ kind_field "mention";
          Misc.Json.field "entity" (Misc.Json.int entity);
          Misc.Json.field "form" (string_to_json (Form.to_string form))
        ]
    | Term term ->
      Misc.Json.object_
        [ kind_field "term"; Misc.Json.field "term" (Misc.Json.int term) ]

  and inline_to_json ~loc_to_json (inline : 'loc inline) =
    match inline with
    | Text text ->
      Misc.Json.object_
        [ kind_field "text"; Misc.Json.field "text" (string_to_json text) ]
    | Annotated { annotation; content } ->
      Misc.Json.object_
        [ kind_field "annotated";
          Misc.Json.field "annotation"
            (annotation_to_json ~loc_to_json annotation);
          Misc.Json.field "content" (inlines_to_json ~loc_to_json content)
        ]

  and inlines_to_json ~loc_to_json content =
    Misc.Json.array (List.map (inline_to_json ~loc_to_json) content)

  let rec block_to_json ~loc_to_json (block : 'loc block) =
    Misc.Json.object_
      [ Misc.Json.field "kind" (string_to_json (Kind.to_string block.kind));
        Misc.Json.field "content" (inlines_to_json ~loc_to_json block.content);
        Misc.Json.field "children"
          (Misc.Json.array
             (List.map (child_to_json ~loc_to_json) block.children))
      ]

  and child_to_json ~loc_to_json (child : 'loc child) =
    Misc.Json.object_
      [ Misc.Json.field "relation"
          (string_to_json (Relation.to_string child.relation));
        Misc.Json.field "block" (block_to_json ~loc_to_json child.block)
      ]

  let entity_to_json ~loc_to_json (entity : 'loc entity) =
    Misc.Json.object_
      [ Misc.Json.field "id" (Misc.Json.int entity.id);
        Misc.Json.field "loc" (loc_to_json entity.loc)
      ]

  let glossary_entry_to_json (entry : glossary_entry) =
    let url =
      match entry.url with
      | None -> []
      | Some url -> [ Misc.Json.field "url" (string_to_json url) ]
    in
    Misc.Json.object_
      ([ Misc.Json.field "id" (Misc.Json.int entry.id);
         Misc.Json.field "term" (string_to_json entry.term);
         Misc.Json.field "category" (string_to_json entry.category);
         Misc.Json.field "description" (string_to_json entry.description)
       ]
      @ url)

  let diagnostic_to_json ~loc_to_json (diagnostic : 'loc diagnostic) =
    Misc.Json.object_
      [ Misc.Json.field "loc" (loc_to_json diagnostic.loc);
        Misc.Json.field "title" (string_to_json diagnostic.title);
        Misc.Json.field "entities"
          (Misc.Json.array
             (List.map (entity_to_json ~loc_to_json) diagnostic.entities));
        Misc.Json.field "glossary"
          (Misc.Json.array
             (List.map glossary_entry_to_json diagnostic.glossary));
        Misc.Json.field "body"
          (Misc.Json.array
             (List.map (block_to_json ~loc_to_json) diagnostic.body))
      ]

  let response_to_json ~loc_to_json (response : 'loc response) =
    Misc.Json.object_
      [ Misc.Json.field "version" (Misc.Json.int response.version);
        Misc.Json.field "diagnostics"
          (Misc.Json.array
             (List.map (diagnostic_to_json ~loc_to_json) response.diagnostics))
      ]
end

module Raw = struct
  module Position = struct
    type t =
      { line : int;
        col : int
      }

    let to_json (position : t) =
      Misc.Json.object_
        [ Misc.Json.field "line" (Misc.Json.int position.line);
          Misc.Json.field "col" (Misc.Json.int position.col)
        ]
  end

  module Location = struct
    type t =
      { file : string;
        start : Position.t;
        end_ : Position.t
      }

    let to_json (location : t) =
      Misc.Json.object_
        [ Misc.Json.field "file" (string_to_json location.file);
          Misc.Json.field "start" (Position.to_json location.start);
          Misc.Json.field "end" (Position.to_json location.end_)
        ]
  end

  type diagnostic = Location.t Generic.diagnostic

  type response = Location.t Generic.response

  let response_of_diagnostics diagnostics : response =
    { version = protocol_version; diagnostics }

  let diagnostic_to_json (diagnostic : diagnostic) =
    Generic.diagnostic_to_json ~loc_to_json:Location.to_json diagnostic

  let response_to_json (response : response) =
    Generic.response_to_json ~loc_to_json:Location.to_json response
end
