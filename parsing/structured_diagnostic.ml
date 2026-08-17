module Protocol = Structured_diagnostic_protocol

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
  type t = Protocol.Form.t =
    | Name
    | Pronoun
end

module Kind = struct
  type t = Protocol.Kind.t =
    | Explanation
    | Background
    | Suggestion
end

module Relation = struct
  type t = Protocol.Relation.t =
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

let to_protocol ~location t : _ Protocol.Generic.diagnostic =
  let rec protocol_of_inline (inline : Inline.t) : _ Protocol.Generic.inline =
    match inline with
    | Text text -> Text text
    | Annotated { annotation; content } ->
      Annotated
        { annotation = protocol_of_annotation annotation;
          content = List.map protocol_of_inline content
        }
  and protocol_of_annotation (annotation : Annotation.t) :
      _ Protocol.Generic.annotation =
    match annotation with
    | Code -> Code
    | Source loc -> Source (location loc)
    | Mention { entity; form } ->
      Mention { entity = Entities.Id.to_int entity; form }
    | Term id -> Term (Glossary.Id.to_int id)
  and protocol_of_block (block : Block.t) : _ Protocol.Generic.block =
    { kind = block.kind;
      content = List.map protocol_of_inline block.content;
      children = List.map protocol_of_child block.children
    }
  and protocol_of_child ((relation, block) : Relation.t * Block.t) :
      _ Protocol.Generic.child =
    { relation; block = protocol_of_block block }
  in
  let protocol_of_entity ((id, loc) : Entities.Id.t * Location.t) :
      _ Protocol.Generic.entity =
    { id = Entities.Id.to_int id; loc = location loc }
  in
  let protocol_of_entry ((id, entry) : Glossary.Id.t * Glossary.Entry.t) :
      Protocol.Generic.glossary_entry =
    { id = Glossary.Id.to_int id;
      term = entry.term;
      category = entry.category;
      description = entry.description;
      url = entry.url
    }
  in
  { loc = location t.loc;
    title = t.title;
    entities = List.map protocol_of_entity (Entities.to_list t.entities);
    glossary = List.map protocol_of_entry (Glossary.to_list t.glossary);
    body = List.map protocol_of_block t.body
  }

let raw_location (loc : Location.t) : Protocol.Raw.Location.t =
  let position (position : Lexing.position) : Protocol.Raw.Position.t =
    { line = position.pos_lnum; col = position.pos_cnum - position.pos_bol }
  in
  { file = loc.loc_start.pos_fname;
    start = position loc.loc_start;
    end_ = position loc.loc_end
  }

let to_raw_diagnostic t = to_protocol ~location:raw_location t

let raw_response ts =
  Protocol.Raw.response_of_diagnostics (List.map to_raw_diagnostic ts)
