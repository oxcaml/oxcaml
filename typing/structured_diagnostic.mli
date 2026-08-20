module Location_key : sig
  type t

  val of_location : Location.t -> t

  val equal : t -> t -> bool
end

module Entities : sig
  module Id : sig
    type t

    val to_int : t -> int
  end

  type t

  val empty : t

  val intern : t -> Location.t -> t * Id.t

  val find : t -> Id.t -> Location.t option

  val to_list : t -> (Id.t * Location.t) list
end

module Glossary : sig
  module Entry : sig
    type t =
      { term : string;
        category : string;
        description : string;
        url : string option
      }
  end

  module Id : sig
    type t

    val to_int : t -> int
  end

  type t

  val empty : t

  val intern : t -> Entry.t -> t * Id.t

  val find : t -> Id.t -> Entry.t option

  val to_list : t -> (Id.t * Entry.t) list
end

module Form : sig
  type t = Structured_diagnostic_protocol.Form.t =
    | Name
    | Pronoun
end

module Kind : sig
  type t = Structured_diagnostic_protocol.Kind.t =
    | Explanation
    | Background
    | Suggestion
end

module Relation : sig
  type t = Structured_diagnostic_protocol.Relation.t =
    | Claim
    | Elaboration
end

module Annotation : sig
  type t =
    | Code
    | Source of Location.t
    | Mention of
        { entity : Entities.Id.t;
          form : Form.t
        }
    | Term of Glossary.Id.t
end

module Inline : sig
  type t =
    | Text of string
    | Annotated of
        { annotation : Annotation.t;
          content : t list
        }
end

module Block : sig
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

val locations : t -> Inline.t list -> Location.t list

val to_protocol :
  location:(Location.t -> 'loc) ->
  t ->
  'loc Structured_diagnostic_protocol.Generic.diagnostic

val raw_location : Location.t -> Structured_diagnostic_protocol.Raw.Location.t

val to_raw_diagnostic : t -> Structured_diagnostic_protocol.Raw.diagnostic

val raw_response : t list -> Structured_diagnostic_protocol.Raw.response
