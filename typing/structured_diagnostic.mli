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
  type t =
    | Name
    | Pronoun
end

module Kind : sig
  type t =
    | Explanation
    | Background
    | Suggestion
end

module Relation : sig
  type t =
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
