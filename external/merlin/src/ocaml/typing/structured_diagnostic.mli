module Location_key : sig
  type t

  val of_location : Location.t -> t

  val equal : t -> t -> bool
end

module Glossary_entry : sig
  type t =
    { term : string;
      category : string;
      description : string;
      url : string option
    }
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
        { entity : Location.t;
          form : Form.t
        }
    | Term of Glossary_entry.t
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

  val equal : t -> t -> bool
end

type t =
  { loc : Location.t;
    body : Block.t list
  }

module Json : sig
  val to_value :
    string:(string -> 'json) ->
    int:(int -> 'json) ->
    array:('json list -> 'json) ->
    object_:((string * 'json) list -> 'json) ->
    t ->
    'json

  val from_value :
    string:('json -> string) ->
    int:('json -> int) ->
    array:('json -> 'json list) ->
    field:(string -> 'json -> 'json) ->
    optional_field:(string -> 'json -> 'json option) ->
    'json ->
    t
end

val to_json : t -> string
val of_json : string -> (t, string) result
