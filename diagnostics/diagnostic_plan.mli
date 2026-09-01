module Annotation : sig
  type t =
    | Code
    | Source of Location.t
    | Mention of
        { entity : Location.t;
          form : Structured_diagnostic.Form.t
        }
    | Term of Structured_diagnostic.Glossary.Entry.t
end

module Inline : sig
  type t =
    | Text of string
    | Annotated of
        { annotation : Annotation.t;
          content : t list
        }
end

type t

val create :
  kind:Structured_diagnostic.Kind.t ->
  content:Inline.t list ->
  children:(Structured_diagnostic.Relation.t * t) list ->
  t

val children : t -> (Structured_diagnostic.Relation.t * t) list

val with_children :
  t -> (Structured_diagnostic.Relation.t * t) list -> t

val equal : t -> t -> bool

val to_diagnostic :
  loc:Location.t -> t list -> Structured_diagnostic.t
