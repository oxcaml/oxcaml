module Phrase : sig
  type case =
    | Subject
    | Possessive

  type number =
    | Singular
    | Plural

  type word =
    | Text of string
    | Code of string

  type 'term segment

  type 'term t = 'term segment list
end

val txt : string -> _ Phrase.segment

val code : string -> _ Phrase.segment

val copula : _ Phrase.segment

val copula_agreeing : Phrase.number -> _ Phrase.segment

val term : 'term -> 'term Phrase.segment

val ref_source : Location.t -> 'term Phrase.segment list -> 'term Phrase.segment

type subject =
  { name : Phrase.word list;
    span : Location.t option
  }

val mention : case:Phrase.case -> subject -> _ Phrase.segment

val pronoun : case:Phrase.case -> subject -> _ Phrase.segment

module Statement : sig
  type 'term clause =
    | Subordinate of 'term Phrase.t
    | Coordinate of 'term Phrase.t

  type 'term t
end

type 'term plan =
  { statement : 'term Statement.t option;
    children : (Structured_diagnostic.Relation.t * 'term plan) list
  }

val sentence :
  ?kind:Structured_diagnostic.Kind.t ->
  ?subject:subject ->
  ?clause:'term Statement.clause ->
  'term Phrase.t ->
  'term Statement.t

val fragment :
  ?kind:Structured_diagnostic.Kind.t -> 'term Phrase.t -> 'term Statement.t

val pronominalize : 'term plan list -> 'term plan list

val realize :
  loc:Location.t ->
  title:string ->
  term_entry:('term -> Structured_diagnostic.Glossary.Entry.t) ->
  term_words:('term -> 'term Phrase.t) ->
  'term plan list ->
  Structured_diagnostic.t
