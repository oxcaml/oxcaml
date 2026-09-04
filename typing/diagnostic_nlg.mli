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

val ordinal : int -> string

val longident_name : Longident.t -> string option

type subject =
  { name : Phrase.word list;
    span : Location.t option
  }

val subject : ?span:Location.t -> Phrase.word list -> subject

val mention : case:Phrase.case -> subject -> _ Phrase.segment

val pronoun : case:Phrase.case -> subject -> _ Phrase.segment

type 'term aside

type 'term beat

type 'term story = 'term beat

val note :
  ?subject:subject -> ?asides:'term aside list -> 'term Phrase.t -> 'term aside

val background : 'term Phrase.t -> 'term aside

val suggest : 'term Phrase.t -> 'term aside

val claim :
  ?subject:subject -> ?asides:'term aside list -> 'term Phrase.t -> 'term beat

val but :
  ?subject:subject -> ?asides:'term aside list -> 'term Phrase.t -> 'term beat

val sub_claim :
  ?subject:subject -> ?asides:'term aside list -> 'term Phrase.t -> 'term aside

val child : 'term beat -> 'term aside

val story : 'term beat list -> 'term story

val plain :
  claim:'term Phrase.t ->
  ?contrast:'term Phrase.t ->
  ?background:'term Phrase.t list ->
  ?suggestions:'term Phrase.t list ->
  unit ->
  'term story

val beheaded : 'term beat -> 'term beat

val reframe : 'term beat -> 'term story list -> 'term beat

val pronominalize : 'term story list -> 'term story list

val pronominalize_one : 'term story -> 'term story

val realize :
  term_entry:('term -> Structured_diagnostic.Glossary_entry.t) ->
  term_words:('term -> 'term Phrase.t) ->
  loc:Location.t ->
  'term story list ->
  Structured_diagnostic.t

val rendered_children :
  term_entry:('term -> Structured_diagnostic.Glossary_entry.t) ->
  term_words:('term -> 'term Phrase.t) ->
  'term beat ->
  Structured_diagnostic.Block.t
