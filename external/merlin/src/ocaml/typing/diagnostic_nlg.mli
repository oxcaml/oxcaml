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

type 'term fragment

type necessity =
  | Inherit
  | Necessary
  | Unnecessary

type verbosity =
  | Full
  | Minimal

val state : ?subject:subject -> 'term Phrase.t -> 'term fragment

val but : ?subject:subject -> 'term Phrase.t -> 'term fragment

val reason : ?subject:subject -> 'term Phrase.t -> 'term fragment

val rule : 'term Phrase.t -> 'term fragment

val is_rule : 'term fragment -> bool

val suggestion : 'term Phrase.t -> 'term fragment

val with_children : 'term fragment list -> 'term fragment -> 'term fragment

val group : 'term fragment list -> 'term fragment

val block :
  ?necessity:necessity -> 'term fragment list -> 'term fragment

val explanation :
  ?necessity:necessity -> 'term fragment list -> 'term fragment

val focus :
  on:'term fragment -> 'term fragment list -> 'term fragment list

val without_text : 'term fragment -> 'term fragment

val clip :
  verbosity:verbosity -> 'term fragment list -> 'term fragment list

val naturalize : 'term fragment list -> 'term fragment list

val realize :
  term_entry:('term -> Structured_diagnostic.Glossary_entry.t) ->
  term_words:('term -> 'term Phrase.t) ->
  'term fragment list ->
  Structured_diagnostic.Block.t list

val rendered_children :
  term_entry:('term -> Structured_diagnostic.Glossary_entry.t) ->
  term_words:('term -> 'term Phrase.t) ->
  'term fragment ->
  Structured_diagnostic.Block.t
