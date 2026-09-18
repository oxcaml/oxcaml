module Nonempty : sig
  type (+'head, +'tail) t = ( :: ) of 'head * 'tail list

  val to_list : ('a, 'a) t -> 'a list

  val append : ('head, 'tail) t -> 'tail list -> ('head, 'tail) t
end

module Phrase : sig
  type case =
    | Subject
    | Object
    | Possessive

  type number =
    | Singular
    | Plural

  type word =
    | Text of string
    | Code of string

  type 'term segment

  type 'term t = ('term segment, 'term segment) Nonempty.t
end

val word : Phrase.word -> _ Phrase.segment

val txt : string -> _ Phrase.segment

val code : string -> _ Phrase.segment

(** Agrees with an immediately preceding mention, including contractions. *)
val copula : _ Phrase.segment

val copula_agreeing : Phrase.number -> _ Phrase.segment

val term : 'term -> 'term Phrase.segment

val ref_source : Location.t -> 'term Phrase.t -> 'term Phrase.segment

val ordinal : int -> string

val longident_name : Longident.t -> string option

module Noun : sig
  type t

  (** Referents retain their identity when reused in clauses. *)
  val described : ?loc:Location.t -> ?number:Phrase.number -> string -> t

  (** Named nouns retain their identifier independently of their full name. *)
  val named :
    ?loc:Location.t -> ?number:Phrase.number -> ?kind:string -> string -> t

  (** A common noun phrase, such as ["array elements"], without a referent. *)
  val common : ?number:Phrase.number -> string -> t

  val identifier : t -> string option

  val short : t -> t

  val with_kind : kind:string -> t -> t

  (** The possessed noun determines agreement, independently of its owner. Named
      nouns omit their determiner after a possessive. *)
  val possessive : t -> t -> t

  val source : t -> t
end

(** Compatibility with low-level phrase construction. *)
type subject = Noun.t

val subject :
  ?span:Location.t ->
  ?number:Phrase.number ->
  (Phrase.word, Phrase.word) Nonempty.t ->
  subject

val mention : ?case:Phrase.case -> subject -> _ Phrase.segment

(** Mentions choose names or pronouns during naturalization. Source references
    additionally request a location in text output. *)
val located_mention : ?case:Phrase.case -> subject -> _ Phrase.segment

module Property : sig
  type 'term t

  (** Lexical complements such as ["borrowed"] or ["being read"]. *)
  val text : string -> _ t

  val code : string -> _ t

  val term : 'term -> 'term t
end

module Predicate : sig
  type 'term t

  val is : 'term Property.t -> 'term t

  (** Declare a verb's forms once; clauses select the form for their subject. *)
  val transitive : singular:string -> plural:string -> Noun.t -> _ t
end

module Clause : sig
  type 'term t
end

type category =
  [ `Statement
  | `Dependent
  | `Rule
  | `Suggestion
  | `Group
  | `Block
  | `Explanation ]

type ('term, +'category) node

type 'term fragment = ('term, category) node

type necessity =
  | Inherit
  | Necessary
  | Unnecessary

type verbosity =
  | Full
  | Minimal

(** Low-level phrase construction. These infer a subject from mentions; prefer
    [Dsl] for clauses with explicit grammatical roles. *)
val state : 'term Phrase.t -> ('term, [> `Statement]) node

(** These helpers supply their connective; phrases contain only the clause. *)
val but : 'term Phrase.t -> ('term, [> `Dependent]) node

val reason : 'term Phrase.t -> ('term, [> `Dependent]) node

val and_ : 'term Phrase.t -> ('term, [> `Dependent]) node

(** An elaboration without a connective. *)
val elaborate : 'term Phrase.t -> ('term, [> `Dependent]) node

val rule : 'term Phrase.t -> ('term, [> `Rule]) node

val is_rule : ('term, _) node -> bool

val suggestion : 'term Phrase.t -> ('term, [> `Suggestion]) node

(** Declarative authoring, for example:
    [Dsl.(state (clause value (is (expected portable))))]. Clauses preserve the
    grammatical subject. All subjects and objects are nouns; copular complements
    are properties. Use [Verbatim] for hand-authored segments. *)
module Dsl : sig
  val named :
    ?loc:Location.t -> ?number:Phrase.number -> ?kind:string -> string -> Noun.t

  val described : ?loc:Location.t -> ?number:Phrase.number -> string -> Noun.t

  val noun : ?number:Phrase.number -> string -> Noun.t

  val possessive : Noun.t -> Noun.t -> Noun.t

  (** Requests the noun's source location without changing its grammatical role.
      Predicates choose the object form, clauses the subject form. *)
  val source : Noun.t -> Noun.t

  val property : string -> _ Property.t

  val term : 'term -> 'term Property.t

  val expected : 'term Property.t -> 'term Property.t

  val always : 'term Property.t -> 'term Property.t

  val stronger_than : 'term Property.t -> 'term Property.t

  val weaker_than : 'term Property.t -> 'term Property.t

  val alternatives :
    ('term Property.t, 'term Property.t) Nonempty.t -> 'term Property.t

  val clause : Noun.t -> 'term Predicate.t -> 'term Clause.t

  val is : 'term Property.t -> 'term Predicate.t

  val used_inside : Noun.t -> _ Predicate.t

  val used_as : 'term Property.t -> 'term Predicate.t

  (** References the annotation site with the whole predicate. *)
  val annotated_as : at:Location.t -> 'term Property.t -> 'term Predicate.t

  (** References just the written annotation or declaration. *)
  val annotated : at:Location.t -> 'term Property.t -> 'term Predicate.t

  val declared : at:Location.t -> 'term Property.t -> 'term Predicate.t

  val state : 'term Clause.t -> ('term, [> `Statement]) node

  val but : 'term Clause.t -> ('term, [> `Dependent]) node

  val reason : 'term Clause.t -> ('term, [> `Dependent]) node

  val and_ : 'term Clause.t -> ('term, [> `Dependent]) node

  val rule : 'term Clause.t -> ('term, [> `Rule]) node
end

(** Explicit escape hatches for exceptional wording and compatibility with
    low-level phrases. The caller is responsible for their grammar. *)
module Verbatim : sig
  val property : 'term Phrase.t -> 'term Property.t

  val phrase : 'term Property.t -> 'term Phrase.segment
end

(** Append children, preserving any required opening already present. *)
val with_children :
  'term fragment list -> ('term, 'category) node -> ('term, 'category) node

(** A block starts with a statement and resets the pronoun context. *)
val block :
  ?necessity:necessity ->
  (('term, [< `Statement]) node, 'term fragment) Nonempty.t ->
  ('term, [> `Block]) node

(** An explanation starts with explanatory text, never a rule or suggestion.
    Necessity is inherited by descendants unless they override it. *)
val explanation :
  ?necessity:necessity ->
  ( ('term, [< `Statement | `Dependent | `Explanation]) node,
    'term fragment )
  Nonempty.t ->
  ('term, [> `Explanation]) node

(** [on] must be the explanation node present in [fragments]. *)
val focus :
  on:('term, [< `Explanation]) node ->
  'term fragment list ->
  'term fragment list

val children : ('term, _) node -> 'term fragment list

val clip : verbosity:verbosity -> 'term fragment list -> 'term fragment list

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
