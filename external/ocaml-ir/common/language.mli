module T : sig
  type t =
    | Assembly
    | Cfg
    | Cmm
    | Flambda2
    | Lambda
    | Linear
    | Parse_tree
    | Ppx
    | Raw_flambda2
    | Raw_lambda
    | Typed_tree
    | Inlining_report
    | Check_allocations
  [@@deriving equal, compare, enumerate, sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val all : t list

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]
end

include module type of struct
  include T
end

module Map : sig
  include Map.S with type key = T.t

  val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
end

module Set : Set.S with type elt = T.t

val t_of_string : string -> t option
val t_of_string_exn : string -> t
val string_of_t : t -> string

val camel_case_of_t : t -> string
  [@@ocaml.doc
    " Similar to [string_of_t] but returns the string in camel case, i.e.:\n\
    \    [camel_case_of_t Raw_flambda2 = \"RawFlambda2\"] "]

val unique_letter_binding : t -> string option
  [@@ocaml.doc
    " Returns a string consisting of one or more characters mnemonically similar to the\n\
    \    language name, corresponding to the keys that should be used within editors to \
     bind\n\
    \    actions relevant to that language. "]

val first : unit -> t

val next : t -> t list
  [@@ocaml.doc
    " Get the next language in compilation order. It may return several languages if\n\
    \    different compilation path are supported (eg. with flambda or without). In this \
     case\n\
    \    the first valid language of the returned list (according to the context) should \
     be\n\
    \    taken as the \"next\" one. It may return nothing when the language is the last \
     one.*"]
