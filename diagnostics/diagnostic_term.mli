type mode_term =
  | Reported_mode of Mode.Reported_mode.t
  | Alloc_mode of Mode.Alloc.atom

type concept =
  | Unsafe_mode_crossing
  | With_bounds

type t =
  | Mode_term of mode_term
  | Modality_term of Mode.Modality.atom
  | Written_modality_term of string
  | Concept_term of concept

module Side : sig
  type t =
    | Expected
    | Actual

  val equal : t -> t -> bool

  val other : t -> t

  val select : t -> expected:'a -> actual:'a -> 'a
end

type sides =
  { expected_name : t Diagnostic_nlg.Phrase.t;
    actual_name : t Diagnostic_nlg.Phrase.t
  }

val side_name : sides -> Side.t -> t Diagnostic_nlg.Phrase.t

val mode_property : Mode.Reported_mode.t -> t Diagnostic_nlg.Property.t

val mode_const_property :
  'a Mode.Alloc.Axis.t -> 'a -> t Diagnostic_nlg.Property.t

val mode_word : Mode.Reported_mode.t -> t Diagnostic_nlg.Phrase.segment

val modality_word : Mode.Modality.atom -> t Diagnostic_nlg.Phrase.segment

val concept_word : concept -> t Diagnostic_nlg.Phrase.segment

val mode_const_word :
  'a Mode.Alloc.Axis.t -> 'a -> t Diagnostic_nlg.Phrase.segment

val words : t -> t Diagnostic_nlg.Phrase.t

type documentation =
  { description : string;
    url : string option
  }

val set_documentation : (t -> documentation option) -> unit

val entry : t -> Structured_diagnostic.Glossary_entry.t

type diagnostic =
  { loc : Location.t;
    fragments : t Diagnostic_nlg.fragment list
  }

val realize :
  t Diagnostic_nlg.fragment list -> Structured_diagnostic.Block.t list

val rendered_children :
  t Diagnostic_nlg.fragment -> Structured_diagnostic.Block.t

val diagnose :
  loc:Location.t ->
  (unit -> t Diagnostic_nlg.fragment list) ->
  diagnostic option
