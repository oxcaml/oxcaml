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
  { expected_name : t Diagnostic_nlg.Phrase.segment list;
    actual_name : t Diagnostic_nlg.Phrase.segment list
  }

val side_name : sides -> Side.t -> t Diagnostic_nlg.Phrase.segment list

val mode_word : Mode.Reported_mode.t -> t Diagnostic_nlg.Phrase.segment

val modality_word : Mode.Modality.atom -> t Diagnostic_nlg.Phrase.segment

val concept_word : concept -> t Diagnostic_nlg.Phrase.segment

val mode_const_word :
  'a Mode.Alloc.Axis.t -> 'a -> t Diagnostic_nlg.Phrase.segment

val words : t -> t Diagnostic_nlg.Phrase.t

val entry : t -> Structured_diagnostic.Glossary_entry.t

val realize :
  loc:Location.t -> t Diagnostic_nlg.story list -> Structured_diagnostic.t

val rendered_children : t Diagnostic_nlg.beat -> Structured_diagnostic.Block.t

val diagnose :
  loc:Location.t ->
  (unit -> t Diagnostic_nlg.story list) ->
  Structured_diagnostic.t option
