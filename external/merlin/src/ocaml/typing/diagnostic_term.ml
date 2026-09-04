module Nlg = Diagnostic_nlg

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

module Side = struct
  type t =
    | Expected
    | Actual

  let equal left right =
    match left, right with
    | Expected, Expected | Actual, Actual -> true
    | Expected, Actual | Actual, Expected -> false

  let other = function Expected -> Actual | Actual -> Expected

  let select t ~expected ~actual =
    match t with Expected -> expected | Actual -> actual
end

type sides =
  { expected_name : t Nlg.Phrase.segment list;
    actual_name : t Nlg.Phrase.segment list
  }

let side_name sides side =
  Side.select side ~expected:sides.expected_name ~actual:sides.actual_name

let mode_word (mode : Mode.Reported_mode.t) : t Nlg.Phrase.segment =
  Nlg.term (Mode_term (Reported_mode mode))

let modality_word (atom : Mode.Modality.atom) : t Nlg.Phrase.segment =
  Nlg.term (Modality_term atom)

let concept_word (concept : concept) : t Nlg.Phrase.segment =
  Nlg.term (Concept_term concept)

let mode_const_word ax c : t Nlg.Phrase.segment =
  Nlg.term (Mode_term (Alloc_mode (Mode.Alloc.Atom (ax, c))))

let display_parts (t : t) : string * string option =
  match t with
  | Mode_term (Reported_mode mode) -> (
    match Mode.Reported_mode.describe `Actual mode with
    | [] -> Mode.Reported_mode.name mode, None
    | description :: _ ->
      Mode.Reported_mode.name description.displayed, description.suffix)
  | Mode_term (Alloc_mode (Mode.Alloc.Atom (axis, mode))) ->
    Format_doc.asprintf "%a" (Mode.Alloc.Const.print_axis axis) mode, None
  | Written_modality_term name -> "@@ " ^ name, None
  | Modality_term (Atom (ax, m)) ->
    Format_doc.asprintf "@@@@ %a" (Mode.Modality.Per_axis.print ax) m, None
  | Concept_term concept ->
    ( (match concept with
      | Unsafe_mode_crossing -> "unsafe mode crossing"
      | With_bounds -> "with-bounds"),
      None )

let is_code (t : t) : bool =
  match t with
  | Mode_term _ | Modality_term _ | Written_modality_term _ -> true
  | Concept_term (Unsafe_mode_crossing | With_bounds) -> false

let words (t : t) : t Nlg.Phrase.segment list =
  let name, suffix = display_parts t in
  (if is_code t then Nlg.code name else Nlg.txt name)
  :: (match suffix with None -> [] | Some suffix -> [Nlg.txt suffix])

let display (t : t) : string =
  let name, suffix = display_parts t in
  name ^ Option.value ~default:"" suffix

let entry (t : t) : Structured_diagnostic.Glossary_entry.t =
  let entry ~category ?(description = "") ?url () =
    { Structured_diagnostic.Glossary_entry.term = display t;
      category;
      description;
      url
    }
  in
  match t with
  | Mode_term _ -> entry ~category:"Mode" ()
  | Modality_term _ | Written_modality_term _ -> entry ~category:"Modality" ()
  | Concept_term Unsafe_mode_crossing ->
    entry ~category:"Mode crossing"
      ~description:
        "A record or variant marked [@@unsafe_allow_any_mode_crossing] claims \
         the mode crossing written in its kind annotation, whatever its \
         definition would justify; the compiler takes the claim on trust."
      ~url:"https://oxcaml.org/documentation/kinds/types/" ()
  | Concept_term With_bounds ->
    entry ~category:"Kind"
      ~description:
        "The part of a kind that makes a type's mode crossing depend on the \
         types it contains: 'a list crosses portability only when 'a does, \
         written `with 'a`."
      ~url:"https://oxcaml.org/documentation/kinds/intro/" ()

let realize ~loc (stories : t Nlg.story list) : Structured_diagnostic.t =
  Nlg.realize ~term_entry:entry ~term_words:words ~loc stories

let rendered_children (beat : t Nlg.beat) : Structured_diagnostic.Block.t =
  Nlg.rendered_children ~term_entry:entry ~term_words:words beat

let diagnose ~loc make_stories =
  let snapshot = Btype.snapshot () in
  Fun.protect
    ~finally:(fun () -> Btype.backtrack snapshot)
    (fun () ->
      match make_stories () with
      | [] -> None
      | stories -> Some (realize ~loc stories)
      | exception ((Out_of_memory | Stack_overflow) as unrecoverable) ->
        raise unrecoverable
      | exception _ -> None)
