module Diagnostic = Structured_diagnostic
module Location_key = Structured_diagnostic.Location_key
module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase
module Statement = Nlg.Statement
module Step_mode = Mode.Hint_chain.Mode

let first = function [] -> None | element :: _ -> Some element

let same_chars (left : Location.t) (right : Location.t) =
  Location_key.equal
    (Location_key.of_location left)
    (Location_key.of_location right)

module Source = struct
  type t =
    { file : string;
      text : string
    }

  let create ~file ~text = { file; text }

  let length t = String.length t.text

  let sub t ~pos ~len = String.sub t.text pos len

  let holds t (loc : Location.t) =
    (not (Location.is_none loc)) && String.equal loc.loc_start.pos_fname t.file
end

module Documentation = struct
  type t =
    { description : string;
      url : string option
    }

  type lookup =
    { of_mode : Mode.Alloc.atom -> t option;
      of_modality : Mode.Modality.atom -> t option
    }
end

module Pronouns = struct
  type t =
    | Use_pronouns
    | Names_only
end

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

module Orientation = struct
  type t =
    | Got_is_actual
    | Got_is_expected

  let reverse = function
    | Got_is_actual -> Got_is_expected
    | Got_is_expected -> Got_is_actual

  let got_side : t -> Side.t = function
    | Got_is_actual -> Actual
    | Got_is_expected -> Expected

  let expected_side t = Side.other (got_side t)

  let side_of_position t (position : Includecore.position) =
    match position with First -> got_side t | Second -> expected_side t

  let expected_and_actual t ~got ~expected =
    match t with
    | Got_is_actual -> expected, got
    | Got_is_expected -> got, expected
end

module Bound = struct
  type t =
    | Exact
    | Loosened

  let of_loosening (loosening : Mode.loosening) =
    match loosening with Not_loosened -> Exact | Loosened -> Loosened

  let comparative t ~(side : Side.t) =
    match t with
    | Exact -> ""
    | Loosened ->
      Side.select side ~expected:"stronger than " ~actual:"weaker than "
end

type concept =
  | Unsafe_mode_crossing
  | With_bounds
  | Applicative_functor
  | Generative_functor
  | Atomic_field

type term =
  | Mode_term of Step_mode.t
  | Modality_term of Mode.Modality.atom
  | Concept_term of concept

type subject = Nlg.subject

let subject ?span name = { Nlg.name; span }

let sentence_subject (subject : subject) =
  match subject.Nlg.span with
  | None -> None
  | Some (_ : Location.t) -> Some subject

let subject_words subject =
  List.map
    (function
      | Phrase.Text text -> Nlg.txt text | Phrase.Code text -> Nlg.code text)
    subject.Nlg.name

let explicit_subject_words (subject : subject) =
  match subject.Nlg.span with
  | None -> subject_words subject
  | Some loc -> [Nlg.ref_source loc (subject_words subject)]

let phrase (segments : term Phrase.segment list) : term Phrase.t = segments

let described_point (atom : Mode.Mode_description.atom) : Step_mode.t =
  match atom with
  | Exact point -> point
  | Local_to_parent_region { displayed = _; semantic } -> semantic

let term_display_parts (t : term) : string * string option =
  match t with
  | Mode_term mode -> (
    let ({ first; alternatives = _ } : Mode.Mode_description.t) =
      Step_mode.describe mode
    in
    match first with
    | Exact point -> Step_mode.name point, None
    | Local_to_parent_region { displayed; semantic = _ } ->
      Step_mode.name displayed, Some " to the parent region")
  | Modality_term (Atom (ax, m)) ->
    Format_doc.asprintf "@@@@ %a" (Mode.Modality.Per_axis.print ax) m, None
  | Concept_term concept ->
    ( (match concept with
      | Unsafe_mode_crossing -> "unsafe mode crossing"
      | With_bounds -> "with-bounds"
      | Applicative_functor -> "applicative"
      | Generative_functor -> "generative"
      | Atomic_field -> "[@atomic]"),
      None )

let term_is_code (t : term) : bool =
  match t with
  | Mode_term _ | Modality_term _ | Concept_term Atomic_field -> true
  | Concept_term
      ( Unsafe_mode_crossing | With_bounds | Applicative_functor
      | Generative_functor ) ->
    false

let term_words (t : term) : term Phrase.segment list =
  let open Nlg in
  let name, suffix = term_display_parts t in
  (if term_is_code t then code name else txt name)
  :: (match suffix with None -> [] | Some suffix -> [txt suffix])

let term_display (t : term) : string =
  let name, suffix = term_display_parts t in
  name ^ Option.value ~default:"" suffix

let mode_word (mode : Step_mode.t) : term Phrase.segment =
  Nlg.term (Mode_term mode)

let modality_word (atom : Mode.Modality.atom) : term Phrase.segment =
  Nlg.term (Modality_term atom)

let concept_word (concept : concept) : term Phrase.segment =
  Nlg.term (Concept_term concept)

let mode_const_word ax c : term Phrase.segment =
  mode_word (Mode.hint_mode_of_alloc_atom (Mode.Alloc.Atom (ax, c)))

let max_quoted_source_length = 40

let snippet_of_loc ~source (loc : Location.t) =
  if not (Source.holds source loc)
  then None
  else begin
    let start = loc.loc_start.pos_cnum in
    let stop = loc.loc_end.pos_cnum in
    if start < 0 || stop <= start || stop > Source.length source
    then None
    else begin
      let text =
        String.trim (Source.sub source ~pos:start ~len:(stop - start))
      in
      if
        String.length text = 0
        || String.length text > max_quoted_source_length
        || String.contains text '\n'
      then None
      else Some text
    end
  end

module Access = struct
  type t =
    | Read
    | Write

  let equal left right =
    match left, right with
    | Read, Read | Write, Write -> true
    | Read, Write | Write, Read -> false
end

module Meaning = struct
  type fact =
    | Mutable_access of
        { part : Mode.Hint.mutable_part;
          access : Access.t
        }
    | Lazy_allocated_on_heap
    | Lazy_forced
    | Module_allocated_on_heap
    | Legacy_construct of Mode.Hint.legacy
    | Toplevel_expression
    | Tailcall_function
    | Tailcall_argument
    | Function_return_default
    | Stack_allocated
    | Always_dynamic of Mode.Hint.always_dynamic
    | Has_branches
    | Layout_poly_instantiated
    | Borrowed
    | Region_escape of Mode.Hint.region
    | Quoted_computation
    | Spliced
    | Static_not_guaranteed of Compilation_unit.t option

  type shared_staticity =
    | Of_functor of Location.t
    | Of_functor_parameter of Location.t

  type reroute =
    | Mode_crossing
    | Partial_application_capture
    | Allocation of Mode.Hint.allocation
    | Contains of Mode.Hint.contains
    | Contained_by of Mode.Hint.is_contained_by
    | Shared_staticity of shared_staticity
    | Functor_application of Location.t
    | Functor_applied_at of Location.t

  type t =
    | Nothing_to_say
    | Unexplained
    | User_annotation of Location.t
    | Capture of Mode.Hint.closure_details
    | Signature_argument of Mode.Hint.function_argument
    | Fact of fact
    | Reroute of reroute

  let reroute_of_allocation (allocation : Mode.Hint.allocation) =
    match allocation.txt with
    | Captured_by_partial_application -> Reroute Partial_application_capture
    | Unknown | Optional_argument | Function_coercion | Float_projection
    | Lpoly_captured_environment ->
      Reroute (Allocation allocation)

  let interpret ~source (s : Mode.Hint_chain.step) : t =
    match s.kind with
    | Mode.Hint_chain.Morph Unknown -> Nothing_to_say
    | Mode.Hint_chain.Morph Skip -> Nothing_to_say
    | Mode.Hint_chain.Morph (Close_over (_, details)) ->
      Capture details
    | Mode.Hint_chain.Morph (Is_closed_by (_, details)) ->
      Capture details
    | Mode.Hint_chain.Morph Crossing -> Reroute Mode_crossing
    | Mode.Hint_chain.Morph (Functor_to_parameter loc) ->
      Reroute (Shared_staticity (Of_functor loc))
    | Mode.Hint_chain.Morph (Parameter_to_functor loc) ->
      Reroute (Shared_staticity (Of_functor_parameter loc))
    | Mode.Hint_chain.Morph (Functor_to_application loc) ->
      Reroute (Functor_application loc)
    | Mode.Hint_chain.Morph (Application_to_functor loc) ->
      Reroute (Functor_applied_at loc)
    | Mode.Hint_chain.Morph (Allocation_r alloc)
    | Mode.Hint_chain.Morph (Allocation_l alloc)
    | Mode.Hint_chain.Morph (Allocation alloc) ->
      reroute_of_allocation alloc
    | Mode.Hint_chain.Morph (Contains_l (_, contains)) ->
      Reroute (Contains contains)
    | Mode.Hint_chain.Morph (Contains_r (_, contains)) ->
      Reroute (Contains contains)
    | Mode.Hint_chain.Morph (Is_contained_by (_, c)) ->
      Reroute (Contained_by c)
    | Mode.Hint_chain.Morph (Function_argument fa) ->
      Signature_argument fa
    | Mode.Hint_chain.Const Unknown -> Unexplained
    | Mode.Hint_chain.Const (Explicit_annotations locs) -> (
      match s.axis with
      | None -> Unexplained
      | Some _ -> (
        match
          List.find_opt
            (fun loc ->
              match snippet_of_loc ~source loc with
              | Some word -> String.equal word (Step_mode.name s.mode)
              | None -> false)
            locs
        with
        | Some loc -> User_annotation loc
        | None -> Unexplained))
    | Mode.Hint_chain.Const Lazy_allocated_on_heap ->
      Fact Lazy_allocated_on_heap
    | Mode.Hint_chain.Const (Legacy legacy) -> Fact (Legacy_construct legacy)
    | Mode.Hint_chain.Const Toplevel_expression -> Fact Toplevel_expression
    | Mode.Hint_chain.Const Tailcall_function -> Fact Tailcall_function
    | Mode.Hint_chain.Const Tailcall_argument -> Fact Tailcall_argument
    | Mode.Hint_chain.Const (Mutable_read part) ->
      Fact (Mutable_access { part; access = Read })
    | Mode.Hint_chain.Const (Mutable_write part) ->
      Fact (Mutable_access { part; access = Write })
    | Mode.Hint_chain.Const Lazy_forced -> Fact Lazy_forced
    | Mode.Hint_chain.Const Function_return -> Fact Function_return_default
    | Mode.Hint_chain.Const Stack_expression -> Fact Stack_allocated
    | Mode.Hint_chain.Const Module_allocated_on_heap ->
      Fact Module_allocated_on_heap
    | Mode.Hint_chain.Const (Always_dynamic x) -> Fact (Always_dynamic x)
    | Mode.Hint_chain.Const Branching -> Fact Has_branches
    | Mode.Hint_chain.Const Lpoly_inst -> Fact Layout_poly_instantiated
    | Mode.Hint_chain.Const (Is_used_in closure) ->
      Capture { closure; closed = s.pinpoint }
    | Mode.Hint_chain.Const (Borrowed (_, _)) -> Fact Borrowed
    | Mode.Hint_chain.Const (Escape_region region) ->
      Fact (Region_escape region)
    | Mode.Hint_chain.Const Quoted_computation -> Fact Quoted_computation
    | Mode.Hint_chain.Const (Spliced _) -> Fact Spliced
    | Mode.Hint_chain.Const (Contained_by c) -> Reroute (Contained_by c)
    | Mode.Hint_chain.Const (Cmx_not_guaranteed unit) ->
      Fact (Static_not_guaranteed unit)

  let is_region_escape : fact -> bool = function
    | Region_escape _ -> true
    | Mutable_access _ | Lazy_allocated_on_heap | Lazy_forced
    | Module_allocated_on_heap | Legacy_construct _ | Toplevel_expression
    | Tailcall_function | Tailcall_argument | Function_return_default
    | Stack_allocated | Always_dynamic _ | Has_branches
    | Layout_poly_instantiated | Borrowed | Quoted_computation | Spliced
    | Static_not_guaranteed _ ->
      false
end

let interpret = Meaning.interpret

let rec normalize ~source (chain : Mode.Hint_chain.t) : Mode.Hint_chain.t =
  match chain with
  | [] -> []
  | s :: rest -> begin
    let rest = normalize ~source rest in
    match interpret ~source s with
    | Nothing_to_say -> rest
    | Reroute Mode_crossing -> rest
    | Reroute (Allocation _) ->
      begin match rest with
      | [] -> rest
      | next :: _ ->
        if
          String.equal (Step_mode.name s.mode)
            (Step_mode.name next.Mode.Hint_chain.mode)
        then rest
        else s :: rest
      end
    | Reroute
        ( Partial_application_capture | Contains _ | Contained_by _
        | Shared_staticity _ | Functor_application _ | Functor_applied_at _ )
    | Unexplained | User_annotation _ | Capture _ | Signature_argument _
    | Fact _ ->
      s :: rest
    end

module Message = struct
  type t =
    { pinpoint : Mode.Hint.pinpoint;
      mode : Step_mode.t;
      axis : Mode.Axis.packed option;
      meaning : Meaning.t
    }

  let of_step ~source (s : Mode.Hint_chain.step) : t =
    { pinpoint = s.pinpoint;
      mode = s.mode;
      axis = s.axis;
      meaning = interpret ~source s
    }

  let of_chain ~source (chain : Mode.Hint_chain.t) : t list =
    List.map (of_step ~source) (normalize ~source chain)

  let is_informative (t : t) =
    match t.meaning with
    | Nothing_to_say | Unexplained -> false
    | User_annotation _ | Capture _ | Signature_argument _ | Fact _ | Reroute _
      ->
      true
end

let same_axis = Mode.Axis.equal_packed

let human_desc : Mode.Hint.pinpoint_desc -> string = function
  | Unknown -> "this value"
  | Ident _ -> "this identifier"
  | Function -> "the function"
  | Module -> "the module"
  | Functor -> "the functor"
  | Functor_parameter -> "the functor's parameter"
  | Structure -> "the structure"
  | Lazy -> "the lazy expression"
  | Quote -> "the quoted expression"
  | Allocation -> "the allocation"
  | Expression -> "the expression"
  | Effect_match -> "the pattern match with effect cases"
  | Effect_try -> "the try-with with effect cases"
  | Class -> "the class"
  | Object -> "the object"
  | Loop -> "the loop"
  | Letop -> "the letop"
  | Cases_result -> "the result of the cases"
  | Pattern -> "the pattern"
  | Structure_item _ -> "the structure item"

type function_binding =
  { name : string;
    prefix : string
  }

let string_contains_at string ~pos needle =
  let needle_length = String.length needle in
  pos >= 0
  && pos + needle_length <= String.length string
  && String.equal (String.sub string pos needle_length) needle

let contains_substring string substring =
  let rec loop pos =
    pos + String.length substring <= String.length string
    && (string_contains_at string ~pos substring || loop (pos + 1))
  in
  loop 0

let explicit_mode_annotation_before_loc ~source ~(mode : string)
    (loc : Location.t) =
  if not (Source.holds source loc)
  then None
  else begin
    let line_start = loc.loc_start.pos_bol in
    let stop = loc.loc_start.pos_cnum in
    if line_start < 0 || stop <= line_start || stop > Source.length source
    then None
    else begin
      let prefix = Source.sub source ~pos:line_start ~len:(stop - line_start) in
      let needle = "@ " ^ mode in
      let rec find_last pos found =
        if pos + String.length needle > String.length prefix
        then found
        else
          find_last (pos + 1)
            (if string_contains_at prefix ~pos needle
             then Some (pos + 2)
             else found)
      in
      match find_last 0 None with
      | None -> None
      | Some annotation_start ->
        let start_cnum = line_start + annotation_start in
        let start_pos = { loc.loc_start with pos_cnum = start_cnum } in
        let end_pos =
          { loc.loc_start with pos_cnum = start_cnum + String.length mode }
        in
        Some
          { Location.loc_start = start_pos;
            loc_end = end_pos;
            loc_ghost = false
          }
    end
  end

let function_binding_before_loc ~source (loc : Location.t) =
  if not (Source.holds source loc)
  then None
  else begin
    let line_start = loc.loc_start.pos_bol in
    let stop = loc.loc_start.pos_cnum in
    if line_start < 0 || stop <= line_start || stop > Source.length source
    then None
    else begin
      let prefix = Source.sub source ~pos:line_start ~len:(stop - line_start) in
      let rec find_keyword pos =
        if pos < 0
        then None
        else if string_contains_at prefix ~pos "let "
        then Some (pos + 4)
        else if string_contains_at prefix ~pos "and "
        then Some (pos + 4)
        else find_keyword (pos - 1)
      in
      match find_keyword (String.length prefix - 4) with
      | None -> None
      | Some start ->
        let rec skip_spaces pos =
          if pos < String.length prefix && Char.equal prefix.[pos] ' '
          then skip_spaces (pos + 1)
          else pos
        in
        let start = skip_spaces start in
        let start =
          if string_contains_at prefix ~pos:start "rec "
          then skip_spaces (start + 4)
          else start
        in
        let start =
          if start < String.length prefix && Char.equal prefix.[start] '('
          then skip_spaces (start + 1)
          else start
        in
        let is_ident_char = function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
          | _ -> false
        in
        let rec find_end pos =
          if pos < String.length prefix && is_ident_char prefix.[pos]
          then find_end (pos + 1)
          else pos
        in
        let stop = find_end start in
        let name = String.sub prefix start (stop - start) in
        let after_name = String.sub prefix stop (String.length prefix - stop) in
        if
          Int.equal start stop || String.equal name "_"
          || contains_substring after_name "="
        then None
        else Some { name; prefix }
    end
  end

let subject_of_loc ~source ~fallback loc =
  match snippet_of_loc ~source loc with
  | Some s ->
    subject
      ?span:(if Location.is_none loc then None else Some loc)
      [Phrase.Code s]
  | None ->
    subject
      ?span:(if Location.is_none loc then None else Some loc)
      [Phrase.Text fallback]

let subject_of_pinpoint ~source ((loc, desc) : Mode.Hint.pinpoint) =
  let open Nlg in
  match desc, function_binding_before_loc ~source loc with
  | Function, Some { name; prefix = _ } ->
    subject ?span:(Some loc) [Phrase.Text "the function "; Phrase.Code name]
  | Structure_item (_, id), (Some _ | None) ->
    subject ?span:(Some loc) [Phrase.Code (Ident.name id)]
  | ( ( Unknown | Ident _ | Function | Module | Functor | Functor_parameter
      | Structure | Lazy | Quote | Allocation | Expression | Effect_match
      | Effect_try | Class | Object | Loop | Letop | Cases_result | Pattern ),
      None )
  | ( ( Unknown | Ident _ | Module | Functor | Functor_parameter | Structure
      | Lazy | Quote | Allocation | Expression | Effect_match | Effect_try
      | Class | Object | Loop | Letop | Cases_result | Pattern ),
      Some _ ) ->
    subject_of_loc ~source ~fallback:(human_desc desc) loc

let description_words ({ first; alternatives } : Mode.Mode_description.t) :
    term Phrase.segment list =
  let open Nlg in
  mode_word (described_point first)
  :: List.concat_map
       (fun (alternative : Mode.Mode_description.atom) ->
         [txt " or "; mode_word (described_point alternative)])
       alternatives

let step_mode_segments mode : term Phrase.segment list =
  description_words (Step_mode.describe mode)

let mode_matches_description (description : Mode.Mode_description.t) mode =
  Step_mode.equal (described_point description.first) mode

let described_mode_segments (description : Mode.Mode_description.t) mode :
    term Phrase.segment list =
  if mode_matches_description description mode
  then description_words description
  else step_mode_segments mode

let ordinal n =
  let suffix =
    if n mod 100 >= 11 && n mod 100 <= 13
    then "th"
    else
      begin match n mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
      end
  in
  string_of_int n ^ suffix

let labelled_argument_words label =
  let open Nlg in
  [txt " "; code label; txt " argument"]

let argument_phrase ~callee
    ({ label; index_in_callee_arrow_type; _ } : Mode.Hint.function_argument) :
    term Phrase.segment list =
  let open Nlg in
  let position : term Phrase.segment list =
    match label with
    | Labelled label | Position label -> labelled_argument_words ("~" ^ label)
    | Optional label -> labelled_argument_words ("?" ^ label)
    | Unlabelled ->
      [txt (" " ^ ordinal (index_in_callee_arrow_type + 1) ^ " argument")]
  in
  Nlg.pronoun ~case:Possessive callee :: position

let mutable_part_noun (part : Mode.Hint.mutable_part) :
    term Phrase.segment list * Phrase.number =
  let open Nlg in
  match part with
  | Record_field f -> [txt "mutable field "; code f], Singular
  | Array_elements -> [txt "array elements"], Plural

let containing_text ~modality_relevant (containing : Mode.Hint.containing) =
  let with_modality noun =
    if modality_relevant then noun ^ ", with some modality" else noun
  in
  match containing with
  | Tuple -> "as an element of the tuple"
  | Record (field, Modality) ->
    with_modality ("as field " ^ field ^ " of the record")
  | Array Modality -> with_modality "as an element of the array"
  | Constructor (name, Modality) -> with_modality ("via constructor " ^ name)
  | Structure (_, Modality) -> with_modality "in the structure"

let containment_modality_relevant ?next (m : Message.t) =
  match next with
  | None -> true
  | Some (next : Message.t) -> not (Step_mode.equal m.mode next.mode)

let capture_use_of_next (m : Message.t) (next : Message.t) =
  match m.meaning with
  | Capture { closure = _; closed }
    when not (same_chars (fst m.pinpoint) (fst closed)) ->
    let crosses_axes =
      match m.axis, next.axis with
      | Some a, Some b -> not (same_axis a b)
      | Some _, None | None, Some _ | None, None -> false
    in
    if crosses_axes && same_chars (fst next.pinpoint) (fst closed)
    then Some closed
    else None
  | Capture _ | Nothing_to_say | Unexplained | User_annotation _
  | Signature_argument _ | Fact _ | Reroute _ ->
    None

let desugared_access_callee (callee : Mode.Hint.pinpoint)
    (argument : Mode.Hint.pinpoint) =
  let outer = fst callee and inner = fst argument in
  outer.loc_start.pos_cnum <= inner.loc_start.pos_cnum
  && inner.loc_end.pos_cnum <= outer.loc_end.pos_cnum

let cause_sentences ~source ~(subject : subject) ?next (m : Message.t) =
  let open Nlg in
  let statement ?subject:sentence_subject segments =
    sentence ?subject:sentence_subject (phrase segments)
  in
  let about_subject segments =
    statement ?subject:(sentence_subject subject) segments
  in
  let single_cause () =
    let open Nlg in
    let subj = Nlg.mention ~case:Subject subject in
    let is_ rest = Some (phrase [subj; copula; txt (" " ^ rest)]) in
    let fact_phrase (fact : Meaning.fact) =
      match fact with
      | Mutable_access { part; access } ->
        let verb =
          match access with Access.Read -> "read" | Access.Write -> "written"
        in
        let noun, number = mutable_part_noun part in
        Some
          (phrase
             ((Nlg.mention ~case:Possessive subject :: txt " " :: noun)
             @ [copula_agreeing number; txt (" being " ^ verb)]))
      | Lazy_allocated_on_heap | Module_allocated_on_heap | Legacy_construct _
      | Layout_poly_instantiated ->
        None
      | Lazy_forced -> is_ "a lazy value being forced"
      | Toplevel_expression -> is_ "a top-level expression"
      | Tailcall_function -> is_ "the function of a tail call"
      | Tailcall_argument -> is_ "an argument of a tail call"
      | Function_return_default -> None
      | Stack_allocated ->
        Some (phrase [subj; copula; txt " allocated with "; code "stack_"])
      | Always_dynamic x ->
        let what =
          match (x : Mode.Hint.always_dynamic) with
          | Application -> "function applications"
          | Try_with -> "try-with clauses"
          | Generative_functor -> "generative functor applications"
        in
        Some (phrase [txt (what ^ " are always dynamic")])
      | Has_branches -> Some (phrase [subj; txt " has branches"])
      | Borrowed -> is_ "borrowed"
      | Region_escape (loc, Borrow) ->
        let escape = txt " escapes a borrow region" in
        Some
          (phrase
             [ subj;
               (if Location.is_none loc then escape else ref_source loc [escape])
             ])
      | Quoted_computation -> is_ "the quote of a computation"
      | Spliced -> is_ "spliced"
      | Static_not_guaranteed (Some unit) ->
        Some
          (phrase
             [ code (Compilation_unit.name_as_string unit);
               txt
                 " is neither a core library nor the current library, and only \
                  those can be ";
               mode_const_word (Monadic Staticity) Mode.Staticity.Static ])
      | Static_not_guaranteed None ->
        Some
          (phrase
             [ txt "parameter modules are always ";
               mode_const_word (Monadic Staticity) Mode.Staticity.Dynamic ])
    in
    let reroute_phrase (reroute : Meaning.reroute) =
      match reroute with
      | Mode_crossing ->
        Some
          (phrase
             [ subj;
               txt " crosses modes based on ";
               Nlg.pronoun ~case:Possessive subject;
               txt " type" ])
      | Partial_application_capture -> is_ "captured by a partial application"
      | Allocation _ -> None
      | Contains { containing; contained } ->
        let contained = subject_of_pinpoint ~source contained in
        let modality_relevant = containment_modality_relevant ?next m in
        Some
          (phrase
             [ subj;
               txt " contains ";
               Nlg.mention ~case:Subject contained;
               txt (" (" ^ containing_text ~modality_relevant containing ^ ")")
             ])
      | Contained_by { containing; container } ->
        let container = subject_of_pinpoint ~source container in
        let modality_relevant = containment_modality_relevant ?next m in
        Some
          (phrase
             [ subj;
               copula;
               txt " contained in ";
               Nlg.mention ~case:Subject container;
               txt (" (" ^ containing_text ~modality_relevant containing ^ ")")
             ])
      | Shared_staticity shared ->
        let related =
          match shared with
          | Of_functor loc -> subject_of_loc ~source ~fallback:"the functor" loc
          | Of_functor_parameter loc ->
            subject_of_loc ~source ~fallback:"the functor parameter" loc
        in
        Some
          (phrase
             [ subj;
               txt " shares the staticity of ";
               Nlg.mention ~case:Subject related ])
      | Functor_application loc ->
        let applied = subject_of_loc ~source ~fallback:"the functor" loc in
        Some
          (phrase
             [ subj;
               copula;
               txt " an application of ";
               Nlg.mention ~case:Subject applied ])
      | Functor_applied_at loc ->
        let application =
          subject_of_loc ~source ~fallback:"this application" loc
        in
        Some
          (phrase
             [ subj;
               copula;
               txt " applied at ";
               Nlg.mention ~case:Subject application ])
    in
    match m.meaning with
    | Nothing_to_say -> None
    | Unexplained | User_annotation _ -> None
    | Capture { closure; closed } ->
      if same_chars (fst m.pinpoint) (fst closed)
      then begin
        let closure = subject_of_pinpoint ~source closure in
        Some
          (phrase
             [ subj;
               copula;
               txt " used inside ";
               Nlg.mention ~case:Subject closure ])
      end
      else begin
        let use_mode =
          match next with
          | None -> []
          | Some (next : Message.t) -> (
            match capture_use_of_next m next with
            | Some _ ->
              (txt " as " :: step_mode_segments next.mode) @ [txt " data"]
            | None -> [])
        in
        let closed = subject_of_pinpoint ~source closed in
        Some
          (phrase
             ([subj; txt " uses "; Nlg.mention ~case:Subject closed] @ use_mode))
      end
    | Signature_argument { callee; _ } ->
      let desugared = desugared_access_callee callee m.pinpoint in
      let callee = subject_of_pinpoint ~source callee in
      let source_words =
        if desugared
        then [Nlg.mention ~case:Subject callee]
        else [txt "the signature of "; Nlg.mention ~case:Subject callee]
      in
      let argument_words = [Nlg.mention ~case:Subject subject] in
      Some
        (phrase
           (source_words
           @ (txt " requires " :: argument_words)
           @ (txt " to be " :: step_mode_segments m.mode)))
    | Fact fact -> fact_phrase fact
    | Reroute reroute -> reroute_phrase reroute
  in
  match m.meaning with
  | Fact Lazy_allocated_on_heap ->
    [ about_subject
        [Nlg.mention ~case:Subject subject; copula; txt " a lazy expression"];
      about_subject
        [Nlg.mention ~case:Subject subject; copula; txt " allocated on the heap"]
    ]
  | Fact Module_allocated_on_heap ->
    [ about_subject [Nlg.mention ~case:Subject subject; copula; txt " a module"];
      about_subject
        [Nlg.mention ~case:Subject subject; copula; txt " allocated on the heap"]
    ]
  | Fact (Legacy_construct legacy) ->
    let what =
      match (legacy : Mode.Hint.legacy) with
      | Toplevel -> "a top-level definition"
      | Compilation_unit -> "a compilation unit"
      | Class -> "a class"
      | Quoted -> "a quoted expression's result"
    in
    [ about_subject [Nlg.mention ~case:Subject subject; copula; txt (" " ^ what)];
      statement
        [txt (String.capitalize_ascii what ^ " always has the legacy modes")] ]
  | Fact Layout_poly_instantiated ->
    [ about_subject
        [Nlg.mention ~case:Subject subject; copula; txt " layout-polymorphic"];
      about_subject
        [Nlg.mention ~case:Subject subject; copula; txt " instantiated here"] ]
  | Reroute (Allocation { txt = desc; loc }) ->
    let located words =
      if Location.is_none loc then words else [ref_source loc words]
    in
    let specific, general =
      match (desc : Mode.Hint.allocation_desc) with
      | Unknown -> [copula; txt " an allocation"], None
      | Optional_argument ->
        ( [copula; txt " boxed as an optional argument"],
          Some [txt "boxing an optional argument allocates"] )
      | Function_coercion ->
        ( [copula; txt " partially applied"],
          Some [txt "partial application allocates"] )
      | Float_projection ->
        ( [copula; txt " a float-record projection"],
          Some [txt "a float-record projection allocates"] )
      | Lpoly_captured_environment ->
        [txt " captures a layout-polymorphic environment"], None
      | Captured_by_partial_application ->
        [copula; txt " captured by a partial application"], None
    in
    about_subject (Nlg.mention ~case:Subject subject :: located specific)
    :: Option.to_list (Option.map statement general)
  | Nothing_to_say | Unexplained | User_annotation _ -> []
  | Capture _ | Signature_argument _ | Fact _ | Reroute _ ->
    Option.to_list (Option.map statement (single_cause ()))

module Rule = struct
  type accessed =
    | Field
    | Array_elements

  type mutable_axis =
    | On_contention
    | On_visibility

  type t =
    | Nonportable_closure
    | Portable_function_contends_captures
    | Mutable_write_requirement of accessed * mutable_axis
    | Mutable_read_requirement of accessed * mutable_axis
    | Local_escape

  let explains : t -> Side.t = function
    | Nonportable_closure | Portable_function_contends_captures -> Actual
    | Mutable_write_requirement _ | Mutable_read_requirement _ | Local_escape ->
      Expected

  let accessed (part : Mode.Hint.mutable_part) : accessed =
    match part with Record_field _ -> Field | Array_elements -> Array_elements

  let accessed_nouns : accessed -> string * string = function
    | Field -> "a mutable field", "the value"
    | Array_elements -> "mutable array elements", "the array"

  let sentence : t -> term Phrase.segment list =
    let open Nlg in
    function
    | Nonportable_closure ->
      [ txt "a function that closes over ";
        mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended;
        txt " data";
        copula;
        txt " ";
        mode_const_word (Comonadic Portability)
          Mode.Portability.Const.Nonportable ]
    | Portable_function_contends_captures ->
      [ txt "values used inside a ";
        mode_const_word (Comonadic Portability) Mode.Portability.Const.Portable;
        txt " function";
        copula_agreeing Plural;
        txt " ";
        mode_const_word (Monadic Contention) Mode.Contention.Const.Contended ]
    | Mutable_write_requirement (accessed, mutable_axis) ->
      let part, owner = accessed_nouns accessed in
      txt ("writing " ^ part ^ " requires " ^ owner ^ " to be ")
      ::
      (match mutable_axis with
      | On_contention ->
        [mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended]
      | On_visibility ->
        [ mode_const_word (Monadic Visibility) Mode.Visibility.Const.Write;
          txt " or ";
          mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read_write
        ])
    | Mutable_read_requirement (accessed, mutable_axis) ->
      let part, owner = accessed_nouns accessed in
      txt ("reading " ^ part ^ " requires " ^ owner ^ " to be ")
      ::
      (match mutable_axis with
      | On_contention ->
        [ mode_const_word (Monadic Contention) Mode.Contention.Const.Shared;
          txt " or ";
          mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended
        ]
      | On_visibility ->
        [ mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read;
          txt " or ";
          mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read_write
        ])
    | Local_escape ->
      [ mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
        txt " values cannot escape their region" ]
end

let message_is_closure (m : Message.t) =
  match m.meaning with
  | Capture _ -> true
  | Nothing_to_say | Unexplained | User_annotation _ | Signature_argument _
  | Fact _ | Reroute _ ->
    false

let chain_is_understood (chain : Message.t list) =
  List.for_all
    (fun (m : Message.t) ->
      match m.meaning with
      | Nothing_to_say | Unexplained | User_annotation _ | Capture _ | Fact _
      | Signature_argument _ ->
        true
      | Reroute _ -> false)
    chain

let terminal_message (chain : Message.t list) =
  match List.rev chain with [] -> None | m :: _ -> Some m

let message_mutable_access ~access:wanted (m : Message.t) :
    Mode.Hint.mutable_part option =
  match m.meaning with
  | Fact (Mutable_access { part; access }) ->
    if Access.equal access wanted then Some part else None
  | Fact _ | Nothing_to_say | Unexplained | User_annotation _ | Capture _
  | Signature_argument _ | Reroute _ ->
    None

let terminal_mutable_write chain =
  match terminal_message chain with
  | None -> None
  | Some m -> message_mutable_access ~access:Write m

let terminal_mutable_read chain =
  match terminal_message chain with
  | None -> None
  | Some m -> message_mutable_access ~access:Read m

let message_is_region_escape (m : Message.t) =
  match m.meaning with
  | Fact fact -> Meaning.is_region_escape fact
  | Nothing_to_say | Unexplained | User_annotation _ | Capture _
  | Signature_argument _ | Reroute _ ->
    false

let has_escape_region chain = List.exists message_is_region_escape chain

let detect ~axis ~actual ~expected : Rule.t list =
  let is_portability = same_axis axis (Mode.Axis.P Portability) in
  let is_contention = same_axis axis (Mode.Axis.P Contention) in
  let is_visibility = same_axis axis (Mode.Axis.P Visibility) in
  let rules = [] in
  let rules =
    if
      is_portability && chain_is_understood actual
      && List.exists message_is_closure actual
    then Rule.Nonportable_closure :: rules
    else rules
  in
  let rules =
    if
      is_contention && chain_is_understood actual
      && List.exists message_is_closure actual
    then Rule.Portable_function_contends_captures :: rules
    else rules
  in
  let mutable_axis =
    if is_contention
    then Some Rule.On_contention
    else if is_visibility
    then Some Rule.On_visibility
    else None
  in
  let terminal_access terminal_of =
    match mutable_axis with
    | None -> None
    | Some mutable_axis ->
      begin match terminal_of actual with
      | Some part -> Some (part, mutable_axis)
      | None ->
        begin match terminal_of expected with
        | Some part -> Some (part, mutable_axis)
        | None -> None
        end
      end
  in
  let rules =
    match terminal_access terminal_mutable_write with
    | Some (part, mutable_axis) ->
      Rule.Mutable_write_requirement (Rule.accessed part, mutable_axis) :: rules
    | None -> rules
  in
  let rules =
    match terminal_access terminal_mutable_read with
    | Some (part, mutable_axis) ->
      Rule.Mutable_read_requirement (Rule.accessed part, mutable_axis) :: rules
    | None -> rules
  in
  let rules =
    if has_escape_region actual || has_escape_region expected
    then Rule.Local_escape :: rules
    else rules
  in
  List.rev rules

let pronominalize_one (plan : term Nlg.plan) : term Nlg.plan =
  match Nlg.pronominalize [plan] with
  | [plan] -> plan
  | [] | _ :: _ :: _ ->
    Misc.fatal_error "Mode_diagnostics: pronominalize changed the plan count"

let elaboration (sentence : term Statement.t) :
    Diagnostic.Relation.t * term Nlg.plan =
  Diagnostic.Relation.Elaboration, { statement = Some sentence; children = [] }

let rec elaboration_spine (sentences : term Statement.t list) =
  match sentences with
  | [] -> []
  | sentence :: rest ->
    [ ( Diagnostic.Relation.Elaboration,
        { Nlg.statement = Some sentence; children = elaboration_spine rest } )
    ]

let claims plans = List.map (fun plan -> Diagnostic.Relation.Claim, plan) plans

let plan_rules ~axis ~actual ~expected ~explains :
    (Diagnostic.Relation.t * term Nlg.plan) list =
  detect ~axis ~actual ~expected
  |> List.filter (fun rule -> Side.equal (Rule.explains rule) explains)
  |> List.map (fun rule ->
      elaboration
        (Nlg.sentence ~kind:Diagnostic.Kind.Background
           (phrase (Rule.sentence rule))))

let plan_suggestions ~(expected : Message.t list) :
    (Diagnostic.Relation.t * term Nlg.plan) list =
  let open Nlg in
  let function_return_origin =
    match List.rev expected with
    | [] -> false
    | (origin : Message.t) :: _ ->
      begin match origin.meaning with
      | Fact Function_return_default -> true
      | Fact _ | Nothing_to_say | Unexplained | User_annotation _ | Capture _
      | Signature_argument _ | Reroute _ ->
        false
      end
  in
  if function_return_origin
  then
    [ elaboration
        (sentence ~kind:Diagnostic.Kind.Suggestion
           (phrase
              [ txt "use ";
                code "exclave_";
                txt " to return a ";
                mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
                txt " value" ])) ]
  else []

let plan_partial_application_hint ~(axis : Mode.Axis.packed)
    (result_type : Types.type_expr) :
    (Diagnostic.Relation.t * term Nlg.plan) list =
  match axis with
  | Mode.Axis.P Mode.Axis.Areality -> begin
    let rec non_local_arity sure n ty =
      match Types.get_desc ty with
      | Types.Tarrow ((_, _, res_mode), _, res_ty, _) ->
        begin match
          Mode.Locality.Guts.check_const
            (Mode.Alloc.proj_comonadic Areality res_mode)
        with
        | Some Global -> Some (n + 1, true)
        | (None | Some Local) as res ->
          non_local_arity (sure && Option.is_some res) (n + 1) res_ty
        end
      | _ -> if n = 0 then None else Some (n, sure)
    in
    match non_local_arity true 0 result_type with
    | None -> []
    | Some (n, sure) ->
      let arguments = if n = 1 then "argument" else "arguments" in
      let qualifier = if sure then "will" else "may" in
      [ elaboration
          (Nlg.sentence ~kind:Diagnostic.Kind.Background
             (phrase [Nlg.txt "this is a partial application"]));
        elaboration
          (Nlg.sentence ~kind:Diagnostic.Kind.Suggestion
             (phrase
                [ Nlg.txt
                    ("adding " ^ string_of_int n ^ " more " ^ arguments ^ " "
                   ^ qualifier ^ " make the value non-local") ])) ]
    end
  | Mode.Axis.P _ -> []

let signature_origin (origin : Message.t) (next : Message.t) :
    Mode.Hint.function_argument option =
  match origin.meaning with
  | Unexplained | User_annotation _ ->
    begin match next.meaning with
    | Signature_argument fa ->
      if same_chars (fst origin.pinpoint) (fst fa.callee) then Some fa else None
    | Nothing_to_say | Unexplained | User_annotation _ | Capture _ | Fact _
    | Reroute _ ->
      None
    end
  | Nothing_to_say | Capture _ | Signature_argument _ | Fact _ | Reroute _ ->
    None

let is_explicit_function_annotation ~source (m : Message.t) =
  match m.pinpoint with
  | loc, Function -> (
    match function_binding_before_loc ~source loc with
    | None -> false
    | Some { name = _; prefix } ->
      contains_substring prefix ("@ " ^ Step_mode.name m.mode))
  | ( _,
      ( Unknown | Ident _ | Module | Functor | Functor_parameter | Structure
      | Lazy | Quote | Allocation | Expression | Effect_match | Effect_try
      | Class | Object | Loop | Letop | Cases_result | Pattern
      | Structure_item _ ) ) ->
    false

let plan_expected ~source ~axis ~description (chain : Message.t list) =
  let open Nlg in
  let expectation_sentence ?(therefore = false) (subject : subject) mode =
    sentence ?subject:(sentence_subject subject)
      (phrase
         ((if therefore then [txt "therefore, "] else [])
         @ [Nlg.mention ~case:Subject subject; copula; txt " expected to be "]
         @ described_mode_segments description mode))
  in
  let plan_step ~next (m : Message.t) =
    match m.meaning with
    | Capture { closure; closed } ->
      let target = subject_of_pinpoint ~source m.pinpoint in
      let closure = subject_of_pinpoint ~source closure in
      let closed = subject_of_pinpoint ~source closed in
      let expectation_words =
        match m.axis with
        | Some a when not (same_axis a axis) -> description_words description
        | Some _ | None -> described_mode_segments description m.mode
      in
      [ sentence ?subject:(sentence_subject closure)
          (phrase
             [ Nlg.mention ~case:Subject closure;
               txt " closes over ";
               Nlg.mention ~case:Subject closed ]);
        sentence
          (phrase
             ([txt "therefore, "]
             @ explicit_subject_words target
             @ [copula; txt " also expected to be "]
             @ expectation_words)) ]
    | Signature_argument _ ->
      cause_sentences ~source
        ~subject:(subject_of_pinpoint ~source m.pinpoint)
        ~next m
    | Nothing_to_say | Unexplained | User_annotation _ | Fact _ | Reroute _ ->
      let subject = subject_of_pinpoint ~source m.pinpoint in
      cause_sentences ~source ~subject ~next m
      @ [expectation_sentence ~therefore:true subject m.mode]
  in
  let plan_steps ~from steps =
    let rec go prev = function
      | [] -> []
      | step :: rest -> plan_step ~next:prev step @ go step rest
    in
    go from steps
  in
  let plan_origin (origin : Message.t) =
    let subject = subject_of_pinpoint ~source origin.pinpoint in
    match origin.meaning with
    | Fact (Mutable_access { part; access }) ->
      let action =
        match access with Access.Read -> "reading" | Access.Write -> "writing"
      in
      let object_ : term Phrase.segment list =
        match (part : Mode.Hint.mutable_part) with
        | Record_field field -> [txt "the mutable field "; code field]
        | Array_elements -> [txt "array elements"]
      in
      [ sentence
          (phrase
             ((txt (action ^ " ") :: object_)
             @ [ txt " requires ";
                 Nlg.mention ~case:Subject subject;
                 txt " to be " ]
             @ described_mode_segments description origin.mode)) ]
    | User_annotation annotation ->
      [ sentence ?subject:(sentence_subject subject)
          (phrase
             [ Nlg.mention ~case:Subject subject;
               ref_source annotation
                 (copula :: txt " annotated as "
                 :: described_mode_segments description origin.mode) ]) ]
    | Fact Function_return_default ->
      [ sentence ?subject:(sentence_subject subject)
          (phrase
             ([Nlg.mention ~case:Subject subject; txt " must be "]
             @ described_mode_segments description origin.mode
             @ [txt " to be returned"])) ]
    | Nothing_to_say | Unexplained | Capture _ | Signature_argument _ | Fact _
    | Reroute _ -> (
      let annotation =
        if is_explicit_function_annotation ~source origin
        then
          explicit_mode_annotation_before_loc ~source
            ~mode:(Step_mode.name origin.mode)
            (fst origin.pinpoint)
        else None
      in
      match annotation with
      | Some loc ->
        [ sentence ?subject:(sentence_subject subject)
            (phrase
               [ Nlg.mention ~case:Subject subject;
                 ref_source loc
                   (copula :: txt " annotated as "
                   :: described_mode_segments description origin.mode) ]) ]
      | None ->
        let causes = cause_sentences ~source ~subject origin in
        causes
        @ [ expectation_sentence
              ~therefore:(not (List.is_empty causes))
              subject origin.mode ])
  in
  let sentences =
    match List.rev chain with
    | [] -> []
    | [origin] -> plan_origin origin
    | origin :: (arg_step :: after_arg as towards_subject) ->
      begin match signature_origin origin arg_step with
      | Some fa ->
        let callee = subject_of_pinpoint ~source fa.callee in
        let argument = subject_of_pinpoint ~source arg_step.pinpoint in
        let annotation_spans =
          match origin.meaning with
          | User_annotation annotation -> [annotation]
          | Unexplained -> []
          | Nothing_to_say | Capture _ | Signature_argument _ | Fact _
          | Reroute _ ->
            []
        in
        let desugared = desugared_access_callee fa.callee arg_step.pinpoint in
        let argument_part =
          if desugared
          then [Nlg.mention ~case:Subject argument]
          else begin
            let words = argument_phrase ~callee fa in
            match argument.span with
            | None -> words
            | Some loc -> [ref_source loc words]
          end
        in
        let mode_part =
          let words =
            txt " to be " :: described_mode_segments description arg_step.mode
          in
          match annotation_spans with
          | [] -> words
          | loc :: _ -> [ref_source loc words]
        in
        let source_words =
          if desugared
          then [Nlg.mention ~case:Subject callee]
          else [txt "the signature of "; Nlg.mention ~case:Subject callee]
        in
        sentence
          (phrase
             (source_words @ (txt " requires " :: argument_part) @ mode_part))
        :: plan_steps ~from:arg_step after_arg
      | None -> plan_origin origin @ plan_steps ~from:origin towards_subject
      end
  in
  elaboration_spine sentences

let rec drop_signature_terminal (chain : Message.t list) =
  match chain with
  | [] -> []
  | [s] -> [s]
  | s :: (t :: rest_after_t as rest) ->
    begin match rest_after_t, signature_origin t s with
    | [], Some _ -> [s]
    | [], None | _ :: _, Some _ | _ :: _, None ->
      s :: drop_signature_terminal rest
    end

let plan_actual ~source ~description ~bound ~subject_override
    (chain : Message.t list) : term Nlg.plan list =
  let open Nlg in
  let chain = drop_signature_terminal chain in
  let informative =
    List.exists Message.is_informative chain
    ||
    match chain with
    | [] -> false
    | (m : Message.t) :: _ ->
      Option.is_some
        (explicit_mode_annotation_before_loc ~source
           ~mode:(Step_mode.name m.mode) (fst m.pinpoint))
  in
  if not informative
  then []
  else
    match chain with
    | [] -> []
    | (head : Message.t) :: rest ->
      let head_subject =
        Option.value subject_override
          ~default:(subject_of_pinpoint ~source head.pinpoint)
      in
      let annotation (m : Message.t) =
        match m.meaning with
        | User_annotation loc -> Some loc
        | Nothing_to_say | Unexplained | Capture _ | Signature_argument _
        | Fact _ | Reroute _ ->
          explicit_mode_annotation_before_loc ~source
            ~mode:(Step_mode.name m.mode) (fst m.pinpoint)
      in
      let mode_sentence ?(prefix = "") ?(explicit_subject = false)
          ?(bound = Bound.Exact) (subject : subject) (m : Message.t) =
        let predicate =
          match annotation m, (bound : Bound.t) with
          | Some loc, Exact ->
            [ ref_source loc
                (copula :: txt " annotated as "
                :: described_mode_segments description m.mode) ]
          | Some loc, Loosened ->
            copula
            :: txt (" " ^ Bound.comparative bound ~side:Actual)
            :: ref_source loc [txt "the annotated "]
            :: described_mode_segments description m.mode
          | None, (Exact | Loosened) ->
            copula
            :: txt (" " ^ Bound.comparative bound ~side:Actual)
            :: described_mode_segments description m.mode
        in
        let subject_segments, sentence_subject =
          if explicit_subject
          then explicit_subject_words subject, None
          else [Nlg.mention ~case:Subject subject], sentence_subject subject
        in
        sentence ?subject:sentence_subject
          (phrase
             ((if String.equal prefix "" then [] else [txt prefix])
             @ subject_segments @ predicate))
      in
      let rec proof ~bound_stated_by_parent (messages : Message.t list) =
        match messages with
        | [] -> []
        | (m : Message.t) :: rest ->
          let subject = subject_of_pinpoint ~source m.pinpoint in
          let causes = cause_sentences ~source ~subject ?next:(first rest) m in
          let meaning_states_bound =
            Option.is_some (annotation m)
            ||
            match m.meaning with
            | Signature_argument _ -> true
            | Nothing_to_say | Unexplained | User_annotation _ | Capture _
            | Fact _ | Reroute _ ->
              false
          in
          let bound =
            if bound_stated_by_parent || meaning_states_bound
            then []
            else [mode_sentence ~explicit_subject:true subject m]
          in
          let next_bound_stated =
            match rest with
            | next :: _ -> Option.is_some (capture_use_of_next m next)
            | [] -> false
          in
          bound @ causes @ proof ~bound_stated_by_parent:next_bound_stated rest
      in
      let head_causes =
        cause_sentences ~source ~subject:head_subject ?next:(first rest) head
      in
      let first_rest_bound_stated =
        match rest with
        | next :: _ -> Option.is_some (capture_use_of_next head next)
        | [] -> false
      in
      let proof =
        head_causes @ proof ~bound_stated_by_parent:first_rest_bound_stated rest
      in
      [ { Nlg.statement =
            Some (mode_sentence ~prefix:"but " ~bound head_subject head);
          children = elaboration_spine proof
        } ]

type declared_modalities =
  { written : Mode.Modality.atom Location.loc list;
    mutable_implied : Mode.Modality.Const.t
  }

type modality_provenance =
  | Written of
      { atom : Mode.Modality.atom;
        loc : Location.t
      }
  | Implied of
      { written : Mode.Modality.atom;
        written_loc : Location.t;
        atom : Mode.Modality.atom
      }
  | Implied_by_mutable
  | Unwritten
  | Unknown

type expected_decl =
  { decl_loc : Location.t;
    modalities : Mode.Modality.Const.t;
    written : declared_modalities option
  }

let value_axis_of_error_axis (Mode.Axis.P axis) : Mode.Value.Axis.packed =
  match axis with
  | Mode.Axis.Areality -> P (Comonadic Areality)
  | Forkable -> P (Comonadic Forkable)
  | Yielding -> P (Comonadic Yielding)
  | Linearity -> P (Comonadic Linearity)
  | Statefulness -> P (Comonadic Statefulness)
  | Portability -> P (Comonadic Portability)
  | Uniqueness -> P (Monadic Uniqueness)
  | Visibility -> P (Monadic Visibility)
  | Contention -> P (Monadic Contention)
  | Staticity -> P (Monadic Staticity)

let equal_value_axis (Mode.Value.Axis.P a) (Mode.Value.Axis.P b) : bool =
  match a, b with
  | Mode.Value.Axis.Comonadic x, Mode.Value.Axis.Comonadic y -> (
    match Mode.Axis.equal x y with
    | Misc.Is_eq -> true
    | Misc.Is_not_eq -> false)
  | Mode.Value.Axis.Monadic x, Mode.Value.Axis.Monadic y -> (
    match Mode.Axis.equal x y with
    | Misc.Is_eq -> true
    | Misc.Is_not_eq -> false)
  | Mode.Value.Axis.Comonadic _, Mode.Value.Axis.Monadic _
  | Mode.Value.Axis.Monadic _, Mode.Value.Axis.Comonadic _ ->
    false

let modality_on_axis (value_axis : Mode.Value.Axis.packed)
    (Mode.Modality.Atom (axis, _) : Mode.Modality.atom) : bool =
  equal_value_axis
    (Mode.Modality.Axis.to_value (Mode.Modality.Axis.P axis))
    value_axis

let modality_provenance (declared : declared_modalities option)
    (value_axis : Mode.Value.Axis.packed) : modality_provenance =
  match declared with
  | None -> Unknown
  | Some { written; mutable_implied } -> (
    let on_axis = modality_on_axis value_axis in
    match List.find_opt (fun (w : _ Location.loc) -> on_axis w.txt) written with
    | Some { txt = atom; loc } -> Written { atom; loc }
    | None -> (
      let implied =
        List.find_map
          (fun (w : _ Location.loc) ->
            match List.find_opt on_axis (Typemode.implied_modalities w.txt) with
            | Some atom ->
              Some (Implied { written = w.txt; written_loc = w.loc; atom })
            | None -> None)
          written
      in
      match implied with
      | Some implied -> implied
      | None -> (
        match Mode.Modality.Axis.of_value value_axis with
        | Mode.Modality.Axis.P axis ->
          if
            Mode.Modality.Per_axis.is_id axis
              (Mode.Modality.Const.proj axis mutable_implied)
          then Unwritten
          else Implied_by_mutable)))

let declared_modality (decl : expected_decl) (axis : Mode.Axis.packed) :
    Mode.Modality.atom option =
  match Mode.Modality.Axis.of_value (value_axis_of_error_axis axis) with
  | Mode.Modality.Axis.P maxis ->
    let m = Mode.Modality.Const.proj maxis decl.modalities in
    if Mode.Modality.Per_axis.is_id maxis m
    then None
    else Some (Mode.Modality.Atom (maxis, m))

type origin_lift =
  | No_lift
  | Lift of term Statement.t list

let plan_origin_lift ~source ~anchor_loc ~signature_modality
    ~expected_description (expected : Message.t list) : origin_lift =
  let open Nlg in
  let about_subject (subject : subject) segments =
    sentence ?subject:(sentence_subject subject) (phrase segments)
  in
  match List.rev expected with
  | [origin]
    when same_chars (fst origin.pinpoint) anchor_loc
         && mode_matches_description expected_description origin.mode -> begin
    let subject = subject_of_pinpoint ~source origin.pinpoint in
    match origin.meaning with
    | Fact Function_return_default ->
      Lift
        [ about_subject subject
            [Nlg.mention ~case:Subject subject; copula; txt " returned"] ]
    | Fact (Mutable_access { part; access }) ->
      let verb =
        match access with Access.Read -> "read" | Access.Write -> "written"
      in
      let noun, number = mutable_part_noun part in
      Lift
        [ about_subject subject
            ((Nlg.mention ~case:Possessive subject :: txt " " :: noun)
            @ [copula_agreeing number; txt (" being " ^ verb)]) ]
    | Unexplained ->
      begin match signature_modality with
      | Some (decl_loc, atom, Written _) ->
        Lift
          [ about_subject subject
              [ ref_source decl_loc
                  [ txt "the signature declares ";
                    Nlg.mention ~case:Subject subject;
                    txt " ";
                    modality_word atom ] ] ]
      | Some (decl_loc, atom, Implied { written; written_loc = _; atom = _ }) ->
        Lift
          [ about_subject subject
              [ ref_source decl_loc
                  [ txt "the signature declares ";
                    Nlg.mention ~case:Subject subject;
                    txt " ";
                    modality_word written ] ];
            sentence
              (phrase
                 [modality_word written; txt " implies "; modality_word atom])
          ]
      | Some (decl_loc, _, (Implied_by_mutable | Unwritten | Unknown)) ->
        Lift
          [ about_subject subject
              [ ref_source decl_loc
                  ([ txt "the signature requires ";
                     Nlg.mention ~case:Subject subject;
                     txt " to be " ]
                  @ described_mode_segments expected_description origin.mode) ]
          ]
      | None -> Lift []
      end
    | User_annotation _ -> No_lift
    | Nothing_to_say | Capture _ | Signature_argument _ | Fact _ | Reroute _ ->
      No_lift
    end
  | [origin; arg_step]
    when same_chars (fst arg_step.pinpoint) anchor_loc
         && mode_matches_description expected_description arg_step.mode ->
    begin match signature_origin origin arg_step with
    | Some ({ callee; _ } as fa) ->
      let annotation_spans =
        match origin.meaning with
        | User_annotation annotation -> [annotation]
        | Nothing_to_say | Unexplained | Capture _ | Signature_argument _
        | Fact _ | Reroute _ ->
          []
      in
      let desugared = desugared_access_callee callee arg_step.pinpoint in
      let callee_subject = subject_of_pinpoint ~source callee in
      let argument = subject_of_pinpoint ~source arg_step.pinpoint in
      let source_words =
        let words =
          if desugared
          then [Nlg.mention ~case:Subject callee_subject]
          else
            [txt "the signature of "; Nlg.mention ~case:Subject callee_subject]
        in
        match annotation_spans with
        | [] -> words
        | loc :: _ -> [ref_source loc words]
      in
      let argument_words =
        if desugared
        then [Nlg.mention ~case:Subject argument]
        else begin
          let words = argument_phrase ~callee:callee_subject fa in
          match argument.span with
          | None -> words
          | Some loc -> [ref_source loc words]
        end
      in
      Lift
        [ sentence
            (phrase
               (source_words
               @ (txt " requires " :: argument_words)
               @ (txt " to be " :: description_words expected_description))) ]
    | None -> No_lift
    end
  | [] | [_] | [_; _] | _ :: _ :: _ :: _ -> No_lift

let conjoin_segments (parts : term Phrase.segment list list) :
    term Phrase.segment list =
  let open Nlg in
  match parts with
  | [] -> []
  | [p] -> p
  | [p1; p2] -> p1 @ (txt " and " :: p2)
  | first :: rest ->
    let rec go = function
      | [] -> []
      | [last] -> txt ", and " :: last
      | p :: rest -> (txt ", " :: p) @ go rest
    in
    first @ go rest

type constructor_argument =
  { argument_type : string;
    argument_loc : Location.t option;
    crossing : Mode.Crossing.t
  }

let fully_crosses (axis : Mode.Axis.packed) (crossing : Mode.Crossing.t) : bool
    =
  match
    Mode.Crossing.Axis.of_modality
      (Mode.Modality.Axis.of_value (value_axis_of_error_axis axis))
  with
  | Mode.Crossing.Axis.P ax ->
    Mode.Crossing.Per_axis.le ax
      (Mode.Crossing.proj ax crossing)
      (Mode.Crossing.Per_axis.min ax)

type actuality_fallback = Arguments_do_not_cross of constructor_argument list

type extra_rules =
  { for_actual : (Diagnostic.Relation.t * term Nlg.plan) list;
    for_expected : (Diagnostic.Relation.t * term Nlg.plan) list
  }

let no_extra_rules = { for_actual = []; for_expected = [] }

let plan_axis ~extra_rules ~actuality_fallback ~subject_override ~source ~axis
    ~error_loc ~expected_decl ~pronouns ~(actual : Message.t list)
    ~(expected : Message.t list) ~actual_description ~expected_description
    ~actual_bound ~expected_bound : term Nlg.plan list =
  let open Nlg in
  let anchor =
    match actual with (m : Message.t) :: _ -> Some m.pinpoint | [] -> None
  in
  let anchor_loc =
    match anchor with Some (loc, _) -> loc | None -> error_loc
  in
  let signature_modality =
    match expected_decl with
    | None -> None
    | Some decl -> (
      match declared_modality decl axis with
      | None -> None
      | Some atom ->
        let provenance =
          modality_provenance decl.written (value_axis_of_error_axis axis)
        in
        Some (decl.decl_loc, atom, provenance))
  in
  let origin_lift =
    plan_origin_lift ~source ~anchor_loc ~signature_modality
      ~expected_description expected
  in
  let subject =
    match (subject_override : subject option) with
    | Some subject -> subject
    | None -> (
      match anchor with
      | Some pinpoint -> subject_of_pinpoint ~source pinpoint
      | None -> subject_of_loc ~source ~fallback:"this value" error_loc)
  in
  let expected_proof =
    match origin_lift with
    | Lift sentences -> elaboration_spine sentences
    | No_lift ->
      plan_expected ~source ~axis ~description:expected_description expected
  in
  let actual_rules =
    plan_rules ~axis ~actual ~expected ~explains:Actual @ extra_rules.for_actual
  in
  let expected_extras =
    plan_rules ~axis ~actual ~expected ~explains:Expected
    @ extra_rules.for_expected @ plan_suggestions ~expected
  in
  let actuality_explanation =
    match actuality_fallback with
    | None -> []
    | Some (Arguments_do_not_cross arguments) ->
      let axis_name = Format_doc.asprintf "%a" Mode.Axis.print_packed axis in
      let culprits =
        List.filter
          (fun (arg : constructor_argument) ->
            not (fully_crosses axis arg.crossing))
          arguments
      in
      let words =
        match culprits with
        | [] ->
          [ txt "the argument types of ";
            Nlg.mention ~case:Subject subject;
            txt (" do not all cross " ^ axis_name) ]
        | culprits ->
          let types =
            conjoin_segments
              (List.map
                 (fun (arg : constructor_argument) ->
                   let text = [code arg.argument_type] in
                   match arg.argument_loc with
                   | Some loc -> [ref_source loc text]
                   | None -> text)
                 culprits)
          in
          let noun, verb =
            match culprits with
            | [_] -> "the argument type ", " does not cross "
            | _ -> "the argument types ", " do not cross "
          in
          (txt noun :: types)
          @ [ txt " of ";
              Nlg.mention ~case:Subject subject;
              txt (verb ^ axis_name) ]
      in
      [sentence (phrase words)]
  in
  let actual_beat =
    match
      plan_actual ~source ~description:actual_description ~bound:actual_bound
        ~subject_override actual
    with
    | beat :: _ -> { beat with Nlg.children = beat.Nlg.children @ actual_rules }
    | [] ->
      { Nlg.statement =
          Some
            (sentence ?subject:(sentence_subject subject)
               (phrase
                  (txt "but "
                  :: Nlg.mention ~case:Subject subject
                  :: copula
                  :: txt (" " ^ Bound.comparative actual_bound ~side:Actual)
                  :: description_words actual_description)));
        children = elaboration_spine actuality_explanation @ actual_rules
      }
  in
  let expected_beat =
    { Nlg.statement =
        Some
          (sentence ?subject:(sentence_subject subject)
             (phrase
                (Nlg.mention ~case:Subject subject
                :: copula
                :: txt
                     (" expected to be "
                     ^ Bound.comparative expected_bound ~side:Expected)
                :: description_words expected_description)));
      children = expected_proof @ expected_extras
    }
  in
  let beats = [expected_beat; actual_beat] in
  match (pronouns : Pronouns.t) with
  | Use_pronouns -> pronominalize beats
  | Names_only -> beats

type requirement_key =
  { callee_loc : Location.t;
    label : Mode.Hint.argument_label;
    index : int;
    argument_loc : Location.t
  }

let requirement_key (expected : Message.t list) : requirement_key option =
  match List.rev expected with
  | [origin; arg_step] ->
    begin match signature_origin origin arg_step with
    | Some { callee; label; index_in_callee_arrow_type; _ } ->
      Some
        { callee_loc = fst callee;
          label;
          index = index_in_callee_arrow_type;
          argument_loc = fst arg_step.pinpoint
        }
    | None -> None
    end
  | [] | [_] | _ :: _ :: _ -> None

let same_argument_label (left : Mode.Hint.argument_label)
    (right : Mode.Hint.argument_label) =
  match left, right with
  | Unlabelled, Unlabelled -> true
  | Labelled left, Labelled right
  | Optional left, Optional right
  | Position left, Position right ->
    String.equal left right
  | Unlabelled, (Labelled _ | Optional _ | Position _)
  | Labelled _, (Unlabelled | Optional _ | Position _)
  | Optional _, (Unlabelled | Labelled _ | Position _)
  | Position _, (Unlabelled | Labelled _ | Optional _) ->
    false

let same_requirement_key a b =
  same_chars a.callee_loc b.callee_loc
  && same_chars a.argument_loc b.argument_loc
  && a.index = b.index
  && same_argument_label a.label b.label

type axis_input =
  { axis : Mode.Axis.packed;
    actual : Mode.Hint_chain.t;
    expected : Mode.Hint_chain.t;
    actual_description : Mode.Mode_description.t;
    expected_description : Mode.Mode_description.t;
    actual_bound : Bound.t;
    expected_bound : Bound.t
  }

type prepared_axis =
  { input : axis_input;
    expected_messages : Message.t list;
    actual_messages : Message.t list;
    key : requirement_key option;
    has_story : bool
  }

type story =
  { frame : term Nlg.plan;
    axes : string list
  }

let prose frame : story = { frame; axes = [] }

let frames (stories : story list) = List.map (fun s -> s.frame) stories

let blamed_axes (stories : story list) =
  List.concat_map (fun s -> s.axes) stories

let render_error ?extra_rules ?actuality_fallback ?subject_override ~source
    ~error_loc ~expected_decl ~pronouns (axes : axis_input list)
    : (axis_input list * term Nlg.plan) list =
  let prepared =
    List.map
      (fun input ->
        let expected_messages = Message.of_chain ~source input.expected in
        let actual_messages = Message.of_chain ~source input.actual in
        { input;
          expected_messages;
          actual_messages;
          key = requirement_key expected_messages;
          has_story = List.exists Message.is_informative actual_messages
        })
      axes
  in
  let arr = Array.of_list prepared in
  let n = Array.length arr in
  let members = Array.make n [] in
  let member_of = Array.make n false in
  let claimed = Array.make n false in
  for i = 0 to n - 1 do
    match arr.(i).key with
    | None -> ()
    | Some key_i ->
      if not claimed.(i)
      then begin
        let class_ = ref [i] in
        for j = i + 1 to n - 1 do
          match arr.(j).key with
          | Some key_j
            when (not claimed.(j)) && same_requirement_key key_i key_j ->
            class_ := j :: !class_
          | Some _ | None -> ()
        done;
        match List.rev !class_ with
        | [] | [_] -> ()
        | class_ ->
          List.iter (fun j -> claimed.(j) <- true) class_;
          let rep =
            match List.find_opt (fun j -> arr.(j).has_story) class_ with
            | Some j -> j
            | None -> ( match class_ with j :: _ -> j | [] -> assert false)
          in
          let ms =
            List.filter (fun j -> j <> rep && not arr.(j).has_story) class_
          in
          if not (List.is_empty ms)
          then begin
            members.(rep) <- ms;
            List.iter (fun j -> member_of.(j) <- true) ms
          end
      end
  done;
  begin
    let silent i =
      (not claimed.(i))
      && (not member_of.(i))
      && (not (Option.is_some arr.(i).key))
      && (not arr.(i).has_story)
      && not (List.exists Message.is_informative arr.(i).expected_messages)
    in
    let rep = ref None in
    for i = 0 to n - 1 do
      match !rep with
      | None -> if not member_of.(i) then rep := Some i
      | Some rep when i <> rep && silent i ->
        members.(rep) <- members.(rep) @ [i];
        member_of.(i) <- true
      | Some _ -> ()
    done
  end;
  let plan_one (p : prepared_axis) =
    plan_axis
      ~extra_rules:
        (match extra_rules with
        | None -> no_extra_rules
        | Some rules -> rules p.input.axis)
      ~actuality_fallback ~subject_override ~source ~axis:p.input.axis
      ~error_loc ~expected_decl ~pronouns ~actual:p.actual_messages
      ~expected:p.expected_messages
      ~actual_description:p.input.actual_description
      ~expected_description:p.input.expected_description
      ~actual_bound:p.input.actual_bound ~expected_bound:p.input.expected_bound
  in
  Array.mapi
    (fun i (p : prepared_axis) ->
      if member_of.(i)
      then []
      else begin
        let beats = plan_one p in
        let beats =
          beats @ List.concat_map (fun j -> plan_one arr.(j)) members.(i)
        in
        let inputs = p.input :: List.map (fun j -> arr.(j).input) members.(i) in
        let story = { Nlg.statement = None; children = claims beats } in
        [inputs, story]
      end)
    arr
  |> Array.to_list |> List.concat

type modality_subject =
  | Modality_item of string
  | Modality_field of string
  | Modality_constructor_arg of
      { constructor : string;
        index : int
      }

type side_vocabulary =
  { expected_name : term Phrase.segment list;
    actual_name : term Phrase.segment list
  }

let side_name sides side =
  Side.select side ~expected:sides.expected_name ~actual:sides.actual_name

let declaration_sides : side_vocabulary =
  { expected_name = [Nlg.txt "the expected declaration"];
    actual_name = [Nlg.txt "the actual declaration"]
  }

type modality_side =
  { atom : Mode.Modality.atom option;
    provenance : modality_provenance;
    loc : Location.t option
  }

type modality_requirement =
  | Exact_match
  | At_least_as_strong

type modality_input =
  { axis : Mode.Value.Axis.packed;
    subject : modality_subject;
    expected : modality_side;
    actual : modality_side;
    requirement : modality_requirement
  }

type presence_property =
  | Mutability
  | Atomicity

type presence_input =
  { property : presence_property;
    subject : modality_subject;
    declared_on : Side.t;
    expected_loc : Location.t option;
    actual_loc : Location.t option
  }

type functor_shape =
  | Functor_expected of
      { parameters : int;
        units : int
      }
  | Functor_unexpected of
      { parameters : int;
        units : int;
        abstract : bool
      }
  | Generativity of { module_parameter_on : Side.t }

type crossing_difference =
  | Attribute_on_one_side of { declared_on : Side.t }
  | Bounds_differ of
      { expected_only : string list;
        actual_only : string list;
        differing : (string * string * string) list;
        expected_with : string;
        actual_with : string
      }

type crossing_input =
  { difference : crossing_difference;
    expected_loc : Location.t option;
    actual_loc : Location.t option
  }

let collapse_whitespace (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\n' | '\t' -> if Buffer.length buf > 0 then pending := true
      | c ->
        if !pending
        then begin
          Buffer.add_char buf ' ';
          pending := false
        end;
        Buffer.add_char buf c)
    s;
  Buffer.contents buf

let crossing_on_axis (Mode.Value.Axis.P vax as packed) (t : Mode.Crossing.t) :
    (string * string) option =
  match Mode.Crossing.Axis.of_modality (Mode.Modality.Axis.of_value packed) with
  | Mode.Crossing.Axis.P cax ->
    let value = Mode.Crossing.proj cax t in
    if Mode.Crossing.Per_axis.le cax (Mode.Crossing.Per_axis.max cax) value
    then None
    else
      Some
        ( Format_doc.asprintf "%a" Mode.Value.Axis.print vax,
          Format_doc.asprintf "%a" (Mode.Crossing.Per_axis.print cax) value )

let crossing_bounds_difference (expected : Mode.Crossing.t)
    (actual : Mode.Crossing.t) =
  List.fold_left
    (fun (expected_only, actual_only, differing) axis ->
      match crossing_on_axis axis expected, crossing_on_axis axis actual with
      | None, None -> expected_only, actual_only, differing
      | Some (name, _), None -> name :: expected_only, actual_only, differing
      | None, Some (name, _) -> expected_only, name :: actual_only, differing
      | Some (name, e), Some (_, a) ->
        if String.equal e a
        then expected_only, actual_only, differing
        else expected_only, actual_only, (name, e, a) :: differing)
    ([], [], []) Mode.Value.Axis.all
  |> fun (e, a, d) -> List.rev e, List.rev a, List.rev d

let render_crossing_error ~sides (input : crossing_input) : term Nlg.plan =
  let open Nlg in
  let attribute = "[@@unsafe_allow_any_mode_crossing]" in
  let spans =
    match input.actual_loc, input.expected_loc with
    | Some l, _ | None, Some l -> [l]
    | None, None -> []
  in
  let subject = subject ?span:(first spans) [Phrase.Text "the declarations"] in
  let header =
    sentence ?subject:(sentence_subject subject)
      (phrase
         [ Nlg.mention ~case:Subject subject;
           txt " disagree on ";
           concept_word Unsafe_mode_crossing ])
  in
  let located loc words =
    match loc with None -> words | Some l -> [ref_source l words]
  in
  let children =
    match input.difference with
    | Attribute_on_one_side { declared_on } ->
      let declaring = side_name sides declared_on in
      let other = side_name sides (Side.other declared_on) in
      let side_loc side =
        Side.select side ~expected:input.expected_loc ~actual:input.actual_loc
      in
      let declaring_loc = side_loc declared_on in
      let other_loc = side_loc (Side.other declared_on) in
      [ elaboration
          (sentence
             (phrase
                (located declaring_loc
                   ((txt "only " :: declaring)
                   @ [txt " is marked "; code attribute]))));
        ( Diagnostic.Relation.Claim,
          { Nlg.statement =
              Some
                (sentence
                   (phrase
                      (located other_loc
                         ((txt "but " :: other) @ [txt " is not"]))));
            children =
              [ elaboration
                  (sentence ~kind:Diagnostic.Kind.Background
                     (phrase
                        [ code attribute;
                          txt
                            " is part of a type's interface: both declarations \
                             must carry it" ])) ]
          } ) ]
    | Bounds_differ
        { expected_only; actual_only; differing; expected_with; actual_with } ->
      let name_of_expected = sides.expected_name in
      let name_of_actual = sides.actual_name in
      let axes_line ~name ~loc axes =
        match axes with
        | [] -> None
        | axes ->
          let plural = match axes with [_] -> " axis" | _ -> " axes" in
          Some
            (elaboration
               (sentence
                  (phrase
                     (located loc
                        ((txt "only " :: name)
                        @ [ txt
                              (" crosses the " ^ String.concat ", " axes
                             ^ plural) ])))))
      in
      let differing_lines =
        List.map
          (fun (axis, e, a) ->
            elaboration
              (sentence
                 (phrase
                    (txt ("both cross the " ^ axis ^ " axis, but ")
                     :: located input.expected_loc name_of_expected
                    @ txt " to " :: code e :: txt " and "
                      :: located input.actual_loc name_of_actual
                    @ [txt " to "; code a]))))
          differing
      in
      let with_lines =
        if String.equal expected_with actual_with
        then []
        else
          let line ~name ~loc with_ =
            let words =
              (txt "the crossing in " :: name)
              @
              if String.equal with_ ""
              then [txt " has no "; concept_word With_bounds]
              else [txt " includes "; code with_]
            in
            elaboration (sentence (phrase (located loc words)))
          in
          [ line ~name:sides.expected_name ~loc:input.expected_loc expected_with;
            line ~name:sides.actual_name ~loc:input.actual_loc actual_with ]
      in
      let claims =
        List.filter_map
          (fun line -> line)
          [ axes_line ~name:sides.expected_name ~loc:input.expected_loc
              expected_only;
            axes_line ~name:sides.actual_name ~loc:input.actual_loc actual_only
          ]
        @ differing_lines @ with_lines
      in
      let educate =
        elaboration
          (sentence ~kind:Diagnostic.Kind.Background
             (phrase
                [ txt "two declarations that both use ";
                  code attribute;
                  txt " must claim exactly the same mode crossing" ]))
      in
      claims @ [educate]
  in
  pronominalize_one { Nlg.statement = Some header; children }

let render_functor_shape ~in_parameter (shape : functor_shape) : term Nlg.plan =
  let open Nlg in
  let plural n = if n = 1 then " parameter" else " parameters" in
  let this_module =
    if in_parameter then "the parameter's module type" else "this module"
  in
  let this_functor =
    if in_parameter then "the parameter's module type" else "this functor"
  in
  let shape_words ~parameters ~units =
    let count = string_of_int parameters ^ plural parameters in
    match parameters, units with
    | 0, _ -> [concept_word Generative_functor; txt " functor"]
    | _, 0 -> [txt ("functor of " ^ count)]
    | _, _ -> [concept_word Generative_functor; txt (" functor of " ^ count)]
  in
  let claim, contrast, extra =
    match shape with
    | Functor_expected { parameters; units } ->
      ( [txt (this_module ^ " is a structure")],
        txt "but the signature expects a " :: shape_words ~parameters ~units,
        [] )
    | Functor_unexpected { parameters; units; abstract } ->
      ( txt (this_module ^ " is a ") :: shape_words ~parameters ~units,
        [ txt
            (if abstract
             then
               "but the signature expects a module of an abstract module type"
             else "but the signature expects a structure") ],
        if in_parameter
        then []
        else
          let applied_to =
            let count = string_of_int parameters ^ plural parameters in
            match parameters, units with
            | 0, _ -> [txt "apply it to "; code "()"]
            | _, 0 -> [txt ("apply it to its " ^ count)]
            | _, _ -> [txt ("apply it to its " ^ count ^ " and "); code "()"]
          in
          [ elaboration
              (sentence ~kind:Diagnostic.Kind.Suggestion
                 (phrase
                    (txt "if you meant to use the functor's result, "
                    :: applied_to))) ] )
    | Generativity { module_parameter_on } ->
      let claim, contrast =
        match module_parameter_on with
        | Side.Expected ->
          ( [txt (this_functor ^ " takes "); code "()"; txt " as a parameter"],
            [txt "but the signature declares a module parameter there"] )
        | Side.Actual ->
          ( [txt (this_functor ^ " takes a module parameter")],
            [txt "but the signature declares "; code "()"; txt " there"] )
      in
      ( claim,
        contrast,
        [ elaboration
            (sentence ~kind:Diagnostic.Kind.Background
               (phrase
                  [ txt "a functor that takes ";
                    code "()";
                    txt " is ";
                    concept_word Generative_functor;
                    txt " and one that takes a module parameter is ";
                    concept_word Applicative_functor;
                    txt "; its declaration fixes which" ])) ] )
  in
  let story =
    { Nlg.statement = Some (sentence (phrase claim));
      children =
        [ ( Diagnostic.Relation.Claim,
            { Nlg.statement = Some (sentence (phrase contrast));
              children = extra
            } ) ]
    }
  in
  story

let render_presence_error ~sides (input : presence_input) : term Nlg.plan =
  let open Nlg in
  let property_name =
    match input.property with
    | Mutability -> "mutability"
    | Atomicity -> "atomicity"
  in
  let subject : subject =
    let spans =
      match input.actual_loc, input.expected_loc with
      | Some l, _ | None, Some l -> [l]
      | None, None -> []
    in
    match input.subject with
    | Modality_item name -> subject ?span:(first spans) [Phrase.Code name]
    | Modality_field name ->
      subject ?span:(first spans) [Phrase.Text "the field "; Phrase.Code name]
    | Modality_constructor_arg { constructor; index } ->
      subject ?span:(first spans)
        [ Phrase.Text ("the " ^ ordinal index ^ " argument of ");
          Phrase.Code constructor ]
  in
  let side ~declares ~name loc =
    let words =
      match input.property, declares with
      | Mutability, true -> [copula; txt " declared "; code "mutable"]
      | Mutability, false -> [copula; txt " immutable"]
      | Atomicity, true -> [copula; txt " declared "; concept_word Atomic_field]
      | Atomicity, false -> [copula; txt " not atomic"]
    in
    let words = words @ (txt " in " :: name) in
    match loc with None -> words | Some l -> [ref_source l words]
  in
  let header =
    sentence ?subject:(sentence_subject subject)
      (phrase
         [ txt "the declarations of ";
           Nlg.mention ~case:Subject subject;
           txt (" disagree on " ^ property_name) ])
  in
  let expected_line =
    sentence ?subject:(sentence_subject subject)
      (phrase
         (Nlg.mention ~case:Subject subject
         :: side
              ~declares:(Side.equal input.declared_on Expected)
              ~name:sides.expected_name input.expected_loc))
  in
  let actual_line =
    sentence
      (phrase
         (txt "but"
         :: side
              ~declares:(Side.equal input.declared_on Actual)
              ~name:sides.actual_name input.actual_loc))
  in
  let educate =
    let words =
      match input.property with
      | Mutability ->
        [ txt "a field's ";
          code "mutable";
          txt
            " keyword is part of the record's definition and must match on \
             both sides" ]
      | Atomicity ->
        [ txt "a mutable field's ";
          concept_word Atomic_field;
          txt
            " attribute is part of the record's definition and must match on \
             both sides" ]
    in
    [elaboration (sentence ~kind:Diagnostic.Kind.Background (phrase words))]
  in
  let story =
    { Nlg.statement = Some header;
      children =
        [ elaboration expected_line;
          ( Diagnostic.Relation.Claim,
            { Nlg.statement = Some actual_line; children = educate } ) ]
    }
  in
  pronominalize_one story

let render_modality_error ~sides (input : modality_input) : term Nlg.plan =
  let open Nlg in
  let axis_name =
    match input.axis with
    | Mode.Value.Axis.P ax -> Format_doc.asprintf "%a" Mode.Value.Axis.print ax
  in
  let subject : subject =
    let spans =
      match input.actual.loc, input.expected.loc with
      | Some l, _ | None, Some l -> [l]
      | None, None -> []
    in
    match input.subject with
    | Modality_item name -> subject ?span:(first spans) [Phrase.Code name]
    | Modality_field name ->
      subject ?span:(first spans) [Phrase.Text "the field "; Phrase.Code name]
    | Modality_constructor_arg { constructor; index } ->
      subject ?span:(first spans)
        [ Phrase.Text ("the " ^ ordinal index ^ " argument of ");
          Phrase.Code constructor ]
  in
  let side ~name ({ atom; provenance; loc } : modality_side) :
      term Phrase.segment list * term Statement.clause option =
    let in_declaration = txt " in " :: name in
    let effective atom =
      copula :: txt " " :: modality_word atom :: in_declaration
    in
    let words, clause =
      match provenance, atom with
      | Written { atom; loc = _ }, _ ->
        copula :: txt " declared " :: modality_word atom :: in_declaration, None
      | Implied { written; written_loc; atom }, _ ->
        ( effective atom,
          Some
            (Statement.Subordinate
               [ txt "because ";
                 ref_source written_loc [txt "its "; modality_word written];
                 txt " implies ";
                 modality_word atom ]) )
      | Implied_by_mutable, Some atom ->
        ( effective atom,
          Some
            (Statement.Subordinate
               [txt "because mutable fields imply "; modality_word atom]) )
      | (Unwritten | Unknown), Some atom -> effective atom, None
      | (Implied_by_mutable | Unwritten | Unknown), None ->
        txt (" has no " ^ axis_name ^ " modality") :: in_declaration, None
    in
    let words =
      match loc with None -> words | Some l -> [ref_source l words]
    in
    words, clause
  in
  let header =
    sentence ?subject:(sentence_subject subject)
      (phrase
         [ txt "the declarations of ";
           Nlg.mention ~case:Subject subject;
           txt (" disagree on " ^ axis_name) ])
  in
  let expected_line =
    let words, clause = side ~name:sides.expected_name input.expected in
    sentence ?subject:(sentence_subject subject) ?clause
      (phrase (Nlg.mention ~case:Subject subject :: words))
  in
  let actual_line =
    let words, clause = side ~name:sides.actual_name input.actual in
    sentence ?clause (phrase (Nlg.mention ~case:Subject subject :: words))
  in
  let educate =
    match input.requirement with
    | At_least_as_strong -> []
    | Exact_match ->
      [ elaboration
          (sentence ~kind:Diagnostic.Kind.Background
             (phrase
                [ txt
                    "field and constructor-argument modalities must match \
                     exactly on both sides" ])) ]
  in
  let story =
    { Nlg.statement = Some header;
      children =
        [ elaboration expected_line;
          ( Diagnostic.Relation.Claim,
            { Nlg.statement = Some actual_line; children = educate } ) ]
    }
  in
  pronominalize_one story

let prepare_axis ~source
    ({ axis;
       actual_chain;
       expected_chain;
       actual_description;
       expected_description;
       actual_loosening;
       expected_loosening
     } :
      Mode.axis_error) =
  let actual_chain = normalize ~source actual_chain in
  let expected_chain = normalize ~source expected_chain in
  { axis;
    actual = actual_chain;
    expected = expected_chain;
    actual_description;
    expected_description;
    actual_bound = Bound.of_loosening actual_loosening;
    expected_bound = Bound.of_loosening expected_loosening
  }

let axis_name (input : axis_input) : string =
  Format_doc.asprintf "%a" Mode.Axis.print_packed input.axis

let modality_story ~declared_modalities_at ~sides (input : modality_input) :
    story =
  let argument =
    match input.subject with
    | Modality_constructor_arg { index; _ } -> Some index
    | Modality_item _ | Modality_field _ -> None
  in
  let resolve (side : modality_side) : modality_side =
    match side.loc with
    | None -> side
    | Some loc ->
      { side with
        provenance =
          modality_provenance (declared_modalities_at loc ~argument) input.axis
      }
  in
  let input =
    { input with
      expected = resolve input.expected;
      actual = resolve input.actual
    }
  in
  { frame = render_modality_error ~sides input;
    axes =
      [ (match input.axis with
        | Mode.Value.Axis.P ax ->
          Format_doc.asprintf "%a" Mode.Value.Axis.print ax) ]
  }

let mode_stories ?extra_rules ?actuality_fallback ?subject_override ~source
    ~error_loc ~expected_decl ~pronouns (axes : Mode.axis_error list) : story list
    =
  List.map (prepare_axis ~source) axes
  |> render_error ?extra_rules ?actuality_fallback ?subject_override ~source
       ~error_loc ~expected_decl ~pronouns
  |> List.map (fun (inputs, frame) ->
      { frame; axes = List.map axis_name inputs })

let term_entry ~(documentation : Documentation.lookup) (t : term) :
    Diagnostic.Glossary.Entry.t =
  let undocumented ~term ~category =
    { Diagnostic.Glossary.Entry.term; category; description = ""; url = None }
  in
  let entry ~term ~category (documented : Documentation.t option) =
    match documented with
    | Some { description; url } ->
      { Diagnostic.Glossary.Entry.term; category; description; url }
    | None -> undocumented ~term ~category
  in
  let term = term_display t in
  match t with
  | Mode_term mode ->
    let documented =
      match Mode.alloc_atom_of_hint_mode mode with
      | Some atom -> documentation.of_mode atom
      | None -> None
    in
    entry ~term ~category:"Mode" documented
  | Modality_term atom ->
    entry ~term ~category:"Modality" (documentation.of_modality atom)
  | Concept_term concept ->
    let category, description, url =
      match concept with
      | Unsafe_mode_crossing ->
        ( "Mode crossing",
          "A record or variant marked [@@unsafe_allow_any_mode_crossing] \
           claims the mode crossing written in its kind annotation, whatever \
           its definition would justify; the compiler takes the claim on \
           trust.",
          Some "https://oxcaml.org/documentation/kinds/types/" )
      | With_bounds ->
        ( "Kind",
          "The part of a kind that makes a type's mode crossing depend on the \
           types it contains: 'a list crosses portability only when 'a does, \
           written `with 'a`.",
          Some "https://oxcaml.org/documentation/kinds/intro/" )
      | Applicative_functor ->
        ( "Functor",
          "An applicative functor takes a module parameter; applying it twice \
           to the same module yields equal types.",
          Some "https://ocaml.org/manual/5.2/moduleexamples.html" )
      | Generative_functor ->
        ( "Functor",
          "A generative functor takes a () parameter; every application yields \
           fresh types, so it must be applied explicitly.",
          Some "https://ocaml.org/manual/5.2/generativefunctors.html" )
      | Atomic_field ->
        ( "Attribute",
          "A mutable record field marked [@atomic] is read and written with \
           atomic operations, so it can be accessed even when the record is \
           contended.",
          None )
    in
    { Diagnostic.Glossary.Entry.term; category; description; url }

module Inclusion = struct
  open Includemod.Error

  type parameter_kind =
    | Unit_parameter
    | Module_parameter

  type missing_item =
    { noun : string;
      name : string;
      decl_loc : Location.t
    }

  type leaf =
    | Mode_leaf of
        { pinpoint : Mode.Hint.pinpoint;
          error : Mode.Value.error;
          expected_decl : expected_decl option
        }
    | Modality_leaf of modality_input
    | Missing_leaf of missing_item
    | Presence_leaf of presence_input
    | Crossing_leaf of crossing_input
    | Functor_shape_leaf of functor_shape
    | Functor_arity_leaf of
        { position : int;
          surplus_on : Side.t;
          parameter : parameter_kind
        }
    | Zero_alloc_leaf of
        { expected_loc : Location.t option;
          actual_loc : Location.t option
        }

  type direction =
    | Actual_not_included
    | Expected_not_included

  type item =
    | Item_module of string
    | Item_module_type of string
    | Item_type of string
    | Item_extension_constructor of
        { exception_ : bool;
          name : string
        }
    | Item_functor_parameter of int option
    | Direction of direction

  type tree =
    | Leaf of leaf
    | Item of
        { item : item;
          got_loc : Location.t option;
          expected_loc : Location.t option;
          children : tree list
        }

  let modality_input ?(orientation = Orientation.Got_is_actual) ~subject
      ~expected_loc ~actual_loc ~requirement
      (Mode.Modality.Error (ax, { left; right }) : Mode.Modality.error) :
      modality_input =
    let side m loc : modality_side =
      { atom =
          (if Mode.Modality.Per_axis.is_id ax m
           then None
           else Some (Mode.Modality.Atom (ax, m)));
        provenance = Unknown;
        loc
      }
    in
    let expected, actual =
      Orientation.expected_and_actual orientation ~got:(side left actual_loc)
        ~expected:(side right expected_loc)
    in
    { axis = Mode.Modality.Axis.to_value (Mode.Modality.Axis.P ax);
      subject;
      expected;
      actual;
      requirement
    }

  let equate_modality_input ~orientation ~subject ~expected_loc ~actual_loc
      ((_step, error) : Mode.Modality.equate_error) =
    modality_input ~orientation ~subject ~expected_loc ~actual_loc
      ~requirement:Exact_match error

  let field_leaves ~orientation (changes : Includecore.record_change list) =
    List.filter_map
      (fun (change : Includecore.record_change) ->
        match change with
        | Diffing_with_keys.Change
            (Type
               { got = ld1;
                 expected = ld2;
                 reason = Includecore.Modality equate;
                 _
               }) ->
          Some
            (Modality_leaf
               (equate_modality_input ~orientation
                  ~subject:(Modality_field (Ident.name ld1.Types.ld_id))
                  ~expected_loc:(Some ld2.Types.ld_loc)
                  ~actual_loc:(Some ld1.Types.ld_loc) equate))
        | Diffing_with_keys.Change
            (Type
               { got = ld1;
                 expected = ld2;
                 reason = Includecore.(Mutability ord | Atomicity ord) as reason;
                 _
               }) ->
          let property =
            match reason with
            | Includecore.Mutability _ -> Mutability
            | Includecore.Atomicity _ | Includecore.Type _
            | Includecore.Modality _ ->
              Atomicity
          in
          let declared_on = Orientation.side_of_position orientation ord in
          let expected_loc, actual_loc =
            Orientation.expected_and_actual orientation
              ~got:(Some ld1.Types.ld_loc) ~expected:(Some ld2.Types.ld_loc)
          in
          Some
            (Presence_leaf
               { property;
                 subject = Modality_field (Ident.name ld1.Types.ld_id);
                 declared_on;
                 expected_loc;
                 actual_loc
               })
        | Diffing_with_keys.Change (Type { reason = Includecore.Type _; _ })
        | Diffing_with_keys.Change (Name _)
        | Diffing_with_keys.Swap _ | Diffing_with_keys.Move _
        | Diffing_with_keys.Insert _ | Diffing_with_keys.Delete _ ->
          None)
      changes

  let constructor_leaves ~orientation
      (changes : Includecore.variant_change list) =
    List.concat_map
      (fun (change : Includecore.variant_change) ->
        match change with
        | Diffing_with_keys.Change
            (Type { got = cd1, _; expected = cd2, _; reason; _ }) ->
          begin match (reason : Includecore.constructor_mismatch) with
          | Includecore.Modality (i, equate) ->
            [ Modality_leaf
                (equate_modality_input ~orientation
                   ~subject:
                     (Modality_constructor_arg
                        { constructor = Ident.name cd1.Types.cd_id;
                          index = i + 1
                        })
                   ~expected_loc:(Some cd2.Types.cd_loc)
                   ~actual_loc:(Some cd1.Types.cd_loc) equate) ]
          | Includecore.Inline_record changes ->
            field_leaves ~orientation changes
          | Includecore.(
              ( Type _ | Arity | Kind _ | Explicit_return_type _
              | Fixed_representation _ )) ->
            []
          end
        | Diffing_with_keys.Change (Name _)
        | Diffing_with_keys.Swap _ | Diffing_with_keys.Move _
        | Diffing_with_keys.Insert _ | Diffing_with_keys.Delete _ ->
          [])
      changes

  let type_leaves ~orientation ~expected_loc ~actual_loc
      (mismatch : Includecore.type_mismatch) =
    match mismatch with
    | Includecore.Record_mismatch (Includecore.Label_mismatch changes) ->
      field_leaves ~orientation changes
    | Includecore.Variant_mismatch changes ->
      constructor_leaves ~orientation changes
    | Includecore.Unsafe_mode_crossing mismatch ->
      let difference =
        match (mismatch : Includecore.unsafe_mode_crossing_mismatch) with
        | Includecore.Mode_crossing_only_on ord ->
          Attribute_on_one_side
            { declared_on = Orientation.side_of_position orientation ord }
        | Includecore.Bounds_not_equal (got, expected) ->
          let expected, got =
            Orientation.expected_and_actual orientation ~got ~expected
          in
          let with_bounds (umc : Types.unsafe_mode_crossing) =
            collapse_whitespace
              (Format_doc.asprintf "%a" Jkind.With_bounds.format
                 umc.Types.unsafe_with_bounds)
          in
          let expected_only, actual_only, differing =
            crossing_bounds_difference expected.Types.unsafe_mod_bounds
              got.Types.unsafe_mod_bounds
          in
          Bounds_differ
            { expected_only;
              actual_only;
              differing;
              expected_with = with_bounds expected;
              actual_with = with_bounds got
            }
      in
      [Crossing_leaf { difference; expected_loc; actual_loc }]
    | Includecore.(
        ( Arity | Privacy _ | Kind _ | Constraint _ | Manifest _
        | Parameter_jkind _ | Private_variant _ | Private_object _ | Variance
        | Record_mismatch
            ( Inlined_representation _ | Float_representation _
            | Ufloat_representation _ | Mixed_representation _
            | Mixed_representation_with_flat_floats _
            | Representation_shape_mismatch )
        | Unboxed_representation _ | Extensible_representation _
        | With_null_representation _ | Fixed_representation _ | Jkind _ )) ->
      []

  let leaves ls = List.map (fun leaf -> Leaf leaf) ls

  let rec of_all ~env ~fallback (all : all) : tree list =
    match all with
    | In_Compilation_unit (_, { symptom; _ }) ->
      of_signature ~env ~fallback ~orientation:Orientation.Got_is_actual symptom
    | In_Signature s | In_Include_functor_signature s ->
      of_signature ~env ~fallback ~orientation:Orientation.Got_is_actual s
    | In_Module_type d ->
      of_module_type_diff ~env ~fallback ~orientation:Orientation.Got_is_actual
        d
    | In_Module_type_substitution (_, { symptom; _ }) ->
      of_mtd_symptom ~env ~fallback ~orientation:Orientation.Got_is_actual
        symptom
    | In_Type_declaration (id, c) | In_Jkind_declaration (id, c) ->
      of_core ~env ~fallback ~orientation:Orientation.Got_is_actual id c
    | In_Expansion _ -> []

  and of_module_type_diff ~env ~fallback ~orientation
      ({ symptom; _ } : module_type_diff) =
    of_module_type_symptom ~env ~fallback ~orientation symptom

  and of_module_type_symptom ~env ~fallback ~orientation
      (symptom : module_type_symptom) =
    match symptom with
    | Mt_core _ | Invalid_module_alias _ -> []
    | Signature s -> of_signature ~env ~fallback ~orientation s
    | Functor (Params ({ got; expected; _ } as diff)) -> (
      let outer_expected, outer_got =
        Orientation.expected_and_actual orientation ~got ~expected
      in
      let counts params =
        List.fold_left
          (fun (parameters, units) (param : Types.functor_parameter) ->
            match param with
            | Types.Unit -> parameters, units + 1
            | Types.Named _ -> parameters + 1, units)
          (0, 0) params
      in
      match outer_got.params, outer_expected.params with
      | [], expected_params ->
        let parameters, units = counts expected_params in
        [Leaf (Functor_shape_leaf (Functor_expected { parameters; units }))]
      | got_params, [] ->
        let abstract =
          match outer_expected.res with Types.Mty_ident _ -> true | _ -> false
        in
        let parameters, units = counts got_params in
        [ Leaf
            (Functor_shape_leaf
               (Functor_unexpected { parameters; units; abstract })) ]
      | _ :: _, _ :: _ -> of_functor_params ~env ~fallback ~orientation diff)
    | Functor (Result d) -> of_module_type_diff ~env ~fallback ~orientation d
    | After_alias_expansion d ->
      of_module_type_diff ~env ~fallback ~orientation d
    | Mode e ->
      [ Leaf
          (Mode_leaf
             { pinpoint = fallback, Mode.Hint.Module;
               error = e;
               expected_decl = None
             }) ]

  and missing_of_signature_item (item : Types.signature_item) : missing_item =
    let make noun id decl_loc = { noun; name = Ident.name id; decl_loc } in
    match item with
    | Types.Sig_value (id, vd, _) -> make "value" id vd.val_loc
    | Types.Sig_type (id, td, _, _) -> make "type" id td.type_loc
    | Types.Sig_typext (id, ext, _, _) ->
      let noun =
        if Path.same ext.ext_type_path Predef.path_exn
        then "exception"
        else "extension constructor"
      in
      make noun id ext.ext_loc
    | Types.Sig_module (id, _, md, _, _) -> make "module" id md.md_loc
    | Types.Sig_modtype (id, mtd, _) -> make "module type" id mtd.mtd_loc
    | Types.Sig_class (id, cd, _, _) -> make "class" id cd.cty_loc
    | Types.Sig_class_type (id, cltd, _, _) ->
      make "class type" id cltd.clty_loc
    | Types.Sig_jkind (id, jkd, _) -> make "kind" id jkd.jkind_loc

  and of_functor_params ~env ~fallback ~orientation
      ({ got; expected; _ } : functor_params_diff) =
    let patch =
      Includemod.Functor_inclusion_diff.diff env (got.params, got.res)
        (expected.params, expected.res)
    in
    let numbered = match patch with [] | [_] -> false | _ :: _ :: _ -> true in
    let is_unit (param : Types.functor_parameter) =
      match param with Types.Unit -> true | Types.Named _ -> false
    in
    let arity ~position ~surplus_on param =
      [ Leaf
          (Functor_arity_leaf
             { position;
               surplus_on;
               parameter =
                 (if is_unit param then Unit_parameter else Module_parameter)
             }) ]
    in
    List.concat
      (List.mapi
         (fun index change ->
           let position = index + 1 in
           match (change : _ Diffing.change) with
           | Diffing.Keep _ -> []
           | Diffing.Change (_, _, Mismatch d) -> (
             match
               of_module_type_diff ~env ~fallback
                 ~orientation:(Orientation.reverse orientation)
                 d
             with
             | [] -> []
             | children ->
               [ Item
                   { item =
                       Item_functor_parameter
                         (if numbered then Some position else None);
                     got_loc = None;
                     expected_loc = None;
                     children
                   } ])
           | Diffing.Change (_, _, Incompatible_params (p1, p2)) -> (
             let expected_param, actual_param =
               Orientation.expected_and_actual orientation ~got:p1 ~expected:p2
             in
             match actual_param, expected_param with
             | Types.Unit, Types.Named _ ->
               [ Leaf
                   (Functor_shape_leaf
                      (Generativity { module_parameter_on = Expected })) ]
             | Types.Named _, Types.Unit ->
               [ Leaf
                   (Functor_shape_leaf
                      (Generativity { module_parameter_on = Actual })) ]
             | (Types.Unit | Types.Named _), (Types.Unit | Types.Named _) -> [])
           | Diffing.Insert param ->
             arity ~position
               ~surplus_on:(Orientation.expected_side orientation)
               param
           | Diffing.Delete param ->
             arity ~position
               ~surplus_on:(Orientation.got_side orientation)
               param)
         patch)

  and of_signature ~env:_ ~fallback ~orientation
      ({ env; subst; missings; incompatibles } : signature_symptom) =
    let env =
      { Includemod.Functor_inclusion_diff.i_env = env; i_subst = subst }
    in
    List.rev_map
      (fun item -> Leaf (Missing_leaf (missing_of_signature_item item)))
      missings
    @ List.concat_map
        (fun (id, symptom) -> of_sigitem ~env ~fallback ~orientation id symptom)
        incompatibles

  and of_sigitem ~env ~fallback ~orientation id (symptom : sigitem_symptom) =
    match symptom with
    | Core c -> of_core ~env ~fallback ~orientation id c
    | Module_type_declaration { got; expected; symptom } ->
      let expected_loc, got_loc =
        Orientation.expected_and_actual orientation
          ~got:(Some got.Types.mtd_loc) ~expected:(Some expected.Types.mtd_loc)
      in
      [ Item
          { item = Item_module_type (Ident.name id);
            got_loc;
            expected_loc;
            children = of_mtd_symptom ~env ~fallback ~orientation symptom
          } ]
    | Module_type d ->
      [ Item
          { item = Item_module (Ident.name id);
            got_loc = None;
            expected_loc = None;
            children = of_module_type_diff ~env ~fallback ~orientation d
          } ]

  and of_mtd_symptom ~env ~fallback ~orientation
      (symptom : module_type_declaration_symptom) =
    let direction_of_side (side : Side.t) =
      match side with
      | Actual -> Actual_not_included
      | Expected -> Expected_not_included
    in
    let actual_side = direction_of_side (Orientation.got_side orientation) in
    let expected_side =
      direction_of_side (Orientation.expected_side orientation)
    in
    let direction direction ~orientation d =
      Item
        { item = Direction direction;
          got_loc = None;
          expected_loc = None;
          children = of_module_type_diff ~env ~fallback ~orientation d
        }
    in
    match symptom with
    | Illegal_permutation _ -> []
    | Not_less_than d -> [direction actual_side ~orientation d]
    | Not_greater_than d ->
      [direction expected_side ~orientation:(Orientation.reverse orientation) d]
    | Incomparable { less_than; greater_than } ->
      [ direction actual_side ~orientation less_than;
        direction expected_side
          ~orientation:(Orientation.reverse orientation)
          greater_than ]

  and of_core ~env:_ ~fallback ~orientation id (symptom : core_sigitem_symptom)
      =
    match symptom with
    | Value_descriptions { got; expected; symptom = Includecore.Mode e; _ } ->
      let expected_decl =
        match Mode.Modality.to_const_opt expected.Types.val_modalities with
        | Some modalities ->
          Some { decl_loc = expected.Types.val_loc; modalities; written = None }
        | None -> None
      in
      [ Leaf
          (Mode_leaf
             { pinpoint =
                 ( got.Types.val_loc,
                   Mode.Hint.Structure_item (Mode.Hint.Value, id) );
               error = e;
               expected_decl
             }) ]
    | Class_declarations { symptom = Class_mode e; _ } ->
      [ Leaf
          (Mode_leaf
             { pinpoint = fallback, Mode.Hint.Class;
               error = e;
               expected_decl = None
             }) ]
    | Value_descriptions { got; expected; symptom = Includecore.Modality e; _ }
      ->
      [ Leaf
          (Modality_leaf
             (modality_input ~orientation
                ~subject:(Modality_item (Ident.name id))
                ~expected_loc:(Some expected.Types.val_loc)
                ~actual_loc:(Some got.Types.val_loc)
                ~requirement:At_least_as_strong e)) ]
    | Modalities e ->
      [ Leaf
          (Modality_leaf
             (modality_input ~orientation
                ~subject:(Modality_item (Ident.name id))
                ~expected_loc:None ~actual_loc:None
                ~requirement:At_least_as_strong e)) ]
    | Type_declarations { got; expected; symptom } -> begin
      let expected_loc0, got_loc0 =
        Orientation.expected_and_actual orientation
          ~got:(Some got.Types.type_loc)
          ~expected:(Some expected.Types.type_loc)
      in
      match
        type_leaves ~orientation ~expected_loc:expected_loc0
          ~actual_loc:got_loc0 symptom
      with
      | [] -> []
      | children ->
        let got_loc = got_loc0 in
        let expected_loc = expected_loc0 in
        [ Item
            { item = Item_type (Ident.name id);
              got_loc;
              expected_loc;
              children = leaves children
            } ]
      end
    | Value_descriptions
        { got; expected; symptom = Includecore.Zero_alloc _; _ } ->
      [ Leaf
          (Zero_alloc_leaf
             { expected_loc = Some expected.Types.val_loc;
               actual_loc = Some got.Types.val_loc
             }) ]
    | Value_descriptions
        { symptom =
            Includecore.(
              ( Primitive_mismatch _ | Not_a_primitive | Type _
              | Layout_poly_coercion _ ));
          _
        } ->
      []
    | Extension_constructors
        { got;
          expected;
          symptom = Includecore.Constructor_mismatch (_, ext1, ext2, reason)
        } -> begin
      let leaves_ =
        match (reason : Includecore.constructor_mismatch) with
        | Includecore.Modality (i, equate) ->
          [ Modality_leaf
              (equate_modality_input ~orientation
                 ~subject:
                   (Modality_constructor_arg
                      { constructor = Ident.name id; index = i + 1 })
                 ~expected_loc:(Some ext2.Types.ext_loc)
                 ~actual_loc:(Some ext1.Types.ext_loc) equate) ]
        | Includecore.Inline_record changes -> field_leaves ~orientation changes
        | Includecore.(
            ( Type _ | Arity | Kind _ | Explicit_return_type _
            | Fixed_representation _ )) ->
          []
      in
      match leaves_ with
      | [] -> []
      | leaves_ ->
        let expected_loc, got_loc =
          Orientation.expected_and_actual orientation
            ~got:(Some got.Types.ext_loc)
            ~expected:(Some expected.Types.ext_loc)
        in
        [ Item
            { item =
                Item_extension_constructor
                  { exception_ =
                      Path.same got.Types.ext_type_path Predef.path_exn;
                    name = Ident.name id
                  };
              got_loc;
              expected_loc;
              children = leaves leaves_
            } ]
      end
    | Class_declarations { symptom = Class_type _; _ }
    | Extension_constructors { symptom = Includecore.Constructor_privacy; _ }
    | Class_type_declarations _ | Jkind_declarations _ ->
      []
end

type inclusion_site =
  | Module of
      { name : string option;
        body : Location.t
      }
  | Module_type of
      { name : string option;
        body : Location.t
      }

type context =
  { inclusion_site_at : Location.t -> inclusion_site option;
    declared_modalities_at :
      Location.t -> argument:int option -> declared_modalities option;
    constructor_arguments_at :
      Location.t -> Longident.t option -> constructor_argument list option;
    documentation : Documentation.lookup
  }

type request =
  { source : Source.t;
    context : context;
    pronouns : Pronouns.t;
    reported_loc : Location.t
  }

let rec longident_name (lid : Longident.t) : string option =
  match lid with
  | Lident name -> Some name
  | Ldot (prefix, name) -> (
    match longident_name prefix.txt with
    | Some prefix -> Some (prefix ^ "." ^ name.txt)
    | None -> None)
  | Lapply _ -> None

let rec leftmost_functor (lid : Longident.t) : Longident.t =
  match lid with
  | Lapply (f, _) -> leftmost_functor f.txt
  | (Lident _ | Ldot _) as lid -> lid

let inclusion_frame ~loc frame : term Nlg.plan =
  let open Nlg in
  let named ?span words = subject ?span words in
  let subject, predicate =
    match frame with
    | `Unit name ->
      ( named [Phrase.Text "module "; Phrase.Code name],
        [txt " does not match its interface"] )
    | `Site (Module { name = Some name; body }) ->
      ( named ~span:body [Phrase.Text "module "; Phrase.Code name],
        [txt " does not match its signature"] )
    | `Site (Module { name = None; body }) ->
      ( named ~span:body [Phrase.Text "the anonymous module"],
        [txt " does not match its signature"] )
    | `Site (Module_type { name = Some name; body }) ->
      ( named ~span:body [Phrase.Text "the module type "; Phrase.Code name],
        [txt " does not match its declaration"] )
    | `Site (Module_type { name = None; body }) ->
      ( named ~span:body [Phrase.Text "the anonymous module type"],
        [txt " does not match its declaration"] )
    | `Substitution name ->
      ( named ~span:loc
          (match name with
          | Some name -> [Phrase.Text "the new definition of "; Phrase.Code name]
          | None -> [Phrase.Text "the new definition"]),
        [txt " does not match its original definition"] )
    | `Applicative_functor (type_name, constrained) -> (
      ( named ~span:loc [Phrase.Text "the type "; Phrase.Code type_name],
        match constrained with
        | Some name ->
          [ txt " is ill-typed after this ";
            code "with";
            txt " constraint on ";
            code name ]
        | None ->
          [txt " is ill-typed after this "; code "with"; txt " constraint"] ))
    | `Strengthening name ->
      ( named ~span:loc
          (match name with
          | Some name -> [Phrase.Text "module "; Phrase.Code name]
          | None -> [Phrase.Text "the strengthening module"]),
        [txt " does not match the module type it strengthens"] )
    | `Not_a_functor name ->
      ( named ~span:loc
          (match name with
          | Some name -> [Phrase.Text "module "; Phrase.Code name]
          | None -> [Phrase.Text "this module"]),
        [txt " is not a functor, so it cannot be applied"] )
    | `Ill_typed_application name ->
      ( named ~span:loc
          (match name with
          | Some name -> [Phrase.Text "the application of "; Phrase.Code name]
          | None -> [Phrase.Text "this functor application"]),
        [txt " is ill-typed"] )
    | `Application (functor_name, argument) -> (
      ( named ~span:loc
          (match argument with
          | Some argument -> [Phrase.Text "the argument "; Phrase.Code argument]
          | None -> [Phrase.Text "the argument"]),
        match functor_name with
        | Some functor_name ->
          [txt " does not match the parameter of "; code functor_name]
        | None -> [txt " does not match the functor's parameter"] ))
    | `Equation (name, equated_loc) ->
      ( named ~span:loc [Phrase.Text "this definition"],
        txt " does not match the definition of "
        ::
        (match equated_loc with
        | Some l -> [ref_source l [code name]]
        | None -> [code name]) )
    | `Unknown ->
      ( named ~span:loc [Phrase.Text "the module"],
        [txt " does not match its signature"] )
  in
  { Nlg.statement =
      Some
        (sentence ?subject:(sentence_subject subject)
           (phrase (Nlg.mention ~case:Subject subject :: predicate)));
    children = []
  }

let missing_frame (missing : Inclusion.missing_item) : term Nlg.plan =
  let open Nlg in
  let subject =
    subject ~span:missing.decl_loc
      [Phrase.Text ("the " ^ missing.noun ^ " "); Phrase.Code missing.name]
  in
  let sentence =
    sentence ?subject:(sentence_subject subject)
      (phrase
         [ Nlg.mention ~case:Subject subject;
           copula;
           txt " required but not provided" ])
  in
  { statement = Some sentence; children = [] }

let item_frame ~sides (item : Inclusion.item) ~got_loc ~expected_loc ~children :
    term Nlg.plan =
  let open Nlg in
  let spans = List.filter_map (fun span -> span) [expected_loc; got_loc] in
  let named noun name =
    let subject =
      subject ?span:(first spans) [Phrase.Text noun; Phrase.Code name]
    in
    sentence ?subject:(sentence_subject subject)
      (phrase
         [ txt "the declarations of ";
           Nlg.mention ~case:Subject subject;
           txt " do not match" ])
  in
  let header =
    match item with
    | Inclusion.Item_module name -> named "module " name
    | Inclusion.Item_module_type name -> named "module type " name
    | Inclusion.Item_type name -> named "type " name
    | Inclusion.Item_extension_constructor { exception_; name } ->
      named (if exception_ then "exception " else "the constructor ") name
    | Inclusion.Item_functor_parameter None ->
      sentence (phrase [txt "the functors' parameters do not match"])
    | Inclusion.Item_functor_parameter (Some position) ->
      sentence
        (phrase
           [ txt
               ("the declarations of the " ^ ordinal position
              ^ " parameter do not match") ])
    | Inclusion.Direction direction ->
      let not_included, container =
        match direction with
        | Inclusion.Actual_not_included ->
          sides.actual_name, sides.expected_name
        | Inclusion.Expected_not_included ->
          sides.expected_name, sides.actual_name
      in
      sentence
        (phrase (not_included @ (txt " is not included in " :: container)))
  in
  let educate =
    match item with
    | Inclusion.Item_module_type _ ->
      [ elaboration
          (sentence ~kind:Diagnostic.Kind.Background
             (phrase
                [txt "module type declarations must be equal on both sides"]))
      ]
    | Inclusion.Item_module _ | Inclusion.Item_type _
    | Inclusion.Item_extension_constructor _
    | Inclusion.Item_functor_parameter _ | Inclusion.Direction _ ->
      []
  in
  { Nlg.statement = Some header; children = claims children @ educate }

let plain_story ~claim ?contrast ?(educate = []) ?(suggestion = []) () :
    story list =
  let open Nlg in
  let tail =
    List.map
      (fun words ->
        elaboration (sentence ~kind:Diagnostic.Kind.Background (phrase words)))
      educate
    @ List.map
        (fun words ->
          elaboration (sentence ~kind:Diagnostic.Kind.Suggestion (phrase words)))
        suggestion
  in
  let beats =
    match contrast with
    | None ->
      [{ Nlg.statement = Some (sentence (phrase claim)); children = tail }]
    | Some contrast ->
      [ { Nlg.statement = Some (sentence (phrase claim)); children = [] };
        { Nlg.statement = Some (sentence (phrase contrast)); children = tail }
      ]
  in
  [prose { Nlg.statement = None; children = claims beats }]

let violation_crossing_axes (v : Jkind.Violation.t) : string list =
  match v.violation with
  | Jkind.Violation.No_intersection _ -> []
  | Jkind.Violation.Not_a_subjkind (_, _, reasons) ->
    List.filter_map
      (fun reason ->
        match (reason : Jkind.Sub_failure_reason.t) with
        | Axis_disagreement (Jkind_axis.Axis.Pack axis) ->
          Some (Jkind_axis.Axis.name axis)
        | Layout_disagreement | With_bounds_on_left | Constrain_ran_out_of_fuel
          ->
          None)
      reasons

let jkind_crossing_story ~loc ~what (v : Jkind.Violation.t) : story list option
    =
  let open Nlg in
  match violation_crossing_axes v with
  | [] -> None
  | axes ->
    let plural = match axes with [_] -> " axis" | _ -> " axes" in
    Some
      (plain_story
         ~claim:
           [ ref_source loc
               [ txt
                   (what ^ " does not cross the " ^ String.concat ", " axes
                  ^ plural) ] ]
         ~contrast:[txt "but the kind it is checked against requires it to"]
         ~educate:
           [ [ txt "a ";
               code "mod";
               txt
                 " annotation claims a type's values may be used at the \
                  stronger mode on those axes, whatever mode they are held at"
             ] ]
         ())

module Scope = struct
  type t =
    | Explained
    | Kind_check
    | Attribute_or_extension
    | Not_about_modes
end

let typecore_scope : Typecore.error -> Scope.t = function
  | Invalid_atomic_loc_payload | Label_not_atomic _ | Atomic_in_pattern _
  | Modalities_on_atomic_field _ | Block_index_modality_mismatch _
  | Submode_failed _ | Curried_application_complete _ | Mode_mismatch _
  | Uncurried_function_escapes _ | Tail_call_local_returning
  | Bad_tail_annotation _ | Exclave_in_nontail_position
  | Exclave_returns_not_local | Always_heap_allocation _
  | Always_static_allocation _ | Not_allocation | Overwrite_of_invalid_term ->
    Explained
  | Non_value_object _ | Non_value_let_rec _ | Existential_jkind_mismatch _
  | Function_type_not_rep _ | Record_projection_not_rep _ | Record_not_rep _
  | Mutable_var_not_rep _ | Field_value_not_rep _
  | Constructor_arg_projection_not_rep _ | Constructor_arg_value_not_rep _
  | Impossible_function_jkind _ ->
    Kind_check
  | Label_not_mutable _ | Instance_variable_not_mutable _ | Unexpected_mutable _
  | Illegal_mutable_pat | Function_returns_local | Atomic_in_functional_update _
  | Mixed_record_atomic_loc _ | Polymorphic_atomic_loc _
  | Mutable_block_index_polymorphic_field _ | Useless_lpoly ->
    Attribute_or_extension
  | Constructor_arity_mismatch _ | Label_mismatch _ | Pattern_type_clash _
  | Or_pattern_type_clash _ | Multiply_bound_variable _ | Orpat_vars _
  | Expr_type_clash _ | Function_arity_type_clash _ | Apply_non_function _
  | Apply_wrong_label _ | Label_multiply_defined _ | Label_missing _
  | Wrong_name _ | Name_type_mismatch _ | Invalid_format _ | Not_an_object _
  | Undefined_method _ | Undefined_self_method _ | Virtual_class _
  | Private_type _ | Private_label _ | Private_constructor _
  | Unbound_instance_variable _ | Not_subtype _ | Outside_class
  | Value_multiply_overridden _ | Coercion_failure _ | Not_a_function _
  | Too_many_arguments _ | Abstract_wrong_label _ | Scoping_let_module _
  | Not_a_polymorphic_variant_type _ | Incoherent_label_order | Less_general _
  | Modules_not_allowed | Cannot_infer_signature | Not_a_packed_module _
  | Unexpected_existential _ | Invalid_interval | Invalid_for_loop_index
  | Invalid_comprehension_for_range_iterator_index | No_value_clauses
  | Exception_pattern_disallowed
  | Mixed_value_and_exception_patterns_under_guard
  | Effect_pattern_below_toplevel | Invalid_continuation_pattern
  | Inlined_record_escape | Inlined_record_expected | Unrefuted_pattern _
  | Invalid_extension_constructor_payload | Not_an_extension_constructor
  | Probe_format | Probe_name_format _ | Probe_name_undefined _
  | Probe_is_enabled_format | Extension_not_enabled _ | Literal_overflow _
  | Unknown_literal _ | Float32_literal _ | Int8_literal _ | Int16_literal _
  | Untagged_char_literal _ | Illegal_letrec_pat | Illegal_letrec_expr
  | Mixed_poly_nonpoly_bindings | Illegal_class_expr | Letop_type_clash _
  | Andop_type_clash _ | Bindings_type_clash _ | Unbound_existential _
  | Bind_existential _ | Missing_type_constraint | Wrong_expected_kind _
  | Expr_not_a_record_type _ | Constructor_labeled_arg
  | Partial_tuple_pattern_bad_type | Extra_tuple_label _ | Missing_tuple_label _
  | Repeated_tuple_exp_label _ | Repeated_tuple_pat_label _
  | Wrong_expected_record_boxing _ | Expr_record_type_has_wrong_boxing _
  | Invalid_unboxed_access _ | Block_access_bad_record _ | Optional_poly_param
  | Unboxed_int_literals_not_supported | Indeterminate_record_layout _
  | Indeterminate_constructor_layout _ | Invalid_label_for_src_pos _
  | Nonoptional_call_pos_label _ | Unexpected_hole
  | Let_poly_not_yet_implemented | Let_poly_not_syntactic_value
  | Layout_poly_inst_not_yet_supported _ | Function_type_escapes_partial_match _
    ->
    Not_about_modes

let typemod_scope : Typemod.error -> Scope.t = function
  | Not_included _ | Not_included_functor _ | With_mismatch _
  | With_makes_applicative_functor_ill_typed _ | Strengthening_mismatch _ ->
    Explained
  | Cannot_apply _ | Cannot_eliminate_dependency _ | Signature_expected
  | Structure_expected _ | Functor_expected _ | Signature_parameter_expected _
  | Signature_result_expected _ | Recursive_include_functor
  | With_no_component _ | With_changes_module_alias _
  | With_cannot_remove_constrained_type | With_package_manifest _
  | Repeated_name _ | Non_generalizable _ | Non_generalizable_module _
  | Implementation_is_required _ | Interface_not_compiled _
  | Not_allowed_in_functor_body _ | Not_includable_in_functor_body _
  | Not_a_packed_module _ | Incomplete_packed_module _ | Scoping_pack _
  | Recursive_module_require_explicit_type | Apply_generative
  | Cannot_scrape_alias _ | Cannot_scrape_package_type _
  | Badly_formed_signature _ | Cannot_hide_id _ | Invalid_type_subst_rhs
  | Non_packable_local_modtype_subst _ | With_cannot_remove_packed_modtype _
  | Cannot_alias _ | Cannot_pack_parameter
  | Compiling_as_parameterised_parameter
  | Cannot_compile_implementation_as_parameter | Cannot_implement_parameter _
  | Argument_for_non_parameter _ | Cannot_find_argument_type _
  | Inconsistent_argument_types _ | Duplicate_parameter_name _ ->
    Not_about_modes

let typedecl_scope : Typedecl.error -> Scope.t = function
  | Unboxed_mutable_label | Definition_mismatch _ | Jkind_mismatch_of_type _
  | Jkind_mismatch_of_path _ | Unsafe_mode_crossing_on_invalid_type_kind
  | Atomic_field_must_be_mutable _ | Constructor_submode_failed _
  | Non_value_atomic_field ->
    Explained
  | Jkind_mismatch_due_to_bad_inference _ | Jkind_sort _ | Jkind_empty_record
  | Non_representable_in_module _ | Invalid_jkind_in_block _ | Illegal_baggage _
  | Recursive_jkind_definition _ ->
    Kind_check
  | Local_not_enabled | Zero_alloc_attr_unsupported _
  | Zero_alloc_attr_non_function | Zero_alloc_attr_bad_user_arity
  | Missing_immediate_all_void_constructor_attribute _ ->
    Attribute_or_extension
  | Repeated_parameter | Duplicate_constructor _ | Too_many_constructors
  | Duplicate_label _ | Recursive_abbrev _ | Cycle_in_def _
  | Unboxed_recursion _ | Constraint_failed _ | Inconsistent_constraint _
  | Type_clash _ | Non_regular _ | Null_arity_external | Missing_native_external
  | Unbound_type_var _ | Cannot_extend_private_type _ | Not_extensible_type _
  | Extension_mismatch _ | Rebind_wrong_type _ | Rebind_mismatch _
  | Rebind_private _ | Variance _ | Unavailable_type_constructor _
  | Unbound_type_var_ext _ | Val_in_structure | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type _ | Deep_unbox_or_untag_attribute _
  | Illegal_mixed_product _ | Separability _ | Bad_unboxed_attribute _
  | Poly_not_yet_implemented | Boxed_and_unboxed | Nonrec_gadt
  | Invalid_private_row_declaration _ | Unexpected_layout_any_in_primitive _
  | Useless_layout_poly | Bad_or_null_attribute _ | Invalid_reexport _
  | Non_abstract_reexport _ | No_unboxed_version _ | Layout_poly_unsupported
  | Misplaced_flatten_floats | Bad_represent_as_float_array_attribute ->
    Not_about_modes

let typetexp_scope : Typetexp.error -> Scope.t = function
  | Bad_jkind_annot _ -> Explained
  | Bad_univar_jkind _ | Non_value _ | Non_sort _
  | Mismatched_jkind_annotation _ ->
    Kind_check
  | Unbound_type_variable _ | No_type_wildcards _ | Undefined_type_constructor _
  | Type_arity_mismatch _ | Bound_type_variable _ | Recursive_type
  | Type_mismatch _ | Alias_type_mismatch _ | Present_has_conjunction _
  | Present_has_no_type _ | Constructor_mismatch _ | Not_a_variant _
  | Variant_tags _ | Invalid_variable_name _ | Cannot_quantify _
  | Multiple_constraints_on_type _ | Method_mismatch _ | Opened_object _
  | Not_an_object _ | Repeated_tuple_label _ | Unsupported_extension _
  | Polymorphic_optional_param | Did_you_mean_unboxed _
  | Invalid_label_for_call_pos _ | Invalid_variable_stage _ | Lpoly_unsupported
  | Val_poly_and_layout ->
    Not_about_modes

let title (axes : string list) : string =
  match axes with
  | [] -> "Explain mode error"
  | axes -> "Explain mode error (" ^ String.concat ", " axes ^ ")"

let realize ~documentation ~reported_loc stories =
  match stories with
  | [] -> None
  | stories ->
    Some
      (Nlg.realize ~loc:reported_loc
         ~title:(title (blamed_axes stories))
         ~term_entry:(term_entry ~documentation)
         ~term_words (frames stories))

let mode_stories request ?expected_decl ?extra_rules ?actuality_fallback
    ?subject_override axes =
  let { source; pronouns; reported_loc; _ } = request in
  mode_stories ?extra_rules ?actuality_fallback ?subject_override ~source
    ~error_loc:reported_loc ~expected_decl ~pronouns axes

let diagnose_env_lookup request ~loc lookup_error =
  let { source; _ } = request in
  let open Nlg in
  let story beats = [prose { Nlg.statement = None; children = claims beats }] in
  let local_word =
    mode_const_word (Comonadic Areality) Mode.Locality.Const.Local
  in
  match lookup_error with
  | Env.Local_value_used_in_exclave desc ->
    let (item : Mode.Hint.lock_item), name =
      match (desc : Mode.Hint.pinpoint_desc) with
      | Mode.Hint.Ident { category; lid } -> category, longident_name lid
      | Mode.Hint.Structure_item (category, id) ->
        category, Some (Ident.name id)
      | Mode.Hint.Module | Mode.Hint.Functor | Mode.Hint.Functor_parameter
      | Mode.Hint.Structure ->
        Module, None
      | Mode.Hint.Class | Mode.Hint.Object -> Class, None
      | Unknown | Function | Lazy | Quote | Allocation | Expression
      | Effect_match | Effect_try | Loop | Letop | Cases_result | Pattern ->
        Value, None
    in
    let named noun fallback =
      match name with
      | Some name -> subject ~span:loc [Phrase.Text noun; Phrase.Code name]
      | None -> subject ~span:loc [Phrase.Text fallback]
    in
    let subject, claim =
      match (item : Mode.Hint.lock_item) with
      | Mode.Hint.Value ->
        let s = named "the value " "this value" in
        s, [Nlg.mention ~case:Subject s; copula; txt " "; local_word]
      | Module ->
        let s = named "the module " "this module" in
        s, [Nlg.mention ~case:Subject s; copula; txt " "; local_word]
      | Constructor ->
        let s = named "the constructor " "this constructor" in
        s, [Nlg.mention ~case:Subject s; copula; txt " "; local_word]
      | Class ->
        let s =
          match name with
          | Some name -> subject ~span:loc [Phrase.Code name]
          | None -> subject ~span:loc [Phrase.Text "this class"]
        in
        ( s,
          [ Nlg.mention ~case:Subject s;
            copula;
            txt " a class, and classes are always ";
            local_word ] )
    in
    story
      [ { Nlg.statement =
            Some (sentence ?subject:(sentence_subject subject) (phrase claim));
          children = []
        };
        { Nlg.statement =
            Some
              (sentence
                 (phrase
                    [ txt "but ";
                      Nlg.pronoun ~case:Subject subject;
                      copula;
                      txt " used inside ";
                      code "exclave_" ]));
          children =
            [ elaboration
                (sentence ~kind:Diagnostic.Kind.Background
                   (phrase
                      [ code "exclave_";
                        txt " ends the current region early, so the region's ";
                        local_word;
                        txt " values cannot be used inside it" ])) ]
        } ]
  | Env.Mutable_value_used_in_closure (boundary_loc, boundary_desc) ->
    let subject = subject_of_loc ~source ~fallback:"this variable" loc in
    story
      [ { Nlg.statement =
            Some
              (sentence ?subject:(sentence_subject subject)
                 (phrase
                    [ Nlg.mention ~case:Subject subject;
                      copula;
                      txt " a mutable variable" ]));
          children = []
        };
        { Nlg.statement =
            Some
              (sentence
                 (phrase
                    [ txt "but ";
                      Nlg.pronoun ~case:Subject subject;
                      copula;
                      txt " used inside ";
                      ref_source boundary_loc [txt (human_desc boundary_desc)]
                    ]));
          children =
            [ elaboration
                (sentence ~kind:Diagnostic.Kind.Background
                   (phrase
                      [ txt
                          "mutable variables cannot be captured: the capturing \
                           context may outlive them or run in parallel" ]));
              elaboration
                (sentence ~kind:Diagnostic.Kind.Suggestion
                   (phrase
                      [ txt "use a ";
                        code "ref";
                        txt " for mutable state shared across functions" ])) ]
        } ]
  | Env.Unbound_value _ | Env.Unbound_type _ | Env.Unbound_constructor _
  | Env.Unbound_label _ | Env.Unbound_module _ | Env.Unbound_class _
  | Env.Unbound_modtype _ | Env.Unbound_cltype _ | Env.Unbound_jkind _
  | Env.Unbound_settable_variable _ | Env.Not_a_settable_variable _
  | Env.Masked_instance_variable _ | Env.Masked_self_variable _
  | Env.Masked_ancestor_variable _ | Env.Structure_used_as_functor _
  | Env.Abstract_used_as_functor _ | Env.Functor_used_as_structure _
  | Env.Abstract_used_as_structure _ | Env.Generative_used_as_applicative _
  | Env.Illegal_reference_to_recursive_module _
  | Env.Illegal_reference_to_recursive_class_type _ | Env.Cannot_scrape_alias _
  | Env.Non_value_used_in_object _ | Env.No_unboxed_version _
  | Env.Error_from_persistent_env _ | Env.Incompatible_stage _
  | Env.Unbound_in_stage _ ->
    []

let diagnose_unique_use_during_borrowing _request
    ({ region_loc; borrow_occ; cannot_force = { occ; axis } } :
      Uniqueness_analysis.Usage.unique_use_during_borrowing_error) =
  let open Nlg in
  let wanted =
    match axis with
    | Uniqueness ->
      mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique
    | Linearity ->
      mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once
  in
  plain_story
    ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
    ~contrast:
      [ txt "but it is ";
        ref_source borrow_occ.Uniqueness_analysis.Occurrence.loc [txt "borrowed"];
        txt " for the whole of ";
        ref_source region_loc [txt "this borrow"] ]
    ~educate:
      [ [ txt
            "a borrow lends the value for the length of its context: until the \
             context ends, the value is not the borrower's to use" ] ]
    ()

let diagnose_uniqueness _request err =
  let open Nlg in
  let unique_word =
    mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique
  in
  let aliased_word =
    mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Aliased
  in
  let once_word =
    mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once
  in
  let many_word =
    mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Many
  in
  let used_as (axis : Uniqueness_analysis.Maybe_unique.axis) =
    match axis with
    | Uniqueness -> unique_word, aliased_word
    | Linearity -> many_word, once_word
  in
  match err with
  | Uniqueness_analysis.Boundary { cannot_force = { occ; axis }; reason } ->
    let wanted, forced = used_as axis in
    let boundary =
      match reason with
      | Uniqueness_analysis.Paths_from_mod_class -> "another module or class"
      | Uniqueness_analysis.Free_var_of_mod_class
      | Uniqueness_analysis.Out_of_mod_class ->
        "outside the current module or class"
    in
    plain_story
      ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
      ~contrast:[txt ("but it comes from " ^ boundary)]
      ~educate:
        [ [ txt "a value that crosses a module or class boundary is ";
            forced;
            txt
              ": the analysis cannot see how the other side uses it, so it \
               must assume the worst" ] ]
      ()
  | Uniqueness_analysis.Borrowed_value_used_uniquely { occ; axis } ->
    let wanted, forced = used_as axis in
    plain_story
      ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
      ~contrast:[txt "but it is borrowed here, which makes it "; forced]
      ()
  | Uniqueness_analysis.Borrowed_out_of_context loc ->
    plain_story
      ~claim:
        [ref_source loc [code "borrow_"]; txt " is not in a borrowing context"]
      ~educate:
        [ [txt "a borrow may be an argument of a function application"];
          [txt "a borrow may appear on the right-hand side of a let binding"];
          [txt "a borrow may be the scrutinee of a pattern match"] ]
      ()
  | Uniqueness_analysis.Overwrite_changed_tag
      (Uniqueness_analysis.Overwrites.Changed_tag { old_tag; new_tag }) ->
    let tag_name (tag : Uniqueness_analysis.Tag.t) =
      Format_doc.asprintf "%a" Pprintast.Doc.longident tag.name_for_error.txt
    in
    let contrast =
      match old_tag with
      | Uniqueness_analysis.Overwrites.Old_tag_unknown ->
        [txt "but the tag it overwrites is not known here"]
      | Uniqueness_analysis.Overwrites.Old_tag_was tag ->
        [ txt "but it overwrites ";
          ref_source tag.name_for_error.loc [code (tag_name tag)] ]
      | Uniqueness_analysis.Overwrites.Old_tag_mutated order ->
        [ txt
            (match order with
            | Uniqueness_analysis.Par ->
              "but the tag is being changed by a mutation, so it is not known \
               here"
            | Uniqueness_analysis.Seq_before | Uniqueness_analysis.Seq_after ->
              "but the tag was changed by a mutation, so it is not known here")
        ]
    in
    plain_story
      ~claim:
        [ ref_source new_tag.name_for_error.loc
            [txt "this overwrite sets the tag to "; code (tag_name new_tag)] ]
      ~contrast
      ~educate:
        [ [ txt
              "an overwrite reuses the block it is given, and the garbage \
               collector does not support changing a block's tag: the \
               constructor must stay the same" ] ]
      ()
  | Uniqueness_analysis.Cannot_force
      { inner = { cannot_force = { occ; axis }; there; order };
        first_is_of_second
      } -> (
    match Uniqueness_analysis.Usage.extract_occurrence there with
    | None -> []
    | Some there_occ ->
      let here = occ, "used" in
      let other = there_occ, Uniqueness_analysis.Usage.describe there in
      let (first, first_usage), (second, second_usage), second_is_here =
        match order with
        | Uniqueness_analysis.Seq_before -> here, other, false
        | Uniqueness_analysis.Seq_after -> other, here, true
        | Uniqueness_analysis.Par ->
          if
            Location.compare occ.Uniqueness_analysis.Occurrence.loc
              there_occ.Uniqueness_analysis.Occurrence.loc
            < 0
          then here, other, false
          else other, here, true
      in
      let already =
        match order with
        | Uniqueness_analysis.Seq_before | Uniqueness_analysis.Seq_after ->
          "has already been "
        | Uniqueness_analysis.Par -> "is also being "
      in
      let subject =
        match first_is_of_second with
        | Uniqueness_analysis.Self
        | Uniqueness_analysis.Ancestor [Memory_address]
        | Uniqueness_analysis.Descendant [Memory_address] ->
          "it "
        | Uniqueness_analysis.Descendant _ -> "part of it "
        | Uniqueness_analysis.Ancestor _ -> "it is part of a value that "
      in
      let mode_word, rule =
        match axis with
        | Uniqueness ->
          ( mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique,
            [ txt "a value used as ";
              unique_word;
              txt " must have no other use: that is what ";
              unique_word;
              txt " means" ] )
        | Linearity ->
          once_word, [txt "a "; once_word; txt " value may be used at most once"]
      in
      let claim, contrast =
        if second_is_here
        then
          match axis with
          | Uniqueness ->
            ( [ ref_source second.Uniqueness_analysis.Occurrence.loc
                  [txt ("this value is " ^ second_usage ^ " here as ")];
                mode_word ],
              [ txt ("but " ^ subject ^ already);
                ref_source first.Uniqueness_analysis.Occurrence.loc
                  [txt first_usage] ] )
          | Linearity ->
            ( [ txt "this value is ";
                mode_word;
                ref_source second.Uniqueness_analysis.Occurrence.loc
                  [txt (" and " ^ second_usage ^ " here")] ],
              [ txt ("but " ^ subject ^ already);
                ref_source first.Uniqueness_analysis.Occurrence.loc
                  [txt first_usage] ] )
        else
          match axis with
          | Uniqueness ->
            ( [ ref_source second.Uniqueness_analysis.Occurrence.loc
                  [txt ("this value is " ^ second_usage ^ " here")] ],
              [ txt ("but " ^ subject ^ already);
                ref_source first.Uniqueness_analysis.Occurrence.loc
                  [txt (first_usage ^ " as ")];
                mode_word ] )
          | Linearity ->
            ( [ ref_source second.Uniqueness_analysis.Occurrence.loc
                  [txt ("this value is " ^ second_usage ^ " here")] ],
              [ txt ("but " ^ subject ^ "is ");
                mode_word;
                txt (" and " ^ already);
                ref_source first.Uniqueness_analysis.Occurrence.loc
                  [txt first_usage] ] )
      in
      plain_story ~claim ~contrast ~educate:[rule] ())

let diagnose_unexplained request exn =
  match Mode.walk_error_all_exn exn with
  | None -> []
  | Some axes -> mode_stories request axes

let diagnose_typetexp request ~loc ~env err =
  match err with
  | Typetexp.Bad_jkind_annot (_ty, v) ->
    Option.value (jkind_crossing_story ~loc ~what:"this type" v) ~default:[]
  | err ->
    let (_ : Scope.t) = typetexp_scope err in
    diagnose_unexplained request (Typetexp.Error (loc, env, err))

let diagnose_inclusion_leaf request ~sides ~in_parameter (leaf : Inclusion.leaf)
    : story list =
  let { context = { declared_modalities_at; _ }; _ } = request in
  match leaf with
  | Inclusion.Mode_leaf { pinpoint; error; expected_decl } ->
    let expected_decl =
      match expected_decl with
      | None -> None
      | Some (decl : expected_decl) ->
        Some
          { decl with
            written = declared_modalities_at decl.decl_loc ~argument:None
          }
    in
    mode_stories request ?expected_decl
      (Mode.Value.walk_error_all pinpoint error)
  | Inclusion.Modality_leaf input ->
    [modality_story ~declared_modalities_at ~sides input]
  | Inclusion.Missing_leaf missing -> [prose (missing_frame missing)]
  | Inclusion.Presence_leaf input -> [prose (render_presence_error ~sides input)]
  | Inclusion.Crossing_leaf input -> [prose (render_crossing_error ~sides input)]
  | Inclusion.Functor_shape_leaf shape ->
    [prose (render_functor_shape ~in_parameter shape)]
  | Inclusion.Functor_arity_leaf { position; surplus_on; parameter } ->
    let open Nlg in
    let noun =
      match parameter with
      | Inclusion.Unit_parameter -> [code "()"]
      | Inclusion.Module_parameter -> [txt "a parameter"]
    in
    let where = [txt (" in the " ^ ordinal position ^ " position")] in
    let claim, contrast =
      match (surplus_on : Side.t) with
      | Actual ->
        ( (txt "this functor takes " :: noun) @ where,
          [txt "but the signature declares none there"] )
      | Expected ->
        ( (txt "the signature declares " :: noun) @ where,
          [txt "but this functor takes none there"] )
    in
    plain_story ~claim ~contrast ()
  | Inclusion.Zero_alloc_leaf { expected_loc; actual_loc } ->
    let open Nlg in
    let located l words =
      match l with None -> words | Some l -> [ref_source l words]
    in
    plain_story
      ~claim:
        (located actual_loc
           [ txt "the implementation's ";
             code "zero_alloc";
             txt " guarantee is weaker" ])
      ~contrast:
        ((txt "but " :: sides.expected_name)
        @ located expected_loc [txt " requires a stronger one"])
      ~educate:
        [ [ code "zero_alloc";
            txt
              " in a signature is a promise clients may rely on, so the \
               implementation must guarantee at least as much" ] ]
      ~suggestion:
        [ [ txt "strengthen or add the ";
            code "zero_alloc";
            txt " attribute on the implementation" ] ]
      ()

module Explanation = struct
  let realized (children : (Diagnostic.Relation.t * term Nlg.plan) list) =
    let term_entry t : Diagnostic.Glossary.Entry.t =
      { term = term_display t; category = ""; description = ""; url = None }
    in
    let realized =
      Nlg.realize ~loc:Location.none ~title:"" ~term_entry ~term_words
        [{ Nlg.statement = None; children }]
    in
    realized.body

  let equal_annotation (left : Diagnostic.Annotation.t)
      (right : Diagnostic.Annotation.t) =
    match left, right with
    | Code, Code -> true
    | Source left, Source right -> same_chars left right
    | Mention left, Mention right -> (
      Diagnostic.Entities.Id.to_int left.entity
      = Diagnostic.Entities.Id.to_int right.entity
      &&
      match left.form, right.form with
      | Name, Name | Pronoun, Pronoun -> true
      | Name, Pronoun | Pronoun, Name -> false)
    | Term left, Term right ->
      Diagnostic.Glossary.Id.to_int left = Diagnostic.Glossary.Id.to_int right
    | (Code | Source _ | Mention _ | Term _), _ -> false

  let rec equal_inline (left : Diagnostic.Inline.t)
      (right : Diagnostic.Inline.t) =
    match left, right with
    | Text left, Text right -> String.equal left right
    | Annotated left, Annotated right ->
      equal_annotation left.annotation right.annotation
      && equal_inlines left.content right.content
    | (Text _ | Annotated _), _ -> false

  and equal_inlines left right =
    List.length left = List.length right
    && List.for_all2 equal_inline left right

  let rec equal_block (left : Diagnostic.Block.t) (right : Diagnostic.Block.t) =
    (match left.kind, right.kind with
      | Explanation, Explanation
      | Background, Background
      | Suggestion, Suggestion ->
        true
      | (Explanation | Background | Suggestion), _ -> false)
    && equal_inlines left.content right.content
    && equal_children left.children right.children

  and equal_children left right =
    List.length left = List.length right
    && List.for_all2
         (fun ((left_relation : Diagnostic.Relation.t), left_block)
              ((right_relation : Diagnostic.Relation.t), right_block) ->
           (match left_relation, right_relation with
             | Claim, Claim | Elaboration, Elaboration -> true
             | (Claim | Elaboration), _ -> false)
           && equal_block left_block right_block)
         left right

  let same_children (left : term Nlg.plan) (right : term Nlg.plan) =
    let left = realized left.children and right = realized right.children in
    List.length left = List.length right && List.for_all2 equal_block left right
end

let rec diagnose_inclusion_tree request ~sides ~in_parameter
    (tree : Inclusion.tree) : story list =
  match tree with
  | Inclusion.Leaf leaf ->
    diagnose_inclusion_leaf request ~sides ~in_parameter leaf
  | Inclusion.Item { item; got_loc; expected_loc; children } -> begin
    let in_parameter =
      match item with
      | Inclusion.Item_functor_parameter _ -> true
      | Inclusion.Direction _ -> in_parameter
      | Inclusion.Item_module _ | Inclusion.Item_module_type _
      | Inclusion.Item_type _ | Inclusion.Item_extension_constructor _ ->
        false
    in
    let children =
      let rendered =
        List.map (diagnose_inclusion_tree request ~sides ~in_parameter) children
      in
      match children, rendered with
      | ( [ Inclusion.Item { item = Inclusion.Direction _; _ };
            Inclusion.Item { item = Inclusion.Direction _; _ } ],
          [[first]; [second]] )
        when Explanation.same_children first.frame second.frame ->
        [{ first with frame = { first.frame with Nlg.statement = None } }]
      | _ -> List.concat rendered
    in
    match children with
    | [] -> []
    | children ->
      [ { frame =
            item_frame ~sides item ~got_loc ~expected_loc
              ~children:(frames children);
          axes = blamed_axes children
        } ]
    end

let diagnose_inclusion request ~sides tree =
  diagnose_inclusion_tree request ~sides ~in_parameter:false tree

let diagnose_inclusion_frame request frame children =
  let { reported_loc = loc; _ } = request in
  match children with
  | [] -> []
  | children ->
    [ { frame =
          { (inclusion_frame ~loc frame) with
            Nlg.children = claims (frames children)
          };
        axes = blamed_axes children
      } ]

let diagnose_typemod request ~loc ~env err =
  let { context = { inclusion_site_at; _ }; _ } = request in
  match err with
  | Typemod.Not_included (incl_env, all)
  | Typemod.Not_included_functor (incl_env, all) ->
    let site =
      match (all : Includemod.Error.all) with
      | In_Compilation_unit (_, { got; _ }) -> `Unit got
      | In_Signature _ | In_Include_functor_signature _ | In_Module_type _
      | In_Module_type_substitution _ | In_Type_declaration _
      | In_Jkind_declaration _ | In_Expansion _ -> (
        match inclusion_site_at loc with
        | Some site -> `Site site
        | None -> `Unknown)
    in
    diagnose_inclusion_frame request site
      (List.concat_map
         (diagnose_inclusion request ~sides:declaration_sides)
         (Inclusion.of_all
            ~env:
              { Includemod.Functor_inclusion_diff.i_env = incl_env;
                i_subst = Subst.identity
              }
            ~fallback:loc all))
  | Typemod.Strengthening_mismatch (lid, (incl_env, all)) ->
    let sides =
      { expected_name = [Nlg.txt "the module type"];
        actual_name = [Nlg.txt "the module"]
      }
    in
    diagnose_inclusion_frame request
      (`Strengthening (longident_name lid))
      (List.concat_map
         (diagnose_inclusion request ~sides)
         (Inclusion.of_all
            ~env:
              { Includemod.Functor_inclusion_diff.i_env = incl_env;
                i_subst = Subst.identity
              }
            ~fallback:loc all))
  | Typemod.With_makes_applicative_functor_ill_typed (lid, path, (incl_env, all))
    ->
    let sides =
      { expected_name = [Nlg.txt "the functor's parameter"];
        actual_name = [Nlg.txt "the module after substitution"]
      }
    in
    diagnose_inclusion_frame request
      (`Applicative_functor (Path.name path, longident_name lid))
      (List.concat_map
         (diagnose_inclusion request ~sides)
         (Inclusion.of_all
            ~env:
              { Includemod.Functor_inclusion_diff.i_env = incl_env;
                i_subst = Subst.identity
              }
            ~fallback:loc all))
  | Typemod.With_mismatch (lid, (incl_env, all)) ->
    let sides =
      { expected_name = [Nlg.txt "the new definition"];
        actual_name = [Nlg.txt "the original definition"]
      }
    in
    diagnose_inclusion_frame request
      (`Substitution (longident_name lid))
      (List.concat_map
         (diagnose_inclusion request ~sides)
         (Inclusion.of_all
            ~env:
              { Includemod.Functor_inclusion_diff.i_env = incl_env;
                i_subst = Subst.identity
              }
            ~fallback:loc all))
  | err ->
    let (_ : Scope.t) = typemod_scope err in
    diagnose_unexplained request (Typemod.Error (loc, env, err))

let diagnose_includemod_apply request ~env ~app_name ~mty_f ~args =
  let { reported_loc = loc; _ } = request in
  let patch =
    let rec drop_trailing_inserts = function
      | Diffing.Insert _ :: rest -> drop_trailing_inserts rest
      | rest -> List.rev rest
    in
    drop_trailing_inserts
      (List.rev (Includemod.Functor_app_diff.diff env ~f:mty_f ~args))
  in
  let failing =
    List.filter_map
      (fun change ->
        match change with
        | Diffing.Change ((descr, _, _), _, Includemod.Error.Mismatch d) ->
          Some (descr, d)
        | Diffing.Change (_, _, Includemod.Error.Incompatible_params _)
        | Diffing.Delete _ | Diffing.Insert _ | Diffing.Keep _ ->
          None)
      patch
  in
  let shape_claims =
    List.concat_map
      (fun change ->
        match change with
        | Diffing.Delete (descr, _, _) ->
          let named =
            match (descr : Includemod.Error.functor_arg_descr) with
            | Named path -> [Nlg.code (Path.name path)]
            | Anonymous | Unit | Empty_struct -> [Nlg.txt "this argument"]
          in
          plain_story
            ~claim:(named @ [Nlg.txt " is a surplus argument"])
            ~suggestion:[[Nlg.txt "remove it"]]
            ()
        | Diffing.Insert param ->
          let named =
            match (param : Types.functor_parameter) with
            | Named (Some id, _, _) ->
              [Nlg.txt "the parameter "; Nlg.code (Ident.name id)]
            | Named (None, _, _) | Unit -> [Nlg.txt "a parameter"]
          in
          plain_story
            ~claim:(Nlg.txt "no argument is given for " :: named)
            ~suggestion:
              [[Nlg.txt "supply the missing argument in that position"]]
            ()
        | Diffing.Change (_, _, Includemod.Error.Incompatible_params (arg, _))
          ->
          let generative_expected =
            match (arg : Includemod.Error.functor_arg_descr) with
            | Unit -> false
            | Anonymous | Named _ | Empty_struct -> true
          in
          plain_story
            ~claim:
              [ Nlg.txt
                  (if generative_expected
                   then "this argument is a module"
                   else "this argument is ()") ]
            ~contrast:
              [ Nlg.txt
                  (if generative_expected
                   then "but the functor expects () at this position"
                   else "but the functor expects a module at this position") ]
            ()
        | Diffing.Change (_, _, Includemod.Error.Mismatch _) | Diffing.Keep _ ->
          [])
      patch
  in
  let functor_name =
    match (app_name : Includemod.application_name) with
    | Anonymous_functor -> None
    | Named_leftmost_functor lid -> longident_name lid
    | Full_application_path lid -> longident_name (leftmost_functor lid)
  in
  let argument =
    match failing with
    | [(Includemod.Error.Named path, _)] -> Some (Path.name path)
    | _ -> None
  in
  let all_deleted =
    (not (List.is_empty patch))
    && List.for_all
         (fun change ->
           match change with
           | Diffing.Delete _ -> true
           | Diffing.Change _ | Diffing.Insert _ | Diffing.Keep _ -> false)
         patch
  in
  if all_deleted
  then
    plain_story
      ~claim:
        [ (match functor_name with
          | Some name -> Nlg.code name
          | None -> Nlg.txt "this module");
          Nlg.txt " is not a functor, so it cannot be applied" ]
      ()
  else
    let frame =
      if List.is_empty failing
      then `Ill_typed_application functor_name
      else `Application (functor_name, argument)
    in
    diagnose_inclusion_frame request frame
      (let sides =
         { expected_name = [Nlg.txt "the parameter"];
           actual_name = [Nlg.txt "the argument"]
         }
       in
       shape_claims
       @ List.concat_map
           (fun (_, (d : Includemod.Error.module_type_diff)) ->
             Inclusion.of_module_type_symptom
               ~env:
                 { Includemod.Functor_inclusion_diff.i_env = env;
                   i_subst = Subst.identity
                 }
               ~fallback:loc ~orientation:Orientation.Got_is_actual d.symptom
             |> List.concat_map (diagnose_inclusion request ~sides))
           failing)

let diagnose_typedecl request ~loc err =
  let { context = { constructor_arguments_at; _ }; _ } = request in
  match err with
  | Typedecl.Constructor_submode_failed e ->
    let extra_rules _axis =
      { no_extra_rules with
        for_actual =
          [ elaboration
              (Nlg.sentence ~kind:Diagnostic.Kind.Background
                 (phrase
                    [ Nlg.txt
                        "all argument types must mode-cross for rebinding to \
                         succeed" ])) ]
      }
    in
    let arguments =
      Option.value (constructor_arguments_at loc None) ~default:[]
    in
    mode_stories request ~extra_rules
      ~actuality_fallback:(Arguments_do_not_cross arguments)
      (Mode.Value.walk_error_all (loc, Mode.Hint.Unknown) e)
  | Typedecl.Definition_mismatch (ty, env, Some mismatch) ->
    begin match Types.get_desc ty with
    | Types.Tconstr (path, _, _) ->
      let name = Path.name path in
      let equated_loc =
        match Env.find_type path env with
        | (decl : Types.type_declaration) -> Some decl.type_loc
        | exception Not_found -> None
      in
      let sides =
        { expected_name = [Nlg.txt "this definition"];
          actual_name = [Nlg.txt "the definition of "; Nlg.code name]
        }
      in
      diagnose_inclusion_frame request
        (`Equation (name, equated_loc))
        (List.concat_map
           (diagnose_inclusion request ~sides)
           (Inclusion.leaves
              (Inclusion.type_leaves ~orientation:Orientation.Got_is_actual
                 ~expected_loc:(Some loc) ~actual_loc:equated_loc mismatch)))
    | Types.Tvar _ | Types.Tarrow _ | Types.Ttuple _ | Types.Tunboxed_tuple _
    | Types.Tobject _ | Types.Tfield _ | Types.Tquote _ | Types.Tsplice _
    | Types.Tquote_eval _ | Types.Tnil | Types.Tlink _ | Types.Tsubst _
    | Types.Tvariant _ | Types.Tunivar _ | Types.Tpoly _ | Types.Trepr _
    | Types.Tpackage _ | Types.Tof_kind _ | Types.Tmod _ | Types.Tbox _ ->
      []
    end
  | Typedecl.Jkind_mismatch_of_type (_env, _ty, v) ->
    Option.value (jkind_crossing_story ~loc ~what:"this type" v) ~default:[]
  | Typedecl.Jkind_mismatch_of_path (_env, path, v) ->
    Option.value
      (jkind_crossing_story ~loc ~what:("type " ^ Path.name path) v)
      ~default:[]
  | Typedecl.Atomic_field_must_be_mutable name ->
    let open Nlg in
    plain_story
      ~claim:
        [ ref_source loc [code name];
          txt " is declared ";
          code "[@atomic]";
          txt " but is not mutable" ]
      ~educate:
        [ [ txt
              "atomicity describes how a field is written, so only a mutable \
               field can be atomic" ] ]
      ~suggestion:
        [[txt "add "; code "mutable"; txt ", or drop the "; code "[@atomic]"]]
      ()
  | Typedecl.Non_value_atomic_field ->
    let open Nlg in
    plain_story
      ~claim:[ref_source loc [txt "this field is declared "]; code "[@atomic]"]
      ~contrast:[txt "but its type does not have layout "; code "value"]
      ~educate:
        [ [ txt
              "atomic access is implemented on values, which are word-sized \
               and visible to the collector; unboxed layouts have no atomic \
               representation" ] ]
      ~suggestion:[[txt "use the boxed type, or drop the "; code "[@atomic]"]]
      ()
  | Typedecl.Unboxed_mutable_label ->
    let open Nlg in
    plain_story
      ~claim:
        [ ref_source loc [txt "this label is declared "];
          code "mutable";
          txt ", but it belongs to an unboxed record" ]
      ~educate:
        [ [ txt
              "an unboxed record has no heap block and no identity, so there \
               is no cell to mutate" ] ]
      ~suggestion:
        [ [ txt "use a boxed record, or store the unboxed record in a ";
            code "mutable";
            txt " field of one" ] ]
      ()
  | Typedecl.Unsafe_mode_crossing_on_invalid_type_kind ->
    let open Nlg in
    plain_story
      ~claim:
        [ ref_source loc [txt "this declaration is marked "];
          code "[@@unsafe_allow_any_mode_crossing]" ]
      ~contrast:
        [ txt
            "but the attribute applies only to records, unboxed products and \
             variants" ]
      ~educate:
        [ [ txt
              "the attribute overrides the mode bounds computed from a type's \
               fields or constructors; a type with neither has nothing to \
               override" ] ]
      ()
  | err ->
    let (_ : Scope.t) = typedecl_scope err in
    diagnose_unexplained request (Typedecl.Error (loc, err))

let diagnose_typecore request ~loc ~env err =
  let { source; context = { constructor_arguments_at; _ }; _ } = request in
  match err with
  | Typecore.Submode_failed (e, reason) ->
    let extra_rules, actuality_fallback =
      match reason with
      | Typecore.Constructor lid ->
        ( (fun _axis ->
            { no_extra_rules with
              for_actual =
                [ elaboration
                    (Nlg.sentence ~kind:Diagnostic.Kind.Background
                       (phrase
                          [ Nlg.txt
                              "using a constructor across a mode boundary \
                               requires all its argument types to mode-cross" ]))
                ]
            }),
          Some
            (Arguments_do_not_cross
               (Option.value
                  (constructor_arguments_at loc (Some lid))
                  ~default:[])) )
      | Typecore.Application result_type ->
        ( (fun axis ->
            { no_extra_rules with
              for_expected = plan_partial_application_hint ~axis result_type
            }),
          None )
      | Typecore.Other -> (fun _axis -> no_extra_rules), None
    in
    mode_stories request ~extra_rules ?actuality_fallback
      (Mode.Value.walk_error_all (loc, Mode.Hint.Expression) e)
  | Typecore.Curried_application_complete (lbl, e, loc_kind) ->
    let subject_override : subject option =
      match loc_kind with
      | `Prefix -> None
      | `Single_arg ->
        Some
          (subject ~span:loc
             [Phrase.Text "the application up to this argument"])
      | `Entire_apply ->
        let up_to =
          match (lbl : Typedtree.arg_label) with
          | Nolabel -> [Phrase.Text "this argument"]
          | Labelled s | Position s -> [Phrase.Code ("~" ^ s)]
          | Optional s -> [Phrase.Code ("?" ^ s)]
        in
        Some (subject ~span:loc (Phrase.Text "the application up to " :: up_to))
    in
    let restricted_word (axis : Mode.Axis.packed) =
      match axis with
      | Mode.Axis.P Mode.Axis.Areality ->
        Some (mode_const_word (Comonadic Areality) Mode.Locality.Const.Local)
      | Mode.Axis.P Mode.Axis.Linearity ->
        Some (mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once)
      | Mode.Axis.P _ -> None
    in
    let suggestion_phrases =
      match loc_kind with
      | `Prefix ->
        [[Nlg.txt "try wrapping the marked application in parentheses"]]
      | `Single_arg ->
        [ [Nlg.txt "try splitting the application in two"];
          [ Nlg.txt
              "the arguments after this one in the function's type should be \
               applied separately" ] ]
      | `Entire_apply ->
        let named =
          match (lbl : Typedtree.arg_label) with
          | Nolabel -> [Nlg.txt "this argument"]
          | Labelled s | Position s -> [Nlg.code ("~" ^ s)]
          | Optional s -> [Nlg.code ("?" ^ s)]
        in
        [ [Nlg.txt "try splitting the application in two"];
          (Nlg.txt "the arguments after " :: named)
          @ [Nlg.txt " in the function's type should be applied separately"] ]
    in
    let extra_rules axis =
      match restricted_word axis with
      | None -> no_extra_rules
      | Some word ->
        { no_extra_rules with
          for_expected =
            [ elaboration
                (Nlg.sentence ~kind:Diagnostic.Kind.Background
                   (phrase
                      [ Nlg.txt "when passing or calling ";
                        word;
                        Nlg.txt
                          " values, extra arguments are passed in a separate \
                           application" ])) ]
            @ List.map
                (fun words ->
                  elaboration
                    (Nlg.sentence ~kind:Diagnostic.Kind.Suggestion
                       (phrase words)))
                suggestion_phrases
        }
    in
    mode_stories request ~extra_rules ?subject_override
      (Mode.Alloc.walk_error_all (loc, Mode.Hint.Expression) e)
  | Typecore.Mode_mismatch (kind, (step, e)) ->
    let subject_override : subject option =
      match kind with
      | Typecore.Parameter ->
        Some (subject ~span:loc [Phrase.Text "this function's parameter"])
      | Typecore.Return ->
        Some (subject ~span:loc [Phrase.Text "this function's return value"])
    in
    let axes = Mode.Alloc.walk_error_all (loc, Mode.Hint.Expression) e in
    let axes =
      match (step : Mode.equate_step) with
      | Left_le_right -> axes
      | Right_le_left ->
        List.map
          (fun (a : Mode.axis_error) ->
            { a with
              actual_chain = a.expected_chain;
              expected_chain = a.actual_chain;
              actual_description = a.expected_description;
              expected_description = a.actual_description;
              actual_loosening = a.expected_loosening;
              expected_loosening = a.actual_loosening
            })
          axes
    in
    let informative chain =
      List.exists Message.is_informative (Message.of_chain ~source chain)
    in
    let axes =
      match
        List.filter
          (fun (a : Mode.axis_error) ->
            informative a.actual_chain || informative a.expected_chain)
          axes
      with
      | [] -> axes
      | axes -> axes
    in
    mode_stories request ?subject_override axes
  | Typecore.Uncurried_function_escapes e ->
    let subject_override : subject option =
      Some
        (subject ~span:loc [Phrase.Text "this function when partially applied"])
    in
    let extra_rules _axis =
      { no_extra_rules with
        for_actual =
          [ elaboration
              (Nlg.sentence ~kind:Diagnostic.Kind.Background
                 (phrase
                    [ Nlg.txt
                        "partially applying a function closes over the \
                         arguments given so far" ])) ]
      }
    in
    mode_stories request ~extra_rules ?subject_override
      (Mode.Alloc.walk_error_all (loc, Mode.Hint.Expression) e)
  | Typecore.Overwrite_of_invalid_term ->
    let open Nlg in
    plain_story
      ~claim:[ref_source loc [txt "this term cannot be overwritten"]]
      ~contrast:
        [ txt
            "but overwriting works only on tuples, constructors and boxed \
             records" ]
      ~educate:
        [ [ code "overwrite_";
            txt
              " reuses an existing block, so the value must be one that \
               occupies a block of its own" ] ]
      ()
  | Typecore.Block_index_modality_mismatch { mut; err } ->
    let open Nlg in
    let _step, Mode.Modality.Error (ax, { left; right = _ }) = err in
    let axis_name =
      match Mode.Modality.Axis.to_value (Mode.Modality.Axis.P ax) with
      | Mode.Value.Axis.P vax ->
        Format_doc.asprintf "%a" Mode.Value.Axis.print vax
    in
    let actual_words =
      if Mode.Modality.Per_axis.is_id ax left
      then [txt ("no modality on the " ^ axis_name ^ " axis")]
      else
        [ txt "the modality ";
          code (Format_doc.asprintf "%a" (Mode.Modality.Per_axis.print ax) left)
        ]
    in
    plain_story
      ~claim:
        (ref_source loc [txt "this block index reaches a field with "]
        :: actual_words)
      ~contrast:
        [ txt
            ("but a block index over "
            ^ (match mut with true -> "mutable" | false -> "immutable")
            ^ " elements requires the modalities implied by its declaration, \
               and no others") ]
      ~educate:
        [ [ txt
              "this is a current limitation: the block-index primitives are \
               typed with one fixed modality and cannot express others yet" ] ]
      ~suggestion:
        [ [ txt
              "remove the modality from the field, or read the field directly \
               instead of taking an index" ] ]
      ()
  | Typecore.Exclave_in_nontail_position ->
    let open Nlg in
    let subject = subject_of_loc ~source ~fallback:"this expression" loc in
    plain_story
      ~claim:
        [ ref_source loc
            (subject_words subject @ [txt " is not in tail position"]) ]
      ~contrast:
        [ txt "but ";
          code "exclave_";
          txt " must be the last thing the enclosing region evaluates" ]
      ~educate:
        [ [ code "exclave_";
            txt
              " puts a value in the caller's region, so it can only appear \
               where the current region is about to end" ] ]
      ()
  | Typecore.Exclave_returns_not_local ->
    let open Nlg in
    plain_story
      ~claim:
        [ ref_source loc [txt "this expression is "];
          mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
          txt ", because ";
          code "exclave_";
          txt " makes it so" ]
      ~contrast:
        [ txt "but the enclosing function is not declared to return a ";
          mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
          txt " value" ]
      ~educate:
        [ [ txt "a function containing ";
            code "exclave_";
            txt " allocates into its caller's region, so it must itself be ";
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt "-returning" ] ]
      ~suggestion:
        [ [ txt "annotate the function's result as ";
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt ", or drop the ";
            code "exclave_" ] ]
      ()
  | Typecore.Tail_call_local_returning ->
    let open Nlg in
    let subject = subject_of_loc ~source ~fallback:"this call" loc in
    plain_story
      ~claim:
        [ ref_source loc
            (subject_words subject
            @ [ txt " returns a ";
                mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
                txt " value" ]) ]
      ~contrast:
        [ txt "but it is in the tail position of a function that is not ";
          mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
          txt "-returning" ]
      ~educate:
        [ [ txt "a tail call hands its result straight to the caller, so a ";
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt
              "-returning call can only sit in the tail of a local-returning \
               function" ] ]
      ~suggestion:
        [ [ txt "bind the result first, as in ";
            code "let r = ... in r";
            txt ", so the call is no longer in tail position" ] ]
      ()
  | Typecore.Bad_tail_annotation kind -> (
    let open Nlg in
    let claim =
      [ref_source loc [txt "this call is annotated "]; code "[@tail]"]
    in
    match kind with
    | `Conflict ->
      plain_story ~claim
        ~contrast:[txt "but its tail-call annotations contradict each other"]
        ~educate:
          [ [ txt "a call cannot be required to be a tail call by ";
              code "[@tail]";
              txt " and required not to be by ";
              code "[@nontail]" ] ]
        ~suggestion:[[txt "keep only one tail-call annotation"]]
        ()
    | `Not_a_tailcall ->
      plain_story ~claim
        ~contrast:[txt "but it is not in tail position"]
        ~educate:
          [ [ txt
                "a call is a tail call only when its result is the enclosing \
                 function's result" ] ]
        ~suggestion:
          [ [ txt "use ";
              code "[@tail hint]";
              txt " to ask for the optimisation only where it applies" ] ]
        ())
  | Typecore.Always_heap_allocation kind ->
    let open Nlg in
    let what =
      match (kind : Typecore.always_heap_allocation) with
      | Lazy -> "a lazy expression"
      | Module -> "a module"
      | Object -> "an object"
      | List_comprehension -> "a list comprehension"
      | Array_comprehension -> "an array comprehension"
    in
    plain_story
      ~claim:
        [ ref_source loc
            [txt ("the compiler cannot stack-allocate " ^ what ^ " yet")] ]
      ~educate:[[txt "this is a current limitation, not a rule of the language"]]
      ~suggestion:
        [ [ txt "drop the ";
            code "stack_";
            txt " and let this allocate on the heap" ] ]
      ()
  | Typecore.Always_static_allocation kind ->
    let open Nlg in
    let what =
      match (kind : Typecore.always_static_allocation) with
      | Constant -> "a literal"
      | Src_pos -> "a source position literal"
      | Unboxed_unit -> "an unboxed unit literal"
      | Unboxed_bool -> "an unboxed boolean literal"
    in
    plain_story
      ~claim:[ref_source loc [txt (what ^ " is not allocated at runtime")]]
      ~contrast:
        [ txt "but ";
          code "stack_";
          txt " must be applied to something that allocates" ]
      ~educate:
        [ [ code "stack_";
            txt
              " chooses where an allocation happens, and this value needs no \
               allocation to choose from" ] ]
      ~suggestion:[[txt "remove the "; code "stack_"]]
      ()
  | Typecore.Not_allocation ->
    let open Nlg in
    let subject = subject_of_loc ~source ~fallback:"this expression" loc in
    plain_story
      ~claim:
        [ref_source loc (subject_words subject @ [txt " does not allocate"])]
      ~contrast:
        [ txt "but ";
          code "stack_";
          txt " must be applied to something that allocates" ]
      ~educate:
        [ [ txt
              "a record, tuple, array, variant, closure or boxed field read \
               allocates; a variable, constant or function result does not" ];
          [ code "stack_";
            txt
              " chooses where an allocation happens; it cannot move a value \
               that already exists" ] ]
      ~suggestion:[[txt "remove the "; code "stack_"]]
      ()
  | Typecore.Atomic_in_pattern lid ->
    let open Nlg in
    let name =
      match longident_name lid with Some n -> n | None -> "this field"
    in
    plain_story
      ~claim:
        [ ref_source loc [txt "this pattern matches on "];
          code name;
          txt ", which is an atomic field" ]
      ~educate:
        [ [ txt
              "atomic fields are forbidden in patterns: the field may be read \
               zero, one or several times depending on the patterns around it, \
               so it is hard to reason about when the atomic read happens" ] ]
      ~suggestion:
        [ [ txt "match the field with ";
            code "_";
            txt
              " and read it in the body -- a wildcard is allowed, so every \
               field can still be listed" ] ]
      ()
  | Typecore.Label_not_atomic lid ->
    let open Nlg in
    let name =
      match longident_name lid with Some n -> n | None -> "this field"
    in
    plain_story
      ~claim:
        [ref_source loc [code "[%atomic.loc]"]; txt " needs an atomic field"]
      ~contrast:[txt "but "; code name; txt " is not declared atomic"]
      ~suggestion:[[txt "declare the field as "; code "mutable ... [@atomic]"]]
      ()
  | Typecore.Modalities_on_atomic_field lid ->
    let open Nlg in
    let name =
      match longident_name lid with Some n -> n | None -> "this field"
    in
    plain_story
      ~claim:[ref_source loc [code name]; txt " carries a modality of its own"]
      ~contrast:
        [ txt "but a field given to ";
          code "[%atomic.loc]";
          txt " may carry only the modalities implied by ";
          code "mutable" ]
      ~suggestion:[[txt "remove the modality from the field's declaration"]]
      ()
  | Typecore.Invalid_atomic_loc_payload ->
    let open Nlg in
    plain_story
      ~claim:
        [ ref_source loc [code "[%atomic.loc]"];
          txt " takes a record field access, like ";
          code "r.x";
          txt ", but this payload is not one" ]
      ()
  | err ->
    let (_ : Scope.t) = typecore_scope err in
    diagnose_unexplained request (Typecore.Error (loc, env, err))

let error ~source ~context ~pronouns ~loc exn : Diagnostic.t option =
  let request = { source; context; pronouns; reported_loc = loc } in
  let body =
    match exn with
    | Typecore.Error (loc, env, err) -> diagnose_typecore request ~loc ~env err
    | Typemod.Error (loc, env, err) -> diagnose_typemod request ~loc ~env err
    | Includemod.Apply_error { env; app_name; mty_f; args; _ } ->
      diagnose_includemod_apply request ~env ~app_name ~mty_f ~args
    | Typedecl.Error (loc, err) -> diagnose_typedecl request ~loc err
    | Typetexp.Error (loc, env, err) -> diagnose_typetexp request ~loc ~env err
    | Env.Error (Env.Lookup_error (loc, _env, lookup_error)) ->
      diagnose_env_lookup request ~loc lookup_error
    | Uniqueness_analysis.Usage.Unique_use_during_borrowing error ->
      diagnose_unique_use_during_borrowing request error
    | Uniqueness_analysis.Error err -> diagnose_uniqueness request err
    | exn -> diagnose_unexplained request exn
  in
  realize ~documentation:context.documentation
    ~reported_loc:request.reported_loc body
