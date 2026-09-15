module Annotation = Structured_diagnostic.Annotation
module Form = Structured_diagnostic.Form
module Inline = Structured_diagnostic.Inline
module Kind = Structured_diagnostic.Kind
module Location_key = Structured_diagnostic.Location_key
module Relation = Structured_diagnostic.Relation

module Phrase = struct
  type case =
    | Subject
    | Possessive

  type number =
    | Singular
    | Plural

  type word =
    | Text of string
    | Code of string

  type 'term segment =
    | Word of word
    | Copula of number
    | Term of 'term
    | Source of
        { loc : Location.t;
          content : 'term segment list
        }
    | Mention of
        { span : Location.t option;
          name : word list;
          case : case;
          form : Form.t
        }

  type 'term t = 'term segment list
end

let txt s : _ Phrase.segment = Word (Text s)

let code s : _ Phrase.segment = Word (Code s)

let copula : _ Phrase.segment = Copula Singular

let copula_agreeing (number : Phrase.number) : _ Phrase.segment = Copula number

let term t : _ Phrase.segment = Term t

let ref_source loc content : _ Phrase.segment = Source { loc; content }

let ordinal n =
  let suffix =
    let mod100 = n mod 100 in
    if mod100 >= 11 && mod100 <= 13
    then "th"
    else match n mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in
  string_of_int n ^ suffix

let rec longident_name (lid : Longident.t) : string option =
  match lid with
  | Lident name -> Some name
  | Ldot (prefix, name) ->
    Option.map
      (fun prefix -> prefix ^ "." ^ name.txt)
      (longident_name prefix.txt)
  | Lapply _ -> None

type subject =
  { name : Phrase.word list;
    span : Location.t option
  }

let subject ?span name : subject = { name; span }

let sentence_subject (subject : subject) : subject option =
  Option.map (fun (_ : Location.t) -> subject) subject.span

let subject_entity (subject : subject) : Location_key.t option =
  Option.map Location_key.of_location subject.span

let mention ~(case : Phrase.case) (subject : subject) : _ Phrase.segment =
  Mention { span = subject.span; name = subject.name; case; form = Form.Name }

let pronoun ~(case : Phrase.case) (subject : subject) : _ Phrase.segment =
  Mention
    { span = subject.span; name = subject.name; case; form = Form.Pronoun }

type necessity =
  | Inherit
  | Necessary
  | Unnecessary

type verbosity =
  | Full
  | Minimal

type role =
  | Statement
  | Dependent
  | Group
  | Explanation of necessity
  | Block of necessity

type 'term fragment =
  { role : role;
    relation : Relation.t;
    kind : Kind.t;
    subject : subject option;
    content : 'term Phrase.t;
    children : 'term fragment list
  }

let rec nominals (segments : _ Phrase.segment list) :
    (Location_key.t * Form.t) list =
  List.concat_map
    (fun (seg : _ Phrase.segment) ->
      match seg with
      | Word _ | Copula _ | Term _ -> []
      | Source { loc = _; content } -> nominals content
      | Mention { span; name = _; case = _; form } ->
        Option.to_list
          (Option.map (fun span -> Location_key.of_location span, form) span))
    segments

let fragment ?subject ~role ~relation ~kind content : _ fragment =
  (match content with
  | [] -> invalid_arg "Diagnostic_nlg.fragment: empty phrase"
  | _ :: _ -> ());
  let subject = Option.bind subject sentence_subject in
  Option.iter
    (fun (subject : subject) ->
      let mentioned =
        match subject_entity subject with
        | None -> false
        | Some entity ->
          List.exists
            (fun (e, _) -> Location_key.equal e entity)
            (nominals content)
      in
      if not mentioned then
        invalid_arg "Diagnostic_nlg.fragment: subject not mentioned in phrase")
    subject;
  { role; relation; kind; subject; content; children = [] }

let state ?subject content =
  fragment ?subject ~role:Statement ~relation:Relation.Claim
    ~kind:Kind.Explanation content

let but ?subject words =
  fragment ?subject ~role:Dependent ~relation:Relation.Claim
    ~kind:Kind.Explanation (txt "but " :: words)

let reason ?subject content =
  fragment ?subject ~role:Dependent ~relation:Relation.Elaboration
    ~kind:Kind.Explanation content

let rule content =
  fragment ~role:Dependent ~relation:Relation.Elaboration ~kind:Kind.Rule
    content

let is_rule fragment =
  match fragment.kind with
  | Kind.Rule -> true
  | Kind.Explanation | Kind.Suggestion -> false

let suggestion content =
  fragment ~role:Dependent ~relation:Relation.Elaboration ~kind:Kind.Suggestion
    content

let check_block = function
  | { role = Statement; content = _ :: _; _ } :: _ -> ()
  | _ ->
    invalid_arg "Diagnostic_nlg.block: an opening statement is required"

let check_explanation children =
  let rec starts_with_explanation = function
    | [] -> false
    | { content = []; children; _ } :: rest ->
      starts_with_explanation (children @ rest)
    | { kind = Kind.Explanation; _ } :: _ -> true
    | { kind = Kind.Rule | Kind.Suggestion; _ } :: _ -> false
  in
  if not (starts_with_explanation children) then
    invalid_arg "Diagnostic_nlg.explanation: an explanation is required"

let with_children children fragment =
  (match fragment.role with
  | Block _ -> check_block children
  | Explanation _ -> check_explanation children
  | Statement | Dependent | Group -> ());
  { fragment with children }

let group children : _ fragment =
  { role = Group;
    relation = Relation.Claim;
    kind = Kind.Explanation;
    subject = None;
    content = [];
    children
  }

let block ?(necessity = Inherit) children =
  check_block children;
  { (group children) with role = Block necessity }

let explanation ?(necessity = Inherit) children =
  check_explanation children;
  { (group children) with role = Explanation necessity }

let focus ~on fragments =
  let rec mark fragment =
    let role =
      match fragment.role with
      | Explanation _ ->
        Explanation (if fragment == on then Necessary else Unnecessary)
      | Statement | Dependent | Group | Block _ -> fragment.role
    in
    { fragment with role; children = List.map mark fragment.children }
  in
  List.map mark fragments

let without_text fragment =
  let role =
    match fragment.role with
    | (Block _ | Explanation _) as role -> role
    | Statement | Dependent | Group -> Group
  in
  { fragment with role; kind = Kind.Explanation; subject = None; content = [] }

let clip ~verbosity fragments =
  let rec select ~necessary fragment =
    let necessary =
      match fragment.role with
      | Block Necessary | Explanation Necessary -> true
      | Block Unnecessary | Explanation Unnecessary -> false
      | Block Inherit | Explanation Inherit | Statement | Dependent | Group ->
        necessary
    in
    let children = List.filter_map (select ~necessary) fragment.children in
    let fragment = if necessary then fragment else without_text fragment in
    match fragment.content, children with
    | [], [] -> None
    | _ -> Some { fragment with children }
  in
  match verbosity with
  | Full -> fragments
  | Minimal -> List.filter_map (select ~necessary:true) fragments

let naturalize (fragments : 'term fragment list) : 'term fragment list =
  let last_mention s =
    match List.rev (nominals s.content) with
    | [] -> None
    | (e, _) :: _ -> Some e
  in
  let pronouns_of (s : _ fragment) =
    List.filter_map
      (fun (entity, form) ->
        match (form : Form.t) with
        | Pronoun -> Some entity
        | Name -> None)
      (nominals s.content)
  in
  let distinct_entities entities =
    List.rev
      (List.fold_left
         (fun distinct entity ->
           if List.exists (Location_key.equal entity) distinct then distinct
           else entity :: distinct)
         [] entities)
  in
  let rewrite ~prev_last ~prev_pronouns (s : _ fragment) : _ fragment =
    let sentence_entity = Option.bind s.subject subject_entity in
    let rec segment (prev_mention, subject_pending) (seg : _ Phrase.segment) =
      match seg with
      | Word _ | Copula _ | Term _ -> (prev_mention, subject_pending), seg
      | Source { loc; content } ->
        let state, content =
          rewrite_segments (prev_mention, subject_pending) content
        in
        state, Phrase.Source { loc; content }
      | Mention { span = None; name = _; case = _; form = _ } ->
        (prev_mention, subject_pending), seg
      | Mention { span = Some span; name; case; form } ->
        let entity = Location_key.of_location span in
        let entity_is_sentence_subject =
          match sentence_entity with
          | Some e -> Location_key.equal e entity
          | None -> false
        in
        let is_subject = subject_pending && entity_is_sentence_subject in
        let intra =
          match prev_mention with
          | Some e -> Location_key.equal e entity
          | None -> false
        in
        let inter =
          (match prev_last with
          | Some e -> Location_key.equal e entity
          | None -> false)
          && List.for_all
               (fun e -> Location_key.equal e entity)
               prev_pronouns
        in
        let form : Form.t =
          match form with
          | Pronoun -> if intra || inter then Pronoun else Name
          | Name ->
            if entity_is_sentence_subject && intra || is_subject && inter
            then Pronoun
            else Name
        in
        ( (Some entity, subject_pending && not is_subject),
          Phrase.Mention { span = Some span; name; case; form } )
    and rewrite_segments state segments =
      List.fold_left_map segment state segments
    in
    let _state, content = rewrite_segments (None, true) s.content in
    let rewritten = { s with content } in
    match distinct_entities (pronouns_of rewritten) with
    | [] | [_] -> rewritten
    | retained_entity :: _ ->
      let rec disambiguate (seg : _ Phrase.segment) : _ Phrase.segment =
        match seg with
        | Word _ | Copula _ | Term _ -> seg
        | Source { loc; content } ->
          Source { loc; content = List.map disambiguate content }
        | Mention { span; name; case; form } ->
          let switches_referent =
            match span with
            | None -> false
            | Some span ->
              not
                (Location_key.equal
                   (Location_key.of_location span)
                   retained_entity)
          in
          let form : Form.t =
            match form with
            | Pronoun when switches_referent -> Name
            | Pronoun | Name -> form
          in
          Mention { span; name; case; form }
      in
      { rewritten with content = List.map disambiguate rewritten.content }
  in
  let antecedent frames =
    List.find_map
      (fun (_, (last, pronouns)) ->
        Option.map (fun last -> last, pronouns) last)
      frames
  in
  let rec go ~depth frames fragment =
    let enclosing_frames = frames in
    let frames =
      match fragment.role with
      | Block _ -> []
      | Statement | Dependent | Group | Explanation _ -> frames
    in
    let frames, rewritten =
      match fragment.content with
      | [] -> frames, fragment
      | _ :: _ ->
        let frames =
          List.filter (fun (frame_depth, _) -> frame_depth <= depth) frames
        in
        let prev_last, prev_pronouns =
          match antecedent frames with
          | None -> None, []
          | Some (last, pronouns) -> Some last, pronouns
        in
        let rewritten = rewrite ~prev_last ~prev_pronouns fragment in
        (depth, (last_mention fragment, pronouns_of rewritten)) :: frames,
        rewritten
    in
    let child_depth =
      match fragment.content with [] -> depth | _ :: _ -> depth + 1
    in
    let frames, children =
      List.fold_left_map (go ~depth:child_depth) frames fragment.children
    in
    let frames =
      match fragment.role with
      | Block _ -> enclosing_frames
      | Statement | Dependent | Group | Explanation _ -> frames
    in
    frames, { rewritten with children }
  in
  snd (List.fold_left_map (go ~depth:0) [] fragments)

let annotated annotation content : Inline.t = Annotated { annotation; content }

let inline_of_word (word : Phrase.word) : Inline.t =
  match word with
  | Text text -> Inline.Text text
  | Code text -> annotated Annotation.Code [Inline.Text text]

let realize_phrase ~term_entry ~term_words (phrase : _ Phrase.t) :
    Inline.t list =
  let rec segment ~followed_by_more ~pronoun_before (seg : _ Phrase.segment) =
    match seg with
    | Phrase.Word word ->
      false, [inline_of_word word]
    | Phrase.Copula number ->
      let word =
        match number with
        | Phrase.Singular ->
          if pronoun_before && followed_by_more then "'s" else " is"
        | Phrase.Plural -> " are"
      in
      false, [Inline.Text word]
    | Phrase.Term t ->
      let entry = term_entry t in
      let pronoun_before, content =
        segments ~followed_by_more ~pronoun_before (term_words t)
      in
      pronoun_before, [annotated (Annotation.Term entry) content]
    | Phrase.Source { loc; content } ->
      let pronoun_before, content =
        segments ~followed_by_more ~pronoun_before content
      in
      pronoun_before, [annotated (Annotation.Source loc) content]
    | Phrase.Mention { span; name; case; form } -> (
      let as_mention content =
        match span with
        | None -> content
        | Some entity ->
          [annotated (Annotation.Mention { entity; form }) content]
      in
      match form with
      | Form.Pronoun ->
        let word, pronoun_before =
          match case with
          | Phrase.Subject -> "it", true
          | Phrase.Possessive -> "its", false
        in
        pronoun_before, as_mention [Inline.Text word]
      | Form.Name ->
        let words = as_mention (List.map inline_of_word name) in
        let content =
          match case with
          | Phrase.Subject -> words
          | Phrase.Possessive -> words @ [Inline.Text "'s"]
        in
        false, content)
  and segments ~followed_by_more ~pronoun_before segs =
    match segs with
    | [] -> pronoun_before, []
    | seg :: rest ->
      let followed_by_more_here =
        match rest with
        | [] -> followed_by_more
        | _ :: _ -> true
      in
      let pronoun_before, realized =
        segment ~followed_by_more:followed_by_more_here ~pronoun_before seg
      in
      let pronoun_before, realized_rest =
        segments ~followed_by_more ~pronoun_before rest
      in
      pronoun_before, realized @ realized_rest
  in
  snd (segments ~followed_by_more:false ~pronoun_before:false phrase)

let capitalize_opening_word (content : Inline.t list) : Inline.t list =
  let rec capitalize ~in_code (inline : Inline.t) : Inline.t option =
    match inline with
    | Inline.Text "" -> None
    | Inline.Text text ->
      Some
        (if in_code then inline
         else Inline.Text (String.capitalize_ascii text))
    | Inline.Annotated { annotation; content } ->
      let in_code =
        match annotation with
        | Annotation.Code -> true
        | Annotation.Source _ | Annotation.Mention _ | Annotation.Term _ ->
          in_code
      in
      Option.map
        (fun content -> Inline.Annotated { annotation; content })
        (capitalize_content ~in_code content)
  and capitalize_content ~in_code (content : Inline.t list) :
      Inline.t list option =
    match content with
    | [] -> None
    | first :: rest -> (
      match capitalize ~in_code first with
      | Some first -> Some (first :: rest)
      | None ->
        Option.map
          (fun rest -> first :: rest)
          (capitalize_content ~in_code rest))
  in
  Option.value (capitalize_content ~in_code:false content) ~default:content

let rec closing_character (content : Inline.t list) : char option =
  List.fold_left
    (fun closing (inline : Inline.t) ->
      match inline with
      | Inline.Text "" -> closing
      | Inline.Text text -> Some text.[String.length text - 1]
      | Inline.Annotated { annotation = _; content } -> (
        match closing_character content with
        | None -> closing
        | Some character -> Some character))
    None content

let terminate_sentence (content : Inline.t list) : Inline.t list =
  match closing_character content with
  | None | Some ('.' | '?' | '!' | ':') -> content
  | Some _ -> content @ [Inline.Text "."]

let as_sentence (content : Inline.t list) : Inline.t list =
  terminate_sentence (capitalize_opening_word content)

let rec realize_block ~term_entry ~term_words (fragment : _ fragment) :
    Structured_diagnostic.Block.t =
  { kind = fragment.kind;
    content =
      as_sentence (realize_phrase ~term_entry ~term_words fragment.content);
    children =
      List.concat_map (realize_children ~term_entry ~term_words)
        fragment.children
  }

and realize_children ~term_entry ~term_words fragment =
  match fragment.role with
  | Explanation _ ->
    List.concat_map (realize_children ~term_entry ~term_words) fragment.children
  | Statement | Dependent | Group | Block _ ->
    [fragment.relation, realize_block ~term_entry ~term_words fragment]

let realize ~term_entry ~term_words (fragments : _ fragment list) =
  List.concat_map (realize_children ~term_entry ~term_words) fragments
  |> List.map snd

let rendered_children ~term_entry ~term_words fragment :
    Structured_diagnostic.Block.t =
  realize_block ~term_entry ~term_words (group fragment.children)
