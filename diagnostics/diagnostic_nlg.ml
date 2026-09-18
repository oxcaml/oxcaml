module Annotation = Structured_diagnostic.Annotation
module Form = Structured_diagnostic.Form
module Inline = Structured_diagnostic.Inline
module Kind = Structured_diagnostic.Kind
module Location_key = Structured_diagnostic.Location_key
module Relation = Structured_diagnostic.Relation

module Nonempty = struct
  type (+'head, +'tail) t = ( :: ) of 'head * 'tail list

  let to_list (head :: tail : ('a, 'a) t) : 'a list = head :: tail

  let append (head :: tail : (_, _) t) rest = head :: (tail @ rest)
end

module Entity_key = struct
  type t =
    | Located of Location_key.t
    | Unlocated of unit ref

  let equal left right =
    match left, right with
    | Located left, Located right -> Location_key.equal left right
    | Unlocated left, Unlocated right -> left == right
    | Located _, Unlocated _ | Unlocated _, Located _ -> false
end

module Phrase = struct
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

  type 'term segment =
    | Word of word
    | Copula of number option
    | Term of 'term
    | Sequence of 'term segment list
    | Source of
        { loc : Location.t;
          content : 'term segment list
        }
    | Mention of
        { entity : Entity_key.t;
          span : Location.t option;
          name : (word, word) Nonempty.t;
          number : number;
          case : case;
          allow_pronoun : bool;
          form : Form.t
        }

  type 'term t = ('term segment, 'term segment) Nonempty.t
end

let word word : _ Phrase.segment = Phrase.Word word

let txt s : _ Phrase.segment = Phrase.Word (Phrase.Text s)

let code s : _ Phrase.segment = Phrase.Word (Phrase.Code s)

let copula : _ Phrase.segment = Phrase.Copula None

let copula_agreeing (number : Phrase.number) : _ Phrase.segment =
  Phrase.Copula (Some number)

let term t : _ Phrase.segment = Phrase.Term t

let ref_source loc content : _ Phrase.segment =
  Phrase.Source { loc; content = Nonempty.to_list content }

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
  | Longident.Lident name -> Some name
  | Longident.Ldot (prefix, name) ->
    Option.map
      (fun prefix -> prefix ^ "." ^ name.txt)
      (longident_name prefix.txt)
  | Longident.Lapply _ -> None

type subject =
  { entity : Entity_key.t option;
    description : noun_description;
    span : Location.t option;
    number : Phrase.number
  }

and noun_description =
  | Words of (Phrase.word, Phrase.word) Nonempty.t
  | Named of
      { identifier : string;
        kind : string option
      }
  | Possessive of subject * subject
  | At_source of Location.t * subject

let subject ?span ?(number = Phrase.Singular) name : subject =
  let span =
    match span with
    | Some loc when not (Location.is_none loc) -> Some loc
    | Some _ | None -> None
  in
  let entity =
    match span with
    | Some span -> Entity_key.Located (Location_key.of_location span)
    | None -> Entity_key.Unlocated (ref ())
  in
  { entity = Some entity; description = Words name; span; number }

let possessive_suffix (number : Phrase.number)
    (name : (Phrase.word, Phrase.word) Nonempty.t) =
  match number, List.rev (Nonempty.to_list name) with
  | Phrase.Plural, (Phrase.Text text | Phrase.Code text) :: _
    when String.ends_with ~suffix:"s" text ->
    "'"
  | Phrase.Singular, _ | Phrase.Plural, _ -> "'s"

let rec noun_ending noun : (Phrase.word, Phrase.word) Nonempty.t =
  match noun.description with
  | Words words -> words
  | Named { identifier; _ } -> Nonempty.[Phrase.Code identifier]
  | Possessive (_, noun) | At_source (_, noun) -> noun_ending noun

let rec noun_segment ~(case : Phrase.case) ~possessed (noun : subject) :
    _ Phrase.segment =
  let atomic name =
    match noun.entity with
    | Some entity ->
      Phrase.Mention
        { entity;
          span = noun.span;
          name;
          number = noun.number;
          case;
          allow_pronoun = not possessed;
          form = Form.Name
        }
    | None ->
      let content = List.map word (Nonempty.to_list name) in
      let content =
        match (case : Phrase.case) with
        | Phrase.Subject | Phrase.Object -> content
        | Phrase.Possessive ->
          content @ [txt (possessive_suffix noun.number name)]
      in
      Phrase.Sequence content
  in
  match noun.description with
  | Words words -> atomic words
  | Named { identifier; kind } ->
    atomic
      (match kind with
      | None -> Nonempty.[Phrase.Code identifier]
      | Some kind ->
        Nonempty.
          [ Phrase.Text ((if possessed then "" else "the ") ^ kind ^ " ");
            Phrase.Code identifier ])
  | Possessive (owner, part) ->
    let part_case =
      match case with
      | Phrase.Subject | Phrase.Object -> case
      | Phrase.Possessive -> Phrase.Subject
    in
    let content =
      [ noun_segment ~case:Phrase.Possessive ~possessed:false owner;
        txt " ";
        noun_segment ~case:part_case ~possessed:true part ]
    in
    let content =
      match (case : Phrase.case) with
      | Phrase.Subject | Phrase.Object -> content
      | Phrase.Possessive ->
        content @ [txt (possessive_suffix noun.number (noun_ending part))]
    in
    Phrase.Sequence content
  | At_source (loc, noun) ->
    ref_source loc Nonempty.[noun_segment ~case ~possessed noun]

let mention ?(case = Phrase.Subject) noun =
  noun_segment ~case ~possessed:false noun

let located_mention ?(case = Phrase.Subject) subject =
  let words = mention ~case subject in
  match subject.span with
  | None -> words
  | Some loc -> ref_source loc Nonempty.[words]

module Noun = struct
  type t = subject

  let described ?loc ?number description =
    subject ?span:loc ?number Nonempty.[Phrase.Text description]

  let named ?loc ?number ?kind name =
    { (subject ?span:loc ?number Nonempty.[Phrase.Code name]) with
      description = Named { identifier = name; kind }
    }

  let common ?(number = Phrase.Singular) description =
    { description = Words Nonempty.[Phrase.Text description];
      number;
      entity = None;
      span = None
    }

  let rec identifier noun =
    match noun.description with
    | Named { identifier; _ } -> Some identifier
    | At_source (_, noun) -> identifier noun
    | Words _ | Possessive _ -> None

  let rec short noun =
    let description =
      match noun.description with
      | Named name -> Named { name with kind = None }
      | At_source (loc, noun) -> At_source (loc, short noun)
      | Words _ | Possessive _ -> noun.description
    in
    { noun with description }

  let rec with_kind ~kind noun =
    let description =
      match noun.description with
      | Named name -> Named { name with kind = Some kind }
      | At_source (loc, noun) -> At_source (loc, with_kind ~kind noun)
      | Words _ | Possessive _ -> Words Nonempty.[Phrase.Text ("the " ^ kind)]
    in
    { noun with description }

  let possessive owner noun =
    { noun with description = Possessive (owner, noun) }

  let at_source loc noun =
    if Location.is_none loc
    then noun
    else { noun with description = At_source (loc, noun) }

  let source noun =
    match noun.span with None -> noun | Some loc -> at_source loc noun
end

module Property = struct
  type 'term t = Property of 'term Phrase.t

  let text text = Property Nonempty.[txt text]

  let code name = Property Nonempty.[code name]

  let term value = Property Nonempty.[term value]

  let segment (Property content) : _ Phrase.segment =
    Phrase.Sequence (Nonempty.to_list content)

  let prefix prefix property =
    Property Nonempty.[txt (prefix ^ " "); segment property]

  let expected property = prefix "expected to be" property

  let always property = prefix "always" property

  let stronger_than property = prefix "stronger than" property

  let weaker_than property = prefix "weaker than" property

  let alternatives (Nonempty.(first :: rest) : (_, _) Nonempty.t) =
    Property
      Nonempty.(
        segment first
        :: List.concat_map (fun item -> [txt " or "; segment item]) rest)

  let at_source loc (Property content as property) =
    if Location.is_none loc
    then property
    else Property Nonempty.[ref_source loc content]
end

module Predicate = struct
  type 'term t =
    | Is of 'term Property.t
    | Transitive of
        { singular : string;
          plural : string;
          object_ : Noun.t
        }
    | At_source of Location.t * 'term t

  let is complement = Is complement

  let transitive ~singular ~plural object_ =
    Transitive { singular; plural; object_ }

  let at_source loc predicate = At_source (loc, predicate)

  let used_inside object_ =
    Is
      (Property.Property
         Nonempty.[txt "used inside "; mention ~case:Phrase.Object object_])

  let used_as property = Is (Property.prefix "used as" property)

  let annotated_as ~at property =
    at_source at (Is (Property.prefix "annotated as" property))

  let annotated ~at property =
    Is (Property.prefix "annotated" (Property.at_source at property))

  let declared ~at property =
    Is (Property.prefix "declared" (Property.at_source at property))

  let rec phrase ~number : _ t -> _ Phrase.t = function
    | Is complement ->
      Nonempty.[copula_agreeing number; txt " "; Property.segment complement]
    | Transitive { singular; plural; object_ } ->
      let verb =
        match (number : Phrase.number) with
        | Phrase.Singular -> singular
        | Phrase.Plural -> plural
      in
      Nonempty.[txt (" " ^ verb ^ " "); mention ~case:Phrase.Object object_]
    | At_source (loc, predicate) ->
      let content = phrase ~number predicate in
      if Location.is_none loc then content else Nonempty.[ref_source loc content]
end

module Clause = struct
  type 'term t =
    { subject : Entity_key.t option;
      content : 'term Phrase.t
    }

  let make (noun : Noun.t) predicate =
    { subject = noun.entity;
      content =
        Nonempty.(
          mention noun
          :: to_list (Predicate.phrase ~number:noun.number predicate))
    }
end

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

type category =
  [ `Statement
  | `Dependent
  | `Rule
  | `Suggestion
  | `Group
  | `Block
  | `Explanation ]

type ('term, +'category) node =
  { role : role;
    relation : Relation.t;
    kind : Kind.t;
    subject : [`Infer | `Explicit of Entity_key.t option];
    content : 'term Phrase.segment list;
    children : 'term fragment list
  }

and 'term fragment = ('term, category) node

let rec nominals (segments : _ Phrase.segment list) :
    (Entity_key.t * Form.t) list =
  List.concat_map
    (fun (seg : _ Phrase.segment) ->
      match seg with
      | Phrase.Word _ | Phrase.Copula _ | Phrase.Term _ -> []
      | Phrase.Source { loc = _; content } | Phrase.Sequence content ->
        nominals content
      | Phrase.Mention { entity; form; _ } -> [entity, form])
    segments

let fragment ~role ~relation ~kind phrase =
  let content = Nonempty.to_list phrase in
  { role; relation; kind; subject = `Infer; content; children = [] }

let state content =
  fragment ~role:Statement ~relation:Relation.Claim ~kind:Kind.Explanation
    content

let dependent ?connective ~relation content =
  let content =
    match connective with
    | None -> content
    | Some connective -> Nonempty.(txt connective :: to_list content)
  in
  fragment ~role:Dependent ~relation ~kind:Kind.Explanation content

let but content = dependent ~connective:"but " ~relation:Relation.Claim content

let reason content =
  dependent ~connective:"because " ~relation:Relation.Elaboration content

let and_ content =
  dependent ~connective:"and " ~relation:Relation.Elaboration content

let elaborate content = dependent ~relation:Relation.Elaboration content

let rule content =
  fragment ~role:Dependent ~relation:Relation.Elaboration ~kind:Kind.Rule
    content

module Dsl = struct
  let named = Noun.named

  let described = Noun.described

  let noun = Noun.common

  let possessive = Noun.possessive

  let source = Noun.source

  let property = Property.text

  let term = Property.term

  let expected = Property.expected

  let always = Property.always

  let stronger_than = Property.stronger_than

  let weaker_than = Property.weaker_than

  let alternatives = Property.alternatives

  let clause = Clause.make

  let is = Predicate.is

  let used_inside = Predicate.used_inside

  let used_as = Predicate.used_as

  let annotated_as = Predicate.annotated_as

  let annotated = Predicate.annotated

  let declared = Predicate.declared

  let sentence make (clause : _ Clause.t) =
    { (make clause.content) with subject = `Explicit clause.subject }

  let state clause = sentence state clause

  let but clause = sentence but clause

  let reason clause = sentence reason clause

  let and_ clause = sentence and_ clause

  let rule clause = sentence rule clause
end

module Verbatim = struct
  let property content = Property.Property content

  let phrase = Property.segment
end

let is_rule fragment =
  match fragment.kind with
  | Kind.Rule -> true
  | Kind.Explanation | Kind.Suggestion -> false

let suggestion content =
  fragment ~role:Dependent ~relation:Relation.Elaboration ~kind:Kind.Suggestion
    content

let with_children children fragment =
  { fragment with children = fragment.children @ children }

let group children : _ fragment =
  { role = Group;
    relation = Relation.Claim;
    kind = Kind.Explanation;
    subject = `Explicit None;
    content = [];
    children
  }

let block ?(necessity = Inherit)
    (Nonempty.(first :: rest) : ((_, _) node, _ fragment) Nonempty.t) =
  { (group ((first :> _ fragment) :: rest)) with role = Block necessity }

let explanation ?(necessity = Inherit)
    (Nonempty.(first :: rest) : ((_, _) node, _ fragment) Nonempty.t) =
  { (group ((first :> _ fragment) :: rest)) with role = Explanation necessity }

let focus ~(on : (_, _) node) fragments =
  let on = (on :> _ fragment) in
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

let children fragment = fragment.children

let without_text fragment =
  let role =
    match fragment.role with
    | (Block _ | Explanation _) as role -> role
    | Statement | Dependent | Group -> Group
  in
  { fragment with
    role;
    kind = Kind.Explanation;
    subject = `Explicit None;
    content = []
  }

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
        | Form.Pronoun -> Some entity
        | Form.Name -> None)
      (nominals s.content)
  in
  let distinct_entities entities =
    List.rev
      (List.fold_left
         (fun distinct entity ->
           if List.exists (Entity_key.equal entity) distinct
           then distinct
           else entity :: distinct)
         [] entities)
  in
  let rewrite ~prev_last ~prev_pronouns (s : _ fragment) : _ fragment =
    let sentence_entity =
      match s.subject with
      | `Explicit subject -> subject
      | `Infer -> (
        match nominals s.content with
        | [] -> None
        | (entity, _) :: _ -> Some entity)
    in
    let rec segment (prev_mention, subject_pending) (seg : _ Phrase.segment) =
      match seg with
      | Phrase.Word _ | Phrase.Copula _ | Phrase.Term _ ->
        (prev_mention, subject_pending), seg
      | Phrase.Sequence content ->
        let state, content =
          rewrite_segments (prev_mention, subject_pending) content
        in
        state, Phrase.Sequence content
      | Phrase.Source { loc; content } ->
        let state, content =
          rewrite_segments (prev_mention, subject_pending) content
        in
        state, Phrase.Source { loc; content }
      | Phrase.Mention { entity; span; name; number; case; allow_pronoun; form }
        ->
        let entity_is_sentence_subject =
          match sentence_entity with
          | Some e -> Entity_key.equal e entity
          | None -> false
        in
        let is_subject = subject_pending && entity_is_sentence_subject in
        let intra =
          match prev_mention with
          | Some e -> Entity_key.equal e entity
          | None -> false
        in
        let inter =
          (match prev_last with
            | Some e -> Entity_key.equal e entity
            | None -> false)
          && List.for_all (fun e -> Entity_key.equal e entity) prev_pronouns
        in
        let form : Form.t =
          if not allow_pronoun
          then Form.Name
          else
            match form with
            | Form.Pronoun -> if intra || inter then Form.Pronoun else Form.Name
            | Form.Name ->
              if
                intra || (is_subject && inter)
                || (case = Phrase.Possessive && prev_mention = None && inter)
              then Form.Pronoun
              else Form.Name
        in
        ( (Some entity, subject_pending && not is_subject),
          Phrase.Mention
            { entity; span; name; number; case; allow_pronoun; form } )
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
        | Phrase.Word _ | Phrase.Copula _ | Phrase.Term _ -> seg
        | Phrase.Sequence content ->
          Phrase.Sequence (List.map disambiguate content)
        | Phrase.Source { loc; content } ->
          Phrase.Source { loc; content = List.map disambiguate content }
        | Phrase.Mention
            { entity; span; name; number; case; allow_pronoun; form } ->
          let switches_referent =
            not (Entity_key.equal entity retained_entity)
          in
          let form : Form.t =
            match form with
            | Form.Pronoun when switches_referent -> Form.Name
            | Form.Pronoun | Form.Name -> form
          in
          Phrase.Mention
            { entity; span; name; number; case; allow_pronoun; form }
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
        ( (depth, (last_mention fragment, pronouns_of rewritten)) :: frames,
          rewritten )
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

let annotated annotation content : Inline.t =
  Inline.Annotated { annotation; content }

let inline_of_word (word : Phrase.word) : Inline.t =
  match word with
  | Phrase.Text text -> Inline.Text text
  | Phrase.Code text -> annotated Annotation.Code [Inline.Text text]

let realize_phrase ~term_entry ~term_words (phrase : _ Phrase.segment list) :
    Inline.t list =
  let rec segment ~followed_by_more ~agreement (seg : _ Phrase.segment) =
    match seg with
    | Phrase.Word word -> None, [inline_of_word word]
    | Phrase.Copula number ->
      let inferred, pronoun =
        Option.value agreement ~default:(Phrase.Singular, false)
      in
      let word =
        match Option.value number ~default:inferred with
        | Phrase.Singular -> if pronoun && followed_by_more then "'s" else " is"
        | Phrase.Plural -> if pronoun && followed_by_more then "'re" else " are"
      in
      None, [Inline.Text word]
    | Phrase.Term t ->
      let entry = term_entry t in
      let agreement, content =
        segments ~followed_by_more ~agreement (Nonempty.to_list (term_words t))
      in
      agreement, [annotated (Annotation.Term entry) content]
    | Phrase.Sequence content -> segments ~followed_by_more ~agreement content
    | Phrase.Source { loc; content } ->
      let agreement, content = segments ~followed_by_more ~agreement content in
      agreement, [annotated (Annotation.Source loc) content]
    | Phrase.Mention { span; name; number; case; form; _ } ->
      let as_mention content =
        match span with
        | None -> content
        | Some entity ->
          [annotated (Annotation.Mention { entity; form }) content]
      in
      let agreement =
        match case with
        | Phrase.Subject -> Some (number, form = Form.Pronoun)
        | Phrase.Object | Phrase.Possessive -> None
      in
      let content =
        match form with
        | Form.Pronoun ->
          let word =
            match case, number with
            | Phrase.Subject, Phrase.Singular -> "it"
            | Phrase.Subject, Phrase.Plural -> "they"
            | Phrase.Object, Phrase.Singular -> "it"
            | Phrase.Object, Phrase.Plural -> "them"
            | Phrase.Possessive, Phrase.Singular -> "its"
            | Phrase.Possessive, Phrase.Plural -> "their"
          in
          as_mention [Inline.Text word]
        | Form.Name -> (
          let words =
            as_mention (List.map inline_of_word (Nonempty.to_list name))
          in
          match case with
          | Phrase.Subject | Phrase.Object -> words
          | Phrase.Possessive ->
            words @ [Inline.Text (possessive_suffix number name)])
      in
      agreement, content
  and segments ~followed_by_more ~agreement segs =
    match segs with
    | [] -> agreement, []
    | seg :: rest ->
      let followed_by_more_here =
        match rest with [] -> followed_by_more | _ :: _ -> true
      in
      let agreement, realized =
        segment ~followed_by_more:followed_by_more_here ~agreement seg
      in
      let agreement, realized_rest =
        segments ~followed_by_more ~agreement rest
      in
      agreement, realized @ realized_rest
  in
  snd (segments ~followed_by_more:false ~agreement:None phrase)

let capitalize_opening_word (content : Inline.t list) : Inline.t list =
  let rec capitalize ~in_code (inline : Inline.t) : Inline.t option =
    match inline with
    | Inline.Text "" -> None
    | Inline.Text text ->
      Some
        (if in_code then inline else Inline.Text (String.capitalize_ascii text))
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
      List.concat_map
        (realize_children ~term_entry ~term_words)
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
