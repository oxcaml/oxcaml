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

module Statement = struct
  type 'term clause =
    | Subordinate of 'term Phrase.t
    | Coordinate of 'term Phrase.t

  type 'term body =
    | Sentence of
        { subject : subject option;
          main : 'term Phrase.t;
          clause : 'term clause option
        }
    | Fragment of 'term Phrase.t

  type 'term t =
    { kind : Kind.t;
      body : 'term body
    }

  let clause_phrase (clause : _ clause) =
    match clause with Subordinate phrase | Coordinate phrase -> phrase

  let with_clause_phrase (clause : _ clause) phrase : _ clause =
    match clause with
    | Subordinate _ -> Subordinate phrase
    | Coordinate _ -> Coordinate phrase
end

type 'term plan =
  { statement : 'term Statement.t option;
    children : (Relation.t * 'term plan) list
  }

module Plan = struct
  let statement ?(children = []) statement =
    { statement = Some statement; children }

  let group children = { statement = None; children }

  let with_children t children = { t with children }

  let without_statement t = { t with statement = None }
end

let claims (plans : 'term plan list) : (Relation.t * 'term plan) list =
  List.map (fun plan -> Relation.Claim, plan) plans

let elaboration (statement : 'term Statement.t) : Relation.t * 'term plan =
  Relation.Elaboration, Plan.statement statement

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

let sentence ?(kind = Kind.Explanation) ?subject ?clause main : _ Statement.t =
  (match main with
  | [] -> invalid_arg "Diagnostic_nlg.sentence: empty main phrase"
  | _ :: _ -> ());
  Option.iter
    (fun clause ->
      match Statement.clause_phrase clause with
      | [] -> invalid_arg "Diagnostic_nlg.sentence: empty clause phrase"
      | _ :: _ -> ())
    clause;
  Option.iter
    (fun (subject : subject) ->
      let mentioned =
        match subject_entity subject with
        | None -> false
        | Some entity ->
          List.exists
            (fun (e, _) -> Location_key.equal e entity)
            (nominals main)
      in
      if not mentioned then
        invalid_arg
          "Diagnostic_nlg.sentence: subject not mentioned in the main phrase")
    subject;
  { kind; body = Statement.Sentence { subject; main; clause } }

type 'term aside = Relation.t * 'term plan

type 'term beat = 'term plan

type 'term story = 'term beat

let said ?kind ?subject segments =
  sentence ?kind ?subject:(Option.bind subject sentence_subject) segments

let note ?subject ?(asides = []) segments : _ aside =
  Relation.Elaboration,
  Plan.statement ~children:asides (said ?subject segments)

let background segments : _ aside =
  elaboration (said ~kind:Kind.Background segments)

let suggest segments : _ aside =
  elaboration (said ~kind:Kind.Suggestion segments)

let claim ?subject ?(asides = []) segments : _ beat =
  Plan.statement ~children:asides (said ?subject segments)

let but ?subject ?(asides = []) segments : _ beat =
  claim ?subject ~asides (txt "but " :: segments)

let sub_claim ?subject ?asides segments : _ aside =
  Relation.Claim, claim ?subject ?asides segments

let child (beat : _ beat) : _ aside = Relation.Claim, beat

let story (beats : _ beat list) : _ story = Plan.group (claims beats)

let plain ~claim:claim_phrase ?contrast ?(background = []) ?(suggestions = [])
    () : _ story =
  let extra kind phrase = elaboration (sentence ~kind phrase) in
  let extras =
    List.map (extra Kind.Background) background
    @ List.map (extra Kind.Suggestion) suggestions
  in
  let claim_plans =
    match contrast with
    | None -> [Plan.statement ~children:extras (sentence claim_phrase)]
    | Some contrast ->
      [ Plan.statement (sentence claim_phrase);
        Plan.statement ~children:extras (sentence contrast) ]
  in
  Plan.group (claims claim_plans)

let beheaded (beat : _ beat) : _ beat = Plan.without_statement beat

let reframe (beat : _ beat) (stories : _ story list) : _ beat =
  Plan.with_children beat (claims stories)

let pronominalize (plans : 'term plan list) : 'term plan list =
  let statement_nominals (s : _ Statement.t) =
    match s.body with
    | Fragment phrase -> nominals phrase
    | Sentence { subject = _; main; clause } -> (
      nominals main
      @
      match clause with
      | None -> []
      | Some clause -> nominals (Statement.clause_phrase clause))
  in
  let last_mention s =
    match List.rev (statement_nominals s) with
    | [] -> None
    | (e, _) :: _ -> Some e
  in
  let pronouns_of (s : _ Statement.t) =
    List.filter_map
      (fun (entity, form) ->
        match (form : Form.t) with
        | Pronoun -> Some entity
        | Name -> None)
      (statement_nominals s)
  in
  let distinct_entities entities =
    List.rev
      (List.fold_left
         (fun distinct entity ->
           if List.exists (Location_key.equal entity) distinct then distinct
           else entity :: distinct)
         [] entities)
  in
  let rewrite ~prev_last ~prev_pronouns (s : _ Statement.t) : _ Statement.t =
    let sentence_entity =
      match s.body with
      | Fragment _ -> None
      | Sentence { subject; main = _; clause = _ } ->
        Option.bind subject subject_entity
    in
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
        let form : Form.t =
          match form with
          | Pronoun -> Pronoun
          | Name ->
            let intra =
              entity_is_sentence_subject
              && (match prev_mention with
                 | Some e -> Location_key.equal e entity
                 | None -> false)
            in
            let inter =
              is_subject
              && (match prev_last with
                 | Some e -> Location_key.equal e entity
                 | None -> false)
              && List.for_all
                   (fun e -> Location_key.equal e entity)
                   prev_pronouns
            in
            if intra || inter then Pronoun else Name
        in
        ( (Some entity, subject_pending && not is_subject),
          Phrase.Mention { span = Some span; name; case; form } )
    and rewrite_segments state segments =
      List.fold_left_map segment state segments
    in
    let body : _ Statement.body =
      match s.body with
      | Fragment phrase ->
        let _state, phrase = rewrite_segments (None, true) phrase in
        Fragment phrase
      | Sentence { subject; main; clause } ->
        let state, main = rewrite_segments (None, true) main in
        let _state, clause =
          match clause with
          | None -> state, None
          | Some clause ->
            let state, phrase =
              rewrite_segments state (Statement.clause_phrase clause)
            in
            state, Some (Statement.with_clause_phrase clause phrase)
        in
        Sentence { subject; main; clause }
    in
    let rewritten = { s with body } in
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
      let disambiguate_phrase (p : _ Phrase.t) = List.map disambiguate p in
      let body : _ Statement.body =
        match rewritten.body with
        | Fragment phrase -> Fragment (disambiguate_phrase phrase)
        | Sentence { subject; main; clause } ->
          Sentence
            { subject;
              main = disambiguate_phrase main;
              clause =
                Option.map
                  (fun clause ->
                    Statement.with_clause_phrase clause
                      (disambiguate_phrase (Statement.clause_phrase clause)))
                  clause
            }
      in
      { rewritten with body }
  in
  let rec go_plan state plan =
    let state, statement =
      match plan.statement with
      | None -> state, None
      | Some s ->
        let prev_last, prev_pronouns = state in
        let s' = rewrite ~prev_last ~prev_pronouns s in
        (last_mention s, pronouns_of s'), Some s'
    in
    let state, children =
      List.fold_left_map
        (fun state (relation, child) ->
          let state, child = go_plan state child in
          state, (relation, child))
        state plan.children
    in
    state, { statement; children }
  in
  snd (List.fold_left_map go_plan (None, []) plans)

let pronominalize_one (plan : 'term plan) : 'term plan =
  match pronominalize [plan] with
  | [plan] -> plan
  | [] | _ :: _ :: _ ->
    invalid_arg "Diagnostic_nlg.pronominalize_one: plan count changed"

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

let realize_blocks ~term_entry ~term_words (plans : _ plan list) :
    Structured_diagnostic.Block.t list =
  let realize_phrase = realize_phrase ~term_entry ~term_words in
  let realize_statement (s : _ Statement.t) =
    match s.body with
    | Fragment phrase -> realize_phrase phrase
    | Sentence { subject = _; main; clause } ->
      let main = realize_phrase main in
      let content =
        match clause with
        | None -> main
        | Some clause ->
          let separator =
            match clause with
            | Statement.Subordinate _ -> " "
            | Statement.Coordinate _ -> ", "
          in
          let realized = realize_phrase (Statement.clause_phrase clause) in
          main @ (Inline.Text separator :: realized)
      in
      as_sentence content
  in
  let rec block_of_plan (p : _ plan) =
    let kind, content =
      match p.statement with
      | None -> Kind.Explanation, []
      | Some s -> s.kind, realize_statement s
    in
    let children =
      List.map
        (fun (relation, child) -> relation, block_of_plan child)
        p.children
    in
    { Structured_diagnostic.Block.kind; content; children }
  in
  List.map block_of_plan plans

let realize ~term_entry ~term_words ~loc (stories : _ story list) :
    Structured_diagnostic.t =
  { Structured_diagnostic.loc;
    body = realize_blocks ~term_entry ~term_words stories
  }

let rendered_children ~term_entry ~term_words (beat : _ beat) :
    Structured_diagnostic.Block.t =
  match realize_blocks ~term_entry ~term_words [Plan.group beat.children] with
  | [block] -> block
  | [] | _ :: _ :: _ ->
    invalid_arg "Diagnostic_nlg.rendered_children: block count changed"
