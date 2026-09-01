module Diagnostic = Structured_diagnostic

let position ~line ~line_start ~column =
  { Lexing.pos_fname = "test.ml";
    pos_lnum = line;
    pos_bol = line_start;
    pos_cnum = line_start + column
  }

let location ~line ~line_start =
  { Location.loc_start = position ~line ~line_start ~column:0;
    loc_end = position ~line ~line_start ~column:1;
    loc_ghost = false
  }

let mention entity text : Diagnostic.Inline.t =
  Diagnostic.Inline.Annotated
    { annotation =
        Diagnostic.Annotation.Mention { entity; form = Diagnostic.Form.Name };
      content = [Diagnostic.Inline.Text text]
    }

let check_round_trip diagnostic =
  let json = Diagnostic.to_json diagnostic in
  match Diagnostic.of_json json with
  | Error error -> failwith error
  | Ok decoded ->
    let encoded_again = Diagnostic.to_json decoded in
    if not (String.equal json encoded_again)
    then failwith "structured diagnostic JSON did not round-trip"

let () =
  let first = location ~line:1 ~line_start:0 in
  let second = location ~line:2 ~line_start:10 in
  let entities, first_id =
    Diagnostic.Entities.intern Diagnostic.Entities.empty first
  in
  let entities, second_id = Diagnostic.Entities.intern entities second in
  let diagnostic : Diagnostic.t =
    { loc = first;
      entities;
      glossary = Diagnostic.Glossary.empty;
      body =
        [ { kind = Diagnostic.Kind.Explanation;
            content = [mention first_id "first"; mention second_id "second"];
            children = []
          } ]
    }
  in
  check_round_trip diagnostic

let () =
  let loc = location ~line:1 ~line_start:0 in
  let glossary_entry : Diagnostic.Glossary.Entry.t =
    { term = "portable";
      category = "mode";
      description = "May be used from another domain";
      url = None
    }
  in
  let mention text : Diagnostic_plan.Inline.t =
    Diagnostic_plan.Inline.Annotated
      { annotation =
          Diagnostic_plan.Annotation.Mention
            { entity = loc; form = Diagnostic.Form.Name };
        content = [Diagnostic_plan.Inline.Text text]
      }
  in
  let term : Diagnostic_plan.Inline.t =
    Diagnostic_plan.Inline.Annotated
      { annotation = Diagnostic_plan.Annotation.Term glossary_entry;
        content = [Diagnostic_plan.Inline.Text "portable"]
      }
  in
  let leaf content =
    Diagnostic_plan.create ~kind:Diagnostic.Kind.Explanation ~content
      ~children:[]
  in
  let plan =
    Diagnostic_plan.create ~kind:Diagnostic.Kind.Explanation ~content:[]
      ~children:
        [ Diagnostic.Relation.Claim, leaf [mention "first"; term];
          Diagnostic.Relation.Claim, leaf [mention "again"; term] ]
  in
  let diagnostic = Diagnostic_plan.to_diagnostic ~loc [plan] in
  if List.length (Diagnostic.Entities.to_list diagnostic.entities) <> 1
  then failwith "diagnostic plan did not deduplicate entities";
  if List.length (Diagnostic.Glossary.to_list diagnostic.glossary) <> 1
  then failwith "diagnostic plan did not deduplicate glossary entries";
  check_round_trip diagnostic
