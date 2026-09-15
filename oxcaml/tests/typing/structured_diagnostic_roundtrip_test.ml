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
  let diagnostic : Diagnostic.t =
    { loc = first;
      body =
        [ { kind = Diagnostic.Kind.Explanation;
            content = [mention first "first"; mention second "second"];
            children = []
          } ]
    }
  in
  check_round_trip diagnostic

let () =
  let loc = location ~line:1 ~line_start:0 in
  let glossary_entry : Diagnostic.Glossary_entry.t =
    { term = "portable";
      category = "mode";
      description = "May be used from another domain";
      url = None
    }
  in
  let term : Diagnostic.Inline.t =
    Diagnostic.Inline.Annotated
      { annotation = Diagnostic.Annotation.Term glossary_entry;
        content = [Diagnostic.Inline.Text "portable"]
      }
  in
  let source : Diagnostic.Inline.t =
    Diagnostic.Inline.Annotated
      { annotation = Diagnostic.Annotation.Source loc;
        content = [Diagnostic.Inline.Text "let x = ()"]
      }
  in
  let code : Diagnostic.Inline.t =
    Diagnostic.Inline.Annotated
      { annotation = Diagnostic.Annotation.Code;
        content = [Diagnostic.Inline.Text "x"]
      }
  in
  let leaf content : Diagnostic.Block.t =
    { kind = Diagnostic.Kind.Explanation; content; children = [] }
  in
  let diagnostic : Diagnostic.t =
    { loc;
      body =
        [ { kind = Diagnostic.Kind.Background;
            content = [mention loc "first"; term];
            children =
              [ Diagnostic.Relation.Claim, leaf [source];
                Diagnostic.Relation.Elaboration, leaf [code] ]
          } ]
    }
  in
  check_round_trip diagnostic
