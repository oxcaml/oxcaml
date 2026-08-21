(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 expect;
*)

module Annotation = Structured_diagnostic.Annotation
module Block = Structured_diagnostic.Block
module Entities = Structured_diagnostic.Entities
module Glossary = Structured_diagnostic.Glossary
module Inline = Structured_diagnostic.Inline

let span ?(ghost = false) file start stop : Location.t =
  let position pos_cnum : Lexing.position =
    { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum }
  in
  { loc_start = position start; loc_end = position stop; loc_ghost = ghost }

let mention entity form content : Inline.t =
  Annotated { annotation = Annotation.Mention { entity; form }; content }

let print_location (loc : Location.t) =
  Format.printf "%s %d-%d@." loc.loc_start.pos_fname loc.loc_start.pos_cnum
    loc.loc_end.pos_cnum

let entry ?url term category description : Glossary.Entry.t =
  { term; category; description; url }

let diagnostic ?(entities = Entities.empty) ?(glossary = Glossary.empty)
      ?(body = []) () : Structured_diagnostic.t =
  { loc = span "a.ml" 0 30;
    title = "the argument escapes its region";
    entities;
    glossary;
    body
  }

[%%expect {|
module Annotation = Structured_diagnostic.Annotation
module Block = Structured_diagnostic.Block
module Entities = Structured_diagnostic.Entities
module Glossary = Structured_diagnostic.Glossary
module Inline = Structured_diagnostic.Inline
val span : ?ghost:bool -> string -> int -> int -> Location.t = <fun>
val mention :
  Structured_diagnostic.Entities.Id.t ->
  Structured_diagnostic.Form.t -> Inline.t list -> Inline.t = <fun>
val print_location : Location.t -> unit = <fun>
val entry : ?url:string -> string -> string -> string -> Glossary.Entry.t =
  <fun>
val diagnostic :
  ?entities:Entities.t ->
  ?glossary:Glossary.t ->
  ?body:Structured_diagnostic.Block.t list -> unit -> Structured_diagnostic.t =
  <fun>
|}]

let () =
  let entities, first = Entities.intern Entities.empty (span "a.ml" 0 3) in
  let entities, repeated = Entities.intern entities (span "a.ml" 0 3) in
  let entities, ghost =
    Entities.intern entities (span ~ghost:true "a.ml" 0 3)
  in
  let entities, other_file = Entities.intern entities (span "b.ml" 0 3) in
  Format.printf "%d %d %d %d of %d@." (Entities.Id.to_int first)
    (Entities.Id.to_int repeated) (Entities.Id.to_int ghost)
    (Entities.Id.to_int other_file)
    (List.length (Entities.to_list entities))

[%%expect {|
0 0 0 1 of 2
|}]

let () =
  let mode = entry "portable" "Mode" "can be sent to another thread" in
  let modality = entry "portable" "Modality" "portable in a shared container" in
  let glossary, first = Glossary.intern Glossary.empty mode in
  let glossary, second = Glossary.intern glossary modality in
  let glossary, repeated = Glossary.intern glossary mode in
  Format.printf "%d %d %d@." (Glossary.Id.to_int first)
    (Glossary.Id.to_int second) (Glossary.Id.to_int repeated);
  List.iter
    (fun (id, (entry : Glossary.Entry.t)) ->
      Format.printf "%d %s %s@." (Glossary.Id.to_int id) entry.category
        entry.term)
    (Glossary.to_list glossary)

[%%expect {|
0 1 0
0 Mode portable
1 Modality portable
|}]

let () =
  let entities, argument = Entities.intern Entities.empty (span "a.ml" 4 12) in
  let entities, result = Entities.intern entities (span "a.ml" 20 26) in
  let content =
    [ mention argument Structured_diagnostic.Form.Name
        [ Inline.Text "the argument" ];
      Inline.Text " escapes into ";
      mention result Structured_diagnostic.Form.Name [ Inline.Text "it" ];
      mention argument Structured_diagnostic.Form.Pronoun
        [ Inline.Text "it" ];
      Inline.Annotated
        { annotation = Annotation.Source (span "a.ml" 20 26);
          content = [ Inline.Text "the region" ]
        }
    ]
  in
  List.iter print_location
    (Structured_diagnostic.locations (diagnostic ~entities ()) content)

[%%expect {|
a.ml 4-12
a.ml 20-26
|}]

let () =
  let scratch, _ = Entities.intern Entities.empty (span "a.ml" 4 12) in
  let _, unknown = Entities.intern scratch (span "a.ml" 20 26) in
  let entities, known = Entities.intern Entities.empty (span "a.ml" 4 12) in
  let content =
    [ mention known Structured_diagnostic.Form.Name
        [ Inline.Text "the argument" ];
      mention unknown Structured_diagnostic.Form.Name
        [ Inline.Text "nothing here" ]
    ]
  in
  List.iter print_location
    (Structured_diagnostic.locations (diagnostic ~entities ()) content)

[%%expect {|
a.ml 4-12
|}]

let () =
  let entities, argument = Entities.intern Entities.empty (span "a.ml" 4 12) in
  let entities, result = Entities.intern entities (span "b.ml" 20 26) in
  let glossary, local =
    Glossary.intern Glossary.empty
      (entry ~url:"https://oxcaml/local" "local" "Mode"
         "how a value may be used")
  in
  let glossary, portable =
    Glossary.intern glossary
      (entry "portable" "Modality" "safe to send to another thread")
  in
  let body =
    [ { Block.kind = Explanation;
        content =
          [ mention argument Structured_diagnostic.Form.Name
              [ Inline.Text "the argument" ];
            mention result Structured_diagnostic.Form.Pronoun
              [ Inline.Text "it" ];
            Inline.Annotated
              { annotation = Annotation.Term local;
                content = [ Inline.Text "local" ]
              };
            Inline.Annotated
              { annotation = Annotation.Term portable;
                content = [ Inline.Text "portable" ]
              }
          ];
        children =
          [ ( Structured_diagnostic.Relation.Claim,
              { Block.kind = Suggestion;
                content = [ Inline.Text "annotate it" ];
                children = []
              } )
          ]
      }
    ]
  in
  let diagnostic = diagnostic ~entities ~glossary ~body () in
  let kind = function
    | Structured_diagnostic.Kind.Explanation -> "explanation"
    | Structured_diagnostic.Kind.Background -> "background"
    | Structured_diagnostic.Kind.Suggestion -> "suggestion"
  in
  let relation = function
    | Structured_diagnostic.Relation.Claim -> "claim"
    | Structured_diagnostic.Relation.Elaboration -> "elaboration"
  in
  Format.printf "title: %s@." diagnostic.title;
  List.iter
    (fun (id, (loc : Location.t)) ->
      Format.printf "entity %d: %s %d-%d@." (Entities.Id.to_int id)
        loc.loc_start.pos_fname loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (Entities.to_list diagnostic.entities);
  List.iter
    (fun (id, (entry : Glossary.Entry.t)) ->
      Format.printf "glossary %d: %s %s (%s)@." (Glossary.Id.to_int id)
        entry.category entry.term
        (match entry.url with None -> "no url" | Some url -> url))
    (Glossary.to_list diagnostic.glossary);
  List.iter
    (fun (block : Block.t) ->
      Format.printf "block %s with %d child(ren)@." (kind block.kind)
        (List.length block.children);
      List.iter
        (fun (relation_to_child, child) ->
          Format.printf "  %s of a %s block@." (relation relation_to_child)
            (kind child.Block.kind))
        block.children)
    diagnostic.body

[%%expect {|
title: the argument escapes its region
entity 0: a.ml 4-12
entity 1: b.ml 20-26
glossary 0: Mode local (https://oxcaml/local)
glossary 1: Modality portable (no url)
block explanation with 1 child(ren)
  claim of a suggestion block
|}]
