(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 expect;
*)

module Protocol = Structured_diagnostic_protocol
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

let contains json key =
  let width = String.length key in
  let rec from index =
    index + width <= String.length json
    && (String.equal (String.sub json index width) key || from (index + 1))
  in
  from 0

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
module Protocol = Structured_diagnostic_protocol
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
val contains : String.t -> String.t -> bool = <fun>
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
  let projection =
    Structured_diagnostic.to_raw_diagnostic
      (diagnostic ~entities ~glossary ~body ())
  in
  Format.printf "title: %s@." projection.title;
  List.iter
    (fun (entity : Protocol.Raw.Location.t Protocol.Generic.entity) ->
      Format.printf "entity %d: %s %d-%d@." entity.id entity.loc.file
        entity.loc.start.col entity.loc.end_.col)
    projection.entities;
  List.iter
    (fun (glossary : Protocol.Generic.glossary_entry) ->
      Format.printf "glossary %d: %s %s (%s)@." glossary.id glossary.category
        glossary.term
        (match glossary.url with None -> "no url" | Some url -> url))
    projection.glossary;
  List.iter
    (fun (block : Protocol.Raw.Location.t Protocol.Generic.block) ->
      Format.printf "block %s with %d child(ren)@."
        (Protocol.Kind.to_string block.kind)
        (List.length block.children);
      List.iter
        (fun (child : Protocol.Raw.Location.t Protocol.Generic.child) ->
          Format.printf "  %s of a %s block@."
            (Protocol.Relation.to_string child.relation)
            (Protocol.Kind.to_string child.block.kind))
        block.children)
    projection.body

[%%expect {|
title: the argument escapes its region
entity 0: a.ml 4-12
entity 1: b.ml 20-26
glossary 0: Mode local (https://oxcaml/local)
glossary 1: Modality portable (no url)
block explanation with 1 child(ren)
  claim of a suggestion block
|}]

let () =
  let position pos_lnum pos_bol pos_cnum : Lexing.position =
    { pos_fname = "a.ml"; pos_lnum; pos_bol; pos_cnum }
  in
  let raw =
    Structured_diagnostic.raw_location
      { loc_start = position 3 20 25;
        loc_end = position 3 20 31;
        loc_ghost = false
      }
  in
  Format.printf "%s line %d col %d to line %d col %d@." raw.file raw.start.line
    raw.start.col raw.end_.line raw.end_.col

[%%expect {|
a.ml line 3 col 5 to line 3 col 11
|}]

let () =
  List.iter
    (fun spelling -> Format.printf "%s@." spelling)
    [ Protocol.Form.to_string Protocol.Form.Name;
      Protocol.Form.to_string Protocol.Form.Pronoun;
      Protocol.Kind.to_string Protocol.Kind.Explanation;
      Protocol.Kind.to_string Protocol.Kind.Background;
      Protocol.Kind.to_string Protocol.Kind.Suggestion;
      Protocol.Relation.to_string Protocol.Relation.Claim;
      Protocol.Relation.to_string Protocol.Relation.Elaboration
    ]

[%%expect {|
name
pronoun
explanation
background
suggestion
claim
elaboration
|}]

let glossary_json url =
  let glossary, _ =
    Glossary.intern Glossary.empty
      (entry ?url "local" "Mode" "how a value may be used")
  in
  Protocol.Raw.diagnostic_to_json
    (Structured_diagnostic.to_raw_diagnostic (diagnostic ~glossary ()))

let () =
  Format.printf "url emitted when absent: %b@."
    (contains (glossary_json None) "\"url\"");
  Format.printf "url emitted when present: %b@."
    (contains (glossary_json (Some "https://oxcaml/local")) "\"url\"")

[%%expect {|
val glossary_json :
  string option -> Structured_diagnostic_protocol.json_string = <fun>
url emitted when absent: false
url emitted when present: true
|}]

let () =
  let response = Structured_diagnostic.raw_response [ diagnostic () ] in
  let json = Protocol.Raw.response_to_json response in
  Format.printf "version %d of %d, diagnostics field: %b@." response.version
    Protocol.protocol_version (contains json "\"diagnostics\"")

[%%expect {|
version 1 of 1, diagnostics field: true
|}]

let print_bytes label bytes =
  Format.printf "%s:" label;
  String.iter (fun byte -> Format.printf " %02x" (Char.code byte)) bytes;
  Format.printf "@."

let decode_json_string json =
  let decoded = Buffer.create (String.length json) in
  let closing = String.length json - 1 in
  let rec scan index =
    if index < closing then
      if Char.equal (String.get json index) '\\' then
        match String.get json (index + 1) with
        | 'b' -> Buffer.add_char decoded '\b'; scan (index + 2)
        | 'f' -> Buffer.add_char decoded '\012'; scan (index + 2)
        | 'n' -> Buffer.add_char decoded '\n'; scan (index + 2)
        | 'r' -> Buffer.add_char decoded '\r'; scan (index + 2)
        | 't' -> Buffer.add_char decoded '\t'; scan (index + 2)
        | 'u' ->
          let code = "0x" ^ String.sub json (index + 2) 4 in
          Buffer.add_utf_8_uchar decoded (Uchar.of_int (int_of_string code));
          scan (index + 6)
        | literal -> Buffer.add_char decoded literal; scan (index + 2)
      else begin
        Buffer.add_char decoded (String.get json index);
        scan (index + 1)
      end
  in
  scan 1;
  Buffer.contents decoded

let () =
  let lambda = "\xce\xbb" in
  let sample = "\" \\ \n\t\001 " ^ lambda in
  let json = Protocol.string_to_json sample in
  print_bytes "sample " sample;
  print_bytes "encoded" json;
  Format.printf "round trips: %b@."
    (String.equal (decode_json_string json) sample)

[%%expect {|
val print_bytes : string -> string -> unit = <fun>
val decode_json_string : String.t -> string = <fun>
sample : 22 20 5c 20 0a 09 01 20 ce bb
encoded: 22 5c 22 20 5c 5c 20 5c 6e 5c 74 5c 75 30 30 30 31 20 ce bb 22
round trips: true
|}]

let () =
  let title = "a \xce\xbb escapes\n" in
  let diagnostic : Structured_diagnostic.t =
    { loc = span "a.ml" 0 3;
      title;
      entities = Entities.empty;
      glossary = Glossary.empty;
      body = [ { Block.kind = Explanation;
                 content = [ Inline.Text "a \xce\xbb value" ];
                 children = []
               }
             ]
    }
  in
  let json =
    Protocol.Raw.response_to_json
      (Structured_diagnostic.raw_response [ diagnostic ])
  in
  Format.printf "title bytes survive: %b@."
    (contains json "a \xce\xbb escapes");
  Format.printf "inline bytes survive: %b@." (contains json "a \xce\xbb value");
  Format.printf "no bytewise escape: %b@." (not (contains json "\\u00"))

[%%expect {|
title bytes survive: true
inline bytes survive: true
no bytewise escape: true
|}]

let () =
  print_bytes "invalid" (Protocol.string_to_json "\xff")

[%%expect {|
invalid: 22 ef bf bd 22
|}]
