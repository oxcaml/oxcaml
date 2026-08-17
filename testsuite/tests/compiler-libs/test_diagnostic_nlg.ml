(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase
module Statement = Nlg.Statement
module Diagnostic = Structured_diagnostic
module Annotation = Diagnostic.Annotation
module Block = Diagnostic.Block
module Entities = Diagnostic.Entities
module Glossary = Diagnostic.Glossary
module Inline = Diagnostic.Inline

let span file start stop : Location.t =
  let position pos_cnum : Lexing.position =
    { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum }
  in
  { loc_start = position start; loc_end = position stop; loc_ghost = false }

let argument : Nlg.subject =
  { name = [ Phrase.Text "the argument" ]; span = Some (span "a.ml" 4 12) }

let result : Nlg.subject =
  { name = [ Phrase.Text "the result" ]; span = Some (span "a.ml" 20 26) }

let entry term : Glossary.Entry.t =
  { term;
    category = "Mode";
    description = "how a value may be used";
    url = None
  }

let term_words term : string Phrase.t = [ Nlg.txt term ]

let plan ?(children = []) statement : string Nlg.plan =
  { statement = Some statement; children }

let silent children : string Nlg.plan = { statement = None; children }

let realize plans =
  Nlg.realize ~loc:(span "a.ml" 0 30)
    ~title:"the argument escapes its region" ~term_entry:entry ~term_words
    plans

[%%expect {|
module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase
module Statement = Nlg.Statement
module Diagnostic = Structured_diagnostic
module Annotation = Diagnostic.Annotation
module Block = Diagnostic.Block
module Entities = Diagnostic.Entities
module Glossary = Diagnostic.Glossary
module Inline = Diagnostic.Inline
val span : string -> int -> int -> Location.t = <fun>
val argument : Nlg.subject =
  {Nlg.name = [Nlg.Phrase.Text "the argument"];
   span =
    Some
     {Location.loc_start =
       {Lexing.pos_fname = "a.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4};
      loc_end =
       {Lexing.pos_fname = "a.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 12};
      loc_ghost = false}}
val result : Nlg.subject =
  {Nlg.name = [Nlg.Phrase.Text "the result"];
   span =
    Some
     {Location.loc_start =
       {Lexing.pos_fname = "a.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 20};
      loc_end =
       {Lexing.pos_fname = "a.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 26};
      loc_ghost = false}}
val entry : string -> Glossary.Entry.t = <fun>
val term_words : string -> string Phrase.t = <fun>
val plan :
  ?children:(Structured_diagnostic.Relation.t * string Nlg.plan) list ->
  string Nlg.Statement.t -> string Nlg.plan = <fun>
val silent :
  (Structured_diagnostic.Relation.t * string Nlg.plan) list ->
  string Nlg.plan = <fun>
val realize : string Nlg.plan list -> Structured_diagnostic.t = <fun>
|}]

let rec text (content : Inline.t list) =
  String.concat ""
    (List.map
       (fun (inline : Inline.t) ->
         match inline with
         | Text words -> words
         | Annotated { annotation = _; content } -> text content)
       content)

let described_kind (kind : Diagnostic.Kind.t) =
  match kind with
  | Explanation -> "explanation"
  | Background -> "background"
  | Suggestion -> "suggestion"

let described_relation (relation : Diagnostic.Relation.t) =
  match relation with
  | Claim -> "claim"
  | Elaboration -> "elaboration"

let described_annotation (annotation : Annotation.t) =
  match annotation with
  | Code -> "code"
  | Source loc ->
    Printf.sprintf "source %d-%d" loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  | Mention { entity; form } ->
    Printf.sprintf "mention %d %s"
      (Entities.Id.to_int entity)
      (match form with
       | Name -> "name"
       | Pronoun -> "pronoun")
  | Term id -> Printf.sprintf "term %d" (Glossary.Id.to_int id)

let print_body (t : Diagnostic.t) =
  let rec print indent relation (block : Block.t) =
    Format.printf "%s%s%s: %s@." indent relation (described_kind block.kind)
      (text block.content);
    List.iter
      (fun (relation, child) ->
        print (indent ^ "  ") (described_relation relation ^ " ") child)
      block.children
  in
  List.iter (print "" "") t.body

let print_annotations (t : Diagnostic.t) =
  let rec print (content : Inline.t list) =
    List.iter
      (fun (inline : Inline.t) ->
        match inline with
        | Text _ -> ()
        | Annotated { annotation; content } ->
          Format.printf "%s: %s@." (described_annotation annotation)
            (text content);
          print content)
      content
  in
  let rec block (b : Block.t) =
    print b.content;
    List.iter (fun (_, child) -> block child) b.children
  in
  List.iter block t.body

let print_content (t : Diagnostic.t) =
  let rec print_inline indent (inline : Inline.t) =
    match inline with
    | Text text -> Format.printf "%s%S@." indent text
    | Annotated { annotation; content } ->
      Format.printf "%s%s@." indent (described_annotation annotation);
      List.iter (print_inline (indent ^ "  ")) content
  in
  let rec block indent relation (b : Block.t) =
    Format.printf "%s%s%s@." indent relation (described_kind b.kind);
    List.iter (print_inline (indent ^ "  ")) b.content;
    List.iter
      (fun (relation, child) ->
        block (indent ^ "  ") (described_relation relation ^ " ") child)
      b.children
  in
  List.iter (block "" "") t.body

let print_header (t : Diagnostic.t) =
  Format.printf "%S at %s %d-%d@." t.title t.loc.loc_start.pos_fname
    t.loc.loc_start.pos_cnum t.loc.loc_end.pos_cnum

let print_entities (t : Diagnostic.t) =
  List.iter
    (fun (id, (loc : Location.t)) ->
      Format.printf "%d %s %d-%d@." (Entities.Id.to_int id)
        loc.loc_start.pos_fname loc.loc_start.pos_cnum loc.loc_end.pos_cnum)
    (Entities.to_list t.entities)

let print_glossary (t : Diagnostic.t) =
  List.iter
    (fun (id, (entry : Glossary.Entry.t)) ->
      Format.printf "%d %s %s@." (Glossary.Id.to_int id) entry.category
        entry.term)
    (Glossary.to_list t.glossary)

let print_raise f =
  match f () with
  | (_ : string Statement.t) -> Format.printf "no exception@."
  | exception exn -> Format.printf "%s@." (Printexc.to_string exn)

[%%expect {|
val text : Inline.t list -> string = <fun>
val described_kind : Diagnostic.Kind.t -> string = <fun>
val described_relation : Diagnostic.Relation.t -> string = <fun>
val described_annotation : Annotation.t -> string = <fun>
val print_body : Diagnostic.t -> unit = <fun>
val print_annotations : Diagnostic.t -> unit = <fun>
val print_content : Diagnostic.t -> unit = <fun>
val print_header : Diagnostic.t -> unit = <fun>
val print_entities : Diagnostic.t -> unit = <fun>
val print_glossary : Diagnostic.t -> unit = <fun>
val print_raise : (unit -> string Statement.t) -> unit = <fun>
|}]

let () =
  let closure : Nlg.subject =
    { name = [ Phrase.Text "the closure" ]; span = Some (span "a.ml" 30 40) }
  in
  let t =
    realize
      [ plan
          ~children:
            [ ( Claim,
                plan
                  (Nlg.sentence
                     [ Nlg.mention ~case:Subject closure;
                       Nlg.txt " captures ";
                       Nlg.mention ~case:Subject argument
                     ]) )
            ]
          (Nlg.sentence
             ~clause:
               (Coordinate
                  [ Nlg.txt "so ";
                    Nlg.mention ~case:Possessive result;
                    Nlg.txt " region ends"
                  ])
             [ Nlg.mention ~case:Subject argument;
               Nlg.txt " escapes into ";
               Nlg.mention ~case:Subject result
             ])
      ]
  in
  print_body t;
  print_entities t;
  print_annotations t

[%%expect {|
explanation: The argument escapes into the result, so the result's region ends.
  claim explanation: The closure captures the argument.
0 a.ml 4-12
1 a.ml 20-26
2 a.ml 30-40
mention 0 name: The argument
mention 1 name: the result
mention 1 name: the result
mention 2 name: The closure
mention 0 name: the argument
|}]

let () =
  let renamed : Nlg.subject =
    { name = [ Phrase.Text "that same argument" ];
      span = Some (span "a.ml" 4 12)
    }
  in
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.mention ~case:Subject argument;
               Nlg.txt " escapes, and ";
               Nlg.mention ~case:Subject renamed;
               Nlg.txt " is boxed"
             ])
      ]
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes, and that same argument is boxed.
mention 0 name: The argument
mention 0 name: that same argument
|}]

let () =
  let region : Nlg.subject =
    { name = [ Phrase.Text "the region "; Phrase.Code "'a" ]; span = None }
  in
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.mention ~case:Possessive argument;
               Nlg.txt " mode outlives ";
               Nlg.mention ~case:Subject region;
               Nlg.txt "; ";
               Nlg.pronoun ~case:Possessive region;
               Nlg.txt " end is here"
             ])
      ]
  in
  print_body t;
  print_entities t;
  print_annotations t

[%%expect {|
explanation: The argument's mode outlives the region 'a; its end is here.
0 a.ml 4-12
mention 0 name: The argument
code: 'a
|}]

let () =
  let region : Nlg.subject =
    { name = [ Phrase.Text "the region" ]; span = None }
  in
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.mention ~case:Subject region;
               Nlg.txt " ends at the call, and ";
               Nlg.pronoun ~case:Subject region;
               Nlg.copula;
               Nlg.txt " where the frame is popped"
             ])
      ]
  in
  print_body t;
  print_entities t

[%%expect {|
explanation: The region ends at the call, and it's where the frame is popped.
|}]

let () =
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.txt "the argument";
               Nlg.copula_agreeing Singular;
               Nlg.txt " captured"
             ]);
        plan
          (Nlg.sentence
             [ Nlg.txt "the arguments";
               Nlg.copula_agreeing Plural;
               Nlg.txt " captured"
             ])
      ]
  in
  print_body t

[%%expect {|
explanation: The argument is captured.
explanation: The arguments are captured.
|}]

let () =
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.txt "the mode of ";
               Nlg.code "x";
               Nlg.txt " is ";
               Nlg.term "local";
               Nlg.txt ", not ";
               Nlg.term "global";
               Nlg.txt " or ";
               Nlg.term "local"
             ])
      ]
  in
  print_body t;
  print_glossary t;
  print_annotations t

[%%expect {|
explanation: The mode of x is local, not global or local.
0 Mode local
1 Mode global
code: x
term 0: local
term 1: global
term 0: local
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:argument
                [ Nlg.ref_source (span "a.ml" 0 30)
                    [ Nlg.mention ~case:Subject argument; Nlg.txt " escapes" ]
                ]);
           plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.copula;
                  Nlg.txt " boxed"
                ])
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes.
explanation: It's boxed.
source 0-30: The argument escapes
mention 0 name: The argument
mention 0 pronoun: It
|}]

let () =
  let about subject main = plan (Nlg.sentence ~subject main) in
  let t =
    realize
      (Nlg.pronominalize
         [ about argument
             [ Nlg.mention ~case:Subject argument;
               Nlg.copula;
               Nlg.txt " captured"
             ];
           about argument
             [ Nlg.mention ~case:Subject argument;
               Nlg.copula;
               Nlg.txt " boxed"
             ];
           about argument [ Nlg.mention ~case:Subject argument; Nlg.copula ];
           about argument
             [ Nlg.pronoun ~case:Possessive argument;
               Nlg.txt " region";
               Nlg.copula;
               Nlg.txt " gone"
             ]
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument is captured.
explanation: It's boxed.
explanation: It is.
explanation: Its region is gone.
mention 0 name: The argument
mention 0 pronoun: It
mention 0 pronoun: It
mention 0 pronoun: Its
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " escapes, and ";
                  Nlg.mention ~case:Subject argument;
                  Nlg.copula;
                  Nlg.txt " boxed"
                ])
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes, and it's boxed.
mention 0 name: The argument
mention 0 pronoun: it
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " escapes, and ";
                  Nlg.mention ~case:Subject argument;
                  Nlg.txt " outlives ";
                  Nlg.pronoun ~case:Subject result
                ])
         ])
  in
  print_body t;
  print_entities t;
  print_annotations t

[%%expect {|
explanation: The argument escapes, and it outlives the result.
0 a.ml 4-12
1 a.ml 20-26
mention 0 name: The argument
mention 0 pronoun: it
mention 1 name: the result
|}]

let () =
  let about subject main = Nlg.sentence ~subject main in
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             ~children:
               [ ( Elaboration,
                   plan
                     ~children:
                       [ ( Claim,
                           plan
                             (about argument
                                [ Nlg.mention ~case:Subject argument;
                                  Nlg.copula;
                                  Nlg.txt " boxed"
                                ]) )
                       ]
                     (about argument
                        [ Nlg.mention ~case:Subject argument;
                          Nlg.copula;
                          Nlg.txt " captured"
                        ]) )
               ]
             (about argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " escapes its region"
                ])
         ])
  in
  print_body t

[%%expect {|
explanation: The argument escapes its region.
  elaboration explanation: It's captured.
    claim explanation: It's boxed.
|}]

let () =
  let statement kind words = Nlg.sentence ~kind [ Nlg.txt words ] in
  let t =
    realize
      [ plan
          ~children:
            [ (Elaboration, plan (statement Explanation "the region ends"));
              ( Claim,
                plan (statement Suggestion "annotate the argument as global") );
              ( Elaboration,
                plan (statement Suggestion "or bind it outside the region") );
              (Claim, plan (statement Background "a region is a stack frame"));
              ( Elaboration,
                plan (statement Background "modes are checked before types") )
            ]
          (statement Explanation "the argument escapes its region")
      ]
  in
  print_body t

[%%expect {|
explanation: The argument escapes its region.
  elaboration explanation: The region ends.
  claim suggestion: Annotate the argument as global.
  elaboration suggestion: Or bind it outside the region.
  claim background: A region is a stack frame.
  elaboration background: Modes are checked before types.
|}]

let () =
  let t =
    realize
      [ plan
          ~children:
            [ ( Elaboration,
                plan (Nlg.sentence ~kind:Suggestion [ Nlg.txt "annotate it" ])
              )
            ]
          (Nlg.sentence
             [ Nlg.mention ~case:Subject argument;
               Nlg.copula;
               Nlg.term "local"
             ])
      ]
  in
  print_header t;
  print_content t;
  print_entities t;
  print_glossary t

[%%expect {|
"the argument escapes its region" at a.ml 0-30
explanation
  mention 0 name
    "The argument"
  " is"
  term 0
    "local"
  "."
  elaboration suggestion
    "Annotate it"
    "."
0 a.ml 4-12
0 Mode local
|}]

let () =
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.txt ""; Nlg.txt "the argument "; Nlg.txt "escapes" ]);
        plan (Nlg.sentence [ Nlg.code "x"; Nlg.txt " escapes its region" ])
      ]
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes.
explanation: x escapes its region.
code: x
|}]

let () =
  let t =
    realize
      [ plan
          (Nlg.sentence
             [ Nlg.ref_source (span "a.ml" 0 30)
                 [ Nlg.txt "the argument escapes" ]
             ]);
        plan
          (Nlg.sentence
             [ Nlg.mention ~case:Subject argument; Nlg.txt " is boxed" ]);
        plan (Nlg.sentence [ Nlg.term "local"; Nlg.txt " values stay put" ])
      ]
  in
  print_body t;
  print_entities t;
  print_glossary t;
  print_annotations t

[%%expect {|
explanation: The argument escapes.
explanation: The argument is boxed.
explanation: Local values stay put.
0 a.ml 4-12
0 Mode local
source 0-30: The argument escapes
mention 0 name: The argument
term 0: Local
|}]

let () =
  print_content
    (realize
       [ plan (Nlg.sentence [ Nlg.txt "annotate "; Nlg.code "x" ]);
         plan (Nlg.sentence [ Nlg.txt "the mode is "; Nlg.term "local" ])
       ])

[%%expect {|
explanation
  "Annotate "
  code
    "x"
  "."
explanation
  "The mode is "
  term 0
    "local"
  "."
|}]

let () =
  let t =
    realize
      [ plan (Nlg.sentence [ Nlg.txt "the argument escapes." ]);
        plan (Nlg.sentence [ Nlg.txt "why does the argument escape?" ]);
        plan (Nlg.sentence [ Nlg.txt "the argument escapes!" ]);
        plan (Nlg.sentence [ Nlg.txt "the argument escapes here:" ])
      ]
  in
  print_body t

[%%expect {|
explanation: The argument escapes.
explanation: Why does the argument escape?
explanation: The argument escapes!
explanation: The argument escapes here:
|}]

let () =
  print_content (realize [ plan (Nlg.sentence [ Nlg.txt "" ]) ]);
  print_content
    (realize
       [ plan (Nlg.sentence [ Nlg.txt "The argument escapes"; Nlg.txt "." ]) ])

[%%expect {|
explanation
  ""
explanation
  "The argument escapes"
  "."
|}]

let () =
  let t =
    realize
      [ silent
          [ ( Elaboration,
              silent
                [ ( Claim,
                    plan (Nlg.sentence [ Nlg.txt "the argument escapes" ]) )
                ] )
          ]
      ]
  in
  print_content t

[%%expect {|
explanation
  elaboration explanation
    claim explanation
      "The argument escapes"
      "."
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " escapes its region"
                ]);
           silent
             [ ( Elaboration,
                 plan
                   (Nlg.sentence ~subject:argument
                      [ Nlg.mention ~case:Subject argument;
                        Nlg.copula;
                        Nlg.txt " boxed"
                      ]) )
             ]
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes its region.
explanation:
  elaboration explanation: It's boxed.
mention 0 name: The argument
mention 0 pronoun: It
|}]

let () =
  print_content
    (realize
       [ plan
           (Nlg.sentence
              ~clause:(Coordinate [ Nlg.txt "so it is boxed" ])
              [ Nlg.txt "the argument escapes" ]);
         plan
           (Nlg.sentence
              ~clause:(Subordinate [ Nlg.txt "because it is captured" ])
              [ Nlg.txt "the argument escapes" ])
       ])

[%%expect {|
explanation
  "The argument escapes"
  ", "
  "so it is boxed"
  "."
explanation
  "The argument escapes"
  " "
  "because it is captured"
  "."
|}]

let () =
  print_raise (fun () ->
    Nlg.sentence ~clause:(Coordinate []) [ Nlg.txt "the argument escapes" ]);
  print_raise (fun () ->
    Nlg.sentence ~subject:result [ Nlg.mention ~case:Subject argument ]);
  print_raise (fun () -> Nlg.fragment [])

[%%expect {|
Invalid_argument("Diagnostic_nlg.sentence: empty clause phrase")
Invalid_argument("Diagnostic_nlg.sentence: subject not mentioned in the main phrase")
Invalid_argument("Diagnostic_nlg.fragment: empty phrase")
|}]

let () =
  let t =
    realize
      [ plan (Nlg.fragment [ Nlg.txt "the argument escapes" ]);
        plan (Nlg.fragment [ Nlg.txt "and its region ends at the call," ]);
        plan (Nlg.fragment [ Nlg.txt "The argument is boxed." ])
      ]
  in
  print_content t

[%%expect {|
explanation
  "the argument escapes"
explanation
  "and its region ends at the call,"
explanation
  "The argument is boxed."
|}]

let () =
  let words = [ Nlg.txt "the argument escapes" ] in
  print_content
    (realize [ plan (Nlg.sentence words); plan (Nlg.fragment words) ])

[%%expect {|
explanation
  "The argument escapes"
  "."
explanation
  "the argument escapes"
|}]

let () =
  let phrase =
    [ Nlg.ref_source (span "a.ml" 0 30) [ Nlg.mention ~case:Subject argument ];
      Nlg.txt " has mode ";
      Nlg.term "local";
      Nlg.txt ", so annotate ";
      Nlg.code "x"
    ]
  in
  let t = realize [ plan (Nlg.fragment phrase); plan (Nlg.sentence phrase) ] in
  print_content t;
  print_entities t;
  print_glossary t

[%%expect {|
explanation
  source 0-30
    mention 0 name
      "the argument"
  " has mode "
  term 0
    "local"
  ", so annotate "
  code
    "x"
explanation
  source 0-30
    mention 0 name
      "The argument"
  " has mode "
  term 0
    "local"
  ", so annotate "
  code
    "x"
  "."
0 a.ml 4-12
0 Mode local
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:result
                [ Nlg.mention ~case:Subject result;
                  Nlg.txt " outlives the region"
                ]);
           plan
             (Nlg.fragment
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " is captured by ";
                  Nlg.mention ~case:Possessive argument;
                  Nlg.txt " closure"
                ]);
           plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.copula;
                  Nlg.txt " boxed"
                ])
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The result outlives the region.
explanation: the argument is captured by the argument's closure
explanation: It's boxed.
mention 0 name: The result
mention 1 name: the argument
mention 1 name: the argument
mention 1 pronoun: It
|}]

let () =
  let t =
    realize
      (Nlg.pronominalize
         [ plan
             (Nlg.sentence ~subject:argument
                [ Nlg.mention ~case:Subject argument;
                  Nlg.txt " escapes its region"
                ]);
           plan
             (Nlg.fragment
                [ Nlg.pronoun ~case:Subject argument;
                  Nlg.copula;
                  Nlg.txt " boxed"
                ]);
           plan
             (Nlg.fragment
                [ Nlg.pronoun ~case:Subject argument;
                  Nlg.txt " outlives ";
                  Nlg.pronoun ~case:Subject result
                ])
         ])
  in
  print_body t;
  print_annotations t

[%%expect {|
explanation: The argument escapes its region.
explanation: it's boxed
explanation: it outlives the result
mention 0 name: The argument
mention 0 pronoun: it
mention 0 pronoun: it
mention 1 name: the result
|}]

let () =
  let line words = plan (Nlg.fragment ~kind:Background [ Nlg.txt words ]) in
  let t =
    realize
      [ plan
          ~children:
            [ (Claim, line "captured by a closure that outlives the region");
              (Elaboration, line "the region ends at the call");
              (Elaboration, line "the argument is not boxed")
            ]
          (Nlg.sentence ~kind:Explanation [ Nlg.txt "the argument escapes" ])
      ]
  in
  print_body t

[%%expect {|
explanation: The argument escapes.
  claim background: captured by a closure that outlives the region
  elaboration background: the region ends at the call
  elaboration background: the argument is not boxed
|}]
