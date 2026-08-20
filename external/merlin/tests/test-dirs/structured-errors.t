Structured explanations of mode errors, for editors

  $ cat > x.ml <<EOF
  > let test () =
  >   let x = ref 42 in
  >   let foo () = x := 24 in
  >   let (bar @ portable) () = let _ = foo in () in
  >   ignore bar
  > EOF

The value is the versioned envelope of the raw location dialect

  $ $MERLIN single structured-errors -filename x.ml < x.ml \
  >   | jq -c '.value | {version: .version, count: (.diagnostics | length)}'
  {"version":1,"count":1}

Each diagnostic is reported at the span the compiler reports the error at, as a
1-based line with a 0-based byte column

  $ $MERLIN single structured-errors -filename x.ml < x.ml \
  >   | jq -c '.value.diagnostics[0].loc'
  {"file":"x.ml","start":{"line":4,"col":36},"end":{"line":4,"col":39}}

  $ $MERLIN single errors -filename x.ml < x.ml \
  >   | jq -c '.value[0] | {start: .start, end: .end}'
  {"start":{"line":4,"col":36},"end":{"line":4,"col":39}}

The title names the failing axes

  $ $MERLIN single structured-errors -filename x.ml < x.ml \
  >   | jq -r '.value.diagnostics[0].title'
  Explain mode error (portability)

Every mention resolves to an entity the response defines, and every term to a
glossary entry

  $ $MERLIN single structured-errors -filename x.ml < x.ml \
  >   | jq -c '.value.diagnostics[0] | {undefined_entities: (([.. | objects | select(.kind? == "mention") | .entity] | unique) - [.entities[].id] | length), undefined_terms: (([.. | objects | select(.kind? == "term") | .term] | unique) - [.glossary[].id] | length)}'
  {"undefined_entities":0,"undefined_terms":0}

The glossary is documented from Merlin's syntax documentation

  $ $MERLIN single structured-errors -filename x.ml < x.ml \
  >   | jq -c '[.value.diagnostics[0].glossary[] | {category: .category, documented: (.description != "")}] | unique'
  [{"category":"Mode","documented":true}]

A repeated mention is realized as a pronoun, unless the client asks for names

  $ $MERLIN single structured-errors -pronouns true -filename x.ml < x.ml \
  >   | jq -c '[.value.diagnostics[0] | .. | objects | select(.kind? == "mention") | .form] | unique'
  ["name","pronoun"]

  $ $MERLIN single structured-errors -pronouns false -filename x.ml < x.ml \
  >   | jq -c '[.value.diagnostics[0] | .. | objects | select(.kind? == "mention") | .form] | unique'
  ["name"]

Pronouns are the default

  $ $MERLIN single structured-errors -filename x.ml < x.ml > default.json
  $ $MERLIN single structured-errors -pronouns true -filename x.ml < x.ml > explicit.json
  $ diff default.json explicit.json && echo "same"
  same

The buffer comes from stdin, so an unsaved edit is diagnosed at the span it has
in the buffer rather than the one it has on disk

  $ cat > y.ml <<EOF
  > let ok () = ()
  > EOF

  $ cat > edited <<EOF
  > let ok () = ()
  > 
  > let test () =
  >   let x = ref 42 in
  >   let foo () = x := 24 in
  >   let (bar @ portable) () = let _ = foo in () in
  >   ignore bar
  > EOF

  $ $MERLIN single structured-errors -filename y.ml < y.ml \
  >   | jq -c '.value.diagnostics | length'
  0

  $ $MERLIN single structured-errors -filename y.ml < edited \
  >   | jq -c '.value.diagnostics[0].loc.start'
  {"line":6,"col":36}

An error this query does not explain leaves the compiler's own message as the
only one: the envelope is still well formed, and carries no diagnostic

  $ cat > t.ml <<EOF
  > let f (x : int) = x ^ "a"
  > EOF

  $ $MERLIN single errors -filename t.ml < t.ml | jq -c '.value | length'
  1

  $ $MERLIN single structured-errors -filename t.ml < t.ml \
  >   | jq -c '.value | {version: .version, count: (.diagnostics | length)}'
  {"version":1,"count":0}
