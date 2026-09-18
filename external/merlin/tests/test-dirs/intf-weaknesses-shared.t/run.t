The [_intf.ml] definition is shared by two implementations through their
interfaces' include declarations. Strengthening must edit that definition
and retain only annotations supported by both implementations.

The implementations call [app]'s callback once and twice respectively, and
[apply]'s callback twice and once respectively. This makes choosing either
implementation alone observably different from merging both. [hof_once.mli]
also names the shared signature through an alias before writing [include S].

  $ printf 'S .\nB .\n' > .merlin
  $ $OCAMLC -bin-annot-cms -c hof_intf.ml hof_once.mli hof_once.ml \
  >   hof_twice.mli hof_twice.ml

Without an index, shared declarations cannot be safely strengthened.

  $ $MERLIN single intf-weaknesses -filename ./hof_intf.ml \
  >   < hof_intf.ml | jq -c .value
  []
  $ $MERLIN single intf-weaknesses -filename ./hof_once.ml \
  >   < hof_once.ml | jq -c .value
  []

  $ query() {
  >   $MERLIN single intf-weaknesses -index-file "$2" -filename "./$1" < "$1" \
  >     | revert-newlines \
  >     | jq '[.value[] | .intf_file |= split("/")[-1]]'
  > }

Indexing each implementation separately exposes its [once] claim; indexing
both drops that claim from each callback argument while retaining [local].

  $ ocaml-index aggregate hof_intf.cms hof_once.cms hof_once.cmsi -o once.index
  $ ocaml-index aggregate hof_intf.cms hof_twice.cms hof_twice.cmsi \
  >   -o twice.index
  $ ocaml-index aggregate hof_intf.cms hof_once.cms hof_once.cmsi \
  >   hof_twice.cms hof_twice.cmsi -o both.index
  $ query hof_intf.ml once.index > once.json
  $ query hof_intf.ml twice.index > twice.json
  $ query hof_intf.ml both.index > both.json
  $ callback_modes() {
  >   jq -r '.[].edits[]
  >     | select((.start.line == 2 and .start.col == 22)
  >           or (.start.line == 3 and .start.col == 24))
  >     | "\(.start.line):\(.new_text)"' "$1" | sort
  > }
  $ callback_modes once.json
  2: @ local once
  3: @ local
  $ callback_modes twice.json
  2: @ local
  3: @ local once
  $ callback_modes both.json
  2: @ local
  3: @ local

Both implementation buffers and their including interfaces find the same
shared definition and merge over both implementations.

  $ for file in hof_once.ml hof_once.mli hof_twice.ml hof_twice.mli; do
  >   query "$file" both.index > "$file.json"
  >   diff -u both.json "$file.json"
  > done

Every edit belongs to the original module-type definition.

  $ jq -r '.[] | .intf_file, (.edits[].file | split("/")[-1])' both.json \
  >   | sort -u
  hof_intf.ml

Apply every edit, including the floating modality clause, to the actual
shared source and check the complete resulting module type.

  $ mkdir strengthened
  $ jq -j --rawfile source hof_intf.ml '
  >   def offset($p):
  >     ($source | split("\n") | .[:($p.line - 1)]
  >      | map(length + 1) | add // 0) + $p.col;
  >   reduce ([.[].edits[]] | sort_by(.start.line, .start.col) | reverse)[]
  >     as $edit ($source;
  >       offset($edit.start) as $start | offset($edit.end) as $end
  >       | .[:$start] + $edit.new_text + .[$end:])
  > ' both.json > strengthened/hof_intf.ml
  $ sed '/^$/d' strengthened/hof_intf.ml
  module type S = sig
    @@ stateless
    val app : ('a -> 'b @ immutable local once) @ local -> 'a -> unit
    val apply : ('a -> 'b @ immutable local once) @ local -> 'a -> unit
  end

Compile both unchanged implementations and their unchanged including
interfaces against the strengthened shared definition.

  $ cp hof_once.ml hof_once.mli hof_twice.ml hof_twice.mli strengthened/
  $ (cd strengthened && $OCAMLC -c hof_intf.ml hof_once.mli hof_once.ml \
  >   hof_twice.mli hof_twice.ml)

An including interface also declares values before and after the include.
Those values must be strengthened in that interface, while the shared values
must still be strengthened in [hot_intf.ml] using both implementations.

  $ mkdir mixed
  $ cp .merlin mixed/
  $ cp hof_intf.ml mixed/hot_intf.ml
  $ sed 's/Hof_intf/Hot_intf/g' hof_once.ml > mixed/hot_once.ml
  $ cat >> mixed/hot_once.ml <<'EOF'
  > let before = app
  > let after = app
  > EOF
  $ cat > mixed/hot_once.mli <<'EOF'
  > val before : ('a -> 'b) -> 'a -> unit
  > include Hot_intf.S
  > val after : ('a -> 'b) -> 'a -> unit
  > EOF
  $ cp hof_twice.ml mixed/hot_twice.ml
  $ sed 's/Hof_intf/Hot_intf/g' hof_twice.mli > mixed/hot_twice.mli
  $ cd mixed
  $ $OCAMLC -bin-annot-cms -c hot_intf.ml hot_once.mli hot_once.ml \
  >   hot_twice.mli hot_twice.ml
  $ ocaml-index aggregate hot_intf.cms hot_once.cms hot_once.cmsi \
  >   hot_twice.cms hot_twice.cmsi -o both.index
  $ for file in hot_intf.ml hot_once.ml hot_once.mli hot_twice.ml \
  >   hot_twice.mli; do
  >   query "$file" both.index > "$file.json"
  > done

Check that both destinations have edits, and each edit names its action's
file. Queries from the implementation and interface must agree.

  $ jq -e '
  >   map(.intf_file) == ["hot_intf.ml", "hot_once.mli"]
  >   and all(.[]; . as $action
  >     | (.edits | length) > 0
  >       and all(.edits[];
  >         (.file | split("/")[-1]) == $action.intf_file))
  > ' hot_once.mli.json > /dev/null
  $ diff -u hot_once.mli.json hot_once.ml.json
  $ jq '[.[] | select(.intf_file == "hot_intf.ml")]' \
  >   hot_once.mli.json > shared.json
  $ for file in hot_intf.ml hot_twice.ml hot_twice.mli; do
  >   diff -u shared.json "$file.json"
  > done

The two local callback arguments can claim [once]. Their edits must refer
to the declarations on either side of the include, at their original columns.

  $ jq -e '
  >   [.[] | select(.intf_file == "hot_once.mli") | .edits[]
  >    | select(.new_text == " @ local once") | [.start.line, .start.col]]
  >   | sort == [[1, 22], [3, 21]]
  > ' hot_once.mli.json > /dev/null

Apply each action only to its named file. The shared result must match the
already checked two-implementation result, and the include must stay intact.

  $ mkdir strengthened
  $ apply_actions() {
  >   jq -j --arg file "$1" --rawfile source "$1" '
  >     def offset($p):
  >       ($source | split("\n") | .[:($p.line - 1)]
  >        | map(length + 1) | add // 0) + $p.col;
  >     reduce ([.[] | select(.intf_file == $file) | .edits[]]
  >             | sort_by(.start.line, .start.col) | reverse)[]
  >       as $edit ($source;
  >         offset($edit.start) as $start | offset($edit.end) as $end
  >         | .[:$start] + $edit.new_text + .[$end:])
  >   ' hot_once.mli.json > "strengthened/$1"
  > }
  $ apply_actions hot_intf.ml
  $ apply_actions hot_once.mli
  $ diff -u ../strengthened/hof_intf.ml strengthened/hot_intf.ml
  $ test "$(rg -c '^include Hot_intf\.S$' strengthened/hot_once.mli)" = 1
  $ for name in before after; do
  >   rg -q "^val $name : .*\) @ local once -> 'a -> unit" \
  >     strengthened/hot_once.mli
  > done
  $ cp hot_once.ml hot_twice.ml hot_twice.mli strengthened/
  $ (cd strengthened && $OCAMLC -c hot_intf.ml hot_once.mli hot_once.ml \
  >   hot_twice.mli hot_twice.ml)
  $ cd ..
