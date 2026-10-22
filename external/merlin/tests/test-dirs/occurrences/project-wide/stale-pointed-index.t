Test indices pointing at outdated or missing indices.

Each module uses `Lib.x`, and is padded so that its index is large enough to be
granular:

  $ echo "let x = ()" > lib.ml
  $ for m in a b c d e; do
  >   { echo "let () = Lib.x"
  >     for i in $(seq 1 1024); do echo "let ${m}$i = ()"; done
  >   } > $m.ml
  > done
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml a.ml b.ml c.ml d.ml e.ml

Merlin's config points at index A and B. Index A point at C and D, and C points
at E. That is:

a--c--e
 \-d
b

  $ ocaml-index aggregate e.cmt -o e.ocaml-index
  $ ocaml-index aggregate e.ocaml-index c.cmt -o c.ocaml-index
  $ ocaml-index aggregate d.cmt -o d.ocaml-index
  $ ocaml-index aggregate c.ocaml-index d.ocaml-index lib.cmt a.cmt -o a.ocaml-index
  $ ocaml-index aggregate b.cmt -o b.ocaml-index

  $ cat > .merlin << EOF
  > INDEX a.ocaml-index
  > INDEX b.ocaml-index
  > EOF

  $ occurrences () {
  >   $MERLIN single occurrences -scope project -identifier-at 1:4 \
  >     -log-file - -log-section occurrences \
  >     -filename lib.ml < lib.ml 2> log | jq -c '.value[] | [.file, .stale]'
  >   grep "Occurrences status" log
  > }

We can find occurrences of `Lib.x` in every file when the index is up-to-date:

  $ occurrences
  ["$TESTCASE_ROOT/lib.ml",false]
  ["$TESTCASE_ROOT/a.ml",false]
  ["$TESTCASE_ROOT/b.ml",false]
  ["$TESTCASE_ROOT/c.ml",false]
  ["$TESTCASE_ROOT/d.ml",false]
  ["$TESTCASE_ROOT/e.ml",false]
  Occurrences status: included

We change index C, which causes it to be stale, which causes its occurrences to
be missing from the results:

  $ ocaml-index aggregate c.cmt -o c.ocaml-index
  $ occurrences
  ["$TESTCASE_ROOT/lib.ml",false]
  ["$TESTCASE_ROOT/a.ml",false]
  ["$TESTCASE_ROOT/b.ml",false]
  ["$TESTCASE_ROOT/d.ml",false]
  ["$TESTCASE_ROOT/e.ml",false]
  Occurrences status: out-of-sync (source files: [], indices: [$TESTCASE_ROOT/c.ocaml-index])

The same happens if `c` is missing:

  $ rm c.ocaml-index
  $ occurrences
  ["$TESTCASE_ROOT/lib.ml",false]
  ["$TESTCASE_ROOT/a.ml",false]
  ["$TESTCASE_ROOT/b.ml",false]
  ["$TESTCASE_ROOT/d.ml",false]
  ["$TESTCASE_ROOT/e.ml",false]
  Occurrences status: out-of-sync (source files: [], indices: [$TESTCASE_ROOT/c.ocaml-index])
