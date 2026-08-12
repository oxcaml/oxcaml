
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c func.mli func.ml main.ml
  $ $OCAMLC -o main.exe func.cmo main.cmo
  $ ./main.exe
  Hello world!
  $ ocaml-index aggregate func.cmti func.cmt main.cmt -o index.merlin-index

We expect 2 occurrences in func.ml, 1 in func.mli and 1 in main.ml from the
generated index.
  $ $MERLIN single occurrences -scope renaming -identifier-at 1:22 \
  > -I . -index-file index.merlin-index -filename main.ml <main.ml \
  > | jq '.value[] | .file,.start'
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 1,
    "col": 22
  }
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 4,
    "col": 16
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 1,
    "col": 24
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 3,
    "col": 30
  }
  "$TESTCASE_ROOT/func.mli"
  {
    "line": 1,
    "col": 24
  }
