  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ cat >dune <<EOF
  > (executable (name test))
  > EOF

  $ cat >test.ml <<EOF
  > let _ = Test2.foo
  > EOF

  $ cat >test2.ml <<EOF
  > let foo = 42
  > EOF

  $ cat >test2.mli <<EOF
  > val foo : int
  > EOF

NOTE: we need to build the @check target to have the cmt and not only the cmti
# Jane Street: we run these tests with ocamlc rather than dune.
# $ dune build @check
  $ $OCAMLC -bin-annot -c test2.mli test2.ml test.ml

Jump to interface:
  $ $MERLIN single locate -look-for mli -position 1:16 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test2.mli",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

Jump to definition:
  $ $MERLIN single locate -look-for ml -position 1:16 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test2.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
