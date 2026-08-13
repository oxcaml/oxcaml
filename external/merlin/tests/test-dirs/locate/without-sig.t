  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >nosig.ml <<EOF
  > (** Nosig *)
  > let x = 42
  > EOF

  $ cat >main.ml <<EOF
  > let _x = Nosig.x
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main))
  > EOF

# Jane Street: we can't run dune in tests yet, so we use ocamlc instead.
# $ dune build ./main.exe 2> /dev/null
  $ $OCAMLC -bin-annot -o main.exe nosig.ml main.ml

  $ $MERLIN single locate -look-for ml -position 1:15 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nosig.ml",
      "pos": {
        "line": 2,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 1:15 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nosig.ml",
      "pos": {
        "line": 2,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 1:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nosig.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 1:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nosig.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
