  $ $DUNE build @ocaml-index

The occurrence assertion below only needs the generated index to be readable by
Merlin.  `ocaml-index dump` also pretty-prints dotted longidents, which is not a
requirement for this test and is unsafe when reading 5.2 CMT payloads through
the 5.4 overlay longident representation.
  $ ocaml-index stats _build/default/.main.eobjs/cctx.ocaml-index | sed '/^$/d'
  Index "_build/default/.main.eobjs/cctx.ocaml-index" contains:
  - 7 definitions
  - 16 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none

  $ $MERLIN single occurrences -scope project -identifier-at 4:18 -filename main.ml < main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 4,
          "col": 6
        },
        "end": {
          "line": 4,
          "col": 23
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 23
        },
        "stale": false
      }
    ],
    "notifications": []
  }
