Check that merlin rpcs work just as well with cms files as with
cmt files.

  $ $OCAMLC -bin-annot-cms -c record.ml

  $ cat main.ml
  let f (x : Record.t) = x.a

  $ $MERLIN single document -position 1:26 -verbosity 0 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": "field a",
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 1:26 -verbosity 0 -filename ./main.ml < ./main.ml | jq .value[:2]
  [
    {
      "start": {
        "line": 1,
        "col": 25
      },
      "end": {
        "line": 1,
        "col": 26
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 1,
        "col": 23
      },
      "end": {
        "line": 1,
        "col": 26
      },
      "type": "int",
      "tail": "no"
    }
  ]

