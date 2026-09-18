Test Merlin's behavior around unboxed records

Complete a label

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f (x : t) = x.#fo
  > EOF

TODO (unboxed records): support completion of unboxed record labels
  $ $MERLIN single complete-prefix -position 2:21 -prefix "fo" -filename test.ml < test.ml \
  >   | revert-newlines | jq .value.entries[].name -r
  format_of_string
  format
  format4
  format6


Get type of a label in an expression

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f (x : t) = x.#foo
  > EOF

TODO (unboxed records): maybe location should include the `#`?
  $ $MERLIN single type-enclosing -position 2:21 -filename test.ml < test.ml | jq .value[:2]
  [
    {
      "start": {
        "line": 2,
        "col": 19
      },
      "end": {
        "line": 2,
        "col": 22
      },
      "type": "string",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 16
      },
      "end": {
        "line": 2,
        "col": 22
      },
      "type": "string",
      "tail": "no"
    }
  ]

Get type of a label in a pattern

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f #{ foo = _ } = ()
  > EOF

  $ $MERLIN single type-enclosing -position 2:10 -filename test.ml < test.ml | jq .value[:2]
  [
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 12
      },
      "type": "string",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 2,
        "col": 18
      },
      "type": "t",
      "tail": "no"
    }
  ]

Go to definition of a label in an expression

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f (x : t) = x.#foo
  > let _ = #{ foo = "hi" }
  > EOF

  $ $MERLIN single locate -position 2:21 -filename test.ml < test.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 1,
      "col": 12
    }
  }

  $ $MERLIN single locate -position 3:12 -filename test.ml < test.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 1,
      "col": 12
    }
  }

Go to definition of a label in a pattern

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f #{ foo = _ } = ()
  > EOF

  $ $MERLIN single locate -position 2:10 -filename test.ml < test.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 1,
      "col": 12
    }
  }

Get usages of a label in an unboxed record

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let f #{ foo = _ } x =
  >   let _ = x.#foo in
  >   #{ foo = 10 }
  > EOF

  $ $MERLIN single occurrences -identifier-at 2:10 -filename test.ml < test.ml | jq .value
  [
    {
      "start": {
        "line": 1,
        "col": 12
      },
      "end": {
        "line": 1,
        "col": 15
      },
      "stale": false
    },
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 12
      },
      "stale": false
    },
    {
      "start": {
        "line": 3,
        "col": 13
      },
      "end": {
        "line": 3,
        "col": 16
      },
      "stale": false
    },
    {
      "start": {
        "line": 4,
        "col": 5
      },
      "end": {
        "line": 4,
        "col": 8
      },
      "stale": false
    }
  ]

Construct a record

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let (_ : t) = _
  > EOF

TODO (unboxed records): the record is missing the # at the start of it
  $ $MERLIN single construct -position 2:14 -filename test.ml < test.ml | jq .value
  [
    {
      "start": {
        "line": 2,
        "col": 14
      },
      "end": {
        "line": 2,
        "col": 15
      }
    },
    [
      "{ foo = _ }"
    ]
  ]

Destruct a record

  $ cat > test.ml << EOF
  > type t = #{ foo : string }
  > let (x : t) = x
  > EOF

TODO (unboxed records): allow destruction
  $ $MERLIN single case-analysis -start 2:14 -end 2:15 -filename test.ml < test.ml | jq .value
  "Destruct not allowed on non-destructible type: t"
