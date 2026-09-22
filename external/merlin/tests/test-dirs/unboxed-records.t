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

Inherited fields preserve their contents' kind

  $ cat > test.ml << EOF
  > type t : float64 = #{ inherit x : float# }
  > let f (r : t) = r.#x
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml | jq .value
  []

  $ $MERLIN single type-enclosing -position 2:19 -filename test.ml < test.ml | jq -r '.value[0].type'
  float#

Inherited fields require a singleton unboxed record

  $ cat > test.ml << EOF
  > type t = #{ x : int; inherit y : float# }
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml | jq -r '.value[].message'
  Inherited labels are only supported in singleton unboxed records

Construct a module containing an inherited field

  $ cat > test.ml << EOF
  > module M : sig
  >   type t = #{ inherit x : int }
  > end = _
  > EOF

  $ $MERLIN single construct -position 3:6 -filename test.ml < test.ml | revert-newlines | jq -r '.value[1][]'
  struct type t = #{
           inherit x: int } end

Stdlib boxing and unboxing preserve record and scalar types

  $ cat > test.ml << EOF
  > type t = { x : float# }
  > let boxed : t = Stdlib.box #{ x = #1.0 }
  > let unboxed : t# = Stdlib.unbox boxed
  > let scalar : float = Stdlib.box #1.0
  > let raw : float# = Stdlib.unbox scalar
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml | jq .value
  []

  $ $MERLIN single type-enclosing -position 3:6 -filename test.ml < test.ml | jq -r '.value[0].type'
  t#

Abstract box kinds expose unboxed versions

  $ cat > test.ml << EOF
  > module Abs : sig
  >   type t : (value & float64) box
  > end = struct
  >   type t = { i : int; f : float# }
  > end
  > let unbox_abs : Abs.t -> Abs.t# = Stdlib.unbox
  > let box_abs : Abs.t# -> Abs.t = Stdlib.box
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml | jq -r '.value[].message'
