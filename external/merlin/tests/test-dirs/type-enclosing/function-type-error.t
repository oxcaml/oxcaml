Some tests that demonstrate that merlin produces reasonable-enough output when
there is a type error involving a function. The relevant thing to look for is
that merlin isn't outright dropping intermediate ASTs that are unrelated to the
type error. The exact output may change slightly -- that's fine.

  $ $MERLIN single type-enclosing -position 1:16 -filename under.ml <<EOF
  > ((fun x y z -> x + y + z) : int -> int)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 20
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 1
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "int -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 1:28 -filename under.ml <<EOF
  > ((fun x y -> function z -> x + y + z) : int -> int)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 27
        },
        "end": {
          "line": 1,
          "col": 28
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 27
        },
        "end": {
          "line": 1,
          "col": 28
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 27
        },
        "end": {
          "line": 1,
          "col": 32
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 27
        },
        "end": {
          "line": 1,
          "col": 36
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 37
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 1
        },
        "end": {
          "line": 1,
          "col": 37
        },
        "type": "int -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 1:17 -filename under.ml <<EOF
  > ((fun x y ?(z = 100) w -> x + y + z + w) : int -> int)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 16
        },
        "end": {
          "line": 1,
          "col": 19
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": "int -> ?z:int -> int -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 1
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": "int -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 1:13 -filename under.ml <<EOF
  > ((fun ?(z = 100) x y w -> x + y + z + w) : ?z:int -> int)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 15
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 1
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": "?z:int -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
