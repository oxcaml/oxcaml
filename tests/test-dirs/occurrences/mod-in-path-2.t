  $ cat >test.ml <<'EOF'
  > module Mod = struct
  >   type t = A 
  > end
  > let () = 
  >   match Mod.A with
  >   | Mod.A -> ()
  > EOF

We expect module appearing in paths to be highlighted
  $ $MERLIN single occurrences -identifier-at 1:8 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 7
      },
      "end": {
        "line": 1,
        "col": 10
<<<<<<< HEAD
      },
      "stale": false
||||||| c76379cdae
      }
=======
      },
      "stale": false
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "stale": false
    },
    {
      "start": {
        "line": 6,
        "col": 4
      },
      "end": {
        "line": 6,
        "col": 7
      },
      "stale": false
>>>>>>> v5.6-504
    }
  ]

  $ $MERLIN single occurrences -identifier-at 2:11 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 11
      },
      "end": {
        "line": 2,
        "col": 12
      },
      "stale": false
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 13
      },
      "stale": false
    },
    {
      "start": {
        "line": 6,
        "col": 4
      },
      "end": {
        "line": 6,
        "col": 9
      },
      "stale": false
    }
  ]
