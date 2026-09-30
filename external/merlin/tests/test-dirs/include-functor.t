Test that Merlin handles `include functor` correctly, both with and without
statefulness.

  $ $MERLIN server stop-server

  $ cat > foo.ml <<EOF
  > type t
  > EOF

  $ $MERLIN server errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  []

  $ cat > foo.ml <<EOF
  > type t
  > module F (M : sig type t end) = struct type u = t end
  > include functor F
  > EOF

  $ $MERLIN server errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  [
    {
      "start": {
        "line": 3,
        "col": 16
      },
      "end": {
        "line": 3,
        "col": 17
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Signature mismatch in included functor's parameter:\nThe type t is required but not provided\nFile \"foo.ml\", line 2, characters 18-24: Expected declaration"
    }
  ]

  $ $MERLIN single errors -filename foo.ml < foo.ml | revert-newlines | jq .value
  [
    {
      "start": {
        "line": 3,
        "col": 16
      },
      "end": {
        "line": 3,
        "col": 17
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Signature mismatch in included functor's parameter:\nThe type t is required but not provided\nFile \"foo.ml\", line 2, characters 18-24: Expected declaration"
    }
  ]

  $ $MERLIN server stop-server
