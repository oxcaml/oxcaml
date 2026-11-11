  $ $MERLIN server stop-server

  $ cat > main.ml <<EOF
  > let a () = let y = 1 in ()
  > [@@@warning "-26"]
  > let b () = let x = 1 in ()
  > EOF

  $ $MERLIN single errors -filename main.ml < main.ml | jq .value
  [
    {
      "start": {
        "line": 1,
        "col": 15
      },
      "end": {
        "line": 1,
        "col": 16
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 26: unused variable y."
    }
  ]

  $ $MERLIN server errors -filename main.ml < main.ml | jq .value
  [
    {
      "start": {
        "line": 1,
        "col": 15
      },
      "end": {
        "line": 1,
        "col": 16
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 26: unused variable y."
    }
  ]

  $ $MERLIN server stop-server
