Test that we handle the -Ix compiler flag correctly. Note that in Merlin there's no way to
observe a difference between -I and -Ix.

Compile a library
  $ mkdir lib
  $ cat > lib/foo.ml << EOF
  > let bar = "bar"
  > EOF

  $ ( cd lib ; $OCAMLC -bin-annot -bin-annot-occurrences -c foo.ml )

Create a client of the library
  $ mkdir main
  $ cat > main/main.ml << EOF
  > let x = Foo.bar
  > EOF

Validate our testing strategy: Merlin reports an error if the library isn't on the load
path.
  $ $MERLIN single errors -filename main/main.ml < main/main.ml | jq .value
  [
    {
      "start": {
        "line": 1,
        "col": 8
      },
      "end": {
        "line": 1,
        "col": 15
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound module Foo"
    }
  ]

Merlin can find the library when using -I
  $ $MERLIN single errors -I lib/ -filename main/main.ml < main/main.ml | jq .value
  []

Merlin can find the library when using -Ix
  $ $MERLIN single errors -Ix lib/ -filename main/main.ml < main/main.ml | jq .value
  []

Merlin can find the library when using -I via the .merlin file
  $ echo "FLG -I ../lib" > main/.merlin
  $ $MERLIN single errors -filename main/main.ml < main/main.ml | jq .value
  []

Merlin can find the library when using -Ix via the .merlin file
  $ echo "FLG -Ix ../lib" > main/.merlin
  $ $MERLIN single errors -filename main/main.ml < main/main.ml | jq .value
  []
