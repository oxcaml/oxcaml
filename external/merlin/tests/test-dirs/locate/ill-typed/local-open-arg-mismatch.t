Test that locate works on a function inside a local open when the argument has a
type mismatch.

  $ cat >test.ml <<EOF
  > module Foo = struct
  >   let bar : (unit -> unit) -> unit = failwith "todo"
  > end
  > 
  > let component () =
  >   let baz : int = failwith "" in
  >   Foo.(bar baz)
  > ;;
  > EOF

Locate bar (the function being applied with a mismatched argument):
  $ $MERLIN single locate -position 7:9 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 2,
      "col": 6
    }
  }

Locate baz (the ill-typed argument) still works too:
  $ $MERLIN single locate -position 7:12 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 6,
      "col": 6
    }
  }
