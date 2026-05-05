Merlin should be able to resolve a compilation unit whose name is the same as the current
file.

Compile a library named "foo"
  $ $OCAMLC -c foo/foo.ml

Jump to the definition of "Foo.hello".
  $ $MERLIN single locate -position 1:17 -filename bar/foo.ml < bar/foo.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/foo/foo.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }
