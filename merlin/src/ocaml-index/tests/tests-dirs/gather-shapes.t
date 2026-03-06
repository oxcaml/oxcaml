Jane Street only: This test demonstrates using the gather-shapes subcommand, which allows
us to avoid loading the cms files of dependencies.

  $ cat >main.ml <<EOF
  > let x = Foo.Bar.x
  > let a = Foo.a
  > let b = Foo.a + Foo.b
  > module Bar = Foo.Bar
  > EOF

  $ mkdir lib
  $ cat >lib/foo.ml <<EOF
  > let a = Bar.x
  > module Bar = Bar
  > let b = Bar.x
  > EOF

  $ cat >lib/bar.ml <<EOF
  > let x = 0
  > EOF

  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c lib/bar.ml -I lib
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c lib/foo.ml -I lib
  $ ocaml-index gather-shapes -o lib/lib.merlin-shapes lib/*.cms

  $ $OCAMLC -bin-annot -bin-annot-cms -bin-annot-occurrences -c main.ml -I lib
  $ ocaml-index aggregate -o main.merlin-index lib/lib.merlin-shapes main.cms

  $ ocaml-index dump main.merlin-index
  8 uids:
  {uid: Bar.0; locs: "Foo.Bar.x": File "main.ml", line 1, characters 8-17
   uid: Foo.0; locs:
     "Foo.a": File "main.ml", line 2, characters 8-13;
     "Foo.a": File "main.ml", line 3, characters 8-13
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5
   uid: Foo.1; locs: "Foo.Bar": File "main.ml", line 4, characters 13-20
   uid: Main.1; locs: "a": File "main.ml", line 2, characters 4-5
   uid: Foo.2; locs: "Foo.b": File "main.ml", line 3, characters 16-21
   uid: Main.2; locs: "b": File "main.ml", line 3, characters 4-5
   uid: Main.3; locs: "Bar": File "main.ml", line 4, characters 7-10 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

Order matters; if we don't load the shapes file before the cms, we will fail to index
properly:
  $ ocaml-index aggregate -o main-out-of-order.merlin-index main.cms lib/lib.merlin-shapes
  $ ocaml-index dump main-out-of-order.merlin-index
  4 uids:
  {uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5
   uid: Main.1; locs: "a": File "main.ml", line 2, characters 4-5
   uid: Main.2; locs: "b": File "main.ml", line 3, characters 4-5
   uid: Main.3; locs: "Bar": File "main.ml", line 4, characters 7-10 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}
