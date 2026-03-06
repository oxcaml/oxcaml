Jane Street only: This test is similar to transitive-deps.t, except it uses cms files
instead of cmt files.

  $ cat >main.ml <<EOF
  > let x = List.init Foo.x (fun n -> n)
  > EOF

  $ mkdir lib1
  $ cat >lib1/foo.ml <<EOF
  > include Bar
  > EOF

  $ mkdir lib2
  $ cat >lib2/bar.ml <<EOF
  > let x = 21
  > EOF

  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c lib2/bar.ml
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c lib1/foo.ml -I lib2

Here we have an implicit transitive dependency on lib2:
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c main.ml -I lib1

Here we differ from the cmt version of this test. Since cms files do not include the
loadpath, we must explicitly pass all dependencies. Additionally, we make a distinction
between visible and hidden dependencies:
  $ ocaml-index aggregate -o main.uideps main.cms -I lib1 -H lib2 -I "$MERLIN_TEST_OCAML_PATH/lib/ocaml"
  $ ocaml-index aggregate -o lib1/foo.uideps lib1/foo.cms -I lib2 -I "$MERLIN_TEST_OCAML_PATH/lib/ocaml"
  $ ocaml-index aggregate -o lib2/bar.uideps lib2/bar.cms -I "$MERLIN_TEST_OCAML_PATH/lib/ocaml"

  $ ocaml-index aggregate -o test.uideps main.uideps lib1/foo.uideps lib2/bar.uideps

  $ ocaml-index dump main.uideps
  4 uids:
  {uid: Bar.0; locs: "Foo.x": File "main.ml", line 1, characters 18-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5
   uid: Main.1; locs: "n": File "main.ml", line 1, characters 34-35
   uid: Stdlib__List.45; locs:
     "List.init": File "main.ml", line 1, characters 8-17
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

  $ ocaml-index dump lib1/foo.uideps
  1 uids:
  {uid: Bar; locs: "Bar": File "lib1/foo.ml", line 1, characters 8-11 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

  $ ocaml-index dump test.uideps
  5 uids:
  {uid: Bar; locs: "Bar": File "lib1/foo.ml", line 1, characters 8-11
   uid: Bar.0; locs:
     "x": File "lib2/bar.ml", line 1, characters 4-5;
     "Foo.x": File "main.ml", line 1, characters 18-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5
   uid: Main.1; locs: "n": File "main.ml", line 1, characters 34-35
   uid: Stdlib__List.45; locs:
     "List.init": File "main.ml", line 1, characters 8-17
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

