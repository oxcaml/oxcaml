This file demonstrates that when using cms files, we must pass dependencies to ocaml-index
for it to be able to properly index a file. Additionally, we must distinguish between
-I and -H dependencies to match the compiler's behavior.

  $ cat >main.ml <<EOF
  > let x : int = Foo.Bar.x
  > (* The : int demonstrates that Foo.Bar.x is from the visible lib, since Foo.x is a
  >    string in the hidden lib *)
  > EOF

  $ mkdir visible_lib
  $ cat >visible_lib/foo.ml <<EOF
  > module Bar = Bar
  > EOF

  $ cat >visible_lib/bar.ml <<EOF
  > let x = 0
  > EOF

  $ mkdir hidden_lib
  $ cat >hidden_lib/foo.ml <<EOF
  > module Bar = struct
  >   let x = "hello"
  > end
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-cms -bin-annot-occurrences -c visible_lib/bar.ml
  $ $OCAMLC -bin-annot -bin-annot-cms -bin-annot-occurrences -c visible_lib/foo.ml -I visible_lib
  $ $OCAMLC -bin-annot -bin-annot-cms -bin-annot-occurrences -c hidden_lib/foo.ml
  $ $OCAMLC -bin-annot -bin-annot-cms -bin-annot-occurrences -c main.ml -H hidden_lib -I visible_lib

If we use cmt files and don't include dependencies, ocaml-index will succeed because cmt
files include the load-path in them:
  $ ocaml-index aggregate -o main.uideps main.cmt
  $ ocaml-index dump main.uideps
  2 uids:
  {uid: Bar.0; locs: "Foo.Bar.x": File "main.ml", line 1, characters 14-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

If we use cms files and don't include dependencies, ocaml-index will fail to index
identifiers from dependencies:
  $ ocaml-index aggregate -o main.uideps main.cms
  $ ocaml-index dump main.uideps
  1 uids: {uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

If we pass a hidden dependency as a visible one, we can run into trouble. Note that
ocaml-index believes that "Foo.Bar.x" comes from Foo rather than Bar:
  $ ocaml-index aggregate -o main.uideps main.cms -I hidden_lib -I visible_lib
  $ ocaml-index dump main.uideps
  2 uids:
  {uid: Foo.0; locs: "Foo.Bar.x": File "main.ml", line 1, characters 14-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

If we pass dependencies, we get the correct results:
  $ ocaml-index aggregate -o main.uideps main.cms -H hidden_lib -I visible_lib
  $ ocaml-index dump main.uideps
  2 uids:
  {uid: Bar.0; locs: "Foo.Bar.x": File "main.ml", line 1, characters 14-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

Lastly, check that ocaml-index disambiguates based on order the same as the compiler.
Since visible_lib comes first, "Foo" in main.ml corresponds to visible_lib/foo.ml:
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c main.ml -I visible_lib -I hidden_lib
  $ ocaml-index aggregate -o main.uideps main.cms -I visible_lib -I hidden_lib
  $ ocaml-index dump main.uideps
  2 uids:
  {uid: Bar.0; locs: "Foo.Bar.x": File "main.ml", line 1, characters 14-23
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}
