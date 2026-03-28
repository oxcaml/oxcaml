  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >lib.mli <<EOF
  > (****************)
  > (* SOME LICENCE *)
  > (****************)
  > 
  > [@@@ocaml.text "Documentation of Lib"]
  > 
  > [@@@ocaml.doc "Documentation of Lib.a"]
  > type a = int
  > EOF

  $ cat >lib.ml <<EOF
  > type a = int
  > EOF

  $ cat >libimpl.ml <<EOF
  > (****************)
  > (* SOME LICENCE *)
  > (****************)
  > 
  > [@@@ocaml.text "Documentation of Libimpl"]
  > 
  > [@@@ocaml.doc "Documentation of Libimpl.a"]
  > type a = int
  > EOF

  $ cat >main.ml <<EOF
  > type t = Lib.a
  > type u = Libimpl.a
  > EOF

  $ cat >dune <<EOF
  > (executable (name main))
  > EOF

  $ $OCAMLC -bin-annot-cms -o main.exe lib.mli lib.ml libimpl.ml main.ml

The licence is correctly ignored when looking for the doc of Lib
  $ $MERLIN single document -position 1:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of Lib",
    "notifications": []
  }

Same when the doc is in the ml file
  $ $MERLIN single document -position 2:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of Libimpl",
    "notifications": []
  }
