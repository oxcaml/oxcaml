  $ cat >a.ml <<EOF
  > (** a function *)
  > let b () = ()
  > 
  > (** A function *)
  > let a () = ()
  > 
  > (** a function *)
  > let c () = ()
  > 
  > (** a function with newtypes *)
  > let d () (type a) (a : a) = a
  > 
  > (** last function *)
  > let e () = ()
  > EOF

  $ cat >doc.ml <<EOF
  > (** first function *)
  > let f () = ()
  > 
  > (** second function *)
  > let g () = ()
  > 
  > let () = g (f ())
  > 
  > let list_rev = List.rev
  > 
  > let () = A.a ()
  > let () = A.e ()
  > EOF

documentation for the last defined value (in the same file) is shown
  $ $MERLIN single document -position 7:10 -filename doc.ml < doc.ml |
  > jq '.value'
  "second function"

documentation for the non-last defined value (in the same file) is show
(we care about "non-last" value because of issue #1261)
  $ $MERLIN single document -position 7:13 -filename doc.ml < doc.ml |
  > jq '.value'
  "first function"

  $ $MERLIN single document -position 9:6 -filename doc.ml < doc.ml |
  > jq '.value'
  "No documentation available"

  $ $MERLIN single document -position 9:22 -filename doc.ml < doc.ml |
  > jq '.value'
  "List reversal."

# Jane Street: we can't run dune in tests yet, so we use ocamlc instead.
# $ dune build --root=. ./doc.exe 2> /dev/null
  $ $OCAMLC -bin-annot -o doc.exe a.ml doc.ml
  $ cat >.merlin <<EOF
  > B _build/default/.doc.eobjs/byte
  > S .
  > EOF

  $ $MERLIN single document -position 11:12 -filename doc.ml < doc.ml |
  > jq '.value'
  "A function"

  $ $MERLIN single document -position 12:12 -filename doc.ml < doc.ml |
  > jq '.value'
  "last function"
