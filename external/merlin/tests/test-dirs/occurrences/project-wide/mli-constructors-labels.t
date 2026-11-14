Project-wide occurrences of constructors and labels should be found from their
declarations in an interface, not only from the implementation.

  $ cat >main.mli <<'EOF'
  > type t = A | B of { f : int }
  > type r = { g : int }
  > EOF

  $ cp main.mli main.ml

  $ cat >user.ml <<'EOF'
  > let _ = Main.A
  > let _ = Main.B { f = 1 }
  > let _ = fun (x : Main.r) -> x.Main.g
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c main.mli main.ml user.ml

  $ ocaml-index aggregate main.cmti main.cmt user.cmt

  $ occurrences () {
  >   $MERLIN single occurrences -scope "$1" -identifier-at "$2" \
  >     -index-file project.ocaml-index -filename "$3" <"$3" |
  >   jq -r '.value[] | "\(.file | sub(".*/"; "")):\(.start.line):\(.start.col)"'
  > }

Constructor A, from the interface and from the implementation:
  $ occurrences project 1:9 main.mli
  main.mli:1:9
  main.ml:1:9
  user.ml:1:8

  $ occurrences project 1:9 main.ml
  main.ml:1:9
  main.mli:1:9
  user.ml:1:8

Inline record label f:
  $ occurrences project 1:20 main.mli
  main.mli:1:20
  main.ml:1:20
  user.ml:2:17

Record label g:
  $ occurrences project 2:11 main.mli
  main.mli:2:11
  main.ml:2:11
  user.ml:3:30

Renaming relies on the index's related uids rather than on the cmt:
  $ occurrences renaming 1:9 main.mli
  main.mli:1:9
  main.ml:1:9
  user.ml:1:13

  $ occurrences renaming 2:11 main.mli
  main.mli:2:11
  main.ml:2:11
  user.ml:3:35
