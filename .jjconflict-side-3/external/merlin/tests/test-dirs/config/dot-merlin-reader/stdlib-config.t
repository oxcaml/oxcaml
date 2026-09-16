This test uses ocamlmerlin instead of $MERLIN because $MERLIN configures the stdlib.
Since this test is testing configuration of the stdlib, we want do not want the
configuration that $MERLIN gives.

The STDLIB directive in .merlin is respected
  $ cat > .merlin <<EOF
  > STDLIB /stdlib1
  > EOF

  $ echo | ocamlmerlin single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib1"

  $ rm .merlin

The -ocamlib-path flag is respected
  $ echo | ocamlmerlin single dump-configuration -ocamllib-path /stdlib2 -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib2"

The STDLIB directive in .merlin takes priority over -ocamllib-path
  $ cat > .merlin <<EOF
  > STDLIB /stdlib-from-.merlin
  > EOF

  $ echo | ocamlmerlin single dump-configuration -ocamllib-path /stdlib-from-flag -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib-from-.merlin"
