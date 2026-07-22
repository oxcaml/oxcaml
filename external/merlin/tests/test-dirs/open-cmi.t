Test the -open-cmi flag: like -open, but takes the path of a compiled interface
and opens the corresponding module before typing.

  $ mkdir lib
  $ cat > lib/foo.ml << EOF
  > let bar = "bar"
  > EOF
  $ ( cd lib ; $OCAMLC -bin-annot -c foo.ml )

  $ cat > main.ml << EOF
  > let _x = bar
  > EOF

Without the flag, [bar] is unbound.
  $ $MERLIN single errors -filename main.ml < main.ml | jq '.value[].message'
  "Unbound value bar"

With -open-cmi, [bar] comes from the opened Foo.
  $ $MERLIN single errors -open-cmi lib/foo.cmi -filename main.ml < main.ml \
  > | jq '.value'
  []

The module itself is registered as hidden: only the open's bindings are
visible, direct references to [Foo] remain unbound.
  $ cat > main2.ml << EOF
  > let _x = Foo.bar
  > EOF
  $ $MERLIN single errors -open-cmi lib/foo.cmi -filename main2.ml < main2.ml \
  > | jq '.value[].message'
  "Unbound module Foo"
