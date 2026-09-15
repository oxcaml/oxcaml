Test the -open-cmi flag: it takes the path of a compiled interface consisting
of module aliases, and each member name resolves directly to its alias target.

  $ mkdir lib
  $ cat > lib/foo0.ml << EOF
  > let bar = "bar"
  > EOF
  $ ( cd lib ; $OCAMLC -bin-annot -c foo0.ml )

The alias module is compiled with warning 49 active, so the alias records the
path of foo0.cmi; opening foo.cmi then makes [Foo0] resolvable without [lib]
being on the include path at all.
  $ cat > lib/foo.ml << EOF
  > module Foo0 = Foo0
  > EOF
  $ ( cd lib ; $OCAMLC -bin-annot -c -no-alias-deps foo.ml )

  $ cat > main.ml << EOF
  > let _x = Foo0.bar
  > EOF

Without the flag, [Foo0] is unbound.
  $ $MERLIN single errors -filename main.ml < main.ml | jq '.value[].message'
  "Unbound module Foo0"

With -open-cmi, [Foo0] resolves through the rebinding, loading foo0.cmi via
the path attached in foo.cmi.
  $ $MERLIN single errors -open-cmi lib/foo.cmi -filename main.ml < main.ml \
  > | jq '.value'
  []

The opened interface itself is anonymous: direct references to [Foo] remain
unbound.
  $ cat > main2.ml << EOF
  > let _x = Foo.Foo0.bar
  > EOF
  $ $MERLIN single errors -open-cmi lib/foo.cmi -filename main2.ml < main2.ml \
  > | jq '.value[].message'
  "Unbound module Foo"

Members that are not aliases to other compilation units are rejected by the
compiler; Merlin ignores them rather than dying on a bad configuration.
  $ $MERLIN single errors -open-cmi lib/foo0.cmi -filename main.ml < main.ml \
  > | jq '.value[].message'
  "Unbound module Foo0"
