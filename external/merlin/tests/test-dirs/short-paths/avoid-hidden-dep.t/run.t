Test that short paths will prefer a visible dependency over a hidden one

Start by compiling deps of the test file
  $ $OCAMLC -c hidden/hidden.ml
  $ $OCAMLC -c visible/visible.ml -I hidden

Create a .merlin file
  $ cat > test/.merlin << EOF
  > FLG -short-paths
  > B ../visible
  > S ../visible
  > BH ../hidden
  > SH ../hidden
  > EOF

The type of `Test.foo` either `Hidden.t` or `Visible.Foo.Bar.t`. But `Hidden.t` is not a
valid identifier to write because `Hidden` is a hidden dependency, so Merlin should prefer
`Visible.Foo.Bar.t`
  $ $MERLIN single type-enclosing -position 1:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Visible.Foo.Bar.t

The type of `Test.bar` is `Hidden.u`. There is no valid path that it can be referred to
by, but Merlin should still be able to give `Hidden.u`.
  $ $MERLIN single type-enclosing -position 2:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Hidden.u
