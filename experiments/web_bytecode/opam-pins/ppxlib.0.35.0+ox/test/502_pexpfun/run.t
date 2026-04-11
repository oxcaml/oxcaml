We want to make sure that migrations from 5.2 to previous versions still
produce valid location ranges between parent and child.

  $ cat > test.ml << EOF
  > let make ~foo ~bar = foo ^ bar
  > EOF

We run a custom driver that will read our ast, migrate it back to 5.01, and
check that the locations are valid (the parent range is larger than the child
range).

  $ ./driver.exe -locations-check --impl test.ml -o ignore.ml

Locations should also be well formed for Pparam_newtype
  $ cat > test.ml << EOF
  > let make (type t) (type u) foo = foo
  > EOF

  $ ./driver.exe -locations-check --impl test.ml -o ignore.ml
