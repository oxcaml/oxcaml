The compiler inserts documentation comments with their location set to
`Location.none`. The value for `Location.none` has changed in the compiler (at
4.08.0). We provide a function, `loc_of_attribute` to handle deriving better location
errors for attributes with a none location.

  $ cat > test.ml << EOF
  > let v = 1
  > (** A documentation comment! *)
  > EOF

We run an identity driver that prints the locations of attributes.

  $ ./pp.exe --impl test.ml -o ignore.ml
  File "test.ml", line 2, characters 0-31: ocaml.doc

