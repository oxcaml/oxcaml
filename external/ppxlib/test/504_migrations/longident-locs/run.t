We have a custom driver that will force migration of the AST down to 5.2 and back to
the compiler's version and print it as source code using the compiler's printer,
regardless of ppxlib's internal AST version.

If we run the driver on the following source file:

  $ cat > test.ml << EOF
  > let () = NonExistingModule.foo ()
  > EOF

then the non-existing module should have a sensible error location.

  $ ocamlc -ppx "./driver.exe --as-ppx -locations-check" test.ml test.ml.pp
  File "test.ml", line 1, characters 9-30:
  1 | let () = NonExistingModule.foo ()
               ^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module NonExistingModule
  [2]

Another longident usage:

  $ cat > test.ml << EOF
  > let t = { ThisModule.age = 43 }
  > EOF

  $ ocamlc -ppx "./driver.exe --as-ppx -locations-check" test.ml test.ml.pp
  File "test.ml", line 1, characters 10-24:
  1 | let t = { ThisModule.age = 43 }
                ^^^^^^^^^^^^^^
  Error: Unbound module ThisModule
  [2]

Longidents with Lapplys:

  $ cat > test.ml << EOF
  > module F (A : sig type t end) = struct
  >   module G (B : sig type t end) = struct
  >     type t = A.t * B.t
  >   end
  > end
  > module X = struct end
  > 
  > type t = { v : F(X).G(Int).t }
  > EOF

  $ ocamlc -ppx "./driver.exe --as-ppx -locations-check" test.ml test.ml.pp
  File "test.ml", line 8, characters 15-28:
  8 | type t = { v : F(X).G(Int).t }
                     ^^^^^^^^^^^^^
  Error: Modules do not match: sig end is not included in sig type t end
       The type t is required but not provided
       File "test.ml", line 1, characters 18-24: Expected declaration
  [2]

Note that we have lost information in this migration. In the future when we
bump the internal AST we should be able to update these tests (removing this
comment) to have more precise error locations.
