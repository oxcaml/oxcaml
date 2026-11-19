We should be able to migrate the effect syntax downward and back. This allows users
to run ppx-es on 5.3 on source files that use the effect syntax until we update our
internal AST to fully support it.

We have a custom driver that will force migration of the AST down to 5.2 and back to
the compiler's version and print it as source code using the compiler's printer,
regardless of ppxlib's internal AST version.

If we run the driver on the following source file:

  $ cat > test.ml << EOF
  > let handler f =
  > match f () with
  > | x -> x
  > | effect Random_bits, k -> Effect.Deep.continue k (Random.bits ())
  > EOF

it should successfully roundtrip to 5.2 and print the source code unchanged:

  $ ./driver.exe test.ml --use-compiler-pp
  let handler f =
    match f () with
    | x -> x
    | effect Random_bits,  k -> Effect.Deep.continue k (Random.bits ())
