When the ppxlib driver reports an error by itself, source quotation should work
properly.

We start off by explicitly setting the error reporting style to contextual to
ensure source quotation is enabled:

  $ export OCAML_ERROR_STYLE=contextual

Here we have a driver compiled with a single rule that will raise a located
exception for every "[%raise]" extension point.

We need an input file:

  $ cat > file.ml << EOF
  > let x = [%raise]
  > EOF

When running the driver on this file, it should report the error and show
the relevant quoted source:

  $ ./raising_driver.exe -impl file.ml
  File "file.ml", line 1, characters 8-16:
  1 | let x = [%raise]
              ^^^^^^^^
  Error: An exception, raise be!
  [1]

This should also work when the input is a binary AST as the file contains the name
of the original source file. Our driver should be able to properly set the input
lexbuf and get the source quotation to work, assuming the information in the binary
AST file is correct.

Here we use an identity driver to generate the binary AST for our .ml file above:

  $ ./identity_driver.exe -impl file.ml -dump-ast -o file.pp.ml 

We then call our raising driver on the binary AST, it should be able to report the
error with source quotation:

  $ ./raising_driver.exe -impl file.pp.ml
  File "file.ml", line 1, characters 8-16:
  1 | let x = [%raise]
              ^^^^^^^^
  Error: An exception, raise be!
  [1]
