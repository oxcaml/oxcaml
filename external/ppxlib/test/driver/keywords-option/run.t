This test can only work with OCaml 5.3 or higher.

OCaml 5.3 introduced the new `effect` keyword. To allow old code to compile
under 5.3 it also introduced a `-keyword=version+list` CLI option, allowing one to
override the set of keywords.

The ppxlib driver also has such an option now to properly configure the lexer before
attempting to parse source code.

Let's consider the following source file:

  $ cat > test.ml << EOF
  > let effect = 1
  > EOF

If passed to the driver as is, it will trigger a parse error:

  $ ./driver.exe --impl test.ml -o ignore.ml
  File "test.ml", line 1, characters 4-10:
  1 | let effect = 1
          ^^^^^^
  Error: Syntax error
  [1]

Now, if we use the 5.2 set of keywords, it should happily handle the file:

  $ ./driver.exe --keywords 5.2 --impl test.ml -o ignore.ml

It can also be set using OCAMLPARAM:

  $ OCAMLPARAM=_,keywords=5.2 ./driver.exe --impl test.ml -o ignore.ml

The priority between the CLI option and OCAMLPARAM must be respected, therefore
both of the following invocation should parse:

  $ OCAMLPARAM=_,keywords=5.2 ./driver.exe --keywords 5.3 --impl test.ml -o ignore.ml

  $ OCAMLPARAM=keywords=5.3,_ ./driver.exe --keywords 5.2 --impl test.ml -o ignore.ml
