OCaml 5.6 Migrations
--------------------

1. Primitives

5.6 changed how primitives and `val` are represented. It also introduced primitive aliases but those are tested as part of the encoding tests.

We must make sure that primitives and val are correctly migrated down to 5.6-
primitives and that they correctly round-trip back to 5.6.

  $ cat > test.mli << EOF
  > val x : int -> int
  > external y : int -> int = "some_c_fun"
  > EOF

  $ cat > test.ml << EOF
  > val x : int -> int
  > external y : int -> int = "some_c_fun"
  > EOF

Those should be correctly represented in 5.6- :
(This command won't show anything interesting once our AST is >= 5.6)

  $ ./driver.exe test.mli
  val x : int -> int
  external y : int -> int = "some_c_fun"
  $ ./driver.exe test.ml
  external x : int -> int
  external y : int -> int = "some_c_fun"

And round-trip to 5.6 correctly:

  $ ./driver.exe test.mli --use-compiler-pp
  val x : int -> int
  external y : int -> int = "some_c_fun"
  $ ./driver.exe test.ml --use-compiler-pp
  val x : int -> int
  external y : int -> int = "some_c_fun"

2. Holes

It is now possible to place "holes" in your program anywhere an expression or a
module would appear.

  $ cat > test.ml << EOF
  > let x = 1 + _
  > module M = _
  > EOF

  $ ./driver.exe test.ml
  let x = 1 + ([%ppxlib.migration.pexp_hole ])
  module M = [%ppxlib.migration.pmod_hole ]
  $ ./driver.exe --use-compiler-pp test.ml
  let x = 1 + _
  module M = _
