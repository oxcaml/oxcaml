The --use-compiler-pp flag can be used when using the driver's source code
output, either directly when generating a .corrected file or to force
printing the AST as source using the installed compiler's printer.

Our driver has a deriver and an extension that produces a pattern-matching with
named existentials.

This feature has been introduced in 4.13 so the syntax is unsupported before that.

If we run the driver in source output mode, without the `--use-compiler-pp` flag,
it will successfully print out the source using the 4.13 syntax. If we're running
on an older compiler, like we are for this test, that can be troublesome.

If instead we use the flag, this will force the migration thus causing an error as
named existentials can't be migrated down to 4.12.

Let's consider the following file:

  $ cat > test.ml << EOF
  > [%%named_existentials]
  > EOF

Running the driver will generate a function with a single pattern matching in it:

  $ ./driver.exe test.ml
  let f x = match x with | Constructor (type a) _ -> ()

Now if we run it with `--use-compiler-pp`, we should get the migration error:

  $ ./driver.exe --use-compiler-pp test.ml
  File "test.ml", line 1, characters 0-22:
  1 | [%%named_existentials]
      ^^^^^^^^^^^^^^^^^^^^^^
  Error: migration error: existentials in pattern-matching is not supported before OCaml 4.13
  [1]

This should also work for correction based code gen:

  $ cat > test_inline.ml << EOF
  > type t = int
  > [@@deriving_inline named_existentials]
  > [@@@end]
  > EOF

If we run the driver without `--use-compiler-pp`:

  $ ./driver.exe test_inline.ml -diff-cmd -
  type t = int[@@deriving_inline named_existentials]
  [@@@end ]
  $ cat test_inline.ml.ppx-corrected
  type t = int
  [@@deriving_inline named_existentials]
  let _ = fun (_ : t) -> ()
  let f x = match x with | Constructor (type a) _ -> ()
  let _ = f
  [@@@end]

and with the flag:

  $ ./driver.exe test_inline.ml -diff-cmd - --use-compiler-pp
  File "test_inline.ml", lines 1-2, characters 0-38:
  1 | type t = int
  2 | [@@deriving_inline named_existentials]
  Error: migration error: existentials in pattern-matching is not supported before OCaml 4.13
  [1]
