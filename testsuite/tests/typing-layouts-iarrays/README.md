This directory has tests for iarrays (immutable arrays) of unboxed types. The tests assume the array
contains something that is like a number or a product of numbers.

Using the test framework here still involves a fair amount of copy and paste to
build your new test. This is mainly because we don't have layout polymorphism,
so it's not really possible to build it as one nice big functor. Hopefully we
can improve it in the future.

## Basic use

The files `gen_u_iarray.ml` and `test_gen_u_iarray.ml` contain the basic
framework. Rather than reading them, you are probably better off looking at an
example.  E.g., see `test_int64_u_iarray.ml`.

## Errors

The testing framework is not very helpful in the event of errors - you'll get an
assertion failure with an uninformative backtrace. One way to debug is to
copy the framework and your test file elsewhere, compile and run it as a normal
ocaml program, then comment out parts of the big test functor from
`test_gen_u_iarray.ml` until you locate the line causing the error.  This should
be improved.

## Unboxed products

The file `gen_product_iarray_helpers.ml` has additional infrastructure for
testing iarrays of unboxed products. To add a new test, copy one of the existing
ones (e.g., `test_ignorable_product_iarray_1.ml`) and follow the instructions
in its comments about which parts you need to edit.

Note that tests in this directory use `%makearray_dynamic` to create mutable arrays
which are then frozen to create iarrays.
