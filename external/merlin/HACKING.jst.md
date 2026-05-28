
# Running tests

To run the tests, first set the `MERLIN_TEST_OCAML_PATH` to point to the install
directory for your flambda-backend build.  Then, `dune test` should work.  E.g.,:

```
MERLIN_TEST_OCAML_PATH=/path/to/installed/flambda-backend/compiler/ dune test
```

Note that we are currently only running a subset of the tests.  Tests are
disabled either because:
  * they invoke the compiler through dune and it will take some effort to
    get that working with the flambda-backend compiler.
  * they are irrelevant to us (and difficult to fix).
See the comments in various dune files under `tests/` for the specific reasons
we have disabled specific tests for.

# Writing tests

Merlin's tests are cram tests. To add a new test, do either of the following:
  * Add a cram test in a file `tests/test-dirs/$MY_TEST_NAME.t`
  * Add a cram test in a file `tests/test-dirs/$MY_TEST_NAME.t/run.t`. (Do this if you'd
    like to e.g. add `.ml` files specific to the test in the same directory.)

This naming scheme is enough for `dune test` to pick up on the fact it should run the new
test. You can also run a specific test with the command:

```
dune build @tests/test-dirs/$MY_TEST_NAME
```

Note the lack of `.t` in the command invocation.

See `tests/test-dirs/cms.t/run.t` for an example of a test that we added.
