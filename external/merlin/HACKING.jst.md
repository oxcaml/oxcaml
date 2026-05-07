This file provides instructions specifically for working on the OxCaml version of Merlin.

# Building

As a prerequisite, you need to configure before building. See the "Branching and configuring" section of `../../README.md` for more information.

Once you've done so, to build Merlin, run:
```
$ make build
```

Or from the root of the repository rather than the `external/merlin` directory:
```
$ make -C external/merlin build
```

`make build` configures `dune` and then calls `dune build`. To pass arbitrary arguments to `dune build`, use `make build ARGS="..."`. For example, to build Merlin in watch mode:
```
$ make build ARGS="-w"
```

# Running tests

To run the Merlin testsuite, run:
```
$ make test
```
This will first build an install of OxCaml and then run the Merlin testsuite against it. To override the OxCaml version, you can set the `MERLIN_TEST_OCAML_PATH` environment variable to point to an existing OxCaml install.

Similar to `make build`, `make test` allows you to forward arbitrary arguments to `dune build`. For example, to run a specific test:
```
$ make test ARGS="@tests/test-dirs/enclosing @tests/test-dirs/cms"
```

`make build` and `make test` are similar in that they both call `dune build`. There are two differences:
1. `make test` defaults to passing `@runtest` if there are no arguments provided.
2. `make test` will build the compiler before running the tests.

Note that we are currently only running a subset of the Merlin testsuite, as compared to upstream Merlin.  Tests are disabled either because:
  * they invoke the compiler through dune and it will take some effort to get that working with the OxCaml compiler.
  * they are irrelevant to us (and difficult to fix).
See the comments in various dune files under `tests/` for the specific reasons
we have disabled specific tests for.

# Writing tests

Merlin's tests are cram tests. To add a new test, do either of the following:
  * Add a cram test in a file `tests/test-dirs/$MY_TEST_NAME.t`
  * Add a cram test in a file `tests/test-dirs/$MY_TEST_NAME.t/run.t`. (Do this if you'd
    like to e.g. add `.ml` files specific to the test in the same directory.)

This naming scheme is enough for dune to pick up on the fact it should run the new test. You can also run a specific test with the command:

```
make test @tests/test-dirs/$MY_TEST_NAME
```

Note the lack of `.t` in the command invocation.

See `tests/test-dirs/cms.t/run.t` for an example of a test that we added.
