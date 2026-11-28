#!/bin/sh

# Helper for summarize.ml: summarizes the logs of runs that are not
# successes.

summarize_awk="$1"

summarize () {
  GITHUB_ACTIONS='' awk -f "$summarize_awk"
  echo "exit status: $?"
}

# ocamltest died on x.ml after its first test, which the Makefile reports
# with the "unexpected error" line.
summarize <<'LOG'
Running tests from 'tests/a' ...
 ... testing 'x.ml' with 1 (bytecode) => passed
 ... testing 'x.ml' => unexpected error
Running tests from 'tests/a' ...
 ... testing 'y.ml' with 1 (native) => passed
LOG

# The log has two results for x.ml: it failed, whatever came later.
summarize <<'LOG'
Running tests from 'tests/a' ...
 ... testing 'x.ml' => failed
Running tests from 'tests/a' ...
 ... testing 'x.ml' => passed
LOG

# No test ran.
summarize < /dev/null
