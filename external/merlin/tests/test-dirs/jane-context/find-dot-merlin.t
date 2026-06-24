This file tests that Merlin finds its configuration properly when 

  $ export MERLIN_JANE_CONTEXT=jane
  $ export DONT_MAKE_DUMMY_DOT_MERLIN=true

  $ print_config_info () {
  >   file="$1"
  > 
  >   dot_merlin="$($MERLIN single check-configuration -filename "$file" < "$file" | jq .value.dot_merlins[0])"
  >   echo "Config file: $dot_merlin"
  > 
  >   errors="$($MERLIN single check-configuration -filename "$file" < "$file" | jq .value.failures)"
  >   if [ ! -z "$errors" ] ; then
  >     echo "Errors: $errors"
  >   fi
  > 
  >  unit_name="$($MERLIN single dump-configuration -filename "$file" < "$file" | jq .value.merlin.unit_name)"
  >   echo "Unit name: $unit_name"
  > }

Setup a dune workspace where the build hasn't been run yet:
  $ touch dune-workspace
  $ mkdir src
  $ touch src/foo.ml

  $ print_config_info src/foo.ml
  Config file: "$TESTCASE_ROOT/dune-workspace"
  Errors: [
    "Could not find `.merlin` file to load project configuration. To get full Merlin support for this file, build the relevant target using Dune. This is usually the default target for the directory containing this file."
  ]
  Unit name: null

Simulate a build having been run:
  $ mkdir -p _build/default/src
  $ touch _build/default/src/foo.ml
  $ echo "UNIT_NAME Good" > _build/default/src/.merlin

  $ print_config_info src/foo.ml
  Config file: "$TESTCASE_ROOT/_build/default/src/.merlin"
  Errors: []
  Unit name: "Good"

Simulate a build having been run with the .merlin being promoted:
  $ echo "UNIT_NAME Good" > src/.merlin

  $ print_config_info src/foo.ml
  Config file: "$TESTCASE_ROOT/_build/default/src/.merlin"
  Errors: []
  Unit name: "Good"

The .merlin file in the source directory disagrees with the one in the build
directory. The build directory is preferred:
  $ echo "UNIT_NAME Bad" > src/.merlin

  $ print_config_info src/foo.ml
  Config file: "$TESTCASE_ROOT/_build/default/src/.merlin"
  Errors: []
  Unit name: "Good"

.merlin exists in the source directory but not the build directory:
  $ rm _build/default/src/.merlin
  $ echo "UNIT_NAME Good" > src/.merlin

  $ print_config_info src/foo.ml
  Config file: "$TESTCASE_ROOT/src/.merlin"
  Errors: []
  Unit name: "Good"
