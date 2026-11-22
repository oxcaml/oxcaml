#!/bin/sh

# Helper for test_lists.ml: runs targets of the testsuite's Makefile, most
# of them on lists of tests. A list that is not well formed must fail
# before any test runs.

testsuite="$1"
here=$(pwd)
id="test-lists-$$"

# The options and the variables of the make that runs this test are not
# for the ones started here.
unset MAKEFLAGS MFLAGS MAKELEVEL DIR TEST LIST FILE PROMOTE

run_make () {
  ${MAKE:-make} -C "$testsuite" --no-print-directory \
    TEST_ID="$id" TESTLOG="$here/log" "$@" > output 2>&1
}

# must_fail <text> <make arguments>: the make fails, and says <text>
must_fail () {
  text="$1"
  shift
  if run_make "$@"; then
    echo "make $* succeeded:"
    cat output
    exit 1
  fi
  if ! grep -q -F "$text" output; then
    echo "make $* failed without saying '$text':"
    cat output
    exit 1
  fi
}

# must_pass <make arguments>: the make succeeds
must_pass () {
  if ! run_make "$@"; then
    echo "make $* failed:"
    cat output
    exit 1
  fi
}

# A directory of tests for the lists below: it has three tests, a directory
# with one more test and a directory without tests. ocamltest does not look
# into it when it searches the directory above, because of its name. It
# goes away with this script, and so does the directory that the makes
# started here build tests in, which is named after their TEST_ID.
tests="tests/tool-ocamltest/_test-lists-$$"
trap 'rm -rf "$testsuite/$tests" "$testsuite/_ocamltest.$id"' EXIT
mkdir -p "$testsuite/$tests/empty" "$testsuite/$tests/inner"
# This one starts the testsuite's makefile on a test, as this script does,
# with a log of its own: it must not write into the log of the run that it
# is part of. Nor may its make take the codegen variant of that run for its
# own: the variables that select it are not in the environment of a test.
cat > "$testsuite/$tests/runs_one.ml" <<'TEST'
(* TEST
 script = "sh ${test_source_directory}/runs_one.sh ${ocamlsrcdir}/testsuite";
 script;
*)
TEST
cat > "$testsuite/$tests/runs_one.sh" <<'SCRIPT'
if [ -n "${CODEGEN_VARIANT+set}${VARIANT_ONLY+set}" ]; then
  echo "in the environment of a test:" \
    "CODEGEN_VARIANT=${CODEGEN_VARIANT-} VARIANT_ONLY=${VARIANT_ONLY-}"
  exit 1
fi
unset MAKEFLAGS MFLAGS MAKELEVEL DIR TEST LIST FILE PROMOTE
here=$(pwd)
tests=${test_source_directory#"$1"/}
trap 'rm -rf "$1/_ocamltest.runs-one-$$"' EXIT
${MAKE:-make} -C "$1" --no-print-directory one \
  TEST="$tests/inner/also_passes.ml" TEST_ID="runs-one-$$" \
  TESTLOG="$here/log" > output 2>&1
SCRIPT
cat > "$testsuite/$tests/inner/also_passes.ml" <<'TEST'
(* TEST
 script = "true";
 script;
*)
TEST
cat > "$testsuite/$tests/passes.ml" <<'TEST'
(* TEST
 script = "true";
 script;
*)
TEST
# This one starts a make, whose recipe must see the OCAMLPARAM of the test.
cat > "$testsuite/$tests/starts_make.ml" <<'TEST'
(* TEST
 set OCAMLPARAM = "_,from-the-test=1";
 script = "${MAKE} -f ${test_source_directory}/starts_make.mk";
 script;
*)
TEST
cat > "$testsuite/$tests/starts_make.mk" <<'MAKEFILE'
check:
	@case "$$OCAMLPARAM" in \
	   *from-the-test*) ;; \
	   *) echo "OCAMLPARAM=$$OCAMLPARAM in the recipe"; exit 1;; \
	 esac
MAKEFILE

list_targets=list
if echo | parallel --gnu --no-notice > /dev/null 2>&1; then
  list_targets="list list-parallel"
fi

# refused <text> <line of a list>...: the list is refused, with <text>
refused () {
  text="$1"
  shift
  printf '%s\n' "$@" > refused.list
  for target in $list_targets; do
    must_fail "$text" "$target" FILE="$here/refused.list"
  done
}

for target in $list_targets; do
  must_fail "does not exist" "$target" FILE="$here/no-such-list"
done
refused "names no directory"
refused "names no directory" "   " "  "
refused "is not a directory" "$tests" "tests/no-such-directory"
refused "is not of the form" "$tests" "lib"
refused "is not of the form" "tests"
refused "is not of the form" "."
refused "is not of the form" "/usr"
refused "is not of the form" "./"
refused "is not of the form" "tests/../$tests"
refused "is not of the form" "$tests $tests/inner"
refused "is not of the form" "$tests	$tests/inner"
refused "is not a directory" "tests/*"
refused "2 times" "$tests" "./$tests/"
refused "is inside" "$tests" "$tests/empty"
refused "has no test" "$tests/empty"

# A directory without tests is not a successful run of no test either.
must_fail "No test ran" one DIR="$tests/empty"

# A list as install_for_test writes them is accepted, with blanks before
# the names, and so is a name as the completion of a shell leaves it. Its
# run is a whole run, with and without GNU parallel: 'report' and
# 'promote-failed' find its log.
echo "  ./$tests/" > accepted.list
for target in $list_targets; do
  rm -f log log.one
  must_pass "$target" FILE="$here/accepted.list"
  if ! grep -q "4 tests passed" output; then
    echo "make $target did not run the tests of the list:"
    cat output
    exit 1
  fi
  must_pass report
  must_pass promote-failed
done

# An ocamltest that fails where it is asked for the tests of a list has
# said why. The list is not taken for one whose directories have no test.
cat > no-ocamltest.sh <<'SCRIPT'
echo "ocamltest does not start" >&2
exit 2
SCRIPT
for target in $list_targets; do
  must_fail "ocamltest does not start" "$target" \
    FILE="$here/accepted.list" ocamltest="sh $here/no-ocamltest.sh"
  if grep -q "has no test" output; then
    echo "make $target takes an ocamltest that fails for a list without tests:"
    cat output
    exit 1
  fi
done

# The list of all the tests of a directory is made of the directories
# under it that have tests. A directory without tests is left out of it,
# as one whose tests were all deleted is left out of the list of the
# whole suite, and the list is one that the targets accept.
if ! ${MAKE:-make} -C "$testsuite" -s --no-print-directory \
       test-dirs DIR="$tests" > generated.list 2> output
then
  echo "make test-dirs DIR=$tests failed:"
  cat output
  exit 1
fi
if [ "$(cat generated.list)" != "$tests/inner" ]; then
  echo "make test-dirs DIR=$tests names other directories than $tests/inner:"
  cat generated.list
  exit 1
fi
# The directory may be given as the completion of a shell leaves it.
if ! ${MAKE:-make} -C "$testsuite" -s --no-print-directory \
       test-dirs DIR="./$tests//" > slashes.list 2> output \
   || ! cmp -s generated.list slashes.list
then
  echo "make test-dirs DIR=./$tests// does not give the list of DIR=$tests:"
  cat output slashes.list
  exit 1
fi
for target in $list_targets; do
  must_pass "$target" FILE="$here/generated.list"
  if ! grep -q "1 tests passed" output; then
    echo "make $target did not run the test of the generated list:"
    cat output
    exit 1
  fi
done

# The variables that select a codegen variant for a run are for that run:
# the test that starts a make does not find them in its environment. The
# variant named here is the default one, which every tree can test.
default=$(sed -n 's/^CODEGEN_DEFAULT_VARIANT=//p' \
            "$testsuite/../Makefile.config" 2> /dev/null)
must_pass one TEST="$tests/runs_one.ml" VARIANT_ONLY=1 \
  ${default:+CODEGEN_VARIANT="$default"}

# A variable given on the command line of the testsuite's make does not
# replace, in a make that a test starts, the value that the test has for
# it.
must_pass one TEST="$tests/starts_make.ml" \
  OCAMLPARAM=_,from-the-command-line=1

# Here it is ocamltest that fails, on the directory that it is asked to
# search.
must_fail "Unexpected error in the test infrastructure" all-no-such-directory

# The passes of the test-variants target of the tree that this test tree
# is in. With no variant to test, the target fails: it would run no pass
# and build something else instead. The passes are phony targets: a file
# named like one of them would otherwise stand for a pass that ran.
# Nothing is run here (-n).
root="$testsuite/../.."
# The makefile of the tree has "/usr/bin/env bash" for its shell. A build
# of a tree where that does not exist names another one on the command
# line of its make, which the makes that a test starts do not see: they
# are given the shell here.
shell=$(command -v bash) || shell=/bin/sh
root_make () {
  ${MAKE:-make} -C "$root" SHELL="$shell" "$@"
}
if [ -f "$root/Makefile.common-ox" ]; then
  if root_make -n test-variants TEST_VARIANTS= > output 2>&1
  then
    echo "make test-variants TEST_VARIANTS= succeeded:"
    cat output
    exit 1
  fi
  if ! grep -q -F "TEST_VARIANTS names no codegen variant" output; then
    echo "make test-variants TEST_VARIANTS= failed for another reason:"
    cat output
    exit 1
  fi
  if ! root_make -n -p check-codegen-variant 2> /dev/null \
       | grep '^\.PHONY:' | grep -q ' test-variant-'
  then
    echo "the passes of make test-variants are not phony targets"
    exit 1
  fi
  # What the target ends with, after its passes: it shows the end of the
  # output of each pass, and fails if the passes did, if a pass did not
  # run or if a pass has no summary, whatever the passes returned.
  # report <exit status of the passes> <variant>...
  report () {
    status="$1"
    shift
    root_make -s --no-print-directory test-variants-report \
      test_variants_status="$status" TEST_VARIANTS="$*" \
      test_variants_out="$here/pass outputs" > output 2>&1
  }
  # report_fails <text> <exit status of the passes> <variant>...
  report_fails () {
    text="$1"
    shift
    if report "$@"; then
      echo "make test-variants succeeds (passes: exit $1, of $2 $3):"
      cat output
      exit 1
    fi
    if ! grep -q -F "$text" output; then
      echo "make test-variants fails without saying '$text':"
      cat output
      exit 1
    fi
  }
  mkdir "pass outputs"
  printf 'Running tests\n\nSummary:\n     1 tests passed\n' \
    > "pass outputs/test-variants.complete.out"
  : > "pass outputs/test-variants.empty.out"
  if ! report 0 complete || ! grep -q "1 tests passed" output; then
    echo "make test-variants does not show the summary of a pass:"
    cat output
    exit 1
  fi
  report_fails "1 tests passed" 2 complete
  report_fails "The pass has no summary" 0 complete empty
  report_fails "The pass did not run" 0 complete missing

  # The target itself, with what it does before its passes left out and
  # with passes of this script. It reports every pass after a pass that
  # fails, and then fails, also where the shell of its recipe stops at a
  # command that fails (-e). Each run of the target has a directory of its
  # own for the output of its passes, with a blank in its name, and each
  # pass says in its summary which run it is part of: what the target
  # shows is what its passes wrote this time.
  cat > pass.sh <<'SCRIPT'
printf 'Running tests\n\nSummary:\n     1 tests passed\n'
echo "pass of $1 in run $2"
case "$1" in failing) exit 1;; esac
SCRIPT
  runs=0
  # target <variant>...
  target () {
    runs=$((runs + 1))
    mkdir "passes $runs"
    root_make -s --no-print-directory SHELL="$shell -e" test-variants \
      TEST_VARIANTS="$*" check_codegen_variants= install_for_test_serially= \
      test_variant_pass="sh $here/pass.sh \$* $runs" \
      test_variants_out="$here/passes $runs" > output 2>&1
  }
  # reported <variant>...: the target shows the summary of these passes
  reported () {
    for variant do
      if ! grep -q "^pass of $variant in run $runs\$" output; then
        echo "make test-variants does not show this run's pass of $variant:"
        cat output
        exit 1
      fi
    done
  }
  if ! target complete; then
    echo "make test-variants fails with a pass that succeeds:"
    cat output
    exit 1
  fi
  reported complete
  if target failing complete; then
    echo "make test-variants succeeds with a pass that fails:"
    cat output
    exit 1
  fi
  reported failing complete
fi
