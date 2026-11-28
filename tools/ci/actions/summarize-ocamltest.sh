#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Summarize the ocamltest suite of a build.yml job: the totals, the failed
# tests, and the commands that run the job's configuration locally. The
# summary is printed and, in a workflow, added to the job's summary page.
#
# Usage: summarize-ocamltest.sh [LOG]
#   LOG defaults to _runtest/testsuite/_log.
#
# Environment (all optional):
#   JOB_NAME          the job's display name
#   TEST_OUTCOME      how the step that ran the suite ended, "success" or
#                     "failure": a step that failed without a failed test
#                     in its log did not write the whole log
#   SYSDEPS           the packages that the job installed first
#   MATRIX_CC         the C compiler that configure was given through CC
#   C_INCLUDE_PATH    as the job's steps have it, configure included
#   DISABLE_TESTCASES the tests that the job deleted before configuring
#   BUILD_OCAMLPARAM, OCAMLPARAM, OCAMLRUNPARAM, USE_RUNTIME, CODEGEN_VARIANT
#                     as given to the job's steps
#   COLLECT_METRICS   if "true", the job ran make through run-make-target.sh
#                     with this variable, which profiles the compilations
#   SHARED_BUILD      if "true", the job tests a build that another job made
#                     without any of these variables: they select the
#                     configuration when the tests run. Otherwise the job
#                     builds, and its build runs under them as well.

set -euo pipefail
usage() { echo "Usage: $0 [LOG]" >&2; exit 2; }
[ $# -le 1 ] || usage
log=${1:-_runtest/testsuite/_log}
max_listed=20

# Quote $1 for a shell unless it plainly needs no quoting.
quote() {
  case $1 in
    '' | *[!A-Za-z0-9_,.=:+/@%-]*) printf "'%s'" "${1//\'/\'\\\'\'}" ;;
    *) printf '%s' "$1" ;;
  esac
}

# The job's variables that are set, as VARIABLE=value words.
settings=''
for variable in BUILD_OCAMLPARAM OCAMLPARAM CODEGEN_VARIANT USE_RUNTIME \
    OCAMLRUNPARAM; do
  if [ -n "${!variable:-}" ]; then
    settings+=" $variable=$(quote "${!variable}")"
  fi
done
make_command='make'
if [ "${SHARED_BUILD:-}" = true ]; then
  # Arguments of the make commands that run tests.
  make_variables=$settings
  exports=''
else
  # Exported before the build, as in the job.
  make_variables=''
  if [ "${COLLECT_METRICS:-}" = true ]; then
    # run-make-target.sh derives the profiling settings of each make
    # command from these two variables, here as in the job.
    # shellcheck disable=SC2016 # the reader's shell expands $PWD
    settings+=' COLLECT_METRICS=true GITHUB_WORKSPACE="$PWD"'
    make_command='bash tools/ci/actions/run-make-target.sh'
  fi
  exports=${settings# }
fi

# The lines of standard input as a list under the title $1, cut after
# $max_listed. Prints nothing without input.
listing() {
  awk -v title="$1" -v max="$max_listed" '
    NR == 1 { printf "%s:\n\n", title }
    NR <= max { printf "- `%s`\n", $0 }
    END {
      if (NR > max) printf "- and %d more: see the log\n", NR - max
      if (NR > 0) print ""
    }'
}

first_failed=''

results() {
  local report status=0 totals failed count slowest
  if [ ! -f "$log" ]; then
    echo 'The ocamltest suite did not run.'
    echo
    return
  fi
  report=$(awk -f testsuite/summarize.awk < "$log") || status=$?
  total() { sed -n "s/^ *\([0-9][0-9]*\) $1\$/\1/p" <<< "$report"; }
  if [ -z "$(total 'tests passed')" ]; then
    echo 'The ocamltest suite did not run to completion' \
      "(the report of its log ended with status $status)."
    echo
    return
  fi
  totals="$(total 'tests failed') failed,"
  totals+=" $(total 'unexpected errors') unexpected errors,"
  totals+=" $(total 'tests passed') passed, $(total 'tests skipped') skipped"
  if [ "$(total 'tests considered.*')" = 0 ]; then
    echo 'The ocamltest suite ran no test: its log has no result.'
  elif [ "${TEST_OUTCOME:-}" = failure ] && [ "$status" -eq 0 ]; then
    echo 'The log of the ocamltest suite is incomplete: the step that ran' \
      'the suite failed, and no test in its log did. See the output of' \
      "that step. The log has these results: $totals."
  else
    echo "ocamltest: $totals."
  fi
  echo
  failed=$(
    awk -f testsuite/summarize.awk -v only_report_failed_tests=1 < "$log" |
      sed 's|^|testsuite/|' | sort)
  if [ -n "$failed" ]; then
    listing 'Failed tests' <<< "$failed"
    first_failed=${failed%%$'\n'*}
    count=$(grep -c '' <<< "$failed")
    if [ "${GITHUB_ACTIONS:-}" = true ]; then
      echo "::error title=${JOB_NAME:-ocamltest}::ocamltest failures: $count," \
        "starting with $first_failed" >&2
    fi
  fi
  sed -n '/^List of unexpected errors:$/,/^$/s|^    |testsuite/|p' \
    <<< "$report" | listing 'Unexpected errors'
  # Present only when the tests ran with SHOW_TIMINGS.
  slowest=$(awk -v quote="'" '
    /^Running tests from / { split($0, quoted, quote); dir = quoted[2] }
    match($0, /\(wall clock: [0-9.]+s\)/) {
      split($0, quoted, quote)
      printf "%8.2f s  testsuite/%s/%s\n",
        substr($0, RSTART + 13, RLENGTH - 15), dir, quoted[2]
    }' "$log" | sort -rn | awk 'NR <= 5')
  if [ -n "$slowest" ]; then
    echo 'Slowest tests:'
    echo
    echo '```'
    echo "$slowest"
    echo '```'
    echo
  fi
}

# shellcheck disable=SC2016 # the reader's shell expands $PWD
reproduction() {
  local arguments packages pwd_variable='$PWD'
  [ -f Makefile.config ] || return 0
  arguments=$(sed -n 's/^CONFIGURE_ARGS=//p' Makefile.config)
  read -ra packages <<< "${SYSDEPS:-}"
  if [ ${#packages[@]} -gt 0 ]; then
    echo "The job first installed these packages: \`${packages[*]}\`."
    echo
  fi
  echo 'To run this configuration locally (see "Reproducing a CI' \
    'configuration locally" in HACKING.md):'
  echo
  echo '```'
  if [ -n "${DISABLE_TESTCASES:-}" ]; then
    echo "rm -rf $DISABLE_TESTCASES"
  fi
  if [ -n "${C_INCLUDE_PATH:-}" ]; then
    # HACKING.md says which step of the job fills these directories.
    echo "export C_INCLUDE_PATH=\"${C_INCLUDE_PATH//"$PWD"/$pwd_variable}\""
  fi
  echo autoconf
  echo "${MATRIX_CC:+CC=$(quote "$MATRIX_CC") }./configure" \
    "${arguments//"$PWD"/$pwd_variable}"
  if [ -n "$exports" ]; then
    echo "export $exports"
  fi
  if [ "${SHARED_BUILD:-}" != true ]; then
    echo "$make_command compiler"
    echo "$make_command runtest"
  fi
  echo "$make_command test$make_variables"
  echo '```'
  if [ -n "$first_failed" ]; then
    echo
    if [ -n "$exports" ]; then
      echo 'Then, to run one test again, with the same variables exported:'
    else
      echo 'Then, to run one test again:'
    fi
    echo
    echo '```'
    echo "$make_command test-one TEST=$first_failed$make_variables"
    echo '```'
  fi
}

summary() {
  echo "### ${JOB_NAME:-ocamltest}"
  echo
  results
  reproduction
}

summary | tee -a "${GITHUB_STEP_SUMMARY:-/dev/null}"
