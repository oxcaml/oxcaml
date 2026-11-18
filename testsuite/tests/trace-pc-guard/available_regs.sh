#!/bin/sh

set -eu

dump=available_regs.cmx.dump
available_after=$1

fail () {
    echo "$1" > ${ocamltest_response}
    exit ${TEST_FAIL}
}

[ -e "$dump" ] || fail "not found $dump"

# Each instruction is preceded by its availability sets, when they are not
# empty: "AB={{...}},AA={{...}}" (before and across), or "AB=AA={{...}}" when
# they are equal, possibly over several lines. The set after a probe is the set
# before the instruction that follows it.
result=$(${AWK:-awk} -v expected_after="$available_after" '
  function has_x(set) { return set ~ /\(x\// ? "yes" : "no" }
  /^  AB/ { sets = $0; in_sets = 1; next }
  in_sets && /^   / { sets = sets $0; next }
  {
    in_sets = 0
    if (sets ~ /AB=AA=/) { before = sets; across = sets }
    else {
      i = index(sets, ",AA=")
      before = substr(sets, 1, i); across = substr(sets, i + 1)
    }
    sets = ""
    if (after_probe) {
      after_probe = 0
      if (has_x(before) != expected_after) {
        print "x available after a probe: " has_x(before); failed = 1
      }
    }
    if ($0 ~ /^  trace_pc_guard [0-9]/ && has_x(before) == "yes") {
      probes++
      after_probe = 1
      if (has_x(across) == "yes") {
        print "x available during a probe"; failed = 1
      }
    }
  }
  END {
    if (!failed && probes != 2) print "probes with x available: " probes + 0
  }' "$dump")

[ -z "$result" ] || fail "$result"

exit ${TEST_PASS}
