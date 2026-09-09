#!/bin/sh
# Check the final raw Flambda: fexpr cannot print deleted function declarations.
if awk '
  /^After reaper:/ { final = 1; next }
  !final { next }
  /\(c1\/[^ ]+ c1b\/[^)]+\) =/ { c1++ }
  /\(c2\/[^ ]+ c2b\/[^)]+\) =/ { c2++ }
  /\(specialisation_site\)/ { sites++ }
  /helper\/.*\[deleted\]/ { deleted++ }
  /apply_cont .* c1\// { use_c1++ }
  /apply_cont .* c2\// { use_c2++ }
  /Project_function_slot/ { projection++ }
  /Some h2\// { callee++ }
  END {
    if (final && c1 == 1 && c2 == 1 && sites == 2 && deleted == 2 &&
        use_c1 == 1 && use_c2 == 1 && projection == 1 && callee == 1)
      exit 0
    print "Expected two live sites with deleted first slots and a projected callee"
    exit 1
  }
' "$1"; then
  exit "${TEST_PASS}"
else
  exit "${TEST_FAIL}"
fi
