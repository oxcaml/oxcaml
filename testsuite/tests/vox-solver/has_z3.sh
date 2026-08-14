#!/bin/sh
# Gate for tests that need a real z3: exit 125 tells ocamltest to skip.
# VOX_TEST_Z3 overrides; otherwise take z3 from PATH, or the pinned office
# install that the vox2 corpus was tuned against (z3 4.8.5).
if [ -n "$VOX_TEST_Z3" ]; then exit 0; fi
if command -v z3 > /dev/null 2>&1; then exit 0; fi
if [ -x /j/office/app/z3/prod/4.8.5/install/bin/z3 ]; then exit 0; fi
exit 125
