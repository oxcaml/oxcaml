#!/bin/sh

# The two library trees that the tests of poll insertion set against one
# another: the library root of the installation under test, which holds
# base, and the tree of poll, the variant that differs from base in poll
# insertion only.
#
# usage: . poll_pair.sh
# Sets ocamlopt (the compiler, for base whatever the testsuite is running
# under), stdlib (the library root), other (the name of the other variant),
# other_flag (the flag for its setting), and with_polls and without_polls
# (the two trees, by what their code has).
#
# usage: sh poll_pair.sh available
# For config_script: skips the test when the installation has no tree for
# the other variant, whatever other variants it has.

unset OCAMLPARAM
ocamlopt="$ocamlrun $ocamlopt_byte"
stdlib=$ocamlsrcdir/stdlib
other=poll
other_flag=-enable-poll-insertion
with_polls=$stdlib/variants/$other
without_polls=$stdlib

if [ "$1" = available ]; then
  if [ -d "$stdlib/variants/$other" ]; then
    exit "${TEST_PASS:-0}"
  fi
  echo "no library tree for $other, the variant with the other" \
    "poll-insertion setting" > "$ocamltest_response"
  exit "${TEST_SKIP:-125}"
fi
