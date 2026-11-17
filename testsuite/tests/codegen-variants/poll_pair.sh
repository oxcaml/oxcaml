#!/bin/sh

# The two library trees that the tests of poll insertion set against one
# another: the tree of the default variant of the installation under test,
# which is the library root, and the tree of the variant that differs from
# it in poll insertion only.
#
# usage: . poll_pair.sh
# Sets ocamlopt (the compiler, for the default variant whatever the
# testsuite is running under), stdlib (the library root), default and other
# (the names of the two variants), other_flag (the flag for the setting of
# the other one), and with_polls and without_polls (the two trees, by what
# their code has).
#
# usage: sh poll_pair.sh available
# For config_script: skips the test when the installation has no tree for
# the other variant, whatever other variants it has.

unset OCAMLPARAM
ocamlopt="$ocamlrun $ocamlopt_byte"
stdlib=$ocamlsrcdir/stdlib

config () {
  $ocamlopt -config-var "$1"
}
default=
[ "$(config with_address_sanitizer)" = true ] && default=$default-asan
[ "$(config with_frame_pointers)" = true ] && default=$default-fp
[ "$(config poll_insertion)" = true ] && default=$default-poll
[ "$(config no_stack_checks)" = false ] && default=$default-sc
default=${default#-}
default=${default:-base}
if [ "$(config poll_insertion)" = true ]; then
  other=$(echo "-$default-" | sed -e 's/-poll-/-/' -e 's/^-//' -e 's/-$//')
  other=${other:-base}
  other_flag=-disable-poll-insertion
  with_polls=$stdlib
  without_polls=$stdlib/variants/$other
else
  other=$(echo "$default-poll" | tr '-' '\n' | grep -v '^base$' | sort \
    | tr '\n' '-' | sed -e 's/-$//')
  other_flag=-enable-poll-insertion
  with_polls=$stdlib/variants/$other
  without_polls=$stdlib
fi

if [ "$1" = available ]; then
  if [ -d "$stdlib/variants/$other" ]; then
    exit "${TEST_PASS:-0}"
  fi
  echo "no library tree for $other, the variant with the other" \
    "poll-insertion setting" > "$ocamltest_response"
  exit "${TEST_SKIP:-125}"
fi
