Get a list of all flags that ocaml understands.
  $ OCAMLOPT="$MERLIN_TEST_OCAML_PATH/bin/ocamlopt.opt"
  $ "$OCAMLOPT" --help | grep -oP '(?<=  )-[a-zA-Z\-._0-9]+(?= )' > ocamlopt-flags.txt

  $ OCAMLC="$MERLIN_TEST_OCAML_PATH/bin/ocamlc"
  $ "$OCAMLC" --help | grep -oP '(?<=  )-[a-zA-Z\-._0-9]+(?= )' > ocamlc-flags.txt

  $ OCAML_FLAGS=$(sort -u ocamlopt-flags.txt ocamlc-flags.txt)

Perform a sanity check that the flags are parsed correctly.
  $ for file in "ocamlopt-flags.txt" "ocamlc-flags.txt"; do
  >   for flag in "-I" "-H"; do
  >     if ! cat "$file" | grep -qx -- "$flag"; then
  >       echo "Expected flag missing from parsed help text: $flag"
  >     fi
  >   done
  > done

Verify that each flag is known to Merlin. Most flags appear literally in
mconfig.ml; generated experimental feature switches are checked by parsing them.
If a new flag appears in this list, Merlin should handle it or explicitly ignore it.

  $ is_intentionally_unhandled () {
  >   # These flags are unhandled by Merlin and we really do want to raise an error if
  >   # they are passed.
  >   case "$1" in
  >     "-args" | "-args0" | "-depend")
  >       return 0
  >       ;;
  >     *)
  >       return 1
  >       ;;
  >   esac
  > }

  $ is_known_flag () {
  >   case "$1" in
  >     -experimental-universe)
  >       grep -q "\"$1\"" "$mconfig"
  >       ;;
  >     -experimental-* | -no-experimental-*)
  >       check='.class == "return" and .value.merlin.failures == [] and .value.ocaml.principal'
  >       $MERLIN single dump-configuration -filename flag-check.ml "$1" -principal \
  >         < /dev/null | jq -e "$check" > /dev/null &&
  >       $MERLIN single dump-configuration -filename flag-check.ml -flags "$1 -principal" \
  >         < /dev/null | jq -e "$check" > /dev/null
  >       ;;
  >     *)
  >       grep -q "\"$1\"" "$mconfig"
  >       ;;
  >   esac
  > }

  $ mconfig="../../src/kernel/mconfig.ml"
  > echo "$OCAML_FLAGS" | {
  >   has_unhandled_flags=0
  >   while IFS= read -r flag; do
  >     if ! is_known_flag "$flag" && ! is_intentionally_unhandled "$flag"; then
  >       echo "Unhandled flag: $flag"
  >       has_unhandled_flags=1
  >     fi
  >   done
  >   if [ "$has_unhandled_flags" -eq 1 ]; then
  >     cat <<EOF
  > A flag has been added to the compiler, and Merlin must know how to handle it. If the
  > flag is relevant to Merlin, Merlin should be updated to parse and use it. If not
  > (which is the usual case, especially for backend flags), Merlin needs to be told to
  > ignore the flag. Do this by adding it to either \`ocaml_ignored_flags\` or
  > \`ocaml_ignored_parametrized_flags\` in src/kernel/mconfig.ml, depending on whether
  > the flag takes a parameter. Experimental feature switches are generated from the
  > compiler's registry instead of being listed individually.
  > EOF
  >   fi
  > }

The universe selector accepts both argument forms. These flags must also work
when passed through the configuration's FLG directive.

  $ for flags in '-experimental-universe ai-reviewed -test-optimizations' '-experimental-universe=human-reviewed'; do
  >   printf 'FLG %s\n' "$flags" > .merlin
  >   $MERLIN single check-configuration -filename flag-check.ml < /dev/null \
  >     | jq '.value.failures'
  > done
  []
  []
  $ : > .merlin

Do not silently ignore unknown experimental flags. The old optimizations name
is reserved, so neither switch can be registered as a feature.

  $ for flag in -experimental-optimizations -no-experimental-optimizations; do
  >   $MERLIN single check-configuration -filename flag-check.ml "$flag" \
  >     < /dev/null | jq '.value.failures'
  > done
  [
    "unknown flag -experimental-optimizations"
  ]
  [
    "unknown flag -no-experimental-optimizations"
  ]
