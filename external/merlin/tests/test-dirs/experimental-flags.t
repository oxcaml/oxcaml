Generated experimental feature switches must be accepted on the command line
and in configuration, without swallowing the following flag.

  $ OCAMLOPT="$MERLIN_TEST_OCAML_PATH/bin/ocamlopt.opt"
  $ "$OCAMLOPT" --help | grep -oP '(?<=  )-[a-zA-Z\-._0-9]+(?= )' > ocamlopt-flags.txt

  $ check_flag () {
  >   check='.class == "return" and .value.merlin.failures == [] and .value.ocaml.principal'
  >   $MERLIN single dump-configuration -filename flag-check.ml "$1" -principal \
  >     < /dev/null | jq -e "$check" > /dev/null &&
  >   $MERLIN single dump-configuration -filename flag-check.ml -flags "$1 -principal" \
  >     < /dev/null | jq -e "$check" > /dev/null
  > }
  $ while IFS= read -r flag; do
  >   case "$flag" in
  >     -experimental-universe) ;;
  >     -experimental-* | -no-experimental-*)
  >       check_flag "$flag" || echo "Unhandled flag: $flag"
  >       ;;
  >   esac
  > done < ocamlopt-flags.txt

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
