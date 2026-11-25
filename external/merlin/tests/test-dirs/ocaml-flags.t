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

Verify that each ordinary flag is known to Merlin. Experimental feature switches
and universe selection are covered by the opt-in experimental-flags.t test.
If a new ordinary flag appears in this list, Merlin should handle it or explicitly
ignore it.

  $ is_excluded () {
  >   case "$1" in
  >     "-args" | "-args0" | "-depend")
  >       # These flags should be rejected by Merlin.
  >       return 0
  >       ;;
  >     -experimental-* | -no-experimental-*)
  >       # Checked only by the opt-in experimental-flags.t test.
  >       return 0
  >       ;;
  >     *)
  >       return 1
  >       ;;
  >   esac
  > }

  $ mconfig="../../src/kernel/mconfig.ml"
  > echo "$OCAML_FLAGS" | {
  >   has_unhandled_flags=0
  >   while IFS= read -r flag; do
  >     if ! is_excluded "$flag" && ! grep -q "\"$flag\"" "$mconfig"; then
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
