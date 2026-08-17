Get a list of all flags that ocaml understands.
  $ OCAMLOPT="$MERLIN_TEST_OCAML_PATH/bin/ocamlopt.opt"
  $ "$OCAMLOPT" --help | grep -oP '(?<=  )-[a-zA-Z\-_0-9]+(?= )' > ocamlopt-flags.txt

  $ OCAMLC="$MERLIN_TEST_OCAML_PATH/bin/ocamlc"
  $ "$OCAMLC" --help | grep -oP '(?<=  )-[a-zA-Z\-_0-9]+(?= )' > ocamlc-flags.txt

  $ OCAML_FLAGS=$(sort -u ocamlopt-flags.txt ocamlc-flags.txt)

Perform a sanity check that the flags are parsed correctly.
  $ for file in "ocamlopt-flags.txt" "ocamlc-flags.txt"; do
  >   for flag in "-I" "-H"; do
  >     if ! cat "$file" | grep -qx -- "$flag"; then
  >       echo "Expected flag missing from parsed help text: $flag"
  >     fi
  >   done
  > done

Verify that each flag appears in mconfig.ml. Each flag should either be handled or be in
the list of ignored flags. If a new flag appears in this list, you should either make
Merlin handle it or add it to the list of ignored flags.

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

  $ mconfig="../../src/kernel/mconfig.ml"
  > echo "$OCAML_FLAGS" | {
  >   has_unhandled_flags=0
  >   while IFS= read -r flag; do
  >     if ! grep -q "\"$flag\"" "$mconfig" && ! is_intentionally_unhandled "$flag"; then
  >       echo "Unhandled flag: $flag"
  >       has_unhandled_flags=1
  >     fi 
  >   done
  >   if [ "$has_unhandled_flags" -eq 1 ]; then
  >     cat <<EOF
  > A flag has been added to the compiler, and Merlin must known how to handle it. If the
  > flag is relevant to Merlin, Merlin should be updated to parse and use it. If not
  > (which is the usual case, especially for backend flags), Merlin needs to be told to
  > ignore the flag. Do this by adding it to either \`ocaml_ignored_flags\` or
  > \`ocaml_ignored_parametrized_flags\` in src/kernel/mconfig.ml, depending on whether
  > the flag takes a parameter.
  > EOF
  >   fi
  > }
