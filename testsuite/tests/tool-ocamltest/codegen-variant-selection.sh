#!/bin/sh

# Helper for codegen_variant_selection.ml: runs ocamltest on tests that only
# run a script, under values of codegen_variant that must make them fail, and
# checks what ocamltest says.

ocamltest="$1"

# fails <value of codegen_variant> <text>: the test fails, and ocamltest
# says <text>
fails () {
  cat > selection.ml <<TEST
(* TEST
 codegen_variant = "$1";
 script = "true";
 script;
*)
TEST
  if "$ocamltest" selection.ml > output 2>&1; then
    echo "codegen_variant = \"$1\": the test did not fail:"
    cat output
    exit 1
  fi
  if ! grep -q -F "$2" output; then
    echo "codegen_variant = \"$1\": the test failed without saying '$2':"
    cat output
    exit 1
  fi
}

# excludes <value of codegen_variant> <predicate>: the predicate does not
# hold for the variant that the value selects, whether or not the compiler
# under test has that variant: the test stops there, before a script that
# fails.
excludes () {
  cat > selection.ml <<TEST
(* TEST
 codegen_variant = "$1";
 $2;
 script = "false";
 script;
*)
TEST
  if ! "$ocamltest" selection.ml > output 2>&1 \
     || ! grep -q -F "is not satisfied" output
  then
    echo "codegen_variant = \"$1\": the predicate $2 is satisfied:"
    cat output
    exit 1
  fi
}

# A value that starts with a sign is requirements: signed axes, with or
# without blanks between them and in any order. These ask for frame
# pointers on and stack checks off.
for value in "+fp-sc" "+fp -sc" "-sc+fp"; do
  excludes "$value" no-frame_pointers
  excludes "$value" stack-checks
done

# A value that does not start with a sign is the name of a variant, hyphens
# included: this one has frame pointers and stack checks on.
excludes "fp-sc" no-frame_pointers
excludes "fp-sc" no-stack-checks

# These requirements ask for AddressSanitizer and stack checks, which no
# compiler has together, and for the two other axes off: the variant that
# the test fails for lacking shows how the value was read.
fails "+asan+sc-fp-poll" 'has no codegen variant asan-sc ('
fails "+asan +sc -fp -poll" 'has no codegen variant asan-sc ('
fails " -poll+sc -fp+asan" 'has no codegen variant asan-sc ('

# These start with a sign, and are not requirements.
fails "+fpp" '"fpp" is not an axis'
fails "+base" '"base" is the name of a variant, not an axis'
fails "+fp+fp" 'the axis fp is given twice'
fails "+fp-fp" 'the axis fp is given twice'
fails "+fp sc" '"sc" has no sign'
fails "+fp-" 'a sign is followed by the name of an axis'

# These do not start with a sign, and are not names: the error shows both
# forms where the value is one sign away from requirements.
fails "fp -sc" 'requirements start with a sign, blanks aside'
fails "fp -sc" '"+fp -sc" asks for fp on, sc off'
fails "fp -sc" '"fp-sc" is the name of the variant with these axes on'
fails "fp+sc" 'a name has no sign in front, no blank and no "+"'
fails "fp sc" 'requirements start with a sign, blanks aside'
