#!/bin/sh

# usage: sh archive_member_path.sh <variant>
#
# Checks that archive.cmxa and archive.a hold the unit as it was compiled
# for <variant>. The unit info has the fingerprint of the variant, and polls
# if the variant has them. The object file in the native archive says the
# same in the symbols that it refers to: the link-time guard of the
# variant's settings, and the symbol that only an object file without polls
# refers to.

variant=$1

# usage: has <token>
has () {
  case -$variant- in
    *-$1-*) echo true;;
    *) echo false;;
  esac
}

expected="Codegen fingerprint: frame_pointers=$(has fp),\
 stack_checks=$(has sc), address_sanitizer=$(has asan)
Polled: $(has poll)"
actual=$($ocamlrun $ocamlsrcdir/tools/ocamlobjinfo archive.cmxa \
  | grep -e '^Codegen fingerprint: ' -e '^Polled: ' | sort -u)

# The guard is named after the variant without its poll token.
guard=$(echo "$variant" | sed -e 's/^poll$/base/' -e 's/^poll-//' \
  -e 's/-poll$//' -e 's/-poll-/-/' -e 's/-/_/g')
expected_symbols=caml_runtime_codegen_variant_is_$guard
if [ "$(has poll)" = false ]; then
  expected_symbols="caml_code_not_fully_polled
$expected_symbols"
fi
actual_symbols=$(nm -u archive.a 2>&1 | awk '{ print $NF }' | sed 's/^_//' \
  | grep -e '^caml_runtime_codegen_variant_is_' \
      -e '^caml_code_not_fully_polled$' | sort -u)

if [ "$actual" = "$expected" ] && [ "$actual_symbols" = "$expected_symbols" ]
then
  echo "the unit of the variant's tree"
else
  echo "expected:"
  echo "$expected"
  echo "$expected_symbols"
  echo "in the archive:"
  echo "$actual"
  echo "$actual_symbols"
fi
