#!/bin/bash
# Compiles each source with and without -fdo-counters, with each set of flags,
# and prints whether the code is the same, or else how it differs: the counters
# must not change the generated code. They do take labels and identifiers, so the
# numbers of local labels and of symbols are left out of the comparison.
# usage: codegen.sh OCAMLOPT FLAGS... -- SOURCE...
set -eu
ocamlopt=$1
shift
configs=()
while [ "$1" != -- ]; do
  configs+=("$1")
  shift
done
shift
config=0
for flags in "${configs[@]}"; do
  config=$((config + 1))
  for source in "$@"; do
    name=$(basename "$source" .ml)
    for counters in without with; do
      dir=codegen/$config/$counters
      mkdir -p "$dir"
      extra=
      if [ "$counters" = with ]; then extra=-fdo-counters; fi
      # shellcheck disable=SC2086
      "$ocamlopt" -c $flags $extra -o "$dir/$name.cmx" "$source"
      (cd "$dir" && objdump -dr "$name.o") |
        sed -E 's/\.L[0-9]+/.L/g; s/_[0-9]+\b/_N/g' > "$dir/$name.code"
    done
    if cmp -s "codegen/$config/without/$name.code" "codegen/$config/with/$name.code"
    then echo "$name [$flags]: same code"
    else
      echo "$name [$flags]: different code"
      diff "codegen/$config/without/$name.code" "codegen/$config/with/$name.code" || true
    fi
  done
done
