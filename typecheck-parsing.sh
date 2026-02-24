#!/bin/bash
set -euo pipefail

# Typechecks a single .ml file from parsing/ using OCaml 5.4
# from the opam switch. Compiles all .mli files from utils/ and
# parsing/ first, plus targeted deps from typing/ (multi-pass
# to resolve dependency order).
#
# Usage: ./typecheck-parsing.sh parsing/location.ml

if [ $# -ne 1 ]; then
  echo "Usage: $0 <path-to-ml-file>" >&2
  exit 1
fi

ML_FILE="$1"
if [ ! -f "$ML_FILE" ]; then
  echo "Error: $ML_FILE not found" >&2
  exit 1
fi

SWITCH="--switch=5.4.0"
OUTDIR=$(mktemp -d)
MENHIRLIB=$(opam exec $SWITCH -- \
  ocamlfind query menhirLib 2>/dev/null || true)

trap "rm -rf $OUTDIR" EXIT

# Collect all .mli files from utils/ and parsing/, plus
# targeted deps from typing/ needed by compilation_unit.mli
TYPING_DEPS=(
  typing/global_module.mli
  typing/ident.mli
)
mapfile -t ALL_MLIS < <(
  { find utils/ parsing/ \
      -maxdepth 1 -name '*.mli' -not -path '*/_*'
    for f in "${TYPING_DEPS[@]}"; do
      [ -f "$f" ] && echo "$f"
    done
  } | sort
)

# Multi-pass compilation: each pass tries all .mli files.
# Files compiled in earlier passes get recompiled as new
# dependencies become available, ensuring consistency.
total=${#ALL_MLIS[@]}
pass=0
prev_compiled=-1

while true; do
  pass=$((pass + 1))
  compiled=0
  failed=()

  for mli in "${ALL_MLIS[@]}"; do
    mod=$(basename "$mli" .mli)
    if opam exec $SWITCH -- ocamlc -I "$OUTDIR" \
        ${MENHIRLIB:+-I "$MENHIRLIB"} \
        -c "$mli" -o "$OUTDIR/${mod}.cmi" 2>/dev/null; then
      compiled=$((compiled + 1))
    else
      failed+=("$mli")
    fi
  done

  if [ ${#failed[@]} -eq 0 ] || [ $compiled -eq "$prev_compiled" ]; then
    break
  fi
  prev_compiled=$compiled
done

if [ ${#failed[@]} -gt 0 ]; then
  echo "Note: ${#failed[@]}/$total .mli file(s) could not compile" \
    "(likely auto-generated deps like parser.cmi):" >&2
  for f in "${failed[@]}"; do
    echo "  $(basename "$f")" >&2
  done
fi

echo "Compiled $compiled/$total .mli file(s) in $pass pass(es)."

# Typecheck the .ml file
echo "Typechecking $ML_FILE..."
mod=$(basename "$ML_FILE" .ml)
if opam exec $SWITCH -- ocamlc -I "$OUTDIR" \
    ${MENHIRLIB:+-I "$MENHIRLIB"} \
    -c "$ML_FILE" -o "$OUTDIR/${mod}.cmo" 2>&1; then
  echo "OK"
else
  exit 1
fi
