#!/bin/bash
set -euo pipefail

# Typechecks a single .ml file from toplevel/ (including byte/
# and native/ subdirs) using OCaml 5.4 from the opam switch.
# Compiles all .mli files from utils/, parsing/, typing/,
# bytecomp/, driver/, lambda/, file_formats/, toplevel/,
# toplevel/byte/, and toplevel/native/ first (multi-pass to
# resolve dependency order).
#
# Usage: ./typecheck-toplevel.sh toplevel/toploop.ml

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

PARSER_STUB="$OUTDIR/parser.mli"
cat > "$PARSER_STUB" <<'STUBEOF'
type token = EOF
STUBEOF
opam exec $SWITCH -- ocamlc -c "$PARSER_STUB" -o "$OUTDIR/parser.cmi"

STDLIB_DEPS=(
  stdlib/camlinternalQuote.mli
)
# .ml-only modules (no .mli) that need compilation to produce .cmi
ML_ONLY_DEPS=(
  typing/jkind_intf.ml
)
mapfile -t ALL_MLIS < <(
  { find utils/ parsing/ typing/ bytecomp/ driver/ lambda/ \
         file_formats/ toplevel/ toplevel/byte/ toplevel/native/ \
      -maxdepth 1 -name '*.mli' -not -path '*/_*'
    for f in "${STDLIB_DEPS[@]}"; do
      [ -f "$f" ] && echo "$f"
    done
  } | sort
)

# Multi-pass compilation: each pass tries all .mli files and
# .ml-only modules. Files compiled in earlier passes get
# recompiled as new dependencies become available.
total=${#ALL_MLIS[@]}
pass=0
prev_compiled=-1

while true; do
  pass=$((pass + 1))
  compiled=0
  failed=()

  # Try .ml-only modules (produce .cmi from .ml)
  for ml in "${ML_ONLY_DEPS[@]}"; do
    mod=$(basename "$ml" .ml)
    if [ -f "$ml" ]; then
      opam exec $SWITCH -- ocamlc -I "$OUTDIR" \
          ${MENHIRLIB:+-I "$MENHIRLIB"} \
          -c "$ml" -o "$OUTDIR/${mod}.cmo" 2>/dev/null || true
    fi
  done

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
    "(likely missing deps):" >&2
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
