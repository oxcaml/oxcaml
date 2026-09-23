#!/bin/sh

# Checks the assembly of cells_symbols.ml: each module-block cell must be
# defined and declared global.

exec > "${output}" 2>&1

asm=${test_build_directory}/cells_symbols.s
status=0

check () {
  sym=$1
  if grep -q -E "^[[:space:]]*\.globl[[:space:]]+${sym}[[:space:]]*\$" "$asm"
  then vis=global
  else vis=NOT_GLOBAL; status=1
  fi
  if grep -q -E "^[[:space:]]*${sym}:" "$asm"
  then def=defined
  else def=UNDEFINED; status=1
  fi
  echo "${sym}: ${def}, ${vis}"
}

check camlCells_symbols__cell0
check camlCells_symbols__cell1
echo "cells: $(grep -c -E '^[[:space:]]*camlCells_symbols__cell[0-9]+:' "$asm")"

exit $status
