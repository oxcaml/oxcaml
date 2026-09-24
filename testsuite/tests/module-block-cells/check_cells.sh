#!/bin/sh

# Checks the symbols of the compiled unit cells_lib.o and of ${program}: the
# unit defines its cells but no module block symbol (camlCells_lib), and the
# linked program keeps a cell (cell1 holds the dynamic field n, a GC root)
# and still has no module block.

exec > "${output}" 2>&1

status=0

check_defined() {
  if grep -q " [DdBb] $2\$" "$1"; then
    echo "$3: $2 defined"
  else
    echo "$3: $2 NOT DEFINED"
    status=1
  fi
}

check_absent() {
  if grep -q " $2\$" "$1"; then
    echo "$3: $2 PRESENT"
    status=1
  else
    echo "$3: $2 absent"
  fi
}

object_symbols=${test_build_directory}/cells_lib_symbols.txt
nm "${test_build_directory}/cells_lib.o" > "$object_symbols"
check_defined "$object_symbols" camlCells_lib__cell0 object
check_defined "$object_symbols" camlCells_lib__cell1 object
check_defined "$object_symbols" camlCells_lib__cell2 object
check_defined "$object_symbols" camlCells_lib__cell3 object
check_absent "$object_symbols" camlCells_lib object

program_symbols=${test_build_directory}/cells_symbols_symbols.txt
nm "${program}" > "$program_symbols"
check_defined "$program_symbols" camlCells_lib__cell1 program
check_absent "$program_symbols" camlCells_lib program

exit $status
