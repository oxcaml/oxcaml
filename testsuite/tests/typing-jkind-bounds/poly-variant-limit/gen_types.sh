#! /bin/bash

# This script is used to generate large polymorphic variants. This is used to
# test how jkind inference handles them.

outfile="$1"
exec > "$outfile"

# Function to print out a polymorphic variant with n rows.
gen_type() {
  count="$1"
  echo "type poly_variant_with_${count} = ["
  for i in `seq 1 $count`; do
    echo "  | \`Variant_${i} of int"
  done
  echo "]"
}

gen_type 100
gen_type 101
