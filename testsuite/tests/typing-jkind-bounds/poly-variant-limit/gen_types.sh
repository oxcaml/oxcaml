#! /bin/bash

outfile="$1"
exec > "$outfile"

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
