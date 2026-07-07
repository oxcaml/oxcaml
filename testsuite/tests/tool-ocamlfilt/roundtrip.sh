#!/bin/sh
# Test that identifier encoding and decoding correctly round-trip
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

check_roundtrip () {
  input=$1
  output=$(${OCAMLFILT} --encode --decode "$input" 2>/dev/null)
  if [ "$output" = "$input" ]; then
    echo "round-trip: $input (encoded: $(${OCAMLFILT} --encode "$input"))"
  else
    echo "UNEXPECTED ROUND-TRIP: $input -> $output"
    exit 1
  fi
}

check_roundtrip Foo
check_roundtrip foo_bar
check_roundtrip 'let*'
check_roundtrip '@@'
check_roundtrip 'file.ml:12:34'
check_roundtrip 'Naïveté'
# Missing-location payloads use empty fields as the "unknown" sentinel
# ("__" = no file/line/col, "foo.ml__512" = file + offset, no line).
check_roundtrip '__'
check_roundtrip 'foo.ml__512'
