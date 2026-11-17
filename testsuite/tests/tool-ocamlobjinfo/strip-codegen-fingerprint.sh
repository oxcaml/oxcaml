#!/bin/sh

# Codegen fingerprint and polled-bit values depend on the configure-time
# defaults (frame pointers, address sanitizer, poll insertion), so strip
# them before comparing against the reference.  They are checked by
# codegen_fingerprint.ml.

grep -v -e '^Codegen fingerprint: ' -e '^Polled: ' "$1" > "$1.tmp"
mv "$1.tmp" "$1"
