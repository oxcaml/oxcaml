#!/bin/sh

# Codegen fingerprint values depend on the configure-time defaults
# (frame pointers, address sanitizer), so strip them before comparing
# against the reference.  They are checked by codegen_fingerprint.ml.

grep -v '^Codegen fingerprint: ' "$1" > "$1.tmp"
mv "$1.tmp" "$1"
