#!/bin/sh

# Check that the linker script really moved [hot] ahead of the rest of the
# unit's code, so that the test is not vacuous.

nm "${program}" | awk '
  / camlMoved__hot_[0-9_]*code$/ { hot = $1 }
  / camlMoved__code_begin$/ { begin_ = $1 }
  END {
    if (hot == "" || begin_ == "") { print "symbols not found"; exit 1 }
    if (hot >= begin_) { print "hot was not moved"; exit 1 }
  }'
