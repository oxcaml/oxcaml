#!/bin/sh

# The counters take labels and identifiers, so the numbers of local labels and
# of symbols are left out of the comparison, as is the header naming the file.
sed -E '/file format/d; s/\.L[0-9]+/.L/g; s/_[0-9]+\b/_N/g'
