#!/bin/sh

# remove trailing whitespace at the ends of lines
sed "s/[ \t]*\n/\n"/ | \
sed "s/[ \t]*$//" | \

# hide flaky parts of identifiers, both in .data and .text sections
sed -r 's/caml([0-9A-Za-z_]*)_[0-9]+/caml\1_HIDE_STAMP/g' | \
sed -r 's/_O([0-9A-Za-z_]*)_[0-9]+_code/_O\1_HIDE_STAMP/g' | \
# sed 's/__/./' | \

# hide target triple
sed -r 's/target triple[^\n]*//' | \

# hide calling convention (which depends on fp config)
sed -r 's/(oxcaml_fpcc|oxcaml_nofpcc)/oxcamlcc/'
