#!/bin/sh

# remove trailing whitespace at the ends of lines
sed "s/[ \t]*\n/\n"/ | \
sed "s/[ \t]*$//" | \

# hide flaky parts of identifiers
sed -r 's/caml(.*)_[0-9]+_[0-9]+(_code)?/caml\1_HIDE_STAMP/' | \
# sed 's/__/./' | \

# hide target triple
sed -r 's/target triple[^\n]*//'
