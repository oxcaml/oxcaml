#!/bin/sh

# remove trailing whitespace at the ends of lines
sed "s/[ \t]*\n/\n"/ | \
sed "s/[ \t]*$//"

# CR yusumez: filter on function symbols
