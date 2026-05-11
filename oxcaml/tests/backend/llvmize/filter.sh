#!/bin/sh

# remove trailing whitespace at the ends of lines
sed "s/[ \t]*\n/\n"/ | \
sed "s/[ \t]*$//" | \

# hide flaky parts of identifiers, picked per name-mangling scheme
# (configure-time, surfaced via OXCAML_NAME_MANGLING)
case "$OXCAML_NAME_MANGLING" in
  structured)
    # Cover both .data and .text sections: caml... references can still
    # appear in .data (e.g. references to flat-mangled stdlib symbols),
    # and _Caml... references appear for code compiled under structured.
    sed -E -e 's/(caml[0-9A-Za-z_]*)_[0-9]+_[0-9]+/\1_HIDE_STAMP/g' \
           -e 's/(_Caml[0-9A-Za-z_]*)_[0-9]+_[0-9]+_code/\1_HIDE_STAMP/g'
    ;;
  flat|*)
    sed -r 's/caml(.*)_[0-9]+_[0-9]+(_code)?/caml\1_HIDE_STAMP/'
    ;;
esac | \
# sed 's/__/./' | \

# hide target triple
sed -r 's/target triple[^\n]*//' | \

# hide calling convention (which depends on fp config)
sed -r 's/(oxcaml_fpcc|oxcaml_nofpcc)/oxcamlcc/'
