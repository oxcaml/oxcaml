#!/bin/sh

set -eu

# Extract and normalise the symbol names from the linear output

# Unfortunately, the first of the two number stamp suffix of a mangled name is
# part of the last component of the mangling path in the structured
# name-mangling scheme and so its _length_ is recorded as part of the length of
# that component. As we carefully excluded 'F' from names in the OCaml source,
# we can normalise with the following final step:
case "$OXCAML_NAME_MANGLING" in
  structured)
    last_step='s/F(u?)[0-9]+([^F]*)$/F\1_HIDE_LENGTH_\2/'
    ;;
esac

# In order, process the input to:
# - keep only the lines starting with a symbol name,
# - drop the location, if present,
# - drop the colon after the symbol name,
# - hide the stamp suffix,
# - run a last step for the structured name-mangling scheme only.
sed -E \
  -e '/^[* ]/d' \
  -e 's/ .*//' \
  -e 's/:$//' \
  -e 's/_[0-9]+_[0-9]+_code$/_HIDE_STAMP/' \
  ${last_step+-e "$last_step"}
