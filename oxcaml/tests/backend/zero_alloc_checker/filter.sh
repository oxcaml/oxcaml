#!/bin/sh

# The second sed strips the "./" prefix that dune >= 3.24 adds to source paths.
sed -r 's/caml(.*)_[0-9]+_[0-9]+_code?/caml\1_HIDE_STAMP/' \
| \
    sed -e 's|\([("[/ ]\)\./|\1|g' -e 's|^\./||'
