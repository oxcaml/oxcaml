#!/bin/sh

# CR ocaml 5 all-runtime5: remove __ mangling once we're always using the 5 runtime
sed -r 's/_O(.*)_[0-9]+_code?/_O\1_HIDE_STAMP/' \
| \
    sed 's/__/./g'
