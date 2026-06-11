#!/bin/sh

# Hide the trailing _N_N(_code) stamp suffix on mangled symbols emitted by
# either name-mangling scheme (flat: camlMod__fn_N_N_code,
# structured: _CamlU<len>ModF<len>fn_N_N_code).
#
# CR ocaml 5 all-runtime5: remove __ mangling once we're always using the 5 runtime
sed -r 's/(caml|_Caml)(.*)_[0-9]+_[0-9]+(_code)?/\1\2_HIDE_STAMP/' \
| \
    sed 's/__/./g'
