#!/bin/sh

case "$OXCAML_NAME_MANGLING" in
  structured)
    # Unfortunately, the first of the two number stamps is part of the last
    # component of the mangled name and so its _length_ is recorded as part of
    # the length of that component in the structured name-mangling scheme. It
    # would require some trickery to hide this length away, so let's hope for
    # now that it will be stable enough.
    sed -E 's/(_Caml.*)_[0-9]+_[0-9]+(_code)?/\1_HIDE_STAMP/'
    ;;
  flat|*)
    # CR ocaml 5 all-runtime5: remove __ mangling once we're always using the 5 runtime
    sed -r 's/caml(.*)_[0-9]+_[0-9]+(_code)?/caml\1_HIDE_STAMP/' \
    | \
        sed 's/__/./g'
    ;;
esac
