#!/bin/sh

if [ "$2" = 'otherlibs/dynlink/dynlink.ml' ]; then
  echo '#1 "otherlibs/dynlink/native/dynlink.ml"'
  cat otherlibs/dynlink/native/dynlink.ml
else
  cat "$2"
fi
