#!/bin/sh

# usage: sh cc_override.sh <C compiler> <argument>...
#
# Runs the C compiler with the arguments.  A file that it compiles is told
# which of the flags that an axis of a codegen variant needs are among
# them: CC_KEEPS_FRAME_POINTERS is defined when -fno-omit-frame-pointer is,
# and CC_SANITIZES_ADDRESSES when -fsanitize=address is.

compiles=false
frame_pointers=
sanitizer=
for argument; do
  case $argument in
    -c) compiles=true ;;
    -fno-omit-frame-pointer) frame_pointers=-DCC_KEEPS_FRAME_POINTERS ;;
    -fsanitize=address) sanitizer=-DCC_SANITIZES_ADDRESSES ;;
  esac
done

if $compiles; then
  exec "$@" $frame_pointers $sanitizer
else
  exec "$@"
fi
