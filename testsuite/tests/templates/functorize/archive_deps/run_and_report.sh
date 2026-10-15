#!/bin/sh

# Run "$@" and report how it terminated.  The program is run by a child
# shell whose stderr is discarded, so that the shell's own (non-portable)
# report of a child killed by a signal does not reach the output; the
# trailing [exit] keeps that shell from exec'ing the program directly.
ulimit -c 0
sh -c '"$@"; exit $?' sh "$@" 2>/dev/null
status=$?
if [ "$status" -gt 128 ]
then
  sig=$((status - 128))
  case $sig in
    11) echo "program killed by signal $sig (SIGSEGV)" ;;
    *) echo "program killed by signal $sig" ;;
  esac
else
  echo "program exited with status $status"
fi
