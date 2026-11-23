#!/bin/sh
set -eu
generator=$1
dir=$(mktemp -d "$PWD/archiver_fails.XXXXXX")
trap 'rm -rf "$dir"' EXIT
mkdir "$dir/tmp"

# The archiver cannot create an archive in a directory that does not exist.
status=0
TMPDIR=$dir/tmp "$generator" "$dir/missing/cached-generic-functions" \
  > "$dir/log" 2>&1 || status=$?
if [ "$status" = 0 ] || ! grep -q "Error while creating the archive" "$dir/log"
then
  echo "exit status $status; expected a failure of the archiver:" >&2
  cat "$dir/log" >&2
  exit 1
fi
