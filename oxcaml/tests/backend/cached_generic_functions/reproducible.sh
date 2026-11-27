#!/bin/sh
set -eu
case $1 in
  /*) generator=$1 ;;
  *) generator=$PWD/$1 ;;
esac
dir=$(mktemp -d "$PWD/reproducible.XXXXXX")
trap 'rm -rf "$dir"' EXIT

# The second run has randomized hash tables.
for run in 1 2; do
  mkdir -p "$dir/$run/tmp" "$dir/$run/members"
  if [ "$run" = 2 ]; then
    OCAMLRUNPARAM=${OCAMLRUNPARAM:+$OCAMLRUNPARAM,}R
    export OCAMLRUNPARAM
  fi
  TMPDIR=$dir/$run/tmp "$generator" "$dir/$run/cached-generic-functions" \
    > "$dir/$run/log" 2>&1 || {
    cat "$dir/$run/log" >&2
    exit 1
  }
  if [ -n "$(ls -A "$dir/$run/tmp")" ]; then
    echo "run $run left files in the temporary directory:" >&2
    ls -A "$dir/$run/tmp" | head -n 5 >&2
    exit 1
  fi
  # The names of the members, in order, and their contents: the rest of an
  # archive (dates, owners) depends on the archiver.
  ar t "$dir/$run/cached-generic-functions.a" > "$dir/$run/names"
  (cd "$dir/$run/members" && ar x ../cached-generic-functions.a)
done
if ! cmp -s "$dir/1/names" "$dir/2/names"; then
  echo "two runs gave archives with different members:" >&2
  diff "$dir/1/names" "$dir/2/names" | head -n 10 >&2
  exit 1
fi
if ! diff -r "$dir/1/members" "$dir/2/members" > /dev/null; then
  echo "two runs gave archives with different contents" >&2
  exit 1
fi
