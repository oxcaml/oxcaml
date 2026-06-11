#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")/.."
source scripts/common.sh

function usage () {
  cat <<USAGE
Usage: $0

Asserts that the compiler sources in this repository are in sync with the
copies in Merlin (by comparing against external/merlin/upstream/ocaml_flambda).
For each directory mirrored in external/merlin/upstream/ocaml_flambda, every
compiler file in the working tree (not counting gitignored files) must either be
imported with identical contents or be listed in that directory's ".exclude"
file, and every imported file must still exist in the compiler.

Exits non-zero if anything is out of sync; run
external/merlin/scripts/import-ocaml-source.sh to bring the copies back in sync.
USAGE
}

case "${1-}" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
esac

if [[ $# -gt 0 ]]; then
  usage >&2
  exit 1
fi

root="$(git rev-parse --show-toplevel)"

# Lists the files under directory $1 in the compiler's working tree (tracked
# or untracked, but not gitignored and not deleted from disk), as paths
# relative to the repository root
function files-in-workspace () {
  comm -23 \
    <(git -C "$root" ls-files --cached --others --exclude-standard "$1" | sort) \
    <(git -C "$root" ls-files --deleted "$1" | sort)
}

errors=0
function error () {
  echo "Error: $*" >&2
  errors=$((errors + 1))
}

cd upstream/ocaml_flambda
for dir in */; do
  dir="${dir%/}"
  compiler_files="$(files-in-workspace "$dir")"
  for compiler_file in $compiler_files; do
    name="${compiler_file#"$dir"/}"
    if is-excluded "$dir" "$name"; then continue; fi
    if [[ ! -e "$compiler_file" ]]; then
      error "$compiler_file exists in the compiler, but not in Merlin"
    elif ! cmp -s "$root/$compiler_file" "$compiler_file"; then
      error "$compiler_file is out of sync with Merlin"
    fi
  done
  # The glob skips dotfiles, so the .exclude files themselves are not checked
  for file in "$dir"/*; do
    name="${file#"$dir"/}"
    if ! printf '%s\n' $compiler_files | grep -qxF "$dir/$name"; then
      error "$compiler_file was deleted, but not in Merlin"
    fi
  done
done

if [[ $errors -gt 0 ]]; then
  echo >&2
  echo "Merlin is out of sync with the compiler ($errors problem(s) found);" \
       "run external/merlin/scripts/import-ocaml-source.sh to fix this." >&2
  exit 1
fi
echo "external/merlin/upstream/ocaml_flambda is in sync with the compiler"
