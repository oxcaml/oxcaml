#!/bin/bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

function usage () {
  cat <<USAGE
Usage: $0

Asserts that the compiler sources in this repository are in sync with the
copies in Merlin (by comparing against external/merlin/upstream/ocaml_flambda).
For each directory mirrored in external/merlin/upstream/ocaml_flambda, every
compiler file in the working tree (not counting gitignored files) must either be
imported with identical contents or have the "merlin-exclude" attribute set in
external/merlin/upstream/ocaml_flambda/.gitattributes, and every imported file
must still exist (and not be excluded) in the compiler.

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

# Lists the files under the given directories in the compiler's working tree
# (tracked or untracked, but not gitignored and not deleted from disk), as
# paths relative to the repository root
function files-in-workspace () {
  comm -23 \
    <(git -C "$root" ls-files --cached --others --exclude-standard "$@" | sort) \
    <(git -C "$root" ls-files --deleted "$@" | sort)
}

errors=0
function error () {
  echo "Error: $*" >&2
  errors=$((errors + 1))
}

cd upstream/ocaml_flambda
dirs=(*/)
# The compiler files in the mirrored directories, minus those with the
# merlin-exclude attribute set (git check-attr resolves the paths against the
# .gitattributes in the current directory)
compiler_files="$(files-in-workspace "${dirs[@]%/}" \
  | git check-attr --stdin merlin-exclude \
  | sed -e '/: merlin-exclude: set$/d' -e 's/: merlin-exclude: [a-z]*$//')"

declare -A in_workspace
for compiler_file in $compiler_files; do
  in_workspace["$compiler_file"]=1
  if [[ ! -e "$compiler_file" ]]; then
    error "$compiler_file exists in the compiler, but not in Merlin"
  elif ! cmp -s "$root/$compiler_file" "$compiler_file"; then
    error "$compiler_file is out of sync with Merlin"
  fi
done
for file in */*; do
  if [[ -z "${in_workspace["$file"]-}" ]]; then
    error "$file is in Merlin, but is deleted or merlin-excluded in the compiler"
  fi
done

if [[ $errors -gt 0 ]]; then
  echo >&2
  echo "Merlin is out of sync with the compiler ($errors problem(s) found);" \
       "run external/merlin/scripts/import-ocaml-source.sh to fix this." >&2
  exit 1
fi
echo "external/merlin/upstream/ocaml_flambda is in sync with the compiler"
