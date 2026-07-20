#!/bin/bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
subtree_prefix="$(git rev-parse --show-prefix)"
source scripts/common.sh

# Script arguments with their default values
commitish=HEAD
repository=.
subdirectory=.

function usage () {
  cat <<USAGE
Usage: $0 [COMMITISH [REPO [SUBDIRECTORY]]]

Fetch the new compiler sources and patch Merlin to keep Merlin's local copies of
things in sync. By default, this will pull in compiler changes from the local
repo at the current revision. But you may pass an arbitrary committish (branch,
tag, full (not abbreviated!) commit hash, etc.) to import changes from. You
may also fetch from a remote repository by specifying a REPO, and the
subdirectory of the repo that the compiler is located in can be overridden by any
path (including ".").

The file "upstream/ocaml_flambda/.gitattributes" explicitly lists the compiler
files that we do not import, by setting the "merlin-exclude" attribute on them
(see gitattributes(5) for the pattern syntax; "-merlin-exclude" re-includes
files matched by an earlier pattern). Files that are neither excluded nor
already imported are offered interactively: importing such a file copies it
into both "upstream/ocaml_flambda/" and "src/ocaml/", while declining records
it in the ".gitattributes" file. To start importing a previously ignored
file, remove its entry from the ".gitattributes" file (or add a
"-merlin-exclude" entry for it) and re-run this script.

The SUBDIRECTORY argument is useful when importing from a repository that buries
the relevant compiler files inside a subdirectory. This used to be the case for
flambda (files were under an "ocaml/" directory), although it is no longer the
case.
USAGE
}

# Maps a file under upstream/ocaml_flambda/ to its location in src/ocaml/.
function merlin-target () {
  local base="$1"
  case $base in
    # If you add new files here, you need to apply the full diff manually once,
    # otherwise the merge won't pick up on old changes!

    # Renamed files
    parsing/lexer.mll) echo preprocess/lexer_raw.mll;;
    parsing/parser.mly) echo preprocess/parser_raw.mly;;

    # Merlin moves the modules it depends on from this directory into `typing/`
    # (as of the time of writing, that's `Cmi_format` and `Cmt_format`)
    file_formats/*) echo "${base/#file_formats/typing}";;

    # We can't have these modules in `utils/`, it breaks Merlin's dependency
    # structure
    utils/compilation_unit.ml*|utils/import_info.ml*)
      echo "${base/#utils/typing}";;

    # We can't have this module in `parsing/`, it breaks Merlin's dependency
    # structure
    parsing/unit_info.ml*)
      echo "${base/#parsing/typing}";;

    lambda/mixed_product_bytes.ml*)
      echo "${base/#lambda/typing}";;

    # Most cases are simple
    *) echo "$base";;
  esac
}

case "${1-unused}" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
esac

if [[ $# -le 3 ]]; then
  commitish="${1-$commitish}"
  repository="${2-$repository}"
  # Although the subdirectory argument is probably no longer useful, it doesn't
  # hurt to keep it around in case it ever is of use.
  subdirectory="${3-$subdirectory}"
else
  usage >&2
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  echo "Working directory must be clean before using this script,"
  echo "but currently has the following changes:"
  git status
  exit 1
fi


# Used for patch output
old_base_rev="$(cat upstream/ocaml_flambda/base-rev.txt)"
current_head="$(git symbolic-ref --short HEAD)"

# Get the new oxcaml sources and copy every file without the merlin-exclude
# attribute into upstream/ocaml_flambda
if [ "$repository" != "." ]; then
  git fetch "$repository" "$commitish"
  rev=$(git rev-parse FETCH_HEAD)
else
  rev=$(git rev-parse "$commitish")
fi
new_files=()
cd upstream/ocaml_flambda
echo $rev > base-rev.txt
for dir in */; do
  dir="${dir%/}"
  if [[ "$subdirectory" = "." ]]; then
    fetch_dir="$dir"
  else
    fetch_dir="$subdirectory/$dir"
  fi
  upstream_files="$(git ls-tree --full-tree -r --name-only "$rev" "$fetch_dir")"
  prepare-is-excluded < <(
    for git_file in $upstream_files; do
      echo "$dir/${git_file#"$fetch_dir"/}"
    done
  )
  for git_file in $upstream_files; do
    name="${git_file#"$fetch_dir"/}"
    file="$dir/$name"
    if is-excluded "$file"; then continue; fi
    if [[ -e "$file" ]]; then
      git show "$rev:$git_file" > "$file"
    else
      read -p "Import new file $file? [Y/n] " answer
      case "$answer" in
        y|Y|"")
          echo "Importing $file"
          git show "$rev:$git_file" > "$file"
          new_files+=("$file")
          ;;
        *)
          echo "$dir/$name merlin-exclude" >> .gitattributes
          echo "Set the merlin-exclude attribute for $dir/$name in" \
               "upstream/ocaml_flambda/.gitattributes; remove it from there" \
               "and re-run this script if you change your mind."
          ;;
      esac
    fi
  done
  # Warn about files that were previously imported but no longer exist
  # upstream, so they don't silently go stale.
  for file in "$dir"/*; do
    name="${file#"$dir"/}"
    if ! printf '%s\n' $upstream_files | grep -qxF "$fetch_dir/$name"; then
      rm "$file"
    fi
  done
done
cd ../..

# Annotations for diff3 regions; "@" would be more natural than ":" but confuses
# smerge-mode's highlighting
old_marker="Merlin:$current_head"
parent_marker="Compiler:$old_base_rev"
new_marker="Compiler:$commitish"

# Then patch src/ocaml using the changes you just imported. Newly-imported
# files are still untracked at this point so they don't show up in the diff;
# they are instead copied over verbatim below.
for file in $(git diff --no-ext-diff --name-only); do
  if [[ "$file" = "upstream/ocaml_flambda/base-rev.txt" ]]; then continue; fi

  file=${file#${subtree_prefix}}
  base=${file#upstream/ocaml_flambda/}
  tgt="$(merlin-target "$base")"
  tgt=src/ocaml/$tgt

  if [ -e "$file" ]; then
    # ignore patch output if it worked
    if ! patch --merge=diff3 $tgt <(git diff --no-ext-diff -- $file) > $tgt.out; then
      sed -i \
          -e 's!^<<<<<<<$!& '"$old_marker"'!'    \
          -e 's!^|||||||$!& '"$parent_marker"'!' \
          -e 's!^>>>>>>>$!& '"$new_marker"'!'    \
          $tgt
      cat $tgt.out
    fi
    rm -f $tgt.orig $tgt.out
  else
    # The file was deleted from the compiler, so delete Merlin's copy too. If
    # Merlin had local changes relative to the previously imported copy, record
    # them in a .rej file so they aren't silently lost.
    if git show "HEAD:${subtree_prefix}${file}" \
        | diff -u --label "$parent_marker" --label "$old_marker" - "$tgt" > "$tgt.rej"
    then
      rm "$tgt.rej"
      echo "Deleted $tgt (deleted from the compiler)"
    else
      echo "Deleted $tgt (deleted from the compiler);"
      echo "local Merlin changes recorded in $tgt.rej"
    fi
    rm "$tgt"
  fi
done

# Copy any newly-imported files into src/ocaml
for file in "${new_files[@]}"; do
  tgt="$(merlin-target "$file")"
  if [[ -z "$tgt" ]]; then continue; fi
  tgt=src/ocaml/$tgt
  if [[ -e "$tgt" ]]; then
    echo "Warning: $tgt already exists;" \
         "not overwriting it with upstream/ocaml_flambda/$file"
    continue
  fi
  cp "upstream/ocaml_flambda/$file" "$tgt"
done

# Commit any changes to the .gitattributes file separately from the import
# itself, since they should be included in review.
git add upstream/ocaml_flambda/.gitattributes
if ! git diff --cached --quiet; then
  git commit -m "Update merlin-exclude attributes"
fi

git add .
# Also add any .rej files that were created by patch, even though they're
# ignored.
git add "*.rej" --force &> /dev/null || true
git commit -m "Automated commit: Import compiler changes from $old_base_rev to $rev"
