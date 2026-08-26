#!/bin/bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
subtree_prefix="$(git rev-parse --show-prefix)"

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
# Prints nothing for files that are not synced with Merlin.
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

    .gitattributes) ;;

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
dirs=(*/)
dirs=("${dirs[@]%/}")
if [[ "$subdirectory" = "." ]]; then
  fetch_prefix=""
else
  fetch_prefix="$subdirectory/"
fi
# The compiler files at $rev in the mirrored directories, as paths relative
# to upstream/ocaml_flambda, minus those with the merlin-exclude attribute set
# (git check-attr resolves the paths against the .gitattributes in the current
# directory)
upstream_files="$(git ls-tree --full-tree -r --name-only "$rev" \
                    -- "${dirs[@]/#/$fetch_prefix}" \
                  | sed "s|^$fetch_prefix||" \
                  | git check-attr --stdin merlin-exclude \
                  | sed -e '/: merlin-exclude: set$/d' \
                        -e 's/: merlin-exclude: [a-z]*$//')"
for file in $upstream_files; do
  if [[ -e "$file" ]]; then
    git show "$rev:$fetch_prefix$file" > "$file"
  else
    read -p "Import new file $file? [Y/n] " answer
    case "$answer" in
      y|Y|"")
        echo "Importing $file"
        git show "$rev:$fetch_prefix$file" > "$file"
        new_files+=("$file")
        ;;
      *)
        echo "$file merlin-exclude" >> .gitattributes
        echo "Set the merlin-exclude attribute for $file in" \
             "upstream/ocaml_flambda/.gitattributes; remove it from there" \
             "and re-run this script if you change your mind."
        ;;
    esac
  fi
done
# Remove files that are no longer imported (deleted upstream or newly
# merlin-excluded), so they don't silently go stale.
for file in */*; do
  if ! grep -qxF "$file" <<< "$upstream_files"; then
    rm "$file"
  fi
done
cd ../..

# Annotations for diff3 regions; "@" would be more natural than ":" but confuses
# smerge-mode's highlighting
old_marker="Merlin:$current_head"
parent_marker="Compiler:last-imported"
new_marker="Compiler:$commitish"

# Then patch src/ocaml using the changes you just imported. Newly-imported
# files are still untracked at this point so they don't show up in the diff;
# they are instead copied over verbatim below.
for file in $(git diff --no-ext-diff --name-only); do
  file=${file#${subtree_prefix}}
  base=${file#upstream/ocaml_flambda/}
  tgt="$(merlin-target "$base")"

  if [ -z "$tgt" ]; then continue; fi

  tgt=src/ocaml/$tgt

  if [ -e "$file" ]; then
    # Three-way merge of Merlin's copy with the old and new upstream copies.
    git show "HEAD:${subtree_prefix}${file}" > "$tgt.base"
    # If any of the inputs already contain git conflict markers, we use a
    # marker size greater than 7 to be able to distinguish import-script
    # conflict markers from pre-existing conflict markers. Otherwise, we use the
    # default size of 7 since some tooling expects size 7 markers.
    if grep -qE '^<<<<<<<' "$tgt" "$tgt.base" "$file"; then
      marker_size=14
    else
      marker_size=7
    fi
    if ! git merge-file --diff3 --marker-size="$marker_size" \
           -L "$old_marker" -L "$parent_marker" -L "$new_marker" \
           "$tgt" "$tgt.base" "$file"
    then
      echo "Merge conflicts in $tgt"
      combine_status=0
      scripts/combine-merge-conflicts.py "$tgt" || combine_status="$?"
      case "$combine_status" in
        0) ;;
        21)
          echo "Warning: malformed merge conflicts in $tgt;" \
               "leaving them uncombined."
          ;;
        *)
          echo "Error: combining merge conflicts failed on $tgt with status" \
               "$combine_status" >&2
          exit "$combine_status"
          ;;
      esac
    fi
    rm -f "$tgt.base"
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
git commit -m "Automated commit: Import compiler changes from $rev"
