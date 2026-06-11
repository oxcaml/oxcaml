#!/bin/bash

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
repo at the current revision. But you may pass an arbitrary commitish (branch,
tag, full (not abbreviated!) commit hash, etc.) to import changes from. You
may also fetch from a remote repository by specifying a REPO, and the
subdirectory of the repo that the compiler is located in can be overriden by any
path (including ".").

Each directory in "upstream/ocaml_flambda/" can contain a ".exclude" file that
explicitly lists the files from the corresponding compiler directory that we do
not import (a missing ".exclude" file is treated as an empty one).  Each line
is a glob pattern matched against the file names in that directory; a pattern
starting with "!" re-includes files matched by an earlier pattern.  Files that
are neither excluded nor already imported are offered interactively: importing
such a file copies it into both "upstream/ocaml_flambda/" and "src/ocaml/",
while declining records it in the ".exclude" file.  To start importing a
previously ignored file, remove it from the ".exclude" file (or add a "!" entry
for it) and re-run this script.

The SUBDIRECTORY argument is useful when importing from a repository that buries
the relevant compiler files inside a subdirectory. This used to be the case for
flambda (files were under an "ocaml/" directory), although it is no longer the
case.
USAGE
}

# Succeeds if the file named $2 is listed in $1/.exclude.  Each line of the
# .exclude file is a glob pattern; a pattern starting with "!" re-includes
# files matched by an earlier pattern.  A missing .exclude file is treated as
# an empty one.
function is-excluded () {
  local dir="$1" name="$2" pattern result=1
  if [[ ! -f "$dir/.exclude" ]]; then return 1; fi
  while IFS= read -r pattern; do
    case "$pattern" in
      ''|'#'*) ;;
      '!'*) if [[ "$name" == ${pattern#!} ]]; then result=1; fi;;
      *)    if [[ "$name" == $pattern    ]]; then result=0; fi;;
    esac
  done < "$dir/.exclude"
  return $result
}

# Maps a file under upstream/ocaml_flambda/ to its location in src/ocaml/.
# Prints nothing for files whose changes have to be inspected manually.
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

    # We have to inspect these files by hand, we only care about a subset of the
    # changes
    utils/clflags.ml*|utils/config.ml*) ;;

    # Most cases are simple
    *) echo "$base";;
  esac
}

case "$1" in
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

if ! [ -z "$(git status --porcelain)" ]; then
  echo "Working directory must be clean before using this script,"
  echo "but currently has the following changes:"
  git status
  exit 1
fi


# Used for patch output
old_base_rev="$(cat upstream/ocaml_flambda/base-rev.txt)"
current_head="$(git symbolic-ref --short HEAD)"

# Get the new oxcaml sources and copy every file not listed in a .exclude file
# into upstream/ocaml_flambda
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
  for git_file in $upstream_files; do
    name="${git_file#"$fetch_dir"/}"
    file="$dir/$name"
    if is-excluded "$dir" "$name"; then continue; fi
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
          echo "$name" >> "$dir/.exclude"
          echo "Added $name to $dir/.exclude; remove it from there and re-run" \
               "this script if you change your mind."
          ;;
      esac
    fi
  done
  # Warn about files that were previously imported but no longer exist
  # upstream, so they don't silently go stale.
  for file in "$dir"/*; do
    name="${file#"$dir"/}"
    if ! printf '%s\n' $upstream_files | grep -qxF "$fetch_dir/$name"; then
      echo "Warning: $file no longer exists upstream; consider deleting it."
    fi
  done
done
cd ../..

# Annotations for diff3 regions; "@" would be more natural than ":" but confuses
# smerge-mode's highlighting
old_marker="Merlin:$current_head"
parent_marker="Compiler:$old_base_rev"
new_marker="Compiler:$commitish"

# Then patch src/ocaml using the changes you just imported.  Newly-imported
# files are still untracked at this point so they don't show up in the diff;
# they are instead copied over verbatim below.
for file in $(git diff --no-ext-diff --name-only); do
  file=${file#${subtree_prefix}}
  base=${file#upstream/ocaml_flambda/}
  tgt="$(merlin-target "$base")"
  if [[ -z "$tgt" ]]; then
    printf '\e[7mIgnoring changes to %s, inspect it manually.\e[0m\n' "$base"
    continue
  fi
  tgt=src/ocaml/$tgt

  # Not all files are necessary
  if [ ! -e $tgt ]; then continue; fi

  err=$(patch --merge=diff3 $tgt <(git diff --no-ext-diff -- $file))
  # ignore patch output if it worked
  if [ $? != 0 ]; then
    sed -i \
        -e 's!^<<<<<<<$!& '"$old_marker"'!'    \
        -e 's!^|||||||$!& '"$parent_marker"'!' \
        -e 's!^>>>>>>>$!& '"$new_marker"'!'    \
        $tgt
    echo "$err"
  fi
  rm -f $tgt.orig
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

git add .
git commit -m "Automated commit: Import compiler changes from $old_base_rev to $rev"
