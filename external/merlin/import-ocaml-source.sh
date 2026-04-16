#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

# Script arguments with their default values
commitish=HEAD
repository=.
subdirectory=.
old_subdirectory=.

function usage () {
  cat <<USAGE
Usage: $0 [COMMITISH [REPO [SUBDIRECTORY [OLD_SUBDIRECTORY]]]]

Fetch the new compiler sources and patch Merlin to keep Merlin's local copies of
things in sync. By default, this will pull in compiler changes from the local
repo at the current revision. But you may pass an arbitrary commitish (branch,
tag, full (not abbreviated!) commit hash, etc.) to important changes from. You
may also fetch from a remote repository by specifying a REPO, and the
subdirectory of the repo that the compiler is located in can be overriden by any
path (including ".").

This attempts to import new files from the compiler by running the
"import_added_ocaml_source_files.sh" script. If that doesn't work, you can also
try making matched pairs of files in this repository with the right names: one
in "upstream/ocaml_flambda/", and one in "src/ocaml".  Then running the script
will pull in the named file(s).

The SUBDIRECTORY argument is useful when importing from a repository that buries
the relevant compiler files inside a subdirectory. This used to be the case for
flambda (files were under an "ocaml/" direcotry), although it is no longer the
case. The OLD_SUBDIRECTORY argument is useful for when the directory structure
has changed since the last import.
USAGE
}

function repository-commit () {
  if [[ $repository =~ ^https://github.com/(.*) ]]; then
    echo "${BASH_REMATCH[1]}@$1"
  else
    echo "$repository @ $1"
  fi
}

case "$1" in
  -h|-help|--help|-\?)
    usage
    exit 0
    ;;
esac

if [[ $# -le 4 ]]; then
  commitish="${1-$commitish}"
  repository="${2-$repository}"
  # Although the subdirectory arguments are probably no longer useful, it doesn't hurt
  # to keep them around in case they ever are of use.
  subdirectory="${3-$subdirectory}"
  old_subdirectory="${4-$old_subdirectory}"
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

# First, add any files that have been added since the last import.
./import-added-ocaml-source-files.sh "$commitish" "$repository" "$subdirectory" "$old_subdirectory"

# Then, get the new oxcaml sources and copy into upstream/ocaml_flambda
if [ "$repository" != "." ]; then
  git fetch "$repository" "$commitish"
  rev=$(git rev-parse FETCH_HEAD)
else
  rev=$(git rev-parse "$commitish")
fi
cd upstream/ocaml_flambda
echo $rev > base-rev.txt
for file in $(git ls-tree --name-only -r HEAD | grep -v base-rev.txt); do
  if [[ "$subdirectory" = "." ]]; then
    git_file="$file"
  else
    git_file="$subdirectory/$file"
  fi
  git show "$rev:$git_file" > "$file"
done
git add --intent-to-add .
cd ../..

# Annotations for diff3 regions; "@" would be more natural than ":" but confuses
# smerge-mode's highlighting
old_marker="Merlin:$current_head"
parent_marker="Compiler:$old_base_rev"
new_marker="Compiler:$commitish"

# Then patch src/ocaml using the changes you just imported
for file in $(git diff --no-ext-diff --name-only); do
  file=${file#external/merlin/}
  base=${file#upstream/ocaml_flambda/}
  case $base in
    # If you add new files here, you need to apply the full diff manually once,
    # otherwise the merge won't pick up on old changes!

    # Renamed files
    parsing/lexer.mll) tgt=preprocess/lexer_raw.mll;;
    parsing/parser.mly) tgt=preprocess/parser_raw.mly;;

    # Merlin moves the modules it depends on from this directory into `typing/`
    # (as of the time of writing, that's `Cmi_format` and `Cmt_format`)
    file_formats/*) tgt=${base/#file_formats/typing};;

    # We can't have these modules in `utils/`, it breaks Merlin's dependency
    # structure
    utils/compilation_unit.ml*|utils/import_info.ml*)
      tgt=${base/#utils/typing};;

    # We can't have this module in `parsing/`, it breaks Merlin's dependency
    # structure
    parsing/unit_info.ml*)
      tgt=${base/#parsing/typing};;

    # This src/utils/misc.ml depends on format_doc.ml, but src/ocaml/utils depends on
    # src/utils. So we need to move it to fix a dependency cycle.
    utils/format_doc.ml*)
      tgt=${base/#utils/../utils};;

    # We have to inspect these files by hand, we only care about a subset of the
    # changes
    utils/clflags.ml*|utils/config.ml*)
      printf '\e[7mIgnoring changes to %s, inspect it manually.\e[0m\n' "$base"
      continue;;

    # Most cases are simple
    *) tgt=$base;;
  esac
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

git add .
git commit -m "Automated commit: Import compiler changes from $old_base_rev to $rev"
