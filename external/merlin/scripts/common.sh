# Helpers shared between the scripts in this directory.  This file is meant to
# be sourced (with the current directory at the root of the Merlin subtree),
# not executed.

# Does a batch lookup of git attributes. Before calling is-excluded, you must
# have passed all paths that you intend to pass to is-excluded into
# prepare-is-exluded. (Doing this batch lookup gives a multiple-second
# performance improvement.)
declare -A _merlin_excluded
function prepare-is-excluded () {
  local path attr value
  _merlin_excluded=()
  while IFS= read -r -d '' path \
     && IFS= read -r -d '' attr \
     && IFS= read -r -d '' value; do
    if [[ "$value" == set ]]; then
      _merlin_excluded["$path"]=1
    else
      _merlin_excluded["$path"]=0
    fi
  done < <(sed '/^$/d' | tr '\n' '\0' | git check-attr --stdin -z merlin-exclude)
}

# Succeeds if the merlin-exclude gitattribute is set for the file named $2 in
# directory $1. The attributes are declared in
# upstream/ocaml_flambda/.gitattributes; since git check-attr resolves paths
# relative to the current directory, this must be called with the current
# directory at upstream/ocaml_flambda. Before calling is-excluded, you must have
# passed the given path into prepare-is-excluded.
function is-excluded () {
  [[ -n "${_merlin_excluded["$1"]}" ]]
}
