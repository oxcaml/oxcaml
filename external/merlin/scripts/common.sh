# Helpers shared between the scripts in this directory.  This file is meant to
# be sourced (with the current directory at the root of the Merlin subtree),
# not executed.

# Succeeds if the merlin-exclude gitattribute is set for the file named $2 in
# directory $1. The attributes are declared in
# upstream/ocaml_flambda/.gitattributes; since git check-attr resolves paths
# relative to the current directory, this must be called with the current
# directory at upstream/ocaml_flambda.
function is-excluded () {
  [[ "$(git check-attr merlin-exclude -- "$1/$2")" == *': merlin-exclude: set' ]]
}
