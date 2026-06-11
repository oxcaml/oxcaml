# Helpers shared between the scripts in this directory.  This file is meant to
# be sourced (with the current directory at the root of the Merlin subtree),
# not executed.

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
