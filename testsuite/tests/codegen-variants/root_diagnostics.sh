#!/bin/sh

# usage: sh root_diagnostics.sh <library tree> <compiler> <argument>...
#
# <library tree> is the tree that serves the codegen variant that OCAMLPARAM
# selects, and OCAMLLIB the library root of the installation. When the tree
# is a variant's own (root/variants/<name>) and the variant differs from
# base by more than poll insertion, tries three links that must fail
# and writes what their errors say to the file diagnostics.output; see
# root_diagnostics.ml.

tree=$1
shift
case $tree in
  */variants/*) ;;
  *) exit 0 ;;
esac
root=$OCAMLLIB

# A unit compiled for the base variant, and the same for the selected
# one. The errors are about frame pointers, stack checks and
# AddressSanitizer: a variant with the default settings for these, whose
# object files refer to the same link-time guard, has none to show.
guard_of() {
  nm "$1" | grep -o 'caml_runtime_codegen_variant_is_[a-z_]*' | sort -u
}
( unset OCAMLPARAM; "$@" -c link_guard_unit.ml ) || exit 2
"$@" -c -o selected_unit.cmx link_guard_unit.ml || exit 2
if [ "$(guard_of link_guard_unit.o)" = "$(guard_of selected_unit.o)" ]; then
  exit 0
fi

# Both variants of the test can get here.
rm -rf bare flavor

describe() {
  text=$(tr '\n' ' ' < "$1" | sed -e 's/  */ /g')
  case $text in
    *"was compiled with codegen configuration"*)
      echo "  the error is about a file compiled for another variant" ;;
    *"does not provide codegen variant"*)
      echo "  the error is about a variant that the installation lacks" ;;
    *"Cannot find file"*libasmrunlocal_*)
      echo "  the error is about the runtime library of the variant" ;;
    *)
      echo "  the error is about something else: $text" ;;
  esac
  case $text in
    *"does not contain codegen variant"*)
      echo "  it notes that the root lacks the variant" ;;
    *)
      echo "  it says nothing of the root" ;;
  esac
}

refused() {
  description=$1
  log=$2
  shift 2
  echo "$description:"
  if "$@" > "$log" 2>&1; then
    echo "  not refused"
  else
    describe "$log"
  fi
}

{
  # The unit compiled for the base variant, linked for the selected one.
  # (In a subshell: whether an assignment in front of a function call
  # outlives the call depends on the shell.)
  ( OCAMLLIB=$tree
    refused "the root is the tree of the variant" tree.log \
      "$@" link_guard_unit.cmx -o tree.exe )
  mkdir bare
  for file in "$root"/*; do
    case $(basename "$file") in
      variants) ;;
      *) ln -s "$file" bare/ ;;
    esac
  done
  ( OCAMLLIB=$PWD/bare
    refused "the root has no tree for the variant" bare.log \
      "$@" link_guard_unit.cmx -o bare.exe )
  # A runtime library that only exists for the base variant.
  mkdir flavor
  cp "$root"/libasmrun.a flavor/libasmrunlocal.a
  refused "a runtime library of the base variant alone" flavor.log \
    "$@" -I flavor -runtime-variant local root_diagnostics.ml -o flavor.exe
} > diagnostics.output 2>&1
