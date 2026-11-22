#!/bin/sh

# usage: sh poll_tree_pairs.sh
#
# For every two library trees of the installation under test (those under
# $ocamlsrcdir/stdlib, the default library root) whose codegen variants
# differ only in poll insertion, checks that the two are the same libraries
# but for the polls in their code:
# - they have the same .cmx and .cmxa files, archives and object files (a
#   file that only one tree has is a difference, not a file to leave out),
#   but for the archive of cached generic functions, which is generated
#   for one tree at a time and which an installation need not have;
# - every unit has the same implementation CRC in both;
# - every archive and object file defines the same symbols in both.
# One difference is intended and checked for itself: every object file of a
# unit of the tree without polls refers to caml_code_not_fully_polled, and
# no object file of a unit of the tree with polls does. A reference is not
# a definition, so it is not among the symbols compared.
# Prints what differs, and "ok" when nothing does. A tool that fails is a
# difference too.

root=$ocamlsrcdir/stdlib
ocamlopt="$ocamlrun $ocamlopt_byte"
objinfo="$ocamlrun $ocamlsrcdir/tools/ocamlobjinfo"
OCAMLLIB=$root
LC_ALL=C
export OCAMLLIB LC_ALL

# The name of the default variant, whose tree is the root itself: the
# tokens of its enabled axes, sorted.
config () {
  $ocamlopt -config-var "$1"
}
default=
[ "$(config with_address_sanitizer)" = true ] && default=$default-asan
[ "$(config with_frame_pointers)" = true ] && default=$default-fp
[ "$(config poll_insertion)" = true ] && default=$default-poll
[ "$(config no_stack_checks)" = false ] && default=$default-sc
default=${default#-}
default=${default:-base}

# usage: tree <variant>
# The library tree of the variant, if the installation has one.
tree () {
  if [ "$1" = "$default" ]; then
    echo "$root"
  elif [ -d "$root/variants/$1" ]; then
    echo "$root/variants/$1"
  fi
}

# usage: files <tree> <find expression>
# The files of the tree that match, but for the trees of other variants and
# for the archive of cached generic functions.
files () {
  directory=$1
  shift
  (cd "$directory" \
    && find . \( -name variants -o -name cached-generic-functions.a \) \
         -prune -o \( "$@" \) -print \
    | sort)
}

# usage: crcs <tree> <list of files>
# The units of the .cmx and .cmxa files of the list and their
# implementation CRCs.
crcs () {
  grep -E '\.cmxa?$' "$2" > objinfo.files
  (cd "$1" && xargs $objinfo -no-approx -no-code) < objinfo.files \
    > objinfo.out || return 1
  grep -E '^(File |Name: |CRC of implementation: )' objinfo.out
}

# usage: symbols <tree> <list of files>
# The symbols that each archive and object file of the list defines,
# without the addresses.
symbols () {
  grep -E '\.(a|o)$' "$2" | while read -r file; do
    echo "File $file"
    nm -g "$1/$file" > nm.out || exit 1
    awk 'NF == 3 { print $3 }' nm.out | sort
  done
}

# usage: marks <tree> <list of files>
# For every object file of a unit of the list, whether it refers to the
# symbol of the object files without polls: "<object file> yes" or "no".
# The object file of a unit is the companion of its .cmx file, or a member
# of the companion of its .cmxa file: the archives of C code and of
# generated functions have no .cmxa file and are not looked at. (A file
# that nm cannot read was reported by [symbols].)
marks () {
  grep -E '\.cmxa?$' "$2" | while read -r file; do
    case $file in
      *.cmxa)
        archive=${file%.cmxa}.a
        [ -f "$1/$archive" ] || continue
        nm -u "$1/$archive" 2> /dev/null | awk -v archive="$archive" '
          function show() {
            if (member != "") print archive "(" member ") " found
          }
          /:$/ { show(); member = $0; sub(/:$/, "", member); found = "no" }
          /caml_code_not_fully_polled$/ { found = "yes" }
          END { show() }';;
      *.cmx)
        object=${file%.cmx}.o
        [ -f "$1/$object" ] || continue
        if nm -u "$1/$object" 2> /dev/null \
             | grep -q 'caml_code_not_fully_polled$'
        then echo "$object yes"
        else echo "$object no"
        fi;;
    esac
  done
}

# usage: differ <description>
differ () {
  status=failed
  echo "The library trees of $with and $without $1"
}

status=ok
for with in $default $(config codegen_variants); do
  case -$with- in
    *-poll-*) ;;
    *) continue;;
  esac
  without=$(echo "-$with-" | sed -e 's/-poll-/-/' -e 's/^-//' -e 's/-$//')
  without=${without:-base}
  with_tree=$(tree "$with")
  without_tree=$(tree "$without")
  if [ -z "$with_tree" ] || [ -z "$without_tree" ]; then continue; fi
  echo "$with $without" >> pairs
  files "$with_tree" -name '*.cmx' -o -name '*.cmxa' -o -name '*.a' \
    -o -name '*.o' > "files.$with"
  files "$without_tree" -name '*.cmx' -o -name '*.cmxa' -o -name '*.a' \
    -o -name '*.o' > "files.$without"
  comm -23 "files.$with" "files.$without" > "only.$with"
  comm -13 "files.$with" "files.$without" > "only.$without"
  for name in $with $without; do
    if [ -s "only.$name" ]; then
      differ "do not have the same files: only that of $name has"
      head -n 40 "only.$name"
    fi
  done
  # What follows is about the files that both trees have.
  comm -12 "files.$with" "files.$without" > files.common
  for what in crcs symbols; do
    for name in $with $without; do
      if ! $what "$(tree "$name")" files.common > "$what.$name"; then
        differ "could not be read for their $what: see the tree of $name"
      fi
    done
    if ! cmp -s "$what.$with" "$what.$without"; then
      differ "differ in $what:"
      diff "$what.$with" "$what.$without" | head -n 40
    fi
  done
  marks "$with_tree" files.common > "marks.$with"
  marks "$without_tree" files.common > "marks.$without"
  if grep -q ' yes$' "marks.$with"; then
    differ "are not what they say: object files of $with have no polls:"
    grep ' yes$' "marks.$with" | head -n 20
  fi
  if grep -q ' no$' "marks.$without" || ! [ -s "marks.$without" ]; then
    differ "are not what they say: object files of $without do not say that \
they have no polls:"
    grep ' no$' "marks.$without" | head -n 20
  fi
done
echo $status
