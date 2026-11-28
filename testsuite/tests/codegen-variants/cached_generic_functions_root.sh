#!/bin/sh

# usage: sh cached_generic_functions_root.sh root <library root> <copy>
#        sh cached_generic_functions_root.sh links <directory of the
#          generator> <compiler> <argument>...
#        sh cached_generic_functions_root.sh direct <library tree>
#          <directory of the generator> <compiler> <argument>...
#
# "root" makes a library root that the test can write to: a directory of
# symbolic links to the files of an installation's library root and of its
# variant trees, without their archives of cached generic functions.
#
# "links" runs, with that root as OCAMLLIB and for the codegen variant that
# OCAMLPARAM selects, the links described in cached_generic_functions_root.ml.
#
# "direct" does the first of them with a copy of <library tree> itself as
# OCAMLLIB, when that tree is a variant's own, and writes what happens to
# the file direct.output. When that variant differs from base by more than
# poll insertion, it also tries links whose library root has no
# place for the archive, and one whose root cannot be judged, and writes
# what happens to the file roots.output.

archive=cached-generic-functions.a
cached=-use-cached-generic-functions
source=cached_generic_functions_root.ml

link_tree() {
  mkdir -p "$2"
  for file in "$1"/*; do
    case $(basename "$file") in
      variants | "$archive") ;;
      *) ln -s "$file" "$2/" ;;
    esac
  done
}

# The link fails for want of the archive; the command that the error gives
# is run as it is printed, in another environment than the link: one whose
# OCAMLPARAM selects the other frame-pointer setting than the variant that
# the command names, and the LLVM backend. The generator must follow
# neither, or the archive is not the one that was missing.
recover() {
  if "$@" $cached $source -o missing.exe > missing.log 2>&1; then
    echo "linked without an archive"
    return 1
  fi
  command=$(tr '\n' ' ' < missing.log | sed -e 's/  */ /g' \
              | sed -n -e 's/.*generate it with \(.*\) or drop.*/\1/p')
  case $command in
    generate_cached_generic_functions.exe*) ;;
    *) echo "no command in the error:"; cat missing.log; return 1 ;;
  esac
  # The same link with a missing archive that is named: the archive of
  # the library root, which for a variant with a tree of its own is looked
  # up in that tree first, where the command above would write it; and a
  # file elsewhere, with another extension than that of archives. The error
  # names every file that was looked up, and gives no command: a command
  # for the first would write the archive of this variant in the place of
  # the one of base, and the generator would name the second otherwise.
  path=$(echo "$command" | sed -n -e "s/.* '\\(.*\\)' *\$/\\1/p")
  # (The root may have its archive by now, from the same links for base:
  # it is set aside for this.)
  if [ -e "$OCAMLLIB/$archive" ]; then
    mv "$OCAMLLIB/$archive" root-archive.aside
  fi
  named_missing "$OCAMLLIB/$archive" "$path" "$@"
  status=$?
  if [ -e root-archive.aside ]; then
    mv root-archive.aside "$OCAMLLIB/$archive"
  fi
  [ $status -eq 0 ] || return 1
  named_missing elsewhere/cache.custom elsewhere/cache.custom "$@" \
    || return 1
  echo "a missing archive that is named is reported without a command"
  named=$(echo "$command" | sed -n -e 's/.* -codegen-variant \([^ ]*\) .*/\1/p')
  case "-$named-" in
    *-fp-*) other=0 ;;
    *) other=1 ;;
  esac
  if OCAMLPARAM="_,frame-pointers=$other,llvm-backend=1" sh -c "$command" \
       > generate.log 2>&1; then
    echo "the command of the error message generates the archive"
  else
    echo "the command of the error message fails: $command"
    cat generate.log
    return 1
  fi
}

# named_missing <name> <file> <compiler>...: a link with the archive named
# <name>, which does not exist, must fail with an error that gives no
# command and names <name> and <file>.
named_missing() {
  name=$1
  file=$2
  shift 2
  if "$@" $cached -cached-generic-functions-path "$name" $source \
       -o named.exe > named.log 2>&1; then
    echo "linked without the archive that is named ($name)"
    return 1
  fi
  case $(tr '\n' ' ' < named.log | sed -e 's/  */ /g') in
    *"generate it with"*)
      echo "the error for a missing archive that is named gives a command:" ;;
    *"Cannot find"*"$name"*)
      case $(tr '\n' ' ' < named.log | sed -e 's/  */ /g') in
        *"$file"*) return 0 ;;
      esac
      echo "the error for $name does not name $file:" ;;
    *)
      echo "another error for a missing archive that is named ($name):" ;;
  esac
  cat named.log
  return 1
}

# What the error of a link that must fail says of the archive: whether it
# gives a command that generates one, which would write where there is no
# place for the archive of the selected variant.
no_place() {
  description=$1
  expected=$2
  log=$3
  shift 3
  if "$@" > "$log" 2>&1; then
    echo "$description: linked"
    return
  fi
  case $(tr '\n' ' ' < "$log" | sed -e 's/  */ /g') in
    *"generate it with"*)
      echo "$description: the error gives a command" ;;
    *"$expected"*)
      echo "$description: reported as such, without a command" ;;
    *)
      echo "$description: another error:"
      cat "$log" ;;
  esac
}

guard_of() {
  nm "$1" | grep -o 'caml_runtime_codegen_variant_is_[a-z_]*' | sort -u
}

mode=$1
shift
case $mode in
  root)
    link_tree "$1" "$2"
    for tree in "$1"/variants/*; do
      [ -d "$tree" ] && link_tree "$tree" "$2/variants/$(basename "$tree")"
    done
    ;;
  links)
    PATH=$1:$PATH
    shift
    recover "$@" || exit 0
    "$@" $cached $source -o default-path.exe && ./default-path.exe
    "$@" $cached -cached-generic-functions-path "$OCAMLLIB/$archive" \
      $source -o root-path.exe && ./root-path.exe
    ;;
  direct)
    case $1 in
      */variants/*) ;;
      *) exit 0 ;;
    esac
    tree=$1
    default_root=${tree%/variants/*}
    rm -rf direct-root bare-root embedded-root
    link_tree "$tree" direct-root
    link_tree "$default_root" bare-root
    link_tree "$default_root" embedded-root
    OCAMLLIB=$PWD/direct-root
    export OCAMLLIB
    PATH=$2:$PATH
    shift 2
    {
      recover "$@" && "$@" $cached $source -o direct.exe && ./direct.exe
    } > direct.output 2>&1
    # The rest is about frame pointers, stack checks and AddressSanitizer:
    # a variant with the settings of base for these, whose object files
    # refer to the same link-time guard, has nothing to show.
    ( unset OCAMLPARAM; OCAMLLIB=$default_root "$@" -c -o default_unit.cmx \
        $source ) || exit 2
    "$@" -c -o selected_unit.cmx $source || exit 2
    if [ "$(guard_of default_unit.o)" = "$(guard_of selected_unit.o)" ]; then
      exit 0
    fi
    other_poll=-enable-poll-insertion
    {
      # The root is the tree of this variant. For a link for base, with
      # either poll-insertion setting, it stands for the tree of that
      # variant, and its archive, which now exists, is not for it; the
      # libraries are taken from where they are right.
      ( unset OCAMLPARAM
        no_place "a root built for another variant" \
          "is not built for codegen variant" default.log \
          "$@" -nostdlib -I "$default_root" $cached $source -o default.exe
        no_place "the same root, the other poll setting" \
          "is not built for codegen variant" mismatch.log \
          "$@" $other_poll -nostdlib -I "$default_root" $cached $source \
          -o mismatch.exe )
      # A root that has no tree for this variant and is not one.
      ( OCAMLLIB=$PWD/bare-root
        no_place "a variant without a library tree" \
          "has no library tree for codegen variant" bare.log \
          "$@" -nostdlib -I "$tree" $cached $source -o bare.exe )
      # A root of base without the unit info of the standard library, by
      # which a root is judged, as an installation that only links with
      # -output-obj may be. For base nothing says that it is built for
      # another variant, and its archive is used. For base with poll
      # insertion, which the root also stands for, nothing says that it is
      # built for it.
      ( unset OCAMLPARAM
        rm embedded-root/std_exit.cmx
        OCAMLLIB=$PWD/embedded-root
        description="a root without the unit info of its standard library"
        if generate_cached_generic_functions.exe "$OCAMLLIB/$archive" \
             > embedded.log 2>&1 \
           && "$@" -output-obj $cached $source -o embedded.o \
                >> embedded.log 2>&1; then
          echo "$description: its archive is used"
        else
          echo "$description: refused:"
          cat embedded.log
        fi
        no_place "the same root, the other poll setting" \
          "is not built for codegen variant" embedded-poll.log \
          "$@" $other_poll -output-obj $cached $source -o embedded-poll.o )
    } > roots.output 2>&1
    ;;
  *)
    echo "cached_generic_functions_root.sh: unknown mode $mode"
    exit 2
    ;;
esac
