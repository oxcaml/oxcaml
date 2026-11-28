#!/bin/sh

# usage: sh root_lookups.sh <library root> <variant>
#
# Makes a library root (see library_root.sh) in which the root and
# the tree of <variant> hold different files under the same names, and
# prints, for each way to name such a file, which of the two the compiler
# takes when it compiles for <variant>.  When <variant> is base,
# there is only one tree, and its files are the variant's.  What the
# compilers say is shown only if one of them fails where it should not.

set -eu

. "$(dirname "$0")/library_root.sh"
make_library_root "$1" "$2"
here=$(pwd -P)
default=$here/root
selected=$here/$selected
log=$here/compilers.log
failed=false
trap '$failed && cat "$log" >&2' EXIT

# Every axis is stated, so that nothing depends on the base variant or
# on the one that the testsuite runs under.
axis () { # <token> <flag when on> <flag when off>
  case -$variant- in *-$1-*) echo "$2";; *) echo "$3";; esac
}
variant=$2
flags="$(axis fp -frame-pointers -no-frame-pointers) \
  $(axis sc -stack-checks -no-stack-checks) $(axis asan -asan -no-asan) \
  $(axis poll -enable-poll-insertion -disable-poll-insertion)"
ocamlopt () {
  echo "+ ocamlopt $*" >> "$log"
  "$ocamlsrcdir/ocamlopt.opt" $flags "$@" >> "$log" 2>&1
}

# trees <command> <argument>...: runs the command for the root and
# then for the tree of the variant, with the tree and what a file of it
# says of itself as the first two arguments.
trees () {
  if [ "$selected" != "$default" ]; then "$@" "$default" "the root"; fi
  "$@" "$selected" "the variant's tree"
}

# The files of the trees, compiled outside them.
mkdir build out
OCAMLLIB=$default; export OCAMLLIB
unit () { # <unit> <tree> <text>: <tree>/pkg/<unit>.cmx and so on
  rm -rf build/unit; mkdir -p build/unit "$2/pkg"
  echo "let () = print_endline \"$3\"" > "build/unit/$1.ml"
  (cd build/unit && ocamlopt -c "$1.ml" \
   && ocamlopt -a -o "$1_lib.cmxa" "$1.cmx")
  # By name: a compiler may leave other things next to its outputs.
  for file in "$1.cmi" "$1.cmx" "$1.o" "$1_lib.cmxa" "$1_lib.a"; do
    cp "build/unit/$file" "$2/pkg/"
  done
}
c_object () { # <tree> <text>: <tree>/probe.o, for probe_user.ml
  if [ "$2" = "the variant's tree" ]; then value=3; else value=1; fi
  printf 'long probe(long unit) { (void)unit; return %s; }\n' $value \
    > build/probe.c
  ${cc} -c build/probe.c -o "$1/probe.o"
}
# What own.ml would compile to, from another source.
stale_own () { # <tree> <text>
  if [ "$1" != "$default" ]; then
    rm -rf build/own; mkdir build/own
    echo 'let () = print_endline "a file of the installation"' \
      > build/own/own.ml
    (cd build/own && ocamlopt -c own.ml)
    cp build/own/own.cmx build/own/own.o "$1/"
  fi
}
trees unit marker
trees c_object
trees stale_own
echo 'let () = print_endline "compiled by this invocation"' > "$default/own.ml"
echo 'let () = print_endline "linked"' > out/plain.ml
cat > out/probe_user.ml <<'EOT'
external probe : unit -> bool = "probe"
let () =
  print_endline (if probe () then "the variant's tree" else "the root")
EOT
(cd out && ocamlopt -c plain.ml probe_user.ml)

# from_root <description> <argument>...: links a program with the library
# root as the current directory, and runs it.
from_root () {
  description=$1; shift
  rm -f "$here/program.exe"
  if (cd "$default" && ocamlopt "$@" -o "$here/program.exe") \
     && result=$("$here/program.exe" 2>> "$log"); then
    echo "  $description: $result"
  else
    failed=true; echo "  $description: failed"
  fi
}
matrix () { # <options>...
  from_root "a unit compiled elsewhere" "$@" "$here/out/plain.cmx"
  from_root "pkg/marker.cmx" "$@" pkg/marker.cmx
  from_root "./pkg/marker.cmx" "$@" ./pkg/marker.cmx
  from_root "pkg/marker_lib.cmxa" "$@" -linkall pkg/marker_lib.cmxa
  from_root "probe.o" "$@" probe.o "$here/out/probe_user.cmx"
  from_root "own.ml" "$@" own.ml
  from_root "./own.ml" "$@" ./own.ml
  rm -f "$here/own_lib.cmxa" "$here/own_lib.a"
  (cd "$default" && ocamlopt "$@" -a own.ml -o "$here/own_lib.cmxa") || :
  from_root "own.ml, through an archive" "$@" -linkall "$here/own_lib.cmxa"
}
echo 'The library root is the current directory, and OCAMLLIB is ".":'
OCAMLLIB=.; matrix
echo 'The same with -nocwd:'
matrix -nocwd
echo 'The library root is the current directory, and OCAMLLIB its path:'
OCAMLLIB=$default; matrix
echo 'The same with -nocwd:'
matrix -nocwd

# from_here <description> <argument>...: the same from this directory.
from_here () {
  description=$1; shift
  rm -f program.exe
  if ocamlopt "$@" -o program.exe && result=$(./program.exe 2>> "$log"); then
    echo "$description: $result"
  else
    failed=true; echo "$description: failed"
  fi
}

# An interface that -open-cmi-x names, next to a directory given to -H.
interface () { # <tree> <text>
  rm -rf build/order; mkdir -p build/order "$1/order"
  if [ "$2" = "the variant's tree" ]; then
    echo 'type t = Variant_tree | Default_tree' > build/order/order.ml
  else
    echo 'type t = Default_tree | Variant_tree' > build/order/order.ml
  fi
  (cd build/order && ocamlopt -c order.ml)
  cp build/order/order.cm* "$1/order/"
}
trees interface
cat > out/first.ml <<'EOT'
let () =
  print_endline
    (if Variant_tree < Default_tree
     then "the variant's tree"
     else "the root")
EOT
from_here "An interface given to -open-cmi-x" -H "$default/order" \
  -open-cmi-x "$default/order/order.cmi" out/first.ml

# A file that is merely named like a source of the invocation is not a
# file that the invocation wrote: a C object and a .cmx file, given to the
# link next to an interface of the same name that the command compiles.
trees unit named
echo 'val unused : unit' > "$default/probe.mli"
: > "$default/pkg/named.mli"
from_here "A C object named like an interface that the command compiles" \
  "$default/probe.mli" "$default/probe.o" out/probe_user.cmx
from_here "A .cmx file named like an interface that the command compiles" \
  "$default/pkg/named.mli" "$default/pkg/named.cmx"

# A manifest inside the root, named by a manifest outside it: each tree's
# copy names another unit.
MANIFEST_FILES_ROOT=/; export MANIFEST_FILES_ROOT
manifest () { # <tree> <text>
  dir=build/which-$(basename "$1"); mkdir -p "$dir" "$1/manifests"
  echo "let message = \"$2\"" > "$dir/which.ml"
  (cd "$dir" && ocamlopt -c which.ml)
  for file in which.cmi which.cmx; do
    echo "file $file ${here#/}/$dir/$file"
  done > "$1/manifests/inner.manifest"
}
trees manifest
echo "manifest inner ${default#/}/manifests/inner.manifest" > outer.manifest
echo 'let () = print_endline Which.message' > out/which_user.ml
from_here "A manifest inside the root" -I-manifest "$here/outer.manifest" \
  which.cmx out/which_user.ml

# An interface that the invocation compiles, and that the next unit reads
# through a manifest: the tree of the variant has another interface of
# that name.
signature () { # <tree> <text>
  if [ "$1" != "$default" ]; then
    mkdir build/declared
    echo 'val installed : string' > build/declared/declared.mli
    (cd build/declared && ocamlopt -c declared.mli)
    cp build/declared/declared.cmi "$1/"
  fi
}
trees signature
echo 'val written : string' > "$default/declared.mli"
echo "file declared.cmi ${default#/}/declared.cmi" > declared.manifest
echo 'let text () = Declared.written' > out/declared_user.ml
if ocamlopt -c -I-manifest "$here/declared.manifest" "$default/declared.mli" \
     out/declared_user.ml; then
  echo "An interface of the invocation that a manifest names: as written"
else
  failed=true
  echo "An interface of the invocation that a manifest names: failed"
fi

# The interface file of a unit is the invocation's own when the invocation
# wrote it, and only then.  Compiling a unit writes one unless the unit has
# a .mli file next to it or -cmi-file names its interface.  So a manifest
# that names the file inside the root leads to the one just written in the
# first case, and to the installation's in the two others: there the
# variant's tree has an interface that the root's does not match.
# Whether the unit has an interface file is settled before it is typed:
# with -intf-suffix .cmi, the file that typing writes has the very name
# of an interface file of the unit, and is still a file that the command
# wrote.  (That unit has an interface in the variant's tree only, when the
# variant has a tree of its own: next to the source, one would be the
# unit's interface file.)
interfaces () { # <tree> <text>
  dir=build/noted-$(basename "$1"); mkdir "$dir"
  if [ "$1" = "$selected" ]; then kind=installed; else kind=stale; fi
  for unit in noted_own noted_mli noted_cmi noted_suffix; do
    if [ $unit != noted_suffix ] || [ "$1" != "$default" ]; then
      echo "type $kind = unit" > "$dir/$unit.mli"
      (cd "$dir" && ocamlopt -c $unit.mli)
      cp "$dir/$unit.cmi" "$1/"
    fi
  done
}
trees interfaces
for unit in noted_own noted_mli noted_cmi noted_suffix; do
  echo "file $unit.cmi ${default#/}/$unit.cmi"
done > noted.manifest
echo 'type written = unit' > "$default/noted_own.ml"
echo 'type installed = unit' > "$default/noted_mli.ml"
cp "$default/noted_mli.ml" "$default/noted_mli.mli"
echo 'type installed = unit' > "$default/noted_cmi.ml"
echo 'type written = unit' > "$default/noted_suffix.ml"
echo 'val v : Noted_own.written' > out/reads_own.mli
echo 'val v : Noted_mli.installed' > out/reads_mli.mli
echo 'val v : Noted_cmi.installed' > out/reads_cmi.mli
echo 'val v : Noted_suffix.written' > out/reads_suffix.mli
noted () { # <description> <answer> <argument>...
  description=$1; answer=$2; shift 2
  if ocamlopt -c -I-manifest "$here/noted.manifest" "$@"; then
    echo "$description: $answer"
  else
    failed=true; echo "$description: failed"
  fi
}
noted "The interface that compiling a unit wrote, named by a manifest" \
  "as written" "$default/noted_own.ml" out/reads_own.mli
noted "The same for a unit with a .mli file, which writes none" \
  "the installation's" "$default/noted_mli.ml" out/reads_mli.mli
noted "The same for a unit compiled with -cmi-file" "the installation's" \
  -cmi-file "build/noted-$(basename "$selected")/noted_cmi.cmi" \
  "$default/noted_cmi.ml" out/reads_cmi.mli
noted "The interface that it wrote under the name of an interface file" \
  "as written" -intf-suffix .cmi "$default/noted_suffix.ml" \
  -intf out/reads_suffix.mli

# Two sources in a directory of the root that -I names, the second using
# the first.  The directory stands for the one of the variant's tree,
# which holds an installed unit of that name: the second source is
# compiled against that one, and the link, which takes the unit that the
# invocation compiled, refuses the two.  Sources that depend on each other
# are to be compiled outside the library root, or for its root.
installed () { # <tree> <text>
  if [ "$1" != "$default" ]; then
    mkdir build/prior "$1/lib"
    echo 'let message = "a file of the installation"' > build/prior/prior.ml
    (cd build/prior && ocamlopt -c prior.ml)
    cp build/prior/prior.cmi build/prior/prior.cmx build/prior/prior.o \
      "$1/lib/"
  fi
}
mkdir "$default/lib"
trees installed
echo 'let message = "compiled together"' > "$default/lib/prior.ml"
echo 'let () = print_endline Prior.message' > "$default/lib/later.ml"
rm -f program.exe
if "$ocamlsrcdir/ocamlopt.opt" $flags -I "$default/lib" \
     "$default/lib/prior.ml" "$default/lib/later.ml" -o program.exe \
     > together.log 2>&1; then
  together=$(./program.exe)
elif grep -q 'make inconsistent assumptions over .* Prior' together.log; then
  together=refused
else
  together=$(cat together.log)
fi
if [ "$selected" = "$default" ]; then wanted="compiled together"
else wanted=refused; fi
if [ "$together" = "$wanted" ]; then
  echo "Two dependent sources in a directory of the root: only for the" \
    "variant of the root"
else
  failed=true
  echo "Two dependent sources in a directory of the root: $together"
fi

# The same from inside the root, which stands for the tree of the variant
# as a whole: what a command leaves in the root is not found there, not by
# a later source of the same command and not under its bare name by a
# later command.  A file that a link, -a or -pack is given by its path is
# found.
echo 'let v = 1' > "$default/earlier.ml"
echo 'let () = print_int Earlier.v' > "$default/uses_earlier.ml"
inside () { # <argument>...: runs ocamlopt in the root and says how it ended
  if (cd "$default" && "$ocamlsrcdir/ocamlopt.opt" $flags "$@") \
       > inside.log 2>&1; then
    echo done
  elif grep -q 'Unbound module Earlier' inside.log; then
    echo "not found"
  elif grep -q 'File earlier.cmx not found' inside.log; then
    echo "not found"
  else
    cat inside.log
  fi
}
only_default () { # <description> <outcome>
  if [ "$selected" = "$default" ]; then wanted=done; else wanted="not found"; fi
  if [ "$2" = "$wanted" ]; then
    echo "$1: only for the variant of the root"
  else
    failed=true; echo "$1: $2"
  fi
}
only_default "Two dependent sources compiled inside the root" \
  "$(inside -c earlier.ml uses_earlier.ml)"
inside -for-pack Packed -c earlier.ml > /dev/null
only_default "A file left in the root, given to -pack by its bare name" \
  "$(inside -pack earlier.cmx -o "$here/packed.cmx")"
echo "The same file, given to -pack by its path:" \
  "$(inside -pack ./earlier.cmx -o "$here/packed.cmx")"

# A unit that the invocation compiles, and that -Hx-file names: the source
# by a relative path, the files by absolute ones.  The units are those of
# ../dash-Hx; the tree of the variant has another implementation of C.
hidden () { # <tree> <text>
  rm -rf build/hx; mkdir build/hx "$1/hx"
  cp -r "$(dirname "$0")/../dash-Hx/libb" "$(dirname "$0")/../dash-Hx/libc" \
    build/hx/
  if [ "$1" != "$default" ]; then
    echo 'let installed = ()' >> build/hx/libc/c.ml
  fi
  (cd build/hx \
   && for unit in libc/c.mli libc/c.ml libb/b_alias.mli libb/b_alias.ml; do
        ocamlopt -nocwd -Ix libc -Ix libb -extension layout_poly_alpha -c $unit
      done)
  cp -r build/hx/libb build/hx/libc "$1/hx/"
}
trees hidden
cat > out/pick.ml <<'EOT'
let () =
  ignore (B_alias.M.id 42);
  print_endline "the .cmx file that it wrote"
EOT
MANIFEST_FILES_ROOT=$here
from_here "A unit of the invocation that -Hx-file names" -nocwd \
  -extension layout_poly_alpha -Ix root/hx/libb \
  -Hx-file c.cmi:root/hx/libc/c.cmi -Hx-file c.cmx:root/hx/libc/c.cmx \
  ./root/hx/libc/c.ml ./out/pick.ml

# An archive named like a source of the invocation is an archive like any
# other: it is searched for, and the invocation did not write it.  No
# library root is involved.
OCAMLLIB=$default
mkdir build/named build/named/libdir
echo 'let () = print_string "the one on the load path"' \
  > build/named/libdir/banner.ml
echo 'let () = print_string "the one in the current directory"' \
  > build/named/banner.ml
echo 'let () = print_newline ()' > build/named/app.ml
(cd build/named/libdir && ocamlopt -c banner.ml \
 && ocamlopt -a banner.cmx -o app.cmxa)
named () { # <description> <argument>...
  description=$1; shift
  rm -f program.exe
  if (cd build/named \
      && ocamlopt -linkall -I libdir "$@" app.cmxa ./app.ml \
           -o "$here/program.exe") \
     && result=$(./program.exe 2>> "$log"); then
    echo "$description: $result"
  else
    failed=true; echo "$description: failed"
  fi
}
named "An archive named like a source of the invocation"
named "The same with -nocwd" -nocwd
(cd build/named && ocamlopt -c banner.ml && ocamlopt -a banner.cmx -o app.cmxa)
named "With an archive of that name in the current directory"
named "The same with -nocwd" -nocwd

# A file that the invocation wrote is used without a search only where its
# directory stands for another one.  Everywhere else the search is that of
# a compiler without variants: under -nocwd, the unit that -I leads to is
# linked, and not the source of the same name that the command compiles.
# That holds outside the root, in a directory of the root that the
# variant's tree does not have, and inside the variant's tree.
mkdir -p build/choice elsewhere "$default/noother" "$selected/ours"
echo 'let () = print_endline "the unit that -I leads to"' \
  > build/choice/choice.ml
(cd build/choice && ocamlopt -c choice.ml)
choice () { # <description> <directory>
  echo 'let () = print_endline "the source just compiled"' > "$2/choice.ml"
  rm -f program.exe
  if (cd "$2" && ocamlopt -nocwd -I "$here/build/choice" choice.ml \
        -o "$here/program.exe") \
     && result=$(./program.exe 2>> "$log"); then
    echo "$1: $result"
  else
    failed=true; echo "$1: failed"
  fi
}
choice "A source beside a unit of its name on the load path, with -nocwd" \
  "$here/elsewhere"
choice "The same in a directory of the root that the variant's tree lacks" \
  "$default/noother"
choice "The same inside the variant's tree" "$selected/ours"
