#!/usr/bin/env bash
# Build the AST-dependent libraries (ppxlib, js_of_ocaml and their
# dependencies, see the top-level Makefile) against a staging install of the
# compiler, so that their build overlaps with the rest of `make compiler`.
#
# Run by the jsoo-early rule of the root dune file from the main build
# context (_build/main) once the compiler libraries exist. The staging
# install, _build/_early_install, mirrors what `make _install` assembles,
# restricted to what those libraries need: the boot compiler binaries (the
# main build context compiles with them too), the runtime and stdlib, the
# compiler libraries without the native optcomp archives, and unix. The
# interface files are exactly those the real install ships, as the
# ocaml-compiler-libs shims are generated from the interfaces present.
# `make jsoo-install-shipped WITH_JSOO=1` later installs from the same build.
set -euo pipefail

die () { echo "jsoo-early.sh: $*" >&2; exit 1; }

# Dune runs this from the main build context, <root>/_build/main, or from a
# sandbox copy of it. The dependencies of the rule only order this script
# after the files it needs; they are read from the real build directory.
case $PWD in
  */_build/.sandbox/*/main) root=${PWD%/_build/.sandbox/*} ;;
  */_build/main) root=${PWD%/_build/main} ;;
  *) die "unexpected working directory $PWD" ;;
esac
build=$root/_build/main
stage=$root/_build/_early_install
libdir=$stage/lib/ocaml

# Hard links where possible, as in `make _install`.
link () { cp -l "$@" 2>/dev/null || cp "$@"; }

[ -d "$root/_build/_bootinstall/bin" ] || die "no boot compiler in $root/_build"
[ -d "$root/_build/runtime_stdlib_install" ] \
  || die "no runtime/stdlib in $root/_build"
ocaml_toplevel=$(command -v ocaml) || die "no ocaml toplevel on PATH"

rm -rf "$stage"
mkdir -p "$stage/bin" "$libdir/compiler-libs" "$libdir/unix" "$libdir/stublibs"

# bin: the boot compiler and ocamlrun, plus a toplevel: the builds run a few
# ocaml scripts, and dune only looks for ocaml next to ocamlc. The compiler's
# own toplevel is built late, so this is the system one, which must not see
# this install's stdlib.
for tool in "$root"/_build/_bootinstall/bin/*; do
  ln -s "$tool" "$stage/bin/$(basename "$tool")"
done
link "$root"/_build/runtime_stdlib_install/bin/* "$stage/bin/"
cat > "$stage/bin/ocaml" <<EOF
#!/bin/sh
exec env -u OCAMLLIB -u OCAMLPARAM "$ocaml_toplevel" "\$@"
EOF
chmod +x "$stage/bin/ocaml"

# lib/ocaml: the runtime and stdlib, as in `make _install`. The dynlink
# placeholders stay: dune decides whether native dynlink is supported from
# dynlink.cmxa, and the real dynlink library is not needed here.
link -R "$root"/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib/* \
  "$libdir/"
rm -f "$libdir"/{META,dune-package}

# compiler-libs: the archives, the native artifacts of the libraries linked
# here, and the bytecode artifacts the install rules list (the root dune
# file, the flambda2 interfaces as in `make _install`, toplevel/byte/dune).
for lib in ocamlcommon ocamlfrontend ocamlbytecomp; do
  link "$build/$lib".{cma,cmxa,a} "$libdir/compiler-libs/"
  link "$build/.$lib.objs/native/"*.cmx "$libdir/compiler-libs/"
done
link "$build/toplevel/byte/ocamltoplevel.cma" "$libdir/compiler-libs/"
link "$build/ocamloptcomp_with_flambda2.cma" \
  "$libdir/compiler-libs/ocamloptcomp.cma"
link "$build/compilerlibs/META" "$libdir/compiler-libs/"
byte_sexp=$build/compiler-libs-installation-byte.sexp
sed -n 's/^(\(.*\) as \(.*\))$/\1 \2/p' "$byte_sexp" |
while read -r src dst; do
  mkdir -p "$libdir/$(dirname "$dst")"
  link "$build/$src" "$libdir/$dst"
done
find "$build/middle_end/flambda2" -name 'flambda2*.cmi' -print0 |
while IFS= read -r -d '' cmi; do
  [ -e "$libdir/compiler-libs/$(basename "$cmi")" ] \
    || link "$cmi" "$libdir/compiler-libs/"
done
for m in genprintval trace topdirs toploop topmain; do
  link "$build/toplevel/byte/$m.mli" "$libdir/compiler-libs/"
  link "$build/toplevel/byte/.ocamltoplevel.objs/byte/$m".{cmi,cmt,cmti} \
    "$libdir/compiler-libs/"
done
link "$build"/toplevel/.expunge.eobjs/byte/expunge.cmi \
  "$build"/toplevel/.topstart.eobjs/byte/topstart.cmi \
  "$build"/toplevel/native/.ocamlopttoplevel.objs/byte/opttop{dirs,loop}.cmi \
  "$libdir/compiler-libs/"

# unix, as installed by otherlibs/unix/dune.
link "$build"/otherlibs/unix/{META,unix.cma,unix.cmxa,unix.a,libunix_stubs.a} \
  "$build"/otherlibs/unix/{unix.mli,unixLabels.mli} "$libdir/unix/"
link "$build"/otherlibs/unix/.unix.objs/byte/*.{cmi,cmt,cmti} "$libdir/unix/"
link "$build"/otherlibs/unix/.unix.objs/native/*.cmx "$libdir/unix/"
link "$build"/otherlibs/unix/dllunix_stubs.so "$libdir/stublibs/"

# A sub-make of the one that runs dune: drop its jobserver flags, which also
# drops its SHELL override, so pass this shell (the Makefile's default,
# /usr/bin/env bash, does not exist in the nix build sandbox).
cd "$root"
exec env -u MAKEFLAGS -u MFLAGS -u MAKELEVEL \
  "${MAKE:-make}" -s SHELL="$BASH" jsoo-build OXCAML_INSTALL="$stage"
