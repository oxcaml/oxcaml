#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
install_root="$repo_root/_install"
tmp_install="$repo_root/_install.staging.$$"
bootstrap_bin_default="/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin"
bootstrap_bin="${BOOTSTRAP_BIN:-$bootstrap_bin_default}"
bootstrap_lib="$bootstrap_bin/../lib/ocaml"
local_runtime_lib="$repo_root/_build/install/runtime_stdlib/lib/ocaml_runtime_stdlib"
local_runtime_bin="$repo_root/_build/runtime_stdlib/runtime4"
dune_bin="${DUNE_BIN:-$bootstrap_bin/dune}"

require_file() {
  local path=$1
  if [ ! -e "$path" ]; then
    echo "stage_minimal_5_4_install.sh: missing required artifact: $path" >&2
    exit 1
  fi
}

run_dune() {
  env \
    PATH="$bootstrap_bin:$PATH" \
    RUNTIME_DIR=runtime4 \
    SYSTEM=macosx \
    MODEL=default \
    ASPP='gcc -c -Wno-trigraphs' \
    ASPPFLAGS='' \
    "$dune_bin" "$@"
}

copy_flattened_artifacts() {
  local destination=$1
  shift
  local source
  for source in "$@"; do
    if [ -d "$source" ]; then
      find "$source" -maxdepth 1 -type f \
        \( -name '*.cmi' -o -name '*.cmo' -o -name '*.cmti' -o -name '*.cmt' \) \
        -exec cp -f {} "$destination/" \;
    fi
  done
}

copy_package_dir() {
  local package_name=$1
  local source_dir=$2
  local destination="$tmp_install/lib/ocaml/$package_name"

  mkdir -p "$destination"
  cp -f "$source_dir/META" "$destination/"
  find "$source_dir" -maxdepth 1 -type f \
    \( -name '*.cma' -o -name '*.a' -o -name '*.cmxa' -o -name '*.cmxs' -o -name '*.cmi' \) \
    -exec cp -f {} "$destination/" \;
  find "$source_dir" -maxdepth 1 -type f \
    \( -name 'dll*.so' -o -name 'lib*.a' \) \
    -exec cp -f {} "$destination/" \;
}

echo "[stage] building reduced 5.4 browser toolchain targets"
if [ ! -x "$repo_root/_build/default/main_native.exe" ] \
   || [ ! -x "$repo_root/_build/default/boot_ocamlopt.exe" ] \
   || [ ! -x "$repo_root/_build/default/boot_ocamlj.exe" ] \
   || [ ! -x "$repo_root/_build/default/tools/ocamldep.exe" ] \
   || [ ! -x "$repo_root/_build/default/tools/ocamlmklib.exe" ] \
   || [ ! -x "$repo_root/_build/default/tools/objinfo.exe" ] \
   || [ ! -x "$repo_root/_build/default/ocamlyacc" ]; then
  run_dune build --root=. --workspace=duneconf/boot.ws \
    _build/default/main_native.exe \
    _build/default/boot_ocamlopt.exe \
    _build/default/boot_ocamlj.exe \
    _build/default/tools/ocamldep.exe \
    _build/default/tools/ocamlmklib.exe \
    _build/default/tools/objinfo.exe \
    _build/default/ocamlyacc
fi

if [ ! -f "$repo_root/_build/main/compilerlibs/META" ] \
   || [ ! -f "$repo_root/_build/main/ocamlcommon.cma" ] \
   || [ ! -f "$repo_root/_build/main/ocamlbytecomp.cma" ] \
   || [ ! -f "$repo_root/_build/main/ocamloptcomp.cma" ] \
   || [ ! -f "$repo_root/_build/main/oxcaml_common.cma" ] \
   || [ ! -f "$repo_root/_build/main/utils/oxcaml_utils.cma" ] \
   || [ ! -f "$repo_root/_build/main/toplevel/byte/ocamltoplevel.cma" ] \
   || [ ! -f "$repo_root/_build/main/otherlibs/dynlink/dynlink.cma" ] \
   || [ ! -f "$repo_root/_build/main/otherlibs/str/str.cma" ] \
   || [ ! -f "$repo_root/_build/main/otherlibs/systhreads4/byte/threads.cma" ] \
   || [ ! -f "$repo_root/_build/main/otherlibs/unix/unix.cma" ] \
   || [ ! -f "$repo_root/_build/main/otherlibs/stdlib_stable/stdlib_stable.cma" ]; then
  run_dune build --root=. --workspace=duneconf/main.ws --display=short --only-package=ocaml \
    _build/main/compilerlibs/META \
    _build/main/ocamlcommon.cma \
    _build/main/ocamlbytecomp.cma \
    _build/main/ocamloptcomp.cma \
    _build/main/oxcaml_common.cma \
    _build/main/utils/oxcaml_utils.cma \
    _build/main/toplevel/byte/ocamltoplevel.cma \
    _build/main/otherlibs/dynlink/dynlink.cma \
    _build/main/otherlibs/str/str.cma \
    _build/main/otherlibs/systhreads4/byte/threads.cma \
    _build/main/otherlibs/unix/unix.cma \
    _build/main/otherlibs/stdlib_stable/stdlib_stable.cma
fi

require_file "$repo_root/_build/default/main_native.exe"
require_file "$repo_root/_build/default/boot_ocamlopt.exe"
require_file "$repo_root/_build/default/boot_ocamlj.exe"
require_file "$repo_root/_build/default/tools/ocamldep.exe"
require_file "$repo_root/_build/default/tools/ocamlmklib.exe"
require_file "$repo_root/_build/default/tools/objinfo.exe"
require_file "$repo_root/_build/default/ocamlyacc"
require_file "$repo_root/_build/_bootinstall/bin/ocamllex"
require_file "$bootstrap_bin/ocaml"
require_file "$local_runtime_bin/ocamlrun"
require_file "$local_runtime_bin/ocamlrund"
require_file "$local_runtime_bin/ocamlruni"
require_file "$local_runtime_lib/stdlib.cma"
require_file "$local_runtime_lib/stdlib.cmi"
require_file "$bootstrap_lib/Makefile.config"

rm -rf "$tmp_install"
mkdir -p "$tmp_install/bin" "$tmp_install/lib/ocaml" "$tmp_install/lib/ocaml/compiler-libs" "$tmp_install/lib/ocaml/stublibs"

echo "[stage] copying runtime stdlib"
mkdir -p "$tmp_install/lib/ocaml"
rsync -aL "$local_runtime_lib/" "$tmp_install/lib/ocaml/"
cp -f "$local_runtime_bin/ocamlrun" "$tmp_install/bin/ocamlrun"
cp -f "$local_runtime_bin/ocamlrund" "$tmp_install/bin/ocamlrund"
cp -f "$local_runtime_bin/ocamlruni" "$tmp_install/bin/ocamlruni"
cp -f "$bootstrap_lib/Makefile.config" "$tmp_install/lib/ocaml/Makefile.config"
sed -i.bak \
  -e 's/^NATIVE_COMPILER=true$/NATIVE_COMPILER=false/' \
  -e 's/^NATDYNLINK=true$/NATDYNLINK=false/' \
  -e 's/^CFLAGS=\(.*\)$/CFLAGS=\1 -Wno-incompatible-function-pointer-types/' \
  -e 's/^BYTECODE_CFLAGS=\(.*\)$/BYTECODE_CFLAGS=\1 -Wno-incompatible-function-pointer-types/' \
  "$tmp_install/lib/ocaml/Makefile.config"
rm -f "$tmp_install/lib/ocaml/Makefile.config.bak"
sed -i.bak \
  -e 's/^CAMLextern value caml_get_public_method (value obj, value tag);$/CAMLextern value caml_get_public_method ();/' \
  -e 's/^CAMLextern value caml_set_oo_id(value obj);$/CAMLextern value caml_set_oo_id();/' \
  "$tmp_install/lib/ocaml/caml/mlvalues.h"
rm -f "$tmp_install/lib/ocaml/caml/mlvalues.h.bak"

echo "[stage] copying reduced compiler frontends"
cp -f "$repo_root/_build/default/main_native.exe" "$tmp_install/bin/ocamlc.opt"
ln -sf ocamlc.opt "$tmp_install/bin/ocamlc"
cp -f "$repo_root/_build/default/boot_ocamlopt.exe" "$tmp_install/bin/ocamlopt.opt"
ln -sf ocamlopt.opt "$tmp_install/bin/ocamlopt"
cp -f "$repo_root/_build/default/boot_ocamlj.exe" "$tmp_install/bin/ocamlj.opt"
ln -sf ocamlj.opt "$tmp_install/bin/ocamlj"
cp -f "$repo_root/_build/default/tools/ocamldep.exe" "$tmp_install/bin/ocamldep.opt"
ln -sf ocamldep.opt "$tmp_install/bin/ocamldep"
cp -f "$repo_root/_build/default/tools/ocamlmklib.exe" "$tmp_install/bin/ocamlmklib.opt"
ln -sf ocamlmklib.opt "$tmp_install/bin/ocamlmklib"
cp -f "$repo_root/_build/default/tools/objinfo.exe" "$tmp_install/bin/ocamlobjinfo.opt"
ln -sf ocamlobjinfo.opt "$tmp_install/bin/ocamlobjinfo"
cp -f "$repo_root/_build/default/ocamlyacc" "$tmp_install/bin/ocamlyacc"
cp -f "$repo_root/_build/_bootinstall/bin/ocamllex" "$tmp_install/bin/ocamllex"
if [ -x "$repo_root/_build/main/toplevel/byte/bytetop" ]; then
  cp -f "$repo_root/_build/main/toplevel/byte/bytetop" "$tmp_install/bin/ocaml"
else
  cat > "$tmp_install/bin/ocaml" <<EOF
#!/bin/sh
unset OCAMLLIB
unset CAML_LD_LIBRARY_PATH
exec "$bootstrap_bin/ocaml" "\$@"
EOF
  chmod +x "$tmp_install/bin/ocaml"
fi

echo "[stage] copying compiler-libs surface"
cp -f "$repo_root/_build/main/compilerlibs/META" "$tmp_install/lib/ocaml/compiler-libs/META"
cp -f "$repo_root/_build/main/ocamlcommon.cma" "$tmp_install/lib/ocaml/compiler-libs/"
cp -f "$repo_root/_build/main/ocamlbytecomp.cma" "$tmp_install/lib/ocaml/compiler-libs/"
cp -f "$repo_root/_build/main/ocamloptcomp.cma" "$tmp_install/lib/ocaml/compiler-libs/"
cp -f "$repo_root/_build/main/oxcaml_common.cma" "$tmp_install/lib/ocaml/compiler-libs/"
cp -f "$repo_root/_build/main/utils/oxcaml_utils.cma" "$tmp_install/lib/ocaml/compiler-libs/"
cp -f "$repo_root/_build/main/toplevel/byte/ocamltoplevel.cma" "$tmp_install/lib/ocaml/compiler-libs/"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/compiler-libs" \
  "$repo_root/_build/main/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/.ocamlbytecomp.objs/byte" \
  "$repo_root/_build/main/.ocamloptcomp.objs/byte" \
  "$repo_root/_build/main/.oxcaml_common.objs/byte" \
  "$repo_root/_build/main/utils/.oxcaml_utils.objs/byte" \
  "$repo_root/_build/main/bytecomp/.ocamlbytecomp.objs/byte" \
  "$repo_root/_build/main/file_formats/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/lambda/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/middle_end/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/middle_end/flambda2/.flambda2.objs/byte" \
  "$repo_root/_build/main/middle_end/flambda2/numbers/.flambda2_numbers.objs/byte" \
  "$repo_root/_build/main/middle_end/flambda2/numbers/floats/.flambda2_floats.objs/byte" \
  "$repo_root/_build/main/typing/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/utils/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/parsing/.ocamlcommon.objs/byte" \
  "$repo_root/_build/main/toplevel/byte/.ocamltoplevel.objs/byte" \
  "$repo_root/_build/main/toplevel/.topstart.eobjs/byte"

echo "[stage] copying non-stdlib libraries"
copy_package_dir "dynlink" "$repo_root/_build/main/otherlibs/dynlink"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/dynlink" \
  "$repo_root/_build/main/otherlibs/dynlink/.dynlink_internal_byte.objs/byte"

copy_package_dir "str" "$repo_root/_build/main/otherlibs/str"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/str" \
  "$repo_root/_build/main/otherlibs/str/.str.objs/byte"

mkdir -p "$tmp_install/lib/ocaml/threads"
cp -f "$repo_root/_build/main/otherlibs/systhreads4/META" "$tmp_install/lib/ocaml/threads/"
cp -f "$repo_root/_build/main/otherlibs/systhreads4/byte/threads.cma" \
  "$tmp_install/lib/ocaml/threads/"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/threads" \
  "$repo_root/_build/main/otherlibs/systhreads4/byte/.threads.objs/byte"
cp -f "$repo_root/_build/main/otherlibs/systhreads4/threads.h" \
  "$tmp_install/lib/ocaml/caml/threads.h"

copy_package_dir "unix" "$repo_root/_build/main/otherlibs/unix"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/unix" \
  "$repo_root/_build/main/otherlibs/unix/.unix.objs/byte"
mkdir -p "$tmp_install/lib/ocaml/caml"
find "$repo_root/_build/main/otherlibs/unix/caml" -maxdepth 1 -type f -name '*.h' \
  -exec cp -f {} "$tmp_install/lib/ocaml/caml/" \;

copy_package_dir "stdlib_stable" "$repo_root/_build/main/otherlibs/stdlib_stable"
copy_flattened_artifacts \
  "$tmp_install/lib/ocaml/stdlib_stable" \
  "$repo_root/_build/main/otherlibs/stdlib_stable/.stdlib_stable.objs/byte"

rm -rf "$install_root"
mv "$tmp_install" "$install_root"

echo "[stage] reduced install available at $install_root"
