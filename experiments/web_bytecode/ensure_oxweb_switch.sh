#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
experiment_dir="$repo_root/experiments/web_bytecode"
local_opam_root="$repo_root/.opam-oxweb-root"
local_opam_switch="oxweb"
overlay_repo="$experiment_dir/opam-overlay"
toolchain_bin="$experiment_dir/toolchain/bin"
bytecode_toolchain_bin="$experiment_dir/toolchain/bytecode-bin"
host_path_default="/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"
host_path="${HOST_PATH_OVERRIDE:-$host_path_default}"

mkdir -p "$bytecode_toolchain_bin"
for tool in \
  ocaml \
  ocamlc \
  ocamlc.opt \
  ocamldep \
  ocamldep.opt \
  ocamlrun \
  ocamlrund \
  ocamlruni \
  ocamllex \
  ocamlmklib \
  ocamlobjinfo \
  ocamlobjinfo.opt \
  ocamlyacc
do
  case "$tool" in
    ocamlrun|ocamlrund|ocamlruni)
      ln -sf "$repo_root/_install/bin/$tool" "$bytecode_toolchain_bin/$tool"
      ;;
    *)
      ln -sf "$toolchain_bin/$tool" "$bytecode_toolchain_bin/$tool"
      ;;
  esac
done

cat > "$bytecode_toolchain_bin/ocamlcp" <<EOF
#!/bin/sh
exec "$toolchain_bin/ocamlc" "\$@"
EOF
chmod +x "$bytecode_toolchain_bin/ocamlcp"

cat > "$bytecode_toolchain_bin/ocamlmktop" <<EOF
#!/bin/sh
exec "$toolchain_bin/ocamlc" "\$@"
EOF
chmod +x "$bytecode_toolchain_bin/ocamlmktop"

rm -f \
  "$bytecode_toolchain_bin/ocamlopt" \
  "$bytecode_toolchain_bin/ocamlopt.opt" \
  "$bytecode_toolchain_bin/ocamloptp"

require_package() {
  local package_name=$1
  if ! opam_in_switch list --installed --short | grep -Fxq "$package_name"; then
    missing_packages+=("$package_name")
  fi
}

opam_in_switch() {
  env \
    PATH="$bytecode_toolchain_bin:$host_path" \
    OPAMROOT="$local_opam_root" \
    OPAMSWITCH="$local_opam_switch" \
    opam "$@"
}

"$experiment_dir/stage_minimal_5_4_install.sh"

mkdir -p "$local_opam_root"
if [ ! -f "$local_opam_root/config" ]; then
  opam init --root="$local_opam_root" --bare --disable-sandboxing -y default https://opam.ocaml.org
fi

if ! OPAMROOT="$local_opam_root" opam repository --short list | grep -Fxq oxweb-local; then
  OPAMROOT="$local_opam_root" opam repository --dont-select add oxweb-local "$overlay_repo" -y
fi

if ! OPAMROOT="$local_opam_root" opam switch --short list | grep -Fxq "$local_opam_switch"; then
  env \
    PATH="$bytecode_toolchain_bin:$host_path" \
    OPAMROOT="$local_opam_root" \
    opam switch create "$local_opam_switch" ocaml-system.5.4.0+ox --repositories=oxweb-local,default -y
fi

missing_packages=()
require_package ocamlfind
require_package js_of_ocaml
require_package js_of_ocaml-toplevel
require_package base
require_package core

if [ "${#missing_packages[@]}" -gt 0 ]; then
  opam_in_switch install -y "${missing_packages[@]}"
fi
