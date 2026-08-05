#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
experiment_dir="$repo_root/experiments/web_bytecode"
local_opam_root="$repo_root/.opam-oxweb-root"
local_opam_switch="oxweb"
overlay_repo="$experiment_dir/opam-overlay"
oxcaml_opam_repo="$repo_root/_build/oxcaml-opam-repository"
oxcaml_opam_repo_url="${OXWEB_OXCAML_OPAM_REPO_URL:-https://github.com/oxcaml/opam-repository.git}"
oxcaml_opam_repo_ref="${OXWEB_OXCAML_OPAM_REPO_REF:-231c88c2e564fdca40e15e750aacad5fb0887435}"
toolchain_bin="$experiment_dir/toolchain/bin"
bytecode_toolchain_bin="$experiment_dir/toolchain/bytecode-bin"
host_path_default="/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"
host_path="${HOST_PATH_OVERRIDE:-$host_path_default}"

mkdir -p "$bytecode_toolchain_bin"
ln -sf ../ocaml-wrapper "$toolchain_bin/ocamlc"
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
      ln -sf "../../../../_install/bin/$tool" "$bytecode_toolchain_bin/$tool"
      ;;
    *)
      ln -sf "../bin/$tool" "$bytecode_toolchain_bin/$tool"
      ;;
  esac
done

cat > "$bytecode_toolchain_bin/ocamlcp" <<EOF
#!/bin/sh
tool_dir=\$(CDPATH= cd -- "\$(dirname "\$0")" && pwd)
exec "\$tool_dir/../bin/ocamlc" "\$@"
EOF
chmod +x "$bytecode_toolchain_bin/ocamlcp"

cat > "$bytecode_toolchain_bin/ocamlmktop" <<EOF
#!/bin/sh
tool_dir=\$(CDPATH= cd -- "\$(dirname "\$0")" && pwd)
exec "\$tool_dir/../bin/ocamlc" "\$@"
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

if [ ! -d "$oxcaml_opam_repo/.git" ]; then
  git clone "$oxcaml_opam_repo_url" "$oxcaml_opam_repo"
fi
git -C "$oxcaml_opam_repo" fetch --depth 1 origin "$oxcaml_opam_repo_ref"
git -C "$oxcaml_opam_repo" checkout --detach "$oxcaml_opam_repo_ref"

mkdir -p "$local_opam_root"
if [ ! -f "$local_opam_root/config" ]; then
  opam init --root="$local_opam_root" --bare --disable-sandboxing -y default https://opam.ocaml.org
fi

if ! OPAMROOT="$local_opam_root" opam repository --short list | grep -Fxq oxweb-local; then
  OPAMROOT="$local_opam_root" opam repository --dont-select add oxweb-local "$overlay_repo" -y
fi
if ! OPAMROOT="$local_opam_root" opam repository --all --short list | grep -Fxq oxcaml-official; then
  OPAMROOT="$local_opam_root" opam repository --dont-select add oxcaml-official "$oxcaml_opam_repo" -y
fi

if ! OPAMROOT="$local_opam_root" opam switch --short list | grep -Fxq "$local_opam_switch"; then
  env \
    PATH="$bytecode_toolchain_bin:$host_path" \
    OPAMROOT="$local_opam_root" \
    opam switch create "$local_opam_switch" ocaml-system.5.4.0+ox --repositories=oxweb-local,default -y
fi

opam_in_switch update oxweb-local
opam_in_switch repository set-repos oxweb-local oxcaml-official default -y
opam_in_switch pin add -y --no-action dune 3.22.1
opam_in_switch pin add -y --no-action ocamlfind 1.9.8
opam_in_switch pin add -y --no-action ocaml-compiler-libs \
  "$experiment_dir/opam-pins/ocaml-compiler-libs.v0.17.0"
opam_in_switch pin add -y --no-action ppxlib_ast \
  "$experiment_dir/opam-pins/ppxlib.0.33.0+ox"
opam_in_switch pin add -y --no-action ppxlib \
  "$experiment_dir/opam-pins/ppxlib.0.33.0+ox"
if ! opam_in_switch list --installed --short | grep -Fxq dune; then
  opam_in_switch install -y dune.3.22.1 cmdliner.2.1.1
fi

missing_packages=()
require_package ocamlfind
require_package js_of_ocaml
require_package js_of_ocaml-toplevel
require_package yojson

if [ "${#missing_packages[@]}" -gt 0 ]; then
  opam_in_switch install -y --ignore-constraints-on=ocaml \
    js_of_ocaml.6.0.1+ox1 js_of_ocaml-toplevel.6.0.1+ox1 \
    "${missing_packages[@]}"
fi
