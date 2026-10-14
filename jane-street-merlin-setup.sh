#!/usr/bin/env bash

target_dir=$PWD
cd "$(dirname "$0")"

set -e

# Step 1: Create the directory .local-merlin-bin (if it doesn't exist)

merlin_bin_dir="$target_dir"/.local-merlin-bin
mkdir -p "$merlin_bin_dir"

# Step 2: Populate .local-merlin-bin with executables installed from opam. We
# do this instead of just pointing at the opam install directory because the LSP
# has a different name on opam than within Jane Street (ocamllsp vs ocaml-lsp)
# and needs a wrapper (see below).

function find_opam_bin {
  local opam_name="$1"

  local opam_bin_unreal
  if ! opam_bin_unreal="$(which $opam_name 2>&-)"; then
    echo "Unable to find $opam_name. \
Did you install merlin and ocaml-lsp from opam? \
And did you run \`eval \$(opam env)\`?" >&2
    exit 1
  fi

  realpath "$opam_bin_unreal"
}

function create_link {
  local opam_name="$1"
  local jane_name="$2"

  local opam_bin
  opam_bin=$(find_opam_bin "$opam_name")
  local jane_bin="$merlin_bin_dir/$jane_name"

  if test -e "$jane_bin"; then
    echo "$jane_bin already exists. Skipping."
  else
    ln -s "$opam_bin" "$jane_bin"
  fi
}

create_link ocamlmerlin ocamlmerlin
create_link ocamlmerlin-server ocamlmerlin-server
create_link dot-merlin-reader dot-merlin-reader
create_link dune dune

# Editors put .local-merlin-bin at the end of the LSP's PATH, where another dune
# can shadow ours, so the LSP gets a wrapper that puts it first. Older versions
# of this script made ocaml-lsp a symlink to ocamllsp, so remove it rather than
# writing through it.

ocamllsp_bin=$(find_opam_bin ocamllsp)
lsp_wrapper="$merlin_bin_dir/ocaml-lsp"
rm -f "$lsp_wrapper"
cat > "$lsp_wrapper" <<EOF
#!/bin/sh
export PATH="$merlin_bin_dir:\$PATH"
exec "$ocamllsp_bin" "\$@"
EOF
chmod +x "$lsp_wrapper"

# Step 3: Create the files .local-merlin-binaries and .local-ocaml-lib, which
# editors use to get configuration information.

echo $(realpath "$merlin_bin_dir") > "$target_dir"/.local-merlin-binaries
ocamlc -where > "$target_dir"/.local-ocaml-lib
