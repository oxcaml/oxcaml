#!/usr/bin/env bash

target_dir=$PWD
cd "$(dirname "$0")"

set -e

# Step 1: Create the directory .local-merlin-bin (if it doesn't exist)

merlin_bin_dir="$target_dir"/.local-merlin-bin
mkdir -p "$merlin_bin_dir"

# Step 2: Create symlinks in .local-merlin-bin to executables installed from
# opam. We make symlinks instead of just pointing at the opam install directory
# because the LSP has a different name on opam than within Jane Street (ocamllsp
# vs ocaml-lsp).

function create_link {
  local opam_name="$1"
  local jane_name="$2"

  local opam_bin="$(which $opam_name 2>&- | xargs realpath)"
  local status=$?

  if [[ $status -ne 0 ]]; then
    echo "Unable to find $opam_name. \
Did you install merlin and ocaml-lsp from opam? \
And did you run \`eval \$(opam env)\`?" >&2
    exit $status
  fi

  local jane_bin="$merlin_bin_dir/$jane_name"

  if test -e "$jane_bin"; then
    echo "$jane_bin already exists. Skipping."
  else
    ln -s "$opam_bin" "$jane_bin"
  fi
}

create_link ocamlmerlin ocamlmerlin
create_link ocamllsp ocaml-lsp
create_link ocamlmerlin-server ocamlmerlin-server
create_link dot-merlin-reader dot-merlin-reader

# Step 3: Create the files .local-merlin-binaries and .local-ocaml-lib, which
# editors use to get configuration information.

echo $(realpath "$merlin_bin_dir") > "$target_dir"/.local-merlin-binaries
ocamlc -where > "$target_dir"/.local-ocaml-lib
