#!/usr/bin/env bash

# Boilerplate:
shopt -s nullglob
set -euo pipefail

# Navigate to the repository root:
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

# Invoke the script from within a Nix-provisioned shell with frame pointers:
if [[
  -n "${IN_NIX_SHELL-}" &&
  "${pname-}" == oxcaml &&
  " ${configureFlags-} " == *" --enable-frame-pointers "*
]]
then
  exec ./scripts/profile-compiler-within-ocl-shell.sh
else
  exec ocl shell -variant fp -- ./scripts/profile-compiler-within-ocl-shell.sh
fi
