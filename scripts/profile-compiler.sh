#!/usr/bin/env bash

# HIGH-LEVEL DESCRIPTION:
# This is a standardized job used to evaluate the performance of the compiler:
# not the code the compiler generates, nor building the compiler itself,
# but *the performance of this compiler when compiling other code*.
# This script builds the compiler in this repository from its source,
# notably disabling dev mode (enabling e.g. `-O3`) and enabling frame pointers,
# then builds the standard library with this compiler with memtrace enabled.

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
