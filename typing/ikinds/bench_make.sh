#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF' >&2
Usage: typing/ikinds/bench_make.sh <description...>

Rebuilds the compiler toolchain, then runs three cold Dune builds of the
compiler source tree using that toolchain. Timings are appended to:
  typing/ikinds/benchresults.txt
EOF
}

if [[ $# -lt 1 ]]; then
  usage
  exit 2
fi

desc="$*"

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel)"
results="$script_dir/benchresults.txt"
toolchain_profile="${TOOLCHAIN_PROFILE:-main}"
toolchain_root="$repo_root/_build/install/$toolchain_profile"
build_dir="$repo_root/_build/self"
config_file="$repo_root/Makefile.config"
ocamllib="$repo_root/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"
workspace="$repo_root/duneconf/main.ws"
dune_profile="${DUNE_PROFILE:-dev}"

get_config_var() {
  awk -F= -v k="$1" '$1 == k {print $2; exit}' "$config_file"
}

runtime_dir="$(get_config_var RUNTIME_DIR)"
if [[ "$runtime_dir" == *'$(RUNTIME_SUFFIX)'* ]]; then
  runtime_suffix="$(get_config_var RUNTIME_SUFFIX)"
  runtime_dir="${runtime_dir//'$(RUNTIME_SUFFIX)'/$runtime_suffix}"
fi

arch="$(get_config_var ARCH)"
system="$(get_config_var SYSTEM)"
model="$(get_config_var MODEL)"
aspp="$(get_config_var ASPP)"
asppflags="${ASPPFLAGS:-}"

(cd "$repo_root" && make main_build_profile="$toolchain_profile")

{
  echo
  echo "=== $(date -u +%Y-%m-%dT%H:%M:%SZ) ==="
  echo "desc: $desc"
  echo "toolchain_profile: $toolchain_profile"
  echo "dune_profile: $dune_profile"
  echo
} >>"$results"

for i in 1 2 3; do
  echo "run $i:" >>"$results"
  rm -rf "$build_dir"
  (cd "$repo_root" && \
    PATH="$toolchain_root/bin:$PATH" \
    OCAMLLIB="$ocamllib" \
    RUNTIME_DIR="$runtime_dir" \
    ARCH="$arch" SYSTEM="$system" MODEL="$model" \
    ASPP="$aspp" ASPPFLAGS="$asppflags" \
    DUNE_PROFILE="$dune_profile" \
    /usr/bin/time -p \
    dune build --profile="$dune_profile" --root=. --workspace="$workspace" \
    --build-dir="$build_dir" \
    --only-package=ocaml @install) \
    2>>"$results"
  echo >>"$results"
done
