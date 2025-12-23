#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF' >&2
Usage: typing/ikinds/bench_make.sh <description...>

Bootstraps once (if needed), then measures a self-hosted compiler rebuild
three times and appends wall/user/sys timings to:
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
self_profile="${SELF_PROFILE:-dev}"
toolchain_profile="${TOOLCHAIN_PROFILE:-main}"

get_config_var() {
  local key="$1"
  awk -F= -v k="$key" '$1 == k {print substr($0, index($0, "=") + 1)}' \
    "$repo_root/Makefile.config"
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

timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
git_head="$(git -C "$repo_root" rev-parse HEAD)"
git_status="$(git -C "$repo_root" status --porcelain=v1 || true)"

dirty="clean"
if [[ -n "$git_status" ]]; then
  dirty="dirty"
fi

logdir="$(mktemp -d /tmp/oxcaml-ikinds-bench-XXXXXXXX)"
self_ws="$logdir/self.ws"
self_bin="$logdir/self-bin"
build_dir="$repo_root/_build/self"
toolchain_root="$repo_root/_build/install/$toolchain_profile"

mkdir -p "$self_bin"
ln -sf "$toolchain_root/bin/ocamlc.opt" "$self_bin/ocamlc"
ln -sf "$toolchain_root/bin/ocamlopt.opt" "$self_bin/ocamlopt"
ln -sf "$toolchain_root/bin/ocamldep.opt" "$self_bin/ocamldep"
ln -sf "$toolchain_root/bin/ocamlmklib.opt" "$self_bin/ocamlmklib"
ln -sf "$toolchain_root/bin/ocamlj.opt" "$self_bin/ocamlj"
ln -sf "$repo_root/_build/_bootinstall/bin/ocamllex.opt" "$self_bin/ocamllex"

cat >"$self_ws" <<EOF
(lang dune 2.8)
(context (default
  (name main)
  (profile $self_profile)
  (paths
    (PATH ("$self_bin" :standard))
    (OCAMLLIB
      ("$repo_root/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib")))
  (env (_
    (flags
      (:standard -directory compiler-distro -warn-error +A -alert
        -unsafe_multidomain))
    (ocamlopt_flags (:standard -fno-asan))))))
EOF

seed_compiler="$toolchain_root/bin/ocamlopt.opt"
if [[ ! -x "$seed_compiler" ]]; then
  (cd "$repo_root" && make main_build_profile="$toolchain_profile" compiler)
fi

{
  echo
  echo "=== $timestamp ==="
  echo "desc: $desc"
  echo "git: $git_head ($dirty)"
  echo "host: $(uname -srm)"
  echo "logdir: $logdir"
  echo "profile: $self_profile"
  echo "toolchain_profile: $toolchain_profile"
  echo "cmd: /usr/bin/time -p dune build --workspace=$self_ws --build-dir=$build_dir"
  echo "note: self-hosted rebuild (no system OCaml in timing)"
  echo "runs: 3"
  if [[ -n "$git_status" ]]; then
    echo "dirty files:"
    echo "$git_status" | sed 's/^/  /'
  else
    echo "dirty files: (none)"
  fi
  echo
} >>"$results"

build_targets=(
  --only-package=ocaml
  @install
  tools/regalloc/regalloc.exe
  oxcaml/testsuite/tools/expect.exe
  oxcaml/testsuite/tools/codegen_main.exe
  oxcaml/testsuite/tools/asmgen_"$arch".o
  testsuite/lib/lib.cm{,x}a
  testsuite/lib/testing.cm{,x}a
  tools/dumpobj.bc
)

for i in 1 2 3; do
  build_log="$logdir/run$i.dune.log"
  time_log="$logdir/run$i.time"

  rm -rf "$build_dir"

  if ! (cd "$repo_root" && { \
    ARCH="$arch" RUNTIME_DIR="$runtime_dir" SYSTEM="$system" MODEL="$model" \
    ASPP="$aspp" ASPPFLAGS="$asppflags" \
    /usr/bin/time -p \
    dune build --root=. --workspace="$self_ws" --build-dir="$build_dir" \
    "${build_targets[@]}" \
    >"$build_log"; } \
    2>"$time_log"); then
    {
      echo "run $i: FAIL"
      echo "  build log: $build_log"
      echo "  time log: $time_log"
      sed 's/^/  /' "$time_log"
      echo
    } >>"$results"
    exit 1
  fi

  real="$(awk '/^real /{print $2}' "$time_log")"
  user="$(awk '/^user /{print $2}' "$time_log")"
  sys="$(awk '/^sys /{print $2}' "$time_log")"

  {
    echo "run $i:"
    echo "  real $real"
    echo "  user $user"
    echo "  sys  $sys"
    echo
  } >>"$results"
done
