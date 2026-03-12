#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/bench-ikinds.sh [OPTIONS]

Benchmark representative typing modules in two modes:
  off     no -ikinds
  on      -ikinds

By default this runs in non-principal mode. Use --principal to add
-principal to all compiler invocations.

Options:
  --ocamlc              Use _install/bin/ocamlc.opt
  --ocamlopt            Use _install/bin/ocamlopt.opt (default)
  --principal           Benchmark with -principal
  --non-principal       Benchmark without -principal (default)
  --module NAME         Benchmark only NAME; may be repeated
  --runs N              Hyperfine runs per command (default: 5)
  --warmup N            Hyperfine warmup runs (default: 1)
  --output-dir DIR      Write artifacts to DIR
  --compiler PATH       Compiler to benchmark
  --help                Show this message
EOF
}

repo_root="$(cd "$(dirname "$0")/.." && pwd)"

runs=5
warmup=1
principal=false
modules=()
compiler="$repo_root/_install/bin/ocamlopt.opt"
compiler_kind="ocamlopt"
output_dir=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --ocamlc)
      compiler="$repo_root/_install/bin/ocamlc.opt"
      compiler_kind="ocamlc"
      shift
      ;;
    --ocamlopt)
      compiler="$repo_root/_install/bin/ocamlopt.opt"
      compiler_kind="ocamlopt"
      shift
      ;;
    --principal)
      principal=true
      shift
      ;;
    --non-principal)
      principal=false
      shift
      ;;
    --module)
      modules+=("$2")
      shift 2
      ;;
    --runs)
      runs="$2"
      shift 2
      ;;
    --warmup)
      warmup="$2"
      shift 2
      ;;
    --output-dir)
      output_dir="$2"
      shift 2
      ;;
    --compiler)
      compiler="$2"
      case "$(basename "$compiler")" in
        ocamlopt|ocamlopt.opt)
          compiler_kind="ocamlopt"
          ;;
        *)
          compiler_kind="ocamlc"
          ;;
      esac
      shift 2
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown option: %s\n' "$1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ "${#modules[@]}" -eq 0 ]; then
  modules=(
    typecore
    env
    ctype
    typedecl
    typemod
    jkind
    typeclass
    typetexp
    includemod
    includecore
  )
fi

if ! command -v hyperfine >/dev/null 2>&1; then
  printf 'hyperfine not found in PATH\n' >&2
  exit 1
fi

main="$repo_root/_build/main"
common="$repo_root/_build/main/.ocamlcommon.objs/byte"
common_native="$repo_root/_build/main/.ocamlcommon.objs/native"
stdlib="$repo_root/_install/lib/ocaml"

if [ ! -x "$compiler" ]; then
  printf 'compiler not found or not executable: %s\n' "$compiler" >&2
  exit 1
fi

if [ "$principal" = true ]; then
  principal_flag="-principal"
  principal_suffix="principal"
else
  principal_flag=""
  principal_suffix="nonprincipal"
fi

if [ -z "$output_dir" ]; then
  output_dir="$repo_root/_bench_ikinds_${compiler_kind}_${principal_suffix}_$(date +%Y%m%d_%H%M%S)"
fi

mkdir -p "$output_dir"

if [ "$compiler_kind" = "ocamlopt" ]; then
  artifact_ext="cmx"
  include_flags="-I '$main' -I '$common' -I '$common_native' -I '$stdlib'"
else
  artifact_ext="cmo"
  include_flags="-I '$main' -I '$common' -I '$stdlib'"
fi

for mod in "${modules[@]}"; do
  tmp="$output_dir/tmp_$mod"
  mkdir -p "$tmp"
  if [ "$compiler_kind" = "ocamlopt" ]; then
    cleanup_exts="'$tmp/$mod.cmx' '$tmp/$mod.o' '$tmp/$mod.cmi' '$tmp/$mod.cmt' '$tmp/$mod.cmti' '$tmp/$mod.annot'"
  else
    cleanup_exts="'$tmp/$mod.cmo' '$tmp/$mod.cmi' '$tmp/$mod.cmt' '$tmp/$mod.cmti' '$tmp/$mod.annot'"
  fi
  hyperfine \
    --warmup "$warmup" \
    --runs "$runs" \
    --export-json "$output_dir/$mod.json" \
    --prepare "rm -f $cleanup_exts" \
    --command-name off \
    "'$compiler' $principal_flag -c -o '$tmp/$mod.$artifact_ext' $include_flags '$main/$mod.ml'" \
    --command-name on \
    "'$compiler' $principal_flag -ikinds -c -o '$tmp/$mod.$artifact_ext' $include_flags '$main/$mod.ml'"
done

printf '%s\n' "$output_dir"
