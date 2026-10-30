#!/bin/sh
# ACTUAL COMPILER: assembly and numeric checks for the arm64 policy.
set -eu
compiler_dir=$(CDPATH= cd -- "$(dirname -- "$1")" && pwd)
compiler="$compiler_dir/$(basename -- "$1")"
source=$2
work=$(mktemp -d "${TMPDIR:-/tmp}/oxcaml-fp-contract.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
cp "$source" "$work/numeric.ml"
cd "$work"

# The operands are runtime inputs.  For binary64 a*b is exactly 1 - 2^-54:
# contraction with -1 returns -2^-54; two roundings return positive zero.
# The binary32 pair similarly distinguishes syntax from explicit fmaf.
run_mode () {
  tag=$1
  expected=$2
  environment=$3
  shift 3
  env OCAMLPARAM="$environment" "$compiler" -extension-universe beta -O3 -S \
    "$@" -o "$tag.exe" numeric.ml
  cp numeric.s "$tag.s"
  "./$tag.exe" "$expected" 0x1.0000002p+0 0x1.ffffffcp-1 \
    0x1.0008p+0 0x1.fffp-1
}
run_mode default on ''
run_mode on on '' -ffp-contract=on
run_mode off off '' -ffp-contract=off
run_mode last-on on '' -ffp-contract=off -ffp-contract=on
run_mode last-off off '' -ffp-contract=on -ffp-contract=off
run_mode env-off off '_,ffp-contract=off'

# Four expression shapes must contract with the default and explicit on.
# Turned off, each binary64 multiply rounds before its add/subtract. Sign
# folding through FNMUL and the Float.fma runtime call remain legal.
for file in default.s on.s last-on.s; do
  grep -Eq '^[[:space:]]*fmadd[[:space:]]+d' "$file"
  grep -Eq '^[[:space:]]*fmsub[[:space:]]+d' "$file"
  grep -Eq '^[[:space:]]*fnmsub[[:space:]]+d' "$file"
done
for file in off.s last-off.s env-off.s; do
  if grep -Eq '^[[:space:]]*f(n?madd|n?msub)[[:space:]]+d' "$file"; then
    echo "unexpected binary64 fused operation in $file" >&2
    exit 1
  fi
  grep -Eq '^[[:space:]]*fmul[[:space:]]+d' "$file"
  grep -Eq '^[[:space:]]*fadd[[:space:]]+d' "$file"
  grep -Eq '^[[:space:]]*fsub[[:space:]]+d' "$file"
  grep -Eq '^[[:space:]]*fnmul[[:space:]]+d' "$file"
done
for file in default.s on.s off.s last-on.s last-off.s env-off.s; do
  grep -Eq '^[[:space:]]*fmul[[:space:]]+s' "$file"
  grep -Eq '^[[:space:]]*fadd[[:space:]]+s' "$file"
  if grep -Eq '^[[:space:]]*f(n?madd|n?msub)[[:space:]]+s' "$file"; then
    echo "unexpected implicit float32 contraction in $file" >&2
    exit 1
  fi
done
if env OCAMLPARAM='' "$compiler" -extension-universe beta \
  -ffp-contract=invalid -c numeric.ml >bad.log 2>&1; then
  echo 'invalid contraction policy was accepted' >&2
  exit 1
fi
grep -F "wrong argument 'invalid'" bad.log
if env OCAMLPARAM='_,ffp-contract=invalid' "$compiler" -extension-universe beta \
  -c numeric.ml >bad-env.log 2>&1; then
  echo 'invalid OCAMLPARAM contraction policy was accepted' >&2
  exit 1
fi
grep -F 'invalid OCAMLPARAM ffp-contract value' bad-env.log
env OCAMLPARAM='' "$compiler" -help >help.log
grep -Eq '^[[:space:]]*-ffp-contract[[:space:]]+\{on\|off\}[[:space:]]+Control' help.log
echo 'assembly and option checks passed'
