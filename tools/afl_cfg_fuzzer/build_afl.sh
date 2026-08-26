#!/usr/bin/env bash
#
# Build the AFL-instrumented CFG-validator fuzzer harness.
#
# Instrumentation is delivered through OCAMLPARAM, following the repo's
# BUILD_OCAMLPARAM convention (see Makefile.common-ox). The catch is that the
# boot dune workspace (duneconf/boot.ws) statically clears OCAMLPARAM, and
# BUILD_OCAMLPARAM is only injected into the "main"/"runtime_stdlib" contexts --
# not the boot context that actually compiles the in-tree ocamloptcomp (where
# the validators live) with the opam compiler. So this script derives an
# instrumented copy of the boot workspace, replacing
#     ("OCAMLPARAM" "")
# with
#     ("OCAMLPARAM" "$BUILD_OCAMLPARAM")
# and builds the harness under it.
#
# A dedicated build directory is mandatory: dune does not treat OCAMLPARAM as a
# dependency, so reusing the shared _build would leave instrumented objects
# behind and silently poison a later `make` of the compiler.
#
# Usage:
#   tools/afl_cfg_fuzzer/build_afl.sh [--clean]
#
# Options:
#   --clean            Remove the AFL build directory before building.
#
# Environment:
#   BUILD_OCAMLPARAM   OCAMLPARAM value injected into the build
#                      (default: "_,afl-instrument=1"; must not contain '&').
#   AFL_BUILD_DIR      dune build directory (default: "_build_afl").

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)
cd "$repo_root"

build_ocamlparam=${BUILD_OCAMLPARAM:-_,afl-instrument=1}
build_dir=${AFL_BUILD_DIR:-_build_afl}
workspace=duneconf/afl.ws
target=tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
exe="$build_dir/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe"
corpus=tools/afl_cfg_fuzzer/corpus
findings=tools/afl_cfg_fuzzer/findings

clean=0
for arg in "$@"; do
  case "$arg" in
    --clean) clean=1 ;;
    -h | --help)
      grep '^#' "$0" | sed '1d;s/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "error: unknown argument: $arg" >&2
      exit 2
      ;;
  esac
done

# Locate dune the same way the Makefile does.
dune=$(sed -n 's/^DUNE=//p' Makefile.config 2>/dev/null | head -1 || true)
if [ -z "${dune:-}" ] || [ ! -x "$dune" ]; then
  dune=$(command -v dune || true)
fi
if [ -z "${dune:-}" ]; then
  echo "error: could not find dune (checked Makefile.config DUNE= and PATH)" >&2
  exit 1
fi

# The instrumented workspace reuses the boot context's prerequisites verbatim.
missing=0
for f in dune-project duneconf/boot.ws duneconf/dirs-to-ignore.inc \
  duneconf/ox-extra.inc boot_oc_cflags.sexp; do
  if [ ! -e "$f" ]; then
    echo "error: missing build prerequisite: $f" >&2
    missing=1
  fi
done
if [ "$missing" -ne 0 ]; then
  echo "Run a normal build first (e.g. 'make -s boot-compiler') to generate" >&2
  echo "the workspace prerequisites, then re-run this script." >&2
  exit 1
fi

# Derive the instrumented workspace from the real boot workspace so we track any
# future change to the boot context, swapping only the OCAMLPARAM value. awk's
# literal replacement avoids regex surprises (the only caveat is a bare '&').
awk -v val="$build_ocamlparam" '
  { sub(/\("OCAMLPARAM" ""\)/, "(\"OCAMLPARAM\" \"" val "\")"); print }
' duneconf/boot.ws >"$workspace"

if ! grep -qF "(\"OCAMLPARAM\" \"$build_ocamlparam\")" "$workspace"; then
  echo "error: failed to inject OCAMLPARAM into $workspace" >&2
  echo "       (does duneconf/boot.ws still contain (\"OCAMLPARAM\" \"\") ?)" >&2
  exit 1
fi

echo "Building $target"
echo "  dune:       $dune"
echo "  workspace:  $workspace"
echo "  build dir:  $build_dir"
echo "  OCAMLPARAM: $build_ocamlparam"
if [ "$clean" -eq 1 ]; then
  echo "  cleaning:   rm -rf $build_dir"
  rm -rf "$build_dir"
fi

"$dune" build --root=. --workspace="$workspace" --build-dir="$build_dir" "$target"

if [ ! -x "$exe" ]; then
  echo "error: build reported success but $exe was not produced" >&2
  exit 1
fi

# Confirm the binary is actually instrumented. afl-showmap aborts with "No
# instrumentation detected" on an uninstrumented target, which is the exact
# failure mode a stale build dir would produce.
seed=$(find "$corpus" -maxdepth 1 -name '*.bin' 2>/dev/null | sort | head -1 || true)
if command -v afl-showmap >/dev/null 2>&1 && [ -n "${seed:-}" ]; then
  log=$(mktemp)
  map=$(mktemp)
  AFL_QUIET=1 afl-showmap -m none -o "$map" -- "$exe" "$seed" >"$log" 2>&1 || true
  if grep -q "No instrumentation detected" "$log"; then
    echo "error: $exe is NOT instrumented" >&2
    echo "       BUILD_OCAMLPARAM was: $build_ocamlparam" >&2
    echo "       try re-running with --clean" >&2
    rm -f "$log" "$map"
    exit 1
  fi
  tuples=$(grep -oE "Captured [0-9]+ tuples" "$log" | grep -oE "[0-9]+" | head -1 || true)
  if [ -n "${tuples:-}" ]; then
    echo "instrumentation OK: afl-showmap captured $tuples tuples on $(basename "$seed")"
  else
    echo "warning: could not confirm instrumentation; see afl-showmap output:" >&2
    cat "$log" >&2
  fi
  rm -f "$log" "$map"
else
  echo "note: skipping instrumentation check (afl-showmap or corpus seed not found)"
fi

cat <<EOF

Done. Instrumented harness:
  $exe

This harness signals a finding by raising SIGABRT, so AFL must be able to see
crashes. If core dumps are piped to a handler (core_pattern starts with '|', as
with systemd-coredump), point it at a plain file first (needs root):
  echo core | sudo tee /proc/sys/kernel/core_pattern
Without root you can proceed with AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1, but
crash detection is less reliable.

Start a coverage-guided campaign (classic AFL 2.52b; -m none is required
because the ocamloptcomp-linked binary exceeds AFL's default memory limit):
  AFL_SKIP_CPUFREQ=1 afl-fuzz -m none \\
    -i $corpus -o $findings \\
    -- $exe @@

Replay a finding as a dot graph:
  $exe -to-dot $findings/crashes/<id>
EOF
