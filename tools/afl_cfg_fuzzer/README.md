# AFL fuzzer for the CFG shape validators

This directory contains an [AFL](https://lcamtuf.coredump.cx/afl/) harness that
fuzzes the two *shape-only* CFG analysis validators in the backend:

- **reachability**
  (`Cfg_reachability_validate.For_testing.check_reachability`), and
- **dominators** (`Cfg_dominators_validate.For_testing.check_idom`).

Both validators re-derive an analysis with the in-tree `Flambda2_datalog` engine
and compare it against what the compiler computed. They depend only on the
*shape* of the control-flow graph (its nodes and edges), not on instruction
bodies, so they can be exercised by a tiny standalone program instead of the
whole compiler.

The harness reads a binary graph specification, builds a body-less `Cfg.t` with
that shape, runs the dominator computation, and checks it with the validators.
Any disagreement or unexpected exception is a **finding**: the harness prints
the graph and raises `SIGABRT` so that AFL records a crash.

## Layout

| File | Purpose |
| --- | --- |
| `afl_cfg_fuzzer.ml` | The harness: command-line handling and validation. |
| `corpus.ml` / `corpus.mli` | Seed graphs and binary corpus generation. |
| `graph.ml` / `graph.mli` | Graph parsing, selection, DOT printing and CFG construction. The interface documents the input format and normalization. |
| `afl_cfg_fuzzer_stubs.c` | A one-line C stub that calls `abort()` (classic AFL only treats *signals* as crashes). |
| `dune` | Builds the native harness against `ocamloptcomp`. |
| `_build/instrumented/` | Isolated AFL-instrumented build of the harness and its compiler-library dependencies. |
| `_build/corpus/` | Generated seed inputs: `diamond`, `loop`, `irreducible`, `exn`. |
| `_build/findings/` | AFL campaign output. |
| `build_afl.sh` | Builds the harness **with AFL instrumentation**. |
| `run_afl.sh` | Builds on demand and launches `afl-fuzz` with the right flags. |

## Prerequisites

- A **configured, built repo**. Run `./configure --prefix="$(pwd)/_install"`
  once, then `make -s boot-compiler`. The build scripts reuse the root
  `dune-workspace` and the boot build's generated prerequisites
  (`duneconf/*.inc`, `boot_oc_cflags.sexp`), and will tell you to build first if
  they are missing.
- **Classic AFL 2.52b** with `afl-fuzz` and `afl-showmap` on `PATH`. (This is
  not AFL++; the harness deliberately crashes via `SIGABRT` because 2.52b has no
  `AFL_CRASH_EXITCODE`.)

## Quick start

```bash
# 1. Build the instrumented harness.
tools/afl_cfg_fuzzer/build_afl.sh

# 2. Launch a campaign (builds first if needed).
tools/afl_cfg_fuzzer/run_afl.sh
```

`run_afl.sh` seeds from `tools/afl_cfg_fuzzer/_build/corpus/` and writes results
to `tools/afl_cfg_fuzzer/_build/findings/`. These paths and the instrumented
build are covered by the repository's existing `_build` ignore rules.
Stop it with `Ctrl-C`.

The seed graphs are defined as edge lists in `corpus.ml`. `build_afl.sh`
generates their binary files before checking instrumentation, and `run_afl.sh`
regenerates them when starting a new campaign with the default corpus. A custom
`CORPUS` is left untouched. The binary seeds are not stored in Git.

To generate the seeds manually into an existing directory:

```bash
exe=tools/afl_cfg_fuzzer/_build/instrumented/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
mkdir -p tools/afl_cfg_fuzzer/_build/corpus
"$exe" -write-corpus tools/afl_cfg_fuzzer/_build/corpus
```

## How instrumentation is delivered (OCAMLPARAM)

AFL needs the OCaml code compiled with `-afl-instrument`. That flag is delivered
through `OCAMLPARAM`, following the repo's `BUILD_OCAMLPARAM` convention — but
with two twists that `build_afl.sh` handles for you:

1. **The boot workspace clears `OCAMLPARAM`.** The harness is compiled in the
   boot dune context (which builds the in-tree `ocamloptcomp`, where the
   validators live, using the opam compiler). The root `dune-workspace` sets
   `("OCAMLPARAM" "")`, and the `BUILD_OCAMLPARAM` hook in `Makefile.common-ox`
   only injects into the `main` context. So the script derives
   an instrumented copy of the boot workspace, `duneconf/afl.ws` (git-ignored),
   with `("OCAMLPARAM" "$BUILD_OCAMLPARAM")`.

2. **dune does not track `OCAMLPARAM`.** If the instrumented objects landed in
   the root `_build`, a later `make` of the compiler would silently reuse
   them. The script therefore uses a separate Dune build directory,
   `tools/afl_cfg_fuzzer/_build/instrumented`, isolated from the root `_build`.

After building, the script runs `afl-showmap` on a seed and fails loudly if the
binary is not actually instrumented or the probe fails. The probe has a
five-second timeout, so a looping target cannot stall the build. A healthy build
reports something like
`instrumentation OK: afl-showmap captured 2394 tuples on diamond.bin` — a large
count, because the whole `ocamloptcomp` dependency chain (validators *and*
Datalog) is instrumented, not just the harness module.

### `build_afl.sh` options

```
tools/afl_cfg_fuzzer/build_afl.sh [--clean]
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `BUILD_OCAMLPARAM` | `_,afl-instrument=1` | `OCAMLPARAM` value injected into the build. |
| `AFL_BUILD_DIR` | `tools/afl_cfg_fuzzer/_build/instrumented` | Isolated dune build directory. |

`--clean` removes the Dune build directory first (use it if you change
`BUILD_OCAMLPARAM`, since dune will not otherwise notice). The default corpus
and findings are siblings of that directory and are preserved.

The build directory must be new, empty, or already marked by this script with
`.afl-cfg-fuzzer-build-dir`. Non-empty unmarked directories are rejected before
any cleanup or workspace generation. This also applies to build directories
created by older versions of the script: choose a fresh `AFL_BUILD_DIR` rather
than reusing them.

The resulting binary is:

```
tools/afl_cfg_fuzzer/_build/instrumented/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
```

## Running a campaign

```
tools/afl_cfg_fuzzer/run_afl.sh [--build] [--resume] [afl-fuzz options...]
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `CORPUS` | `tools/afl_cfg_fuzzer/_build/corpus` | Seed input directory. |
| `FINDINGS` | `tools/afl_cfg_fuzzer/_build/findings` | Output directory. |
| `MAX_NODES` | *(harness default: 256)* | Passed to the harness as `-max-nodes`. |

- `--build` forces a rebuild before launching.
- `--resume` continues an existing campaign (`afl-fuzz -i -`).
- Any other argument is forwarded to `afl-fuzz` verbatim (e.g. `-t 200`).

The script always passes the two flags this target requires:

- **`-m none`** — the `ocamloptcomp`-linked binary exceeds AFL's default memory
  limit; under the limit *every* input looks like a crash.
- **`AFL_SKIP_CPUFREQ=1`**.

### `core_pattern` (crash detection)

The harness signals findings with `SIGABRT`, so AFL must be able to observe
crashes. If the kernel pipes core dumps to a handler (`core_pattern` begins with
`|`, as with systemd-coredump), `run_afl.sh` warns and falls back to
`AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1` — this works but is slower and less
reliable. For a serious campaign, point `core_pattern` at a plain file (needs
root):

```bash
echo core | sudo tee /proc/sys/kernel/core_pattern
```

### Performance note

Each execution runs a full process that links the compiler libraries, so
throughput is modest; coverage guidance makes the *mutations* smart, not the
individual runs fast. The harness is deterministic (AFL reports `stability : 100%`).

## Triaging a finding

Crash inputs are saved under `tools/afl_cfg_fuzzer/_build/findings/crashes/` (the
default `FINDINGS`). Filenames containing `sig:06` indicate `SIGABRT`. The
harness uses this signal for both validator disagreements and unexpected
exceptions; replay the input to see which occurred. There are two ways to
inspect one:

```bash
exe=tools/afl_cfg_fuzzer/_build/instrumented/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
crash=tools/afl_cfg_fuzzer/_build/findings/crashes/id:000000,sig:06,...

# Reproduce the crash: prints the failing message and the offending graph,
# then aborts (exit 134).
"$exe" "$crash"

# Just look at the graph, without running the validators or crashing.
"$exe" -to-dot "$crash"
```

### Visualizing the offending graph with `-to-dot`

`-to-dot` writes the *normalized* graph — exactly the shape the harness handed
to the validators — in [Graphviz](https://graphviz.org/) `dot` format on stdout.
Normal edges are solid; exceptional edges are dashed. Because it does not run the
validators, it never crashes, so it is the safe way to look at a finding.

The same dot is also embedded in the crash reproduction above (after the
`afl_cfg_fuzzer: ...` message), but `-to-dot` gives you just the graph, ready to
render:

```bash
# Render to an image (needs the graphviz `dot` tool):
"$exe" -to-dot "$crash" | dot -Tpng -o finding.png
"$exe" -to-dot "$crash" | dot -Tsvg -o finding.svg

# Or save the dot text to paste into an online Graphviz viewer:
"$exe" -to-dot "$crash" > finding.dot
```

For a dead-code finding you will see one reachable region plus a set of
edge-less nodes (the unreachable blocks) that the validators and `Cfg_dominators`
disagree about.

## Input format (summary)

Every byte string is a valid input; there is no reject path. See `graph.mli`
for the authoritative description.

- Bytes 0–1: unsigned 16-bit little-endian seed for the node count;
  `node_count = 1 + (seed mod max_nodes)`.
- Then 4-byte edge records: a 16-bit `source` (top bit = exceptional edge) and a
  16-bit `target`, both taken modulo `node_count`.

The harness normalizes the graph deterministically (drops edges into the entry,
dedups, keeps at most one exception successor per node, truncates normal
successors so a raising terminator fits) so that the dot output and the built
CFG always agree.
