# AFL fuzzer for the CFG shape validators

This directory contains an [AFL](https://lcamtuf.coredump.cx/afl/) harness that
fuzzes the two *shape-only* CFG analysis validators in the backend:

- **reachability** (`Cfg_reachability_validate.check_reachability`), and
- **dominators** (`Cfg_dominators_validate.check_idom`).

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
| `afl_cfg_fuzzer.ml` | The harness. Its header comment is the authoritative spec of the input format and normalization. |
| `afl_cfg_fuzzer_stubs.c` | A one-line C stub that calls `abort()` (classic AFL only treats *signals* as crashes). |
| `dune` | Builds the native harness against `ocamloptcomp`. |
| `corpus/` | Seed inputs: `diamond`, `loop`, `irreducible`, `exn`. |
| `build_afl.sh` | Builds the harness **with AFL instrumentation**. |
| `run_afl.sh` | Builds on demand and launches `afl-fuzz` with the right flags. |

## Prerequisites

- A **configured, built repo**. Run `./configure --prefix="$(pwd)/_install"`
  once, then `make -s boot-compiler`. The build scripts reuse the boot
  workspace's generated prerequisites (`duneconf/boot.ws`,
  `duneconf/*.inc`, `boot_oc_cflags.sexp`) and will tell you to build first if
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

`run_afl.sh` seeds from `corpus/` and writes results to
`tools/afl_cfg_fuzzer/findings/` (both git-ignored). Stop it with `Ctrl-C`.

## How instrumentation is delivered (OCAMLPARAM)

AFL needs the OCaml code compiled with `-afl-instrument`. That flag is delivered
through `OCAMLPARAM`, following the repo's `BUILD_OCAMLPARAM` convention — but
with two twists that `build_afl.sh` handles for you:

1. **The boot workspace clears `OCAMLPARAM`.** The harness is compiled in the
   boot dune context (which builds the in-tree `ocamloptcomp`, where the
   validators live, using the opam compiler). `duneconf/boot.ws` statically sets
   `("OCAMLPARAM" "")`, and the `BUILD_OCAMLPARAM` hook in `Makefile.common-ox`
   only injects into the `main`/`runtime_stdlib` contexts. So the script derives
   an instrumented copy of the boot workspace, `duneconf/afl.ws` (git-ignored),
   with `("OCAMLPARAM" "$BUILD_OCAMLPARAM")`.

2. **dune does not track `OCAMLPARAM`.** If the instrumented objects landed in
   the shared `_build`, a later `make` of the compiler would silently reuse
   them. The script therefore builds into an **isolated build dir**
   (`_build_afl`) that never mixes with `_build`.

After building, the script runs `afl-showmap` on a seed and fails loudly if the
binary is not actually instrumented. A healthy build reports something like
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
| `AFL_BUILD_DIR` | `_build_afl` | Isolated dune build directory. |

`--clean` removes the build dir first (use it if you change `BUILD_OCAMLPARAM`,
since dune will not otherwise notice).

The resulting binary is:

```
_build_afl/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
```

## Running a campaign

```
tools/afl_cfg_fuzzer/run_afl.sh [--build] [--resume] [afl-fuzz options...]
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `CORPUS` | `tools/afl_cfg_fuzzer/corpus` | Seed input directory. |
| `FINDINGS` | `tools/afl_cfg_fuzzer/findings` | Output directory. |
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
throughput is modest (~75–125 exec/s here); coverage guidance makes the
*mutations* smart, not the individual runs fast. The harness is deterministic
(AFL reports `stability : 100%`).

## Triaging a finding

Crash inputs are saved under `tools/afl_cfg_fuzzer/findings/crashes/` (the
default `FINDINGS`). Each filename ends in `sig:06`, i.e. `SIGABRT` — a genuine
validator finding. There are two ways to inspect one:

```bash
exe=_build_afl/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
crash=tools/afl_cfg_fuzzer/findings/crashes/id:000000,sig:06,...

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

Every byte string is a valid input; there is no reject path. See the header of
`afl_cfg_fuzzer.ml` for the authoritative description.

- Bytes 0–1: unsigned 16-bit little-endian seed for the node count;
  `node_count = 1 + (seed mod max_nodes)`.
- Then 4-byte edge records: a 16-bit `source` (top bit = exceptional edge) and a
  16-bit `target`, both taken modulo `node_count`.

The harness normalizes the graph deterministically (drops edges into the entry,
dedups, keeps at most one exception successor per node, truncates normal
successors so a raising terminator fits) so that the dot output and the built
CFG always agree.

## Reproducing the known dead-code disagreement

The validators assume the CFG has **no dead code**. `Cfg_dominators` copes with
unreachable blocks (it processes each component and maps dead roots to
themselves); the dominator validator only derives immediate dominators for
reachable nodes. So an unreachable node makes the two disagree.

By default the harness prunes unreachable nodes before building the CFG, matching
the validators' precondition, so a well-formed campaign finds nothing. To
confirm the fuzzer *does* catch the disagreement, flip the symbolic constant near
the top of `afl_cfg_fuzzer.ml`:

```ocaml
let prune_unreachable_nodes = false
```

then rebuild and run:

```bash
tools/afl_cfg_fuzzer/build_afl.sh
tools/afl_cfg_fuzzer/run_afl.sh
```

AFL finds `sig:06` crashes almost immediately (typically in the deterministic
bit-flip stage, e.g. by enlarging a seed's node count so it grows edge-less
unreachable nodes), each reproducing as
`dominators: expected_idom and idom differ`. **Remember to set the constant back
to `true` afterwards** — that is the intended default.

## Mutation test: does the fuzzer catch a real dominator bug?

The dead-code section above exercises a *precondition mismatch*. To check that the
fuzzer also catches an ordinary miscomputation — a wrong-but-plausible immediate
dominator table that no assertion would flag — you can temporarily inject a bug into
the compiler's dominator algorithm and confirm the (unchanged) Datalog validator
disagrees.

Keep `prune_unreachable_nodes = true` (the default) so the *only* finding is the
injected bug, not the dead-code disagreement.

### The bug

`Cfg_dominators.compute_doms` sets a node's immediate dominator to the intersection
(nearest common dominator) of its predecessors' immediate dominators. Skip the
intersection and just keep the first predecessor's idom:

```diff
--- a/backend/cfg/cfg_dominators.ml
+++ b/backend/cfg/cfg_dominators.ml
@@ compute_doms: merge the predecessors' immediate dominators @@
                 match Label.Tbl.find_opt doms predecessor_label with
                 | None -> ()
                 | Some _ -> (
                   match !new_idom with
                   | None -> new_idom := Some predecessor_label
-                  | Some new_idom_pred ->
-                    new_idom
-                      := Some
-                           (intersect doms order predecessor_label new_idom_pred)
-                  ));
+                  (* BUG: keep the first predecessor's immediate dominator instead
+                     of intersecting it with the remaining predecessors. *)
+                  | Some _ -> ()));
```

Removing the call leaves `intersect` and `order` unused, which is an error under the
module's `[@@@ocaml.warning "+a-..."]`. These two hunks only silence that; they have
no other effect:

```diff
-let intersect : doms -> order -> Label.t -> Label.t -> Label.t =
+let _intersect : doms -> order -> Label.t -> Label.t -> Label.t =
  fun doms post_order b1 b2 ->
```

```diff
-  let stack, order, components = build_order cfg in
+  let stack, _order, components = build_order cfg in
```

At any join with two predecessors that do not dominate each other, the node now hangs
off one branch instead of the branches' common dominator — a wrong result the compiler
would silently accept (`invariant_doms` runs only under `debug = true`).

### Rebuild and confirm directly

```bash
tools/afl_cfg_fuzzer/build_afl.sh
exe=_build_afl/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe

# The diamond seed is now a finding on its very first execution:
"$exe" tools/afl_cfg_fuzzer/corpus/diamond.bin; echo "exit=$?"
```

```
afl_cfg_fuzzer: dominators: expected_idom and idom differ
Graph:
digraph fuzz {
  n0;
  n0 -> n1;
  n0 -> n2;
  n1;
  n1 -> n3;
  n2;
  n2 -> n3;
  n3;
}
exit=134
```

`n3` joins `n1` and `n2`; its true immediate dominator is `n0`, but the bug sets it to
`n1`. The `irreducible` seed crashes for the same reason; `loop` and `exn` are
single-predecessor chains, so their dominators are unaffected.

### Rediscover it with AFL

Because the `diamond` and `irreducible` seeds now crash on their first execution,
`afl-fuzz` refuses to start on the full corpus (it rejects a crashing seed). Seed only
from the non-crashing inputs and let mutation rebuild a mis-dominated join:

```bash
mkdir -p /tmp/afl_demo_corpus
cp tools/afl_cfg_fuzzer/corpus/loop.bin tools/afl_cfg_fuzzer/corpus/exn.bin /tmp/afl_demo_corpus/
CORPUS=/tmp/afl_demo_corpus FINDINGS=/tmp/afl_demo_findings \
  tools/afl_cfg_fuzzer/run_afl.sh
```

Within a couple of minutes AFL reports `sig:06` crashes (found in the `havoc` stage,
since the seeds contain no mis-dominated join to bit-flip into one directly). Reproduce
one:

```bash
crash=$(ls /tmp/afl_demo_findings/crashes/id:* | head -1)
"$exe" "$crash"          # reproduces "dominators: expected_idom and idom differ"
"$exe" -to-dot "$crash"  # just the graph
```

A discovered example: `n1` is joined by its own self-edge and by `n2`, so its immediate
dominator is mis-set (its true idom is `n2`).

```
digraph fuzz {
  n0;
  n0 -> n2 [style=dashed];
  n1;
  n1 -> n1;
  n2;
  n2 -> n1;
}
```

### Revert

The bug is a throwaway experiment; restore the pristine file and rebuild the clean
harness when done:

```bash
git checkout -- backend/cfg/cfg_dominators.ml
tools/afl_cfg_fuzzer/build_afl.sh
```

## A second mutation test: stopping the fixpoint after one pass

The bug above corrupts a single join and is caught by almost any graph with a merge —
the `diamond` seed fails on its very first execution. A subtler mistake is to run the
dominator fixpoint for only one pass instead of iterating to convergence. This one is
invisible on acyclic code and shows up only around back edges, so it is a better test of
whether the fuzzer reaches past the easy shapes.

Keep `prune_unreachable_nodes = true` (the default).

### The bug

`Cfg_dominators.compute_doms` repeats a reverse-post-order sweep until no immediate
dominator changes (`while !changed`). Force it to stop after the first sweep:

```diff
--- a/backend/cfg/cfg_dominators.ml
+++ b/backend/cfg/cfg_dominators.ml
@@ compute_doms: iterate the CHK fixpoint to convergence @@
         | Some dom_label ->
           if not (Label.equal dom_label new_idom)
           then (
             Label.Tbl.replace doms label new_idom;
-            changed := true)))
+            changed := true)));
+    (* BUG: clear [changed] after one reverse-post-order pass, so the fixpoint
+       loop stops after a single iteration instead of running to convergence. *)
+    changed := false
   done;
```

Unlike the previous bug this is a single, self-contained hunk: `changed` stays used, so
no other line needs to change.

In one reverse-post-order pass every node is processed after all of its predecessors
*except* across back edges, where a predecessor is visited later. A reducible acyclic
graph is therefore already exact after one pass — so the `diamond` seed, which was *the*
trigger for the previous bug, now passes. The error appears only when a node's correct
immediate dominator depends on a predecessor whose own dominator has not been finalized
yet, which is exactly what a back edge into a join produces.

### Rebuild and confirm directly

```bash
tools/afl_cfg_fuzzer/build_afl.sh
exe=_build_afl/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe

# The diamond still passes: a DAG is exact after a single pass.
"$exe" tools/afl_cfg_fuzzer/corpus/diamond.bin; echo "exit=$?"   # exit=0

# The irreducible seed is now a finding.
"$exe" tools/afl_cfg_fuzzer/corpus/irreducible.bin; echo "exit=$?"
```

```
afl_cfg_fuzzer: dominators: expected_idom and idom differ
Graph:
digraph fuzz {
  n0;
  n0 -> n1;
  n0 -> n2;
  n1;
  n1 -> n3;
  n2;
  n2 -> n3;
  n3;
  n3 -> n1;
  n3 -> n2;
}
exit=134
```

`n0` branches to `n1` and `n2`, both reach `n3`, and `n3` loops back to both (two loop
entries — an irreducible graph). Every node's true immediate dominator is `n0`. In
reverse-post-order `n3` is visited before `n2`, so on the single pass its dominator is
taken from `n1` alone and set to `n1`; a second pass, once `n2`'s dominator is known,
would intersect the two and correct it to `n0`. The `loop` and `exn` seeds still pass:
their joins' immediate dominators do not change from one pass to the next.

### Rediscover it with AFL

`irreducible` now crashes on its first execution, so (as before) seed only from the
non-crashing inputs — here `diamond`, `loop`, and `exn` — and let mutation build a back
edge into a mis-dominated join:

```bash
mkdir -p /tmp/afl_demo2_corpus
cp tools/afl_cfg_fuzzer/corpus/diamond.bin \
   tools/afl_cfg_fuzzer/corpus/loop.bin \
   tools/afl_cfg_fuzzer/corpus/exn.bin /tmp/afl_demo2_corpus/
CORPUS=/tmp/afl_demo2_corpus FINDINGS=/tmp/afl_demo2_findings \
  tools/afl_cfg_fuzzer/run_afl.sh
```

All three seeds pass the calibration dry run, then AFL reports `sig:06` crashes in the
`havoc` stage (it has to grow a cycle, so no bit-flip finds one directly). Reproduce one:

```bash
crash=$(ls /tmp/afl_demo2_findings/crashes/id:* | head -1)
"$exe" "$crash"          # reproduces "dominators: expected_idom and idom differ"
"$exe" -to-dot "$crash"  # just the graph
```

A discovered example: `n2` and `n3` form a cycle (`n2 -> n3 -> n2`), and the join `n3`
is reached from `n1` and `n2`, so its immediate dominator is settled from one predecessor
before the other is known.

```
digraph fuzz {
  n0;
  n0 -> n1;
  n0 -> n2;
  n1;
  n1 -> n3;
  n2;
  n2 -> n1;
  n2 -> n3;
  n3;
  n3 -> n2;
}
```

### Revert

Restore the pristine file and rebuild the clean harness when done:

```bash
git checkout -- backend/cfg/cfg_dominators.ml
tools/afl_cfg_fuzzer/build_afl.sh
```
