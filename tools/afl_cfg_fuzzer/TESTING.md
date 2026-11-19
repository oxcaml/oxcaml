# Testing the CFG fuzzer

These examples check that the fuzzer reports a known dead-code disagreement and
catches deliberately introduced dominator bugs. See the [README](README.md) for
setup and normal usage.

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

`Cfg_dominators.compute_doms` finds the nearest common dominator of a node's
predecessors. Skip the intersection and use the first predecessor with a known
dominator as the node's immediate dominator:

```diff
--- a/backend/cfg/cfg_dominators.ml
+++ b/backend/cfg/cfg_dominators.ml
@@ compute_doms: intersect the predecessors @@
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
+                  (* BUG: keep the first predecessor's label instead
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
exe=tools/afl_cfg_fuzzer/_build/instrumented/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe

# The diamond seed is now a finding on its very first execution:
"$exe" tools/afl_cfg_fuzzer/_build/corpus/diamond.bin; echo "exit=$?"
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
cp tools/afl_cfg_fuzzer/_build/corpus/loop.bin tools/afl_cfg_fuzzer/_build/corpus/exn.bin /tmp/afl_demo_corpus/
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
exe=tools/afl_cfg_fuzzer/_build/instrumented/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe

# The diamond still passes: a DAG is exact after a single pass.
"$exe" tools/afl_cfg_fuzzer/_build/corpus/diamond.bin; echo "exit=$?"   # exit=0

# The irreducible seed is now a finding.
"$exe" tools/afl_cfg_fuzzer/_build/corpus/irreducible.bin; echo "exit=$?"
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
cp tools/afl_cfg_fuzzer/_build/corpus/diamond.bin \
   tools/afl_cfg_fuzzer/_build/corpus/loop.bin \
   tools/afl_cfg_fuzzer/_build/corpus/exn.bin /tmp/afl_demo2_corpus/
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
