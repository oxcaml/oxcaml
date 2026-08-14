# dev-loop-improvements — final report

Branch `jujacobs/vox/dev-loop-improvements`, based on `jujacobs/optimize-dev-loop`
@ `6929017cbd`. Plan and evidence base: `design-docs/dev-loop-improvements.md`.

## Commits

| sha | what |
| --- | --- |
| `878ec4ed0b` | the design doc: ranking, verification pass over the reports' claims, scope |
| `8c461894d0` | the tooling: wedge recovery, heartbeat, stale-stdlib detection, NOWATCH, dev-configure, promote fixpoint, dev-diff, dev-errors, ocamlopt links, python guard |
| `4105336709` | `AGENTS.md`: setup, new targets, failure modes |
| `88a1362396` | RED: ocamltest `-promote` cannot create a missing reference file |
| `6b3f460c20` | GREEN: it can, and the expectation diff shows exactly that |
| `bb21e087d5` | `dev-diff` handles program-output tests |
| `e7159b3af9` | fixes from the review loop |
| `e95dc422b7` | `dev-test-all` refreshes the test harness; stdlib-digest diagnosis |

## What landed, and whether it was exercised

"Exercised" means run against the failure it targets. I distinguish that from
"reasoned through" per item, because three items are only the latter.

| Item | Convergence | State |
| --- | --- | --- |
| 1. rpc wedge: heartbeat, timeout, watcher restart + one retry, direct fallback | 5/5 | **Exercised, including a real wedge** |
| 2. stale `_runtest` stdlib detector + `dev-refresh-stdlib` | 2/5 | **Exercised against a real SIGSEGV**; cure verified |
| 3a. `ocamlopt`/`ocamlopt.byte` missing from the dev test root | 4/5 | Landed, **not exercised** |
| 3b. `ocamlc.byte` magic mismatch | 4/5 | **Not done** — see below |
| 4a. `-promote` cannot create a missing reference | 1/5 | **Exercised**, red-green |
| 4b. promote iterates to a fixpoint | 1/5 | Partly exercised |
| 4c. `/tmp` scratch → `_build/dev`, `TMPDIR` exported | 1/5 + found | **Exercised** |
| 4d. promote log printed before the verify output | 1/5 | Landed, not exercised |
| 4e. names plain-vs-`-principal` instability | 2/5 | Landed, **not exercised** |
| 5. `make dev NOWATCH=1` | 1/5 | **Exercised in a real restricted sandbox** |
| 6a. `make dev-configure` + `Makefile.config` precondition | 5/5 | **Exercised in a fresh worktree** |
| 6b. `make dev-errors` | 1/5 | Landed, lightly exercised |
| 6c. `make dev-diff` | 1/5 | **Exercised**, both test kinds |
| 6d. `dev-test-all` names its artifact location | 1/5 | Landed, not exercised |
| 6e. python ≥ 3.7 guard | 2/5 | Landed, **not exercised** (needs a 3.6 interpreter) |
| 6f. suppress dune's forwarding notice | 1/5 | **Reversed after review — deleted** |
| 6g. docs: setup, blessed scratch path, never copy `.corrected` | — | Landed |

### The evidence for items 1, 2 and 5

**Item 1, in the wild.** A real wedge occurred twice during this work, with the
signature all five reports describe. Recorded before I killed the first one: the
`dune rpc build` client had waited **8m56s at 0:00:00 CPU time** while the
watcher's dune had used 59s of CPU over 17m58s and its log said `Success, waiting
for filesystem changes...`. On the second occurrence, with
`DEV_RPC_TIMEOUT=420`, the whole recovery ran unattended
(`logs/cmi-strong.log`):

    dev: building via the watcher (progress: make dev-log)
    dev: still building (60s elapsed; progress: make dev-log)
    ... every 60s ...
    dev: the build exceeded 420s; stopping it
    dev: the build timed out after 420s; restarting the watcher and retrying
    dev: watcher stopped
    dev: watcher started (idle timeout 1800s)
    Success

That is a real wedge, not a stub, recovered without intervention.

`make dev-selftest` (`tools/dev-watcher-test.sh`) covers the state machine
against a stub dune whose `rpc build` hangs, ignores signals, dies, fails or
succeeds on command: success does not retry; a timeout bounces the watcher and
retries once, with the watcher really replaced and the abandoned build really
dead; a signal-ignoring build is still bounded; a persistent wedge falls back and
propagates the fallback's exit status; `Connection_dead` recovers without waiting
out the timeout; a genuine build failure reports diagnostics and does not retry.
It needs no compiler and runs in ~15s. Disabling the timeout makes it hang at the
wedge case, which is the pre-fix behaviour — the suite is discriminating for the
thing it exists to check.

**Item 2, against a real segfault.** I made a genuine `.cmi`-shape change by
swapping two dereferenced fields of `Types.value_description`
(`val_type`/`val_kind`) — a declaration-order change needing no other edits,
since record fields are accessed by name — and rebuilt the compiler. The probe
then died exactly as the reports describe:

    PROBE_EXIT=139           # SIGSEGV
    dev: the compiler died on signal 11 reading the installed standard library,
    dev: which is what a stale marshaled .cmi looks like: ...
    dev: Run `make dev-refresh-stdlib` to rebuild it.

`make dev-refresh-stdlib` then cured it (`REFRESH_EXIT=0`, and the check
subsequently passed). The probe costs **20ms**, which is the measurement the
design doc promised; a warm `make dev` is ~0.4s, so it is well under the noise
floor and does not need a flag.

Worth recording as a falsification: my *first* attempt at this used
`val_loc`/`val_zero_alloc`, which also changes the marshaled layout but which a
trivial compile never dereferences — the probe passed. So the detector catches
shape changes the compiler actually trips over when reading the stdlib, which is
the class that manifests as a segfault, and not a layout change that stays latent.
Latent changes can still mislead a larger test. The probe also only reads `.cmi`;
the equivalent `Cmx_format` failure class is not covered.

**Item 5, in its target environment.** Verified by the codex reviewer, which
really does run under `workspace-write` — the environment the option exists for:
cold `make dev NOWATCH=1` passed in 412.58s, warm in 1.43s, with
`dev-watcher.py status` reporting the watcher stopped and no `_build/dev-dune/.rpc`
socket; plain `make dev` failed in that sandbox with
`Unix.Unix_error(Unix.EPERM, "bind", "")` / `Error: bind(): Operation not
permitted`. That both confirms the claimed cause and shows the fallback works
where the watcher cannot.

## The review loop

Two reviewers in their own worktrees under `vox/dev-loop/review/`: one claude
(findings returned as text) and one codex 0.147.0 `gpt-5.6-sol` (`report.md` in
its worktree). They ran independently and **converged on the same three top
defects**, each of which defeated a headline claim rather than being a corner
case. I reproduced every finding before acting on it. Fixes are in `e7159b3af9`.

1. **`make dev-configure` could not be reached in a fresh worktree** — the exact
   situation it was written for. `Makefile.config_if_required` exempts only the
   clean goals and `configure` from including the generated
   `Makefile.build_config`, so make aborted at parse time, before any recipe ran,
   which also made the `Makefile.config` guard in `dev-setup` dead code. Fixed by
   exempting `dev-configure` and `dev-selftest`. Now verified end to end in a
   fresh worktree: exit 0, `configure` and `Makefile.config` created, and
   `dev-selftest` runs without configuring at all.
2. **The timeout was not a bound.** `terminate_process_group` escalated SIGINT →
   SIGTERM and stopped, then waited unconditionally, so a child ignoring both
   defeated the mechanism — one reviewer measured 121s for a 3s timeout. Fixed
   with SIGKILL escalation and a bounded wait per step. Same reproduction now
   returns in 22s where the child would have slept 30s, and the suite has a case
   for it.
3. **Concurrent dev commands read each other's build output** through the fixed
   `_build/dev/rpc-build.log` — and the bad direction is a false green: codex
   showed a build whose own output was `Failure` exiting 0 because it read a
   sibling's `Success`. This was a regression introduced by this branch (the
   output previously lived in a shell variable, i.e. per-process). Fixed by making
   the log per-invocation; verified with the same two-build reproduction.

Also reproduced and fixed: a watcher that never becomes ready failed instead of
falling back; `dev-diff` selected artifacts by basename across the whole tree and
showed an unrelated test's diff (609 test files here share a basename; `test.ml`
occurs 83 times); `dev-stdlib-check` misattributed any ordinary compiler failure
to a stale stdlib and hard-failed `dev-check`, which blocked `dev-ocamlc` — the
tool you would use to investigate; `dev-promote` asserted plain-vs-`-principal`
instability it had not established; `dev-test-all` did not export `TMPDIR`;
`watcher-command` recorded a desired rather than a running command; and the first
failed attempt's output was emitted twice on the cannot-restart path.

**One decision reversed.** Item 6f, suppressing dune's forwarding notice, is
deleted. Codex constructed a counterexample where the filter dropped a legitimate
diagnostic that merely contained one of the matched substrings, and the `dev-errors`
pipe also swallowed dune's exit status. Hiding three lines of known noise is not
worth hiding output. The self-test now pins the notice being *passed through*, so
the decision is recorded rather than merely undone.

**One review suggestion falsified.** Both reviewers proposed delegating
`dev-configure` to `tools/autogen`, the wrapper intended for exactly this, and the
claude reviewer noted that bypassing it skips its `sed` patches. It does not work
on this tree: `tools/autogen`'s `--warnings=all,error` rejects this fork's
`configure.ac` (many `AC_RUN_IFELSE called without default to allow cross
compiling`). Measured: `tools/autogen autoconf27` exits 1, plain `autoconf27`
exits 0. So the suggestion is not adopted, and the reason is now a comment in the
target. Their *other* point was correct and is fixed: my awk version comparison
failed open — awk with no input records exits 0, and a non-numeric `$NF` compares
lexicographically — so it accepted a too-old autoconf whenever the version line
was unusual or absent. Version detection is now gone entirely: `AC_PREREQ([2.71])`
makes an old autoconf fail by itself, so trying each candidate and keeping the one
that works cannot accept a version that would not have worked. That is both
simpler and more correct than what I wrote.

## Deliberately not done

- **Item 3b, the `ocamlc.byte` magic mismatch (4/5 reports) — measured, and the
  measurement rules out both candidate fixes.** No code landed for it, but the
  question is now answered rather than open.

  Baseline, three affected directories: under the dev harness 12 tests fail
  (`formatting` 1, `typing-ocamlc-i` 6, `tool-ocamlc-stop-after` 5); under
  `_runtest` all 12 pass. So they are purely harness failures, as reported.

  I then applied the candidate fix — point `ocamlc.byte` at `main_native.exe` in
  `prepare_test_root` — and re-ran: **exactly the same 12 failures**. The reason is
  visible in the failing command, and it refines the reports' account:

      <runtest>/runtime/ocamlrun <runtest>/ocamlc -use-runtime ... -i -o ... local.ml
      the file '<runtest>/ocamlc' has not the right magic number:
        expected Caml1999X583, got

  ocamltest runs `ocamlc` *through* the in-tree `ocamlrun`, so `ocamlc` must be a
  bytecode image with that runtime's magic. `main.bc` carries trailing magic
  `Caml1999X036` and a shebang pointing at the opam `ocamlrun`
  (`~/.opam/5.4.0/bin/ocamlrun`), which is why the unmodified harness reports
  "got Caml1999X036"; the native executable has no Caml magic at all, which is why
  the redirect reports "got " and fails identically. **No symlink arrangement can
  fix this**: the only real fix is producing a `main.bc` whose magic matches the
  in-tree runtime, i.e. building the bytecode compiler in a context that uses it —
  a build-system change, out of scope here.

  So the answer to the design doc's open question is the skip-list, now on evidence
  rather than as a default. ocamltest already has the mechanism:
  `OCAMLTEST_SKIP_TESTS` (`ocamltest/main.ml:254`), matched against the test
  filename. I did not implement it, because deciding what the dev harness declares
  it does not cover is a scoping decision I would rather you make (open question 1).
  Consequence meanwhile: `dev-test-all` cannot be green, and these tests present as
  regressions. 3a (the missing `ocamlopt`/`ocamlopt.byte` links) did land, and is a
  genuinely separate defect from this one.
- **Item 9's stdlib fingerprinting** stays out of scope for the reasons in the
  design doc. The *detection* the doc promised was initially missing — the claude
  reviewer caught the discrepancy — and is now in `e95dc422b7`: when `dev-test-all`
  output contains "inconsistent assumptions over interface Stdlib", it prints the
  cause and the known-working recovery, including that `rm -rf _build/main` alone
  corrupts dune's incremental state. Not exercised: I did not reproduce the 661-
  failure state.
- **Item 7, a single-instance lock** (2/5, not 3/5 as the synthesis said).
  Deferred as planned: `dev-test-all` legitimately holds the loop for 30-40
  minutes, so a naive exclusive lock serialises a reviewer behind it and creates a
  *new* silent wait. Note that findings 3 above means this branch made the
  concurrency story better in one way (per-invocation logs) without addressing the
  underlying absence of a lock.
- **Item 8** (stale `_runtest` compilerlibs), **item 10** (the expect runner as a
  second full compiler build — the largest remaining throughput cost, 8-10 min per
  iteration), **item 11** (a shared dune cache). Reasons in the design doc; item
  10 is the strongest candidate for the next piece.

## The full suite found a defect nobody had listed

`make dev-test-all` on this branch: **2442 passed, 208 skipped, 1 failed** of 2651
considered. The single failure was the fixture added earlier on this branch, and it
was not flaky — it had caught a real defect in the loop.

`install_for_test` copies ocamltest from the default build dir, but `dev-test-all`
passes `-o boot-compiler`, so make treats that dir as up to date; and the watcher
builds ocamltest into `_build/dev-dune`, which `install_for_test` never reads.
So **a change to ocamltest is invisible to the full suite**, which runs against the
previous harness. Measured: `_build/default/ocamltest/ocamltest.native` was 39
minutes older than the ocamltest source change (10:49:43 vs 11:28:51), and
`_runtest` received that stale copy; the reference in `_runtest` was correct while
the result was the pre-fix `reference not created`.

This is the same class as the stale-stdlib and stale-compilerlibs traps the reports
describe — with the harness itself as the stale artifact — and silent in the same
way. It only surfaced because a test happened to assert on ocamltest's own
behaviour. Fixed in `e95dc422b7` by building `ocamltest/ocamltest.native` before
`install_for_test` (seconds, not a boot-compiler rebuild). Verified: with the
refreshed harness the fixture passes under `_runtest`, where it had failed.

Two changes on this branch are shared beyond the dev loop and so were the reason
for running the suite at all: `Makefile.config_if_required` (the configuration gate
for every target) and `ocamltest/actions_helpers.ml` (promotion for every test).
Neither caused a failure. The whole `tool-ocamltest` directory also passes, 7/7.

## Verification I did not do

- I did run the full suite, and it earned its keep — see below. What is *not*
  verified is a second full-suite run after `e95dc422b7`, so the harness-refresh
  fix is verified only against the single test that exposed it.
- Items 3a, 4d, 4e, 6d, 6e are landed but not exercised (table above). 4e and 6e
  in particular need a fixture that is genuinely unstable between the plain and
  `-principal` runs, and a python 3.6 interpreter, respectively.
- I did not measure whether the 5s-per-signal escalation grace is right; it makes
  the worst case `2 × (timeout + 10s)` plus the fallback, which is negligible at
  the default 1800s timeout and disproportionate at a very short one.

## Open questions for the owner

1. **Item 3b.** The measurement above removes the choice I thought I had: the
   `main_native.exe` redirect cannot work, so it is a skip-list or nothing. What I
   want a ruling on is the scope — skip-list the ~12 tests in the three directories
   I measured, or sweep the whole suite for `ocamlc.byte`/`ocamlopt.byte` actions
   first so `dev-test-all` can actually be asserted green? I would do the sweep,
   since a partial list leaves the full suite red and therefore ignored, which is
   the status quo. Either way the marker text should name what the dev harness does
   not cover, so nobody re-does this forensics.
2. **Promotion of a missing reference file.** A missing reference is also how
   ocamltest spells "this output must be empty", and the claude reviewer counted
   **290** tests under `testsuite/tests/` that run an output check with no
   `.reference` sibling. So `make dev-promote DIR=...` can now create references
   for them, converting "asserts empty" into "asserts whatever it produced". I
   added a comment at the promotion site rather than a mechanism. If you want a
   guard, the cheap version is for `dev-promote` to list newly *created* reference
   files at the end, so they get read.
3. **`dev-stdlib-check` now warns rather than fails** for an ordinary compiler
   failure, and only errors on death by signal. That keeps `dev-ocamlc` usable
   mid-refactor, which I think is right, but it means `make dev` can print a
   warning and still exit 0 with a compiler that cannot compile hello-world.
4. **Where `diff` lives.** Both reviewers judged that artifact archaeology does
   not belong in a file called `dev-watcher.py`. I agree and left it, because the
   better fix they both point at is upstream of it: have `dev-test` pass an
   explicit `OCAMLTESTDIR` so there are two artifact roots instead of three, which
   would make `dev-diff` nearly trivial and remove the source-tree root entirely.
   That is a change to `testsuite/Makefile`'s `exec-one`, shared with the non-dev
   harness, so I did not make it unasked.
