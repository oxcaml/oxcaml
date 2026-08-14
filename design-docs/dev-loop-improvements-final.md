# dev-loop-improvements — final report

Branch `jujacobs/vox/dev-loop-improvements`, based on `jujacobs/optimize-dev-loop`
@ `6929017cbd`. Plan and evidence base: `design-docs/dev-loop-improvements.md`.

## Commits

| sha | what |
| --- | --- |
| `6edb813a48` | the design doc: ranking, verification pass over the reports' claims, scope |
| `8433611e25` | the tooling: wedge recovery, heartbeat, stale-stdlib detection, NOWATCH, dev-configure, promote fixpoint, dev-diff, dev-errors, ocamlopt links, python guard |
| `a14da1dabf` | `AGENTS.md`: setup, new targets, failure modes |
| `d08e0c4d1f` / `de47bdb8d8` | RED / GREEN: ocamltest `-promote` creates a missing reference file |
| `b87de5e4fe` | `dev-diff` handles program-output tests |
| `a7bd9f2024` | fixes from the review loop |
| `fa727a341c` | `dev-test-all` refreshes the test harness; stdlib-digest diagnosis |
| `daab428772` / `48892bda89` | RED / GREEN: `ocamlc.byte` tests skip, naming the compiler |

Plus this report and its amendments.

Rebased onto **`6f4374f24b`** (`Use autoconf27 in development setup`), so these shas
supersede the pre-rebase ones; the tree content was verified identical across the
rebase. The base already changes `AGENTS.md` from `autoconf` to `autoconf27`, so the
one conflict was resolved in favour of `make dev-configure` with the by-hand form
kept as a trailing comment — this branch does not duplicate the base's fix. The rest
of item 6a is still needed, because the doc fix alone does not help a fresh
worktree: `make dev-configure` and the `Makefile.config` precondition are what turn
the failure into an instruction.

## What landed, and whether it was exercised

"Exercised" means run against the failure it targets. I distinguish that from
"reasoned through" per item, because three items are only the latter.

| Item | Convergence | State |
| --- | --- | --- |
| 1. rpc wedge: heartbeat, timeout, watcher restart + one retry, direct fallback | 5/5 | **Exercised, including a real wedge** |
| 2. stale `_runtest` stdlib detector + `dev-refresh-stdlib` | 2/5 | **Exercised against a real SIGSEGV**; cure verified |
| 3a. `ocamlopt`/`ocamlopt.byte` missing from the dev test root | 4/5 | Landed, **not exercised** |
| 3b. `ocamlc.byte` magic mismatch | 4/5 | **Measured, then landed** — red-green, see below |
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

### Item 3b, resolved by measurement and then landed

**Measured first, and the measurement ruled out the fix I expected to make.**

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

  **Landed** in `daab428772` (RED) and `48892bda89` (GREEN):
  `OCAMLTEST_SKIP_BYTECODE_COMPILERS` makes the `ocamlc.byte` and `ocamlopt.byte`
  actions skip rather than fail, with a reason naming the compiler —
  `ocamlc.byte is not runnable under this harness, so this test does not cover it`.
  It mirrors the existing `native_action`/`no_native_compilers` pattern a few lines
  above, which already skips actions for a capability the harness lacks, so it is
  no new mechanism. Implemented per action rather than as a list of test files, so
  there is no list to go stale and the coverage is the whole suite at once.
  `dev-test` exports it; `dev-test-all` deliberately does not.

  Measured effect on the three directories: `formatting` 2 passed / 1 skipped / 0
  failed (was 2/0/1), `typing-ocamlc-i` 0/6/0 (was 0/0/6), `tool-ocamlc-stop-after`
  0/5/0 (was 0/0/5). So `make dev-test DIR=...` can now be green where it could
  not be. Scoping verified in both directions: the same tests still run and pass
  under `_runtest` (`make test-one-no-rebuild DIR=typing-ocamlc-i` → 6 passed, 0
  skipped).

  **A sixth correction to the reports** came out of this. They say `dev-test-all`
  "can never be green" on these tests. That is false: `dev-test-all` uses
  `_runtest`, where `ocamlc.byte` is the real installed bytecode compiler. The
  full-suite run on this branch had **zero** failures in all three directories, and
  the affected tests pass there. Only `dev-test` was ever affected. The consequence
  is worth stating positively: bytecode-compiler behaviour is still covered, by the
  full suite, and the skip message says so rather than implying the coverage is
  gone.

  3a (the missing `ocamlopt`/`ocamlopt.byte` links) is a genuinely separate defect
  and landed earlier.
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
  *new* silent wait. This branch improved the concurrency story in one respect
  (per-invocation build logs, review finding 3) without addressing the absent lock.

  I was offered a cheap partial win — an advisory notice that prints "another dev
  command is running (pid N), started Xm ago" and then proceeds — if it fell out of
  item 1's work. It does not: `build` has no way to know about a sibling dev
  command, because nothing records one. The watcher lease is renewed by every
  command and so cannot distinguish one caller from two, and scanning the process
  table for sibling `dev-watcher.py` processes is the kind of guess that
  misidentifies a reviewer's worktree. Doing it properly means recording running
  commands in `_build/dev/`, which is the lock's bookkeeping minus the lock — so it
  belongs with item 7, not bolted on here.

  Worth recording as evidence for whoever takes it: I hit this myself during this
  work. Two overlapping `make dev-test` runs interleaved their heartbeats into one
  log, so the output read as a single build reporting two different elapsed times
  (`still building (300s...)` next to `still building (330s...)`). I diagnosed it
  with `ps`, which is exactly what the reports describe. So the notice would have
  paid for itself even for me, on this branch, with the heartbeat already in place.
- **Item 8** (stale `_runtest` compilerlibs) and **item 11** (a shared dune cache
  across worktrees). Reasons in the design doc; item 11 is a measurement task
  before it is an implementation task.

## Proposed next piece: the expect runner's build cost (item 10)

This is the largest remaining throughput cost in the corpus and the strongest
candidate for a follow-up, so recording it as a candidate with evidence rather
than a leftover.

**Evidence.** `feedback-type-formers.md:39-46`: after any compiler change,
`dev-test` on an expect test pays the fast watcher build *plus* a `main.ws` build
of `expect.exe` — **~8-10 minutes per iteration** when iterating on parser or
typing changes together with expectations. That session calls it "the dominant cost
of the test loop all session" and "the gap between 'fast loop' and 'fast loop
except for tests'". 1/5 reports, but from the session that did the most
expectation-heavy work, and it is a throughput cost paid on every iteration rather
than an incident.

**Where it comes from.** `dev-test` refreshes the runner through
`dev-expect-runners`, which stops the watcher and runs
`dune build $(ws_main) oxcaml/testsuite/tools/<runner>.exe`. The staleness check is
correct and fires when it should; the cost is that the runner is built in the main
workspace, against the main compiler, not in the boot/dev context the watcher
already maintains.

**Why it is not a friction fix.** Building the runner in the dev context would make
it fast, but it changes what the runner links against, and therefore what expect
tests actually exercise. That is a question about the harness's meaning, not its
speed, and it deserves its own design doc and its own review — which is why it was
kept out of this branch. Whoever picks it up should start by establishing what the
runner is supposed to be built against and why, not by making the build faster.

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

## The `/tmp` rule: what was leaking, and what was left behind

The exposure was wider than the one `mktemp` in `dev-promote`: ocamltest itself
calls `Filename.temp_file` at `ocamltest/filecompare.ml:231` — on **every** failing
test comparison, to hold the diff — and at `ocamltest/actions_helpers.ml:267,309`
for response files. `Filename.temp_file` honours `TMPDIR` and falls back to `/tmp`,
so every failing test run under a caller without `TMPDIR` set was writing there.
Fixed by defaulting `TMPDIR` inside the worktree from `dev-test` and `dev-test-all`,
and by moving `dev-promote`'s own scratch to `_build/dev`.

**Residue check, since files a service user leaves in a sticky directory cannot be
removed afterwards.** I audited both directories rather than assuming a cleanup
fired:

- `/var/tmp` — every entry is root-owned system state (`abrt`, `krb5_0.rcache2`,
  systemd private dirs, `metrics-config.tar.gz`, `Velociraptor_Buffer.bin`).
  Nothing of ours, nothing to remove.
- `/tmp` — no `ocamltest*`, `oxcaml*` or `*dev-promote*` entries at all, confirmed
  by explicit glob rather than by eye. The `jujacobs`-owned entries there belong to
  unrelated tooling (`blueprint-worker.log`, `jira_fields_cache.sexp`,
  `.vscode.all-feature-names-cache.txt`, `exe-server-40358`, `tmux-40358`, krb5
  credential caches) and the agent-tooling directories `.agents`, `.codex`, `.git`,
  `dhp.tmp.*` are all **empty**.

So nothing needed cleaning up. The reason is visible in the source and is worth
recording so the fix is not mistaken for having been urgent in the wrong way:
ocamltest removes these files on the normal path (`Sys.force_remove` at
`filecompare.ml:251` and `actions_helpers.ml:304,358`). The leak is therefore
transient — it only survives when a run is *killed*, which the reports describe
happening repeatedly to wedged runs, and which happened several times during this
work. The rule violation was real and is fixed; the accumulation had not yet
happened.

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

1. ~~Item 3b~~ — **resolved and landed.** Implementing the skip per *action* rather
   than as a list of test files made the scope question moot: there is no list to
   go stale, and the coverage is the whole suite at once. The marker names the
   compiler, as asked.
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
