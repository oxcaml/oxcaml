# dev-loop-improvements

Branch: `jujacobs/vox/dev-loop-improvements`, based on `jujacobs/optimize-dev-loop`
@ `6929017cbd`.

## Problem

`make dev` and friends (`Makefile.common-ox:511-688`, `tools/dev-watcher.py`) give
a fast incremental compiler loop built on a persistent `dune --watch` daemon
driven over dune's RPC. Five agents have now each built one vox piece end to end
with it — including review loops in separate worktrees — and written up the
experience. The loop's happy path is unanimously reported as good. Its failure
paths are not: they are silent, they are indistinguishable from slow-but-working,
and two classes of them hand you a wrong answer rather than an error.

This piece attacks the failure paths, in order of measured cost to the sessions
that hit them.

## Evidence base

Five independent session reports, all against `6929017cbd`, in
`/usr/local/home/jujacobs/vox/dev-loop/`:

| Report | Piece built | Scale of use |
| --- | --- | --- |
| `feedback-from-totality-piece.md` | totality/logicality | 1 fresh setup, ~40 watcher cycles, ~80 single tests, 6 full-suite runs, 4 reviewer worktrees |
| `solver-interface-feedback.md` | solver-interface | 1 initial build, ~6 library rebuilds, ~10 test/promote runs, 3 reviewer worktrees |
| `feedback-type-formers.md` | type-formers | "hundreds of iterations", parser + `Types`-layout + printer changes |
| `feedback-erasure-session.md` | erasure | mode-axis change (world rebuilds), 50+ `dev-ocamlc` runs, 2 full-suite runs, 3 reviewer worktrees |
| `make-dev-feedback-bigint.md` | bigint | fresh worktree, stdlib change, full suite, promotions, 4 sibling worktrees building concurrently |

Convergence counts below are mine, recounted from the reports rather than taken
from the synthesis I was handed; where they differ I say so.

## Verification pass

Every file:line claim in the five reports and in the owner's synthesis was
checked against this worktree. Results:

### Confirmed

| Claim | Status |
| --- | --- |
| `dev-promote` scratch goes to `/tmp` when `TMPDIR` unset | VERIFIED `Makefile.common-ox:647` |
| `dev-check` has `wait-ready --timeout 300` for the ping but no timeout on `dune rpc build` | VERIFIED `Makefile.common-ox:557` vs `560` |
| `dev-runtime` only refreshes on `runtime stdlib otherlibs` source changes (plus `Makefile.config`, `duneconf/runtime_stdlib.ws`) — nothing compiler-side | VERIFIED `Makefile.common-ox:541-553` |
| `dev-setup` only fires when `_install/bin/ocamlopt.opt` or `_runtest/testsuite/Makefile` are missing; no `Makefile.config` precondition | VERIFIED `Makefile.common-ox:532-539`; the obscure failure comes from `_build/_bootinstall: Makefile.config` at `Makefile.common-ox:204` |
| `dev-test` runs in `_build/dev/runtest/testsuite`; `dev-test-all` runs `install_for_test` + `test-files-parallel`, which `cd`s to `_runtest/testsuite` | VERIFIED `Makefile.common-ox:618-626`, `641-643`, `452-463` |
| `dev-check` captures rpc output into a shell variable, so nothing streams | VERIFIED `Makefile.common-ox:559-566` |
| `one LIST=` treats each line as a `DIR`, not a test file | VERIFIED `testsuite/Makefile:338-342` (`exec-one DIR="$LINE"`) |
| `dev-watcher.py` needs python ≥ 3.7 (`add_subparsers(..., required=True)`) | VERIFIED `tools/dev-watcher.py:353` |
| Hand-promoting `.corrected` drops principal-block updates | VERIFIED `ocamltest/ocaml_actions.ml:916-938`: pass 1 writes `<file>.corrected`; if the runner exits 3 ("needs principal") pass 2 runs on that file and the promoted `output` becomes `<file>.corrected.corrected` |
| No flag exists to suppress dune's "build request is being forwarded" notice | VERIFIED against `dune rpc build --help`, dune 3.23.0 |

### Corrected

Five claims did not survive checking. Each of them would have shaped a fix.

1. **`dev-promote` does *not* exit nonzero after a successful promotion.**
   The synthesis says it does. `Makefile.common-ox:645-663` already runs
   `dev-test PROMOTE=1`, and on failure re-runs `dev-test` to verify, printing
   `dev: promoted test output` and exiting 0. What exits nonzero after
   promoting is `make dev-test PROMOTE=1` used directly, which is what
   `feedback-type-formers.md:76` actually reports.
   Root cause found, and it is not in the Makefile:
   `ocamltest/actions_helpers.ml:392-398` promotes and *then* still returns
   `Result.fail_with_reason`. So the fix belongs in ocamltest, and it also
   explains item 4's "multi-reference tests need promote run twice": the failed
   action aborts the test before the remaining references are reached.

2. **`ocamlopt.byte` tests do not fail for the magic-number reason.**
   The synthesis groups `flambda2/` and `layout_poly/` with the `ocamlc.byte`
   magic mismatch. They are a different, simpler defect:
   `tools/dev-watcher.py:240-268` lists `ocamlopt` and `ocamlopt.byte` in
   `overridden` (so the `_runtest` originals are *not* symlinked in) and then
   only re-creates `ocamlopt.opt`. `ocamltest/ocaml_files.ml:48` resolves the
   `ocamlopt.byte` action to `$srcdir/ocamlopt`, which therefore does not
   exist — hence bigint's literal `cannot find file _build/dev/runtest/ocamlopt`.
   Two defects were conflated; the second is a three-line fix.

3. **There are three artifact locations, not two.** `dev-test-all` →
   `_runtest/testsuite/_ocamltest/` and `dev-test TEST=` →
   `_build/dev/runtest/testsuite/_ocamltest/` are both real. But
   `dev-test PROMOTE=1 DIR=…` routes through `promote` → `one DIR=` →
   `exec-one`, which sets `OCAMLTESTDIR=$(BASEDIR_HOST)/$(DIR)/_ocamltest`
   (`testsuite/Makefile:40,351`) — and under the dev root
   `testsuite/tests/<dir>` is a symlink into the source tree
   (`tools/dev-watcher.py:329-336`), so that lands **in the source
   directory**. `dev-test DIR=` without `PROMOTE` uses `files-parallel`, where
   `OCAMLTESTENV` is empty and artifacts stay in the dev root. This is the
   exact mechanism behind `solver-interface-feedback.md:73-84`, which reported
   the symptom correctly but could not name the cause.

4. **"Stale `.corrected` files survive later passing runs" is already false for
   the dev root.** `prepare_test_root_locked` rebuilds `_build/dev/runtest`
   from scratch into `runtest.new` and renames it over the old one on *every*
   `dev-test` (`tools/dev-watcher.py:236-243, 338-343`), which destroys
   `_build/dev/runtest/testsuite/_ocamltest` wholesale. So the suggested "have
   `dev-test` delete the test's `_ocamltest` work dir before running" is
   already satisfied, and I am not building it. Stale artifacts do survive in
   the two places the dev loop does not own: the source tree (via the
   `exec-one` path in correction 3) and `_runtest/` (via `dev-test-all`).

5. **The single-instance lock has two reports behind it, not three.**
   `feedback-type-formers.md:48-58` and `feedback-erasure-session.md:59-65`.
   `make-dev-feedback-bigint.md:19-21` says the opposite thing about a
   different axis ("four watchers on one box, no cross-talk"). The item stays,
   at its real weight.

### Newly found, not in any report

- **ocamltest scatters scratch into `/tmp`** beyond `dev-promote`:
  `Filename.temp_file` at `ocamltest/filecompare.ml:231` (every diff of a
  failing test) and `ocamltest/actions_helpers.ml:267,309` (response files).
  `Filename.temp_file` honours `TMPDIR`, so the dev targets must export it.
  This is squarely the house `/tmp` rule and is broader than the one `mktemp`
  the reports found.
- **`dev-promote` cannot create a missing reference file** for a precise
  reason: `ocamltest/filecompare.ml:221-228` returns `Unexpected_output` when
  the reference does not exist, and `ocamltest/actions_helpers.ml:400+` handles
  that case without ever consulting the `promote` variable — while
  `Filecompare.promote` (`filecompare.ml:254-257`) opens the reference for
  writing and would happily create it. So bigint's item 3 is a three-line fix
  in a branch nobody wired up, not a design limitation.

## Ranked work items

Ranking is by measured cost to the sessions, not by implementation ease. Items
1-6 are what I intend to land; 7-9 are explicitly deferred with reasons.

---

### 1. The `dune rpc build` wedge, and the silence that hides it

**Evidence.** 5/5 reports. This is the only item every session hit.

- erasure: "~10 times in one session", also hit the review agents in fresh
  worktrees, estimated 1.5-2h of the session lost.
- solver-interface: four recurrences, clients killed at 36, 17, 9 and 4 minutes
  elapsed — ~66 minutes of pure waiting, plus the diagnosis time.
- bigint: twice, and "sticky" — after the first wedge, later clients also hang.
- totality: two flavours, both needing manual `make dev-stop`.
- type-formers: `Connection_dead` twice under load.

Silence is a separate 5/5 item but it is the same fix, so they are bundled: the
wedge is expensive *because* it is indistinguishable from a legitimate rebuild.
Sessions diagnosed it by `ps`-ing the daemon's cumulative CPU time. The reports
are unanimous that a heartbeat naming `make dev-log` would have made this
self-diagnosing.

There is live evidence on this box right now: two `make dev-test DIR=vox`
processes in a sibling worktree at 40 and 34 minutes elapsed, 0.0% CPU.

**Decided fix**, in `dev-check`:

1. Echo before the build: `dev: building via the watcher (progress: make dev-log)`.
2. A heartbeat while the build runs: every `DEV_HEARTBEAT ?= 30` seconds, one
   line with elapsed time.
3. Wrap the rpc build in `timeout $(DEV_RPC_TIMEOUT)`, `DEV_RPC_TIMEOUT ?= 1800`,
   overridable.
4. On timeout, or on output matching `Connection_dead` / `Connection terminated`:
   stop the watcher, start it again, and retry exactly once.
5. If the retry also fails, fall back to a direct `dune build $(ws_boot)
   $(dev_build_flag) $(dev_boot_targets)` with no rpc at all, so the command
   completes rather than failing. Say clearly that it fell back.

Retry-once-then-fall-back rather than retry-until-success: an unbounded retry
loop against a systematically broken watcher is a new silent hang, which is the
thing being fixed.

**Alternatives rejected.**

- *Watcher writes a `build-result` marker (status + generation counter) that
  `dev-check` polls*, sidestepping rpc entirely (suggested by
  solver-interface). This is a bigger change that re-implements the part of
  dune's RPC semantics we actually depend on — "has the build for the current
  source generation finished" — with a hand-rolled generation counter that has
  to be correct against inotify races. It also does not help the first build of
  a session, where there is no watcher yet. Held in reserve: if timeout + retry
  turns out to fire routinely rather than rarely, this becomes the right answer
  and gets its own piece.
- *CPU-idle watchdog* (suggested by erasure): restart when the daemon has used
  ~no CPU for N seconds. Rejected as the primary mechanism — a dune legitimately
  blocked on a slow NFS read or on a sibling worktree's lock also shows 0% CPU,
  so this would kill healthy builds on a loaded box, which is exactly the box
  these reports come from. The wall-clock timeout is coarser but cannot
  misclassify.
- *Streaming rpc output instead of a heartbeat.* `dune rpc build` reports
  diagnostics, not progress; the progress lives in the watcher's own log. A
  heartbeat that points at `make dev-log` is honest about where the information
  is. Capturing the output is also load-bearing for the `Success` check at
  `Makefile.common-ox:567`, so streaming means restructuring that too.

**Verification.** Reproducing a wedge on demand is not reliable, so the timeout
and recovery path get exercised against a stub: a fake `dune` on `PATH` whose
`rpc build` sleeps forever, with `DEV_RPC_TIMEOUT` set to a few seconds. That
shows timeout → watcher restart → retry → fallback in order. Separately, the
heartbeat and the fallback are exercised for real by using this branch's own
loop for the rest of the piece.

---

### 2. Stale `_runtest` stdlib after a `.cmi`-shape change

**Evidence.** 2/5 reports, and the most expensive per occurrence: both sessions
that hit it ended up in a debugger.

- totality: `.cmi` shapes changed without a magic bump; the expect runner
  SIGSEGVs; ~45 min lost bisecting the test file and suspecting solver
  nontermination.
- type-formers: changed the `Types` layout with the magic bump deferred; "I was
  in gdb before realizing the compiler was fine and the cmis were stale".
  Notes that "every vox piece that touches `Types` will hit this" — which,
  given what vox is, is a forecast worth taking seriously even at 2/5.

`dev-runtime` cannot catch this: it compares the artifact against
`runtime stdlib otherlibs` sources only (`Makefile.common-ox:547-549`), and
here the sources did not change — the *reader* did.

**Decided fix.** Do not predict; detect. After a successful build in
`dev-check`, and only if the dev compiler exists, compile a one-line file with
the dev compiler against the installed stdlib, with scratch under
`_build/dev/smoke/`. On any failure or signal, print one actionable line —

    dev: the compiler cannot read the installed stdlib; a compiler change may
    dev: have altered .cmi shapes. Run `make dev-refresh-stdlib`.

— and fail. Add `dev-refresh-stdlib` (watcher stop + `runtime-stdlib`) as the
named cure.

Placement matters and the reports get it slightly wrong. totality suggests
"after `prepare-test-root` in `dev-test`". But `dev-ocamlc`/`dev-ocamlopt` read
the same stdlib and erasure used those 50+ times, so the check belongs at the
end of `dev-check`, which every dev command depends on. It also must run *after*
the build, not in `dev-runtime`: `dev-check`'s prerequisites are
`dev-setup dev-runtime dev-start`, so a check in `dev-runtime` would test the
previous compiler binary, not the one the command is about to use.

**Alternatives rejected.**

- *mtime heuristics over `typing/` / `file_formats/`* (suggested by
  type-formers). Rejected, and both reports say why: any such witness rebuilds
  the stdlib on every ordinary `typing/` edit, which is most edits, turning the
  fast loop into a slow one. type-formers explicitly notes "rebuilding stdlib on
  every compiler change is too slow".
- *Bump the cmi magic number.* Not available: the whole failure class exists
  because the magic bump is deliberately deferred during development.
- *Print a hint unconditionally* (type-formers' fallback suggestion: "if
  compiled programs crash after changing Types, run make runtime-stdlib").
  Rejected: a hint printed on every build is noise that is not read, and the
  sessions that needed it were looking at a SIGSEGV, not at `make dev`'s
  output.
- *Refresh automatically on detection* instead of failing with a message.
  Tempting, and I may revisit, but `runtime-stdlib` is minutes long and
  triggering it implicitly from `make dev` reintroduces "a long silent phase
  you did not ask for", which is item 1. Naming the target keeps the cost
  visible and voluntary.

**Verification.** Make a real `.cmi`-shape change (add a field to a type that is
marshaled into cmis) on a scratch commit, rebuild, and show the diagnostic
fires with this message where the baseline segfaults. Also measure the added
per-build cost and report it; if it is not comfortably under the noise floor of
a warm `make dev`, the check moves behind a flag.

---

### 3. `ocamlc.byte` / `ocamlopt.byte` tests under the dev harness

**Evidence.** 4/5 reports (totality, type-formers, erasure, bigint). The cost is
not one big incident but a recurring tax: the failure presents as a magic-number
error buried in a test log, every session reclassifies it from scratch, and
`dev-test-all` can never be green, which devalues the whole full-suite signal.
Affected: `formatting/`, `tool-ocamlc-stop-after/`, `zero-alloc/cmi_test`,
`parsetree/test_ppx`, `templates/`, `typing-ocamlc-i/`, and via `ocamlopt.byte`
`flambda2/` and `layout_poly/`.

Per correction 2 these are two defects:

**3a. `ocamlopt` is simply absent from the dev test root.** Fix: link
`ocamlopt`/`ocamlopt.byte` alongside the existing `ocamlopt.opt` in
`prepare_test_root_locked`. Cheap, and it is a plain omission — `ocamlc.byte`
and `ocamlc` are both created a few lines above.

**3b. `ocamlc.byte` → `main.bc` cannot execute.** `main.bc` is built by the boot
workspace with the opam host compiler (`Caml1999X036`) but the dev test root's
`ocamlrun` is the in-tree one (`X583`). Candidate fixes: point `ocamlc.byte` at
`main_native.exe` (runs, but is no longer a bytecode compiler); build `main.bc`
against a matching runtime; or skip-list with an explicit "unsupported under dev
harness" marker.

**Decided fix: measure before choosing.** This is the one item where the answer
is empirical and I will not decide it from the armchair. The experiment: apply
3a and the `main_native.exe` variant of 3b, run every affected directory under
the dev harness, and compare pass/fail per test against the same directories run
under `_runtest` (the real harness). If the sets agree, the redirect is right and
the skip-list is unnecessary complexity. If any test genuinely discriminates
`ocamlc.byte` from `ocamlc.opt` — `ocamltest/ocaml_actions.ml:1033` warns that
under flambda2 they "sometimes generate equivalent" but not identical output, so
this is a live risk — then those tests get the explicit skip marker and the rest
get the redirect. The design doc will record which, with the measurement.

Whichever way it lands, the outcome to insist on is that a test unsupported
under the dev harness says so in one line, rather than presenting as a
regression.

---

### 4. `dev-promote`: two real defects and two legibility bugs

**Evidence.** 4/5 reports, though as separate sub-items rather than one
converged complaint.

**4a. Cannot create a missing reference file** (bigint 3). Root cause verified
above. Fix in `ocamltest/actions_helpers.ml`: honour `promote` in the
`Unexpected_output` branch. Red-green testable, and the natural home for the
fixture is `testsuite/tests/tool-ocamltest/`, which already exists.

**4b. Multi-reference tests need promote run twice** (bigint 4). Same root
cause: `actions_helpers.ml:397` fails the action *after* promoting, so the test
aborts before later references are reached. Two candidate fixes — make the
promoting branch pass, or loop `dev-promote` to a fixpoint. I intend the second.
Making a promoted check pass silently converts "your output changed" into "no
news", which is the report signal expect tests exist to give; the two-pass
"promote, then verify" shape is right, it just needs to be iterated to a
fixpoint (bounded, 3 attempts) instead of exactly once.

**4c. `/tmp` scratch.** `Makefile.common-ox:647` → `_build/dev/`. Also export
`TMPDIR` into the test environment so ocamltest's own `Filename.temp_file` uses
(`filecompare.ml:231`, `actions_helpers.ml:267,309`) stay inside the worktree.
Binding house rule, and broader than the reports realised.

**4d. Log ordering** (solver-interface 4). The promote log is `cat`ed after the
verify output, so it reads reverse-chronologically. Print it first.

**4e. Say when output is unstable** (erasure 5, totality's last bullet). When
promotion does not converge because the plain and `-principal` runs disagree,
say that in a sentence — `dev: output differs between the plain and -principal
runs; this test needs a Principal{| … |} block by hand` — instead of only
printing a diff. erasure records "a puzzled half hour" on exactly this.

Note that 4a and 4b are changes to ocamltest, not to the dev loop, and would
improve `make promote` for upstream users too.

---

### 5. `make dev NOWATCH=1`

**Evidence.** 1/5 reports (totality 6), and the low count understates it,
because the sessions that needed it worked around it rather than paying for it.
Under a `workspace-write` sandbox — which is how codex reviewers run — the
watcher dies with `bind(): Operation not permitted` because dune's RPC socket
cannot be created, and there is no fallback. The current workaround is to point
review agents at a prebuilt runner borrowed from another worktree, which is
fragile and means reviewers cannot independently build the branch they are
reviewing. Every future review loop pays this.

**Decided fix.** `NOWATCH=1` makes `dev-start` a no-op and `dev-check` run a
plain `dune build $(ws_boot) $(dev_build_flag) $(dev_boot_targets)` with no
`wait-ready`, no rpc, no heartbeat. Slower per invocation, works anywhere.
`dev-expect-runners` keeps working because its `dev-start` becomes a no-op too.

This composes with item 1's fallback: item 1's fallback is the same `dune build`
command reached automatically after a failure, so `NOWATCH=1` is that path made
explicit and unconditional. Implementing them together keeps one code path.

**Verification.** Run `make dev NOWATCH=1` in a worktree with no watcher and
confirm it builds and that no watcher process or socket appears. Then have a
codex reviewer, which really does run under `workspace-write`, build the branch
with it — that is the actual target environment and the only honest test.

---

### 6. Cheap wins with real evidence behind them

Grouped because each is small, but they are not filler — the autoconf item has
the joint-highest convergence count in the whole corpus.

**6a. Fresh-worktree bootstrap — 5/5 reports.** Every single report mentions it:
`configure.ac:19` is `AC_PREREQ([2.71])`, the `autoconf` on PATH is 2.69, and
`configure` is untracked, so every new worktree needs `autoconf27` or a copied
`configure`. With worktree-per-piece *and* worktree-per-reviewer that is 5-8
worktrees per piece, and four of the five sessions resorted to copying
`configure` from a sibling.

Fix: a `dev-configure` target that finds an autoconf ≥ 2.71 on PATH
(`autoconf27`, `autoconf-2.71`, `autoconf`, checking `--version`) and runs it
plus `./configure --prefix="$PWD/_install"`; and a `Makefile.config` precondition
on the dev targets that fails with exactly that hint instead of make's bare
`No rule to make target 'Makefile.config'` from `Makefile.common-ox:204`.

Rejected: *commit the generated `configure`* (suggested by erasure and bigint) —
it is a ~100k-line generated artifact that upstream deliberately does not track;
committing it on a feature branch buys a merge conflict on every rebase. Also
rejected: *an autoconf shim in `vox/bin`* (suggested by solver-interface) — it
lives outside the repo, so it does not help a reviewer in a fresh environment,
which is precisely who gets bitten.

**6b. `make dev-errors` — 1/5 but strongly worded.** erasure calls
`dune diagnostics --root=. --build-dir=$PWD/_build/dev-dune` against the running
watcher "the fastest loop of the entire session". `dev-check` already invokes
exactly this at `Makefile.common-ox:570`; promoting it to a named target is a
two-line change.

**6c. `make dev-diff TEST=…` — 1/5 (totality 4/5), high leverage.** Find the
newest corrected artifact across all *three* roots from correction 3, always
prefer `.corrected.corrected`, print `diff -u` against the source. This is what
makes "never hand-copy `.corrected`" a realistic instruction rather than a
prohibition with no substitute.

**6d. Artifact location from `dev-test-all` — 1/5, cost ~20 min.** One line at
the end naming `_runtest/testsuite/_ocamltest/`. totality re-ran 55 failing
tests serially for want of it.

**6e. Python version guard — 2/5.** Two lines in `tools/dev-watcher.py:353`'s
neighbourhood, replacing a bare `argparse` `TypeError` on python 3.6 with
"python 3.7+ required".

**6f. Suppress dune's "build request is being forwarded" notice — 1/5.** No dune
flag exists (checked against 3.23.0), so filter the line in `dev-check`.

**6g. Document the blessed scratch path — 1/5.** type-formers found
`_build/dev-dune/default/main_native.exe -nostdlib -I _build/dev/runtest/stdlib`
faster and more scriptable than `make dev-ocamlc` for one-file repros. A line in
`AGENTS.md`, with the caveat that it shares item 2's stale-stdlib exposure —
which item 2's detector does not cover, because it bypasses `dev-check`.

Also in `AGENTS.md`: "promote with `make dev-promote`, never by copying
`.corrected`" (totality 5), and the `autoconf27` correction to the setup recipe,
which currently says plain `autoconf` at `AGENTS.md:7`.

---

## Deferred, with reasons

### 7. Single-instance lock (2/5)

type-formers saw ~15 stuck `make dev-test` processes producing no output until
`pkill`; erasure saw the expect-runner refresh collide with a restarting watcher
("Another Dune instance is currently running"). A lock printing "another dev
command is running (pid N), waiting" would convert both into clear messages.

Deferred, not dismissed. `tools/dev-watcher.py` already has an flock helper
(`locked()`, line 50) so the mechanism exists, but the scope question is real:
`dev-test-all` runs for 30-40 minutes and legitimately holds the loop, so a
naive exclusive lock across all dev commands would serialise a reviewer behind
it and produce a *new* silent wait. Doing this properly means deciding which
commands are mutually exclusive and which merely need the watcher steady, and
that deserves its own thinking rather than being bolted on at the end of this
piece. Item 1's heartbeat also removes most of the *silence* that made these
incidents expensive to diagnose, which lowers the urgency. If time remains after
1-6, this is the next item.

### 8. Stale `_runtest` compilerlibs (1/5)

solver-interface 2: `include ocamlcommon` tests link cmis/cmas from `_runtest`,
which only `install_for_test` refreshes. Adding a module is loud but cryptic
(`Unbound module Vox_logic`); *editing* one is silent.

Deferred to a warning at most, and here I disagree with the report's suggested
fix. It proposes staleness detection against "the typing/parsing/utils sources".
But the cure is `install_for_test`, which the same report measures at 15-30
minutes on a loaded box — so a detector that fires on every `typing/` edit
prescribes a half-hour rebuild on every edit, and will simply be ignored, or
worse, obeyed. The report's own cheaper variant ("only for tests whose TEST
block says `include ocamlcommon`") is the right shape, and if I reach it, it
lands as a warning line scoped to those tests, never as an automatic rebuild.
Not a promise for this piece.

### 9. Stdlib *interface* change poisoning `_runtest` (1/5)

bigint 1: after `stdlib.mli` changed, main-context compiler-libs stayed compiled
against the old `Stdlib` digest, producing 661 bogus "inconsistent assumptions
over interface Stdlib" failures; `install_for_test` exits 0 without fixing it,
`rm -rf _build/main` corrupts dune's incremental state and does not self-heal,
and only a full `_build` wipe plus ~25 minutes recovered.

Out of scope for this piece, and I want to be explicit about why, because it is
the highest-damage single incident in the corpus. The root cause is that
`main.ws` finds the stdlib via `OCAMLLIB` → `_build/runtime_stdlib_install`
(`Makefile.common-ox:73`), which is outside dune's dependency tracking. The
suggested fix — fingerprint the installed stdlib `.cmi` digests and force a
main-context invalidation when they change — is implementable, but it prescribes
a ~25 minute rebuild on every stdlib interface change, it only affects
`install_for_test` / `dev-test-all` rather than the fast loop, and getting the
invalidation granularity wrong makes the full suite unusable rather than merely
wrong. The `rm -rf _build/main` non-recovery is a dune bug, not ours.

What is affordable now, and what I will do instead: detection, not healing. When
`dev-test-all` output contains "inconsistent assumptions over interface Stdlib",
print one line naming the cause and the known-working recovery (full `_build`
wipe, ~25 min) — so the next session spends a minute rather than an hour, and
does not reach for `rm -rf _build/main`. The real fix gets its own piece.

### 10. Expect runner rebuilt as a second full compiler build

type-formers 2, and by that session's own account the dominant cost of its test
loop: after any compiler change, `dev-test` on an expect test pays the fast
watcher build *plus* a `main.ws` build of `expect.exe`, ~8-10 minutes per
iteration.

Explicitly out of scope, and flagged as the strongest candidate for the *next*
dev-loop piece. This is the largest remaining throughput cost in the corpus, but
fixing it means building the expect runner in the boot/dev context rather than
the main workspace, which changes what the runner links against and therefore
what expect tests are actually testing. That is a correctness question about the
test harness, not a tooling convenience, and it should not be decided as the
tenth item of a friction-fixing branch.

### 11. Shared dune cache across worktrees

Raised speculatively by solver-interface and echoed by the owner: each worktree
carries `_build/dev-dune` plus `_build/main`/`_build/default`, and with 7+
worktrees on one box that is heavy disk and inotify load, with sibling watchers
visibly competing. Out of scope. It is a measurement task before it is an
implementation task, and it was offered as speculation rather than as a reported
cost.

### Not the dev loop's problem

- The ocamltest `script`-action quirk where defining `stdout`/`stderr` disables
  redirection for a later `run` action (solver-interface). Real, upstream,
  already documented in the z3 test's header comment.
- `dev-test PROMOTE=1 DIR=…` writing `_ocamltest` into the source tree
  (correction 3) is a genuine defect, but the fix is in `testsuite/Makefile`'s
  `exec-one`, which is shared with the non-dev harness. I will note it and, if
  1-6 land with time to spare, fix it by having `dev-test` pass an explicit
  `OCAMLTESTDIR` under the dev root for every mode — which also collapses three
  artifact locations to two and makes `dev-diff` simpler.

## Out of scope for this piece, in one list

Items 9 (stdlib interface fingerprinting), 10 (expect runner build cost), 11
(shared dune cache); the upstream ocamltest `script` quirk; any change to what
the full test suite asserts; any change to release/`make install` builds; and
performance work on the watcher itself.

## Verification standard for this piece

Every landed item is exercised against the failure it targets, not only
reasoned about. Specifically:

- Item 1: stub `dune` whose `rpc build` never returns; show timeout → restart →
  retry → fallback.
- Item 2: real `.cmi`-shape change on a scratch commit; show the diagnostic
  fires where the baseline segfaults; report the added per-build cost.
- Item 3: pass/fail comparison of affected directories, dev harness vs
  `_runtest`.
- Item 4a/4b: red-green fixtures in `testsuite/tests/tool-ocamltest/`, so
  commit 2's diff to the expectation shows exactly what changed.
- Item 5: `NOWATCH=1` with no watcher present, then confirmed by a codex
  reviewer under a real `workspace-write` sandbox.
- Item 6: exercised by using this branch's loop for the remainder of the work.

The final report (`design-docs/dev-loop-improvements-final.md`) states, per item,
whether it was exercised or only reasoned through. No check is weakened to make
a test pass; anything that turns out to be infeasible is reported as such with
evidence.

## Open questions for the owner

1. Item 3b: if the measurement shows any test genuinely discriminating
   `ocamlc.byte` from `ocamlc.opt`, the choice is between a skip-list (honest,
   but `dev-test-all` stays non-green for those tests) and redirecting anyway
   (green, but the dev harness silently stops testing the bytecode compiler).
   My default is the skip-list, with the marker text saying exactly what is not
   covered. Say if you want the other one.
2. Item 2: the check fails with a message rather than auto-refreshing. If you
   would rather `make dev` just fix it and eat the minutes, that is a one-line
   change.
