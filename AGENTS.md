# OxCaml agent guide

## Initial setup

A fresh worktree needs one complete build and test installation:
```sh
make dev-configure   # or, by hand: autoconf27 && ./configure --prefix="$PWD/_install"
make dev
```
This takes about 5 minutes. Subsequent `make dev` invocations are much faster
because they use an incremental background watcher.

`configure` is not tracked in git, so every new worktree needs generating it, and
`configure.ac` requires autoconf >= 2.71 — newer than the `autoconf` on many
systems, where the one to use is `autoconf27`. `make dev-configure` finds a
suitable autoconf and configures with `--prefix="$PWD/_install"`; do that by hand
only if you need different flags.

## Development loop
```sh
# edit compiler code, then typecheck/build:
make dev
# fresh errors from the running watcher, with no build round trip (fastest loop):
make dev-errors
# watch a long build's progress:
make dev-log
# edit compiler code or a test, then build & run a test:
make dev-test TEST=typing-local/regression_class_type.ml
# edit again, test an entire dir:
make dev-test DIR=typing-local
# review a test's new output before accepting it:
make dev-diff TEST=path/to/test.ml
# promote current outputs as expect goldens
make dev-promote TEST=path/to/test.ml
# compile separate files
make dev-ocamlc ARGS='-c file.ml'
make dev-ocamlopt ARGS='file.ml -o file.exe'
# run the full compiler test suite
make dev-test-all
```

Promote with `make dev-promote`, **never** by copying a `.corrected` file. The
expect harness runs twice, plain and `-principal`; the second pass writes
`<test>.ml.corrected.corrected`, so copying `<test>.ml.corrected` silently drops
the principal-block updates and the parallel suite then fails tests that serial
spot checks show green. `make dev-diff` always shows the artifact that supersedes.

For one-file experiments the dev compiler can be called directly, which is faster
and more scriptable than `make dev-ocamlc`:
```sh
_build/dev-dune/default/main_native.exe -nostdlib -I _build/dev/runtest/stdlib
```
Note that this bypasses `make dev`'s checks, including the stale-stdlib check
below.

### When things go wrong

- **A build that seems to hang.** `make dev` reports progress every 30s and names
  `make dev-log`. The watcher's rpc has been seen to wedge — alive, answering
  pings, never starting the build — so the build is bounded by
  `DEV_RPC_TIMEOUT` (default 1800s), after which the watcher is restarted and the
  build retried once, then run directly without the watcher. `make dev-status`
  shows whether the watcher is idle.
- **The compiler segfaults, or tests crash for no reason, after a compiler
  change.** A change to marshaled `.cmi` shapes leaves the previously built
  stdlib unreadable. `make dev` detects this and tells you to run
  `make dev-refresh-stdlib`.
- **A restricted environment where the watcher cannot start** (dune's RPC socket
  cannot be created, e.g. `bind(): Operation not permitted` in a sandbox): use
  `make dev NOWATCH=1`, which skips the watcher and rpc entirely. Slower per
  invocation, works anywhere, and applies to the other `dev-*` targets too.
- **`ocamlc.byte`-flavoured tests.** The dev test root's `ocamlc.byte` is the boot
  `main.bc`, built by the host compiler, so it cannot run under the in-tree
  `ocamlrun`. See `design-docs/dev-loop-improvements.md`.

## Release builds

Benchmark and memtrace only with the compiler produced by `make install`, never
the development boot compiler.

## Review loop

When the user asks you to review loop, that means that you launch several claude and codex agents to review the changes. Don't take their suggestions at face value: carefully triage which issues are real and important, and which aren't, with respect to the original goal. We want to avoid overcomplicating the code as a result of the review loop. The agents themselves should focus on:
* Whether this is the simplest way to do it - AI coding can result in overcomplicated code and diffs that are not optimally small. It is very important to keep the code small and elegant for long term code health. Key question: is this the simplest, most elegant way to do it?
* Is the architecture right? Is everything in the right place? Is there duplicated functionality? Is there a way to do it with less code?
* What are the alternative ways of doing it? Is there a better way?
* Whether there are bugs, ideally proved by a failing test case / repro. Triage whether you agree that the behavior is actually a bug or not.
* Whether there is important missing test coverage, ideally proved by a mutation that currently makes no test fail - it's also important to curtail test growth: the test suite should be compact, and each test should add separate value.
* In general: the key question is whether an expert human software engineer would critique the changeset as-is.

It is very important that review agents have their own worktree to build and run experiments and develop tests.
Once you have the review reports, try to ground claims in experimental reality as much as possible. Reproduce suggested issues. Try out suggested refactorings. Only accept if grounded reality shows that it is a real issue or real improvement.

Try to get the deliverable in the best shape of the highest quality. Use of subagents is almost free compared to human software engineering time, so use them well. The biggest danger is not bugs: the biggest danger is is wrong design decisions. Significant agent effort should be spent on evaluating whether the design decisions are optimal, because otherwise we end up in a morass of slop and progress will slowly grind to a halt. We want to keep the project maximally healthy. If you need human help, don't hesitate to ask for it.

## Worktree structure

Set up a directory named after the change/branch, and in that directory make a worktree called dev, and a subfolder called review where the worktrees of review agents live.

## Design docs and specs

Each change has a design doc, specified by the human. These live in the repo itself, in the design-docs folder. If that folder doesn't exist yet, make it. The design doc should be named after the branch.

If, during development, you come across a decision point where the design doc is ambiguous or simply doesn't specify which route to take, then first decide (1) is there an arguably best route (2) does the decision actually matter much. Only if there is no clear best route, note at the end of the design doc in concise style which route you took and why, and which alternatives you considered, and also notify me so that I can help check if that's the right decision.

## Friction

If you notice friction or other problems that could be fixed by going up a meta level and fixing the setup or tooling or appraoch, try to fix that!