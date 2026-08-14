# OxCaml agent guide

## Initial setup

A fresh worktree needs one complete build and test installation:
```sh
autoconf
./configure --prefix="$PWD/_install"
make dev
```
This takes about 5 minutes. Subsequent `make dev` invocations are much faster
because they use an incremental background watcher.

## Development loop
```sh
# edit compiler code, then typecheck/build:
make dev
# edit compiler code or a test, then build & run a test:
make dev-test TEST=typing-local/regression_class_type.ml
# edit again, test an entire dir:
make dev-test DIR=typing-local
# promote current outputs as expect goldens
make dev-promote TEST=path/to/test.ml
# compile separate files
make dev-ocamlc ARGS='-c file.ml'
make dev-ocamlopt ARGS='file.ml -o file.exe'
# run the full compiler test suite
make dev-test-all
```

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

## Worktree structure

Set up a directory named after the change/branch, and in that directory make a worktree called dev, and a subfolder called review where the worktrees of review agents live.

## Design docs and specs

Each change has a design doc, specified by the human. These live in the repo itself, in the design-docs folder. If that folder doesn't exist yet, make it. The design doc should be named after the branch.

If, during development, you come across a decision point where the design doc is ambiguous or simply doesn't specify which route to take, then first decide (1) is there an arguably best route (2) does the decision actually matter much. Only if there is no clear best route, note at the end of the design doc in concise style which route you took and why, and which alternatives you considered, and also notify me so that I can help check if that's the right decision.