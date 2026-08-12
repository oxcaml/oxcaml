# Compiler Perf CI Plan

- [x] Add build timing for base source built by the base compiler.
- [x] Add build timing for base source built by the head compiler.
- [x] Use the head-built base install for file benchmarks.
- [x] Compare the same base source files for both compilers.
- [x] Switch file benchmarks to paired suite repetitions.
- [x] Report build ratios, corpus ratios, and per-file diagnostics.
- [x] Validate locally with existing installed compilers.
- [x] Push the implementation branch.
- [x] Update the synthetic #6113 test branches and check CI.

## The gate

The main check gates on the **point estimate** of the corpus head/base ratio
`exp(mean(log(r_i)))` against the `--fail-corpus-ratio` threshold (1.015). It
fails when the measured ratio exceeds the threshold.

The earlier design gated on the 99% *lower* confidence bound instead of the
point estimate. That was calibrated against the intuition "only fail when even
the conservative estimate is over," but on a noisy runner it made the gate
under-powered by construction: the confidence margin is subtracted before
comparing to the threshold, so measurement noise (which widens the margin)
*protects* regressions. On the recorded +3% regression (PR #6364) the
lower-bound gate had ~0.35 power at the 60 runs it budgets and let the
regression pass; the point-estimate gate has ~0.96 power there while keeping
the no-op control's false-positive rate at ~0.3% (see `test_verdict.py`, which
replays both recorded datasets against `evaluate_verdict`).

The confidence bound and its log-space margin are still computed and reported,
just for context, not to decide the verdict.

## Stopping and low confidence

The benchmark starts at `--runs` and adds runs in batches until the confidence
margin is tight enough to distinguish a `--precision-target-slowdown` (1.03)
from the failure threshold. If `--max-runs` is reached without achieving that
precision, the result is flagged **LOW CONFIDENCE** (a prominent warning in
the check summary, with the achieved margin) rather than silently passing; the
point-estimate verdict still stands.

## Secondary metrics

- Per-file median ratio (`--fail-any-file-median`, 1.25) still gates as a net
  for large localized slowdowns.
- Build wall-clock time is **report-only**: it is a single (n=1) measurement on
  a shared runner and has historically produced false alarms (a 2.09x reading
  on a no-op change), so it is printed but never fails the check.
