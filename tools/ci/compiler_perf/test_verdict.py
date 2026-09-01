#!/usr/bin/env python3

"""Replay the two recorded CI datasets against the current verdict logic.

The datasets in ``testdata/`` are the per-run head/base corpus CPU-time ratios
extracted from real CI runs:

  fn_raw.txt       PR #6364, a genuine ~3% regression. The old lower-bound
                   gate let it PASS (a false negative); the point-estimate
                   gate must FAIL it.
  control_raw.txt  PR #6363, a no-op change. It must PASS.

This imports ``evaluate_verdict`` from ``benchmark`` so the check tracks the
shipped decision logic rather than a copy of it.
"""

import math
import sys
from pathlib import Path

import benchmark

HERE = Path(__file__).resolve().parent

FAIL_CORPUS_RATIO = 1.015
PRECISION_TARGET = 1.03
PRECISION_GAP = math.log(PRECISION_TARGET) - math.log(FAIL_CORPUS_RATIO)
MAX_RUNS = 60

THRESHOLDS = {
    "fail_corpus_ratio": FAIL_CORPUS_RATIO,
    "fail_any_file_median": 1.25,
    "precision_target_slowdown": PRECISION_TARGET,
}


def load_ratios(name: str) -> list[float]:
    text = (HERE / "testdata" / name).read_text()
    return [
        float(line.split("ratio=")[1])
        for line in text.splitlines()
        if line.strip()
    ]


def replay(name: str) -> tuple[dict, dict]:
    runs = [{"ratio": ratio} for ratio in load_ratios(name)]
    corpus = benchmark.corpus_statistics(runs)
    verdict = benchmark.evaluate_verdict(
        corpus=corpus,
        files=[],
        thresholds=THRESHOLDS,
        precision_gap=PRECISION_GAP,
        reached_max_runs=len(runs) >= MAX_RUNS,
    )
    return corpus, verdict


def main() -> int:
    fn_corpus, fn = replay("fn_raw.txt")
    ctl_corpus, ctl = replay("control_raw.txt")

    print(
        f"FN  #6364 (real +3%): corpus ratio {fn_corpus['ratio']:.3f}  "
        f"lower_bound_99 {fn_corpus['lower_bound_99']:.3f}  "
        f"failures={fn['failures']}"
    )
    print(
        f"CTL #6363 (no-op):    corpus ratio {ctl_corpus['ratio']:.3f}  "
        f"lower_bound_99 {ctl_corpus['lower_bound_99']:.3f}  "
        f"failures={ctl['failures']}"
    )

    assert fn["failures"], (
        "regression dataset #6364 must FAIL the point-estimate gate "
        "(the old lower-bound gate let it pass)"
    )
    assert not ctl["failures"], (
        "control dataset #6363 must PASS the point-estimate gate"
    )
    print("OK: regression fails, control passes")
    return 0


if __name__ == "__main__":
    sys.exit(main())
