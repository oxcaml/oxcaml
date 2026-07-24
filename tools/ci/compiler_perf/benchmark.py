#!/usr/bin/env python3

import argparse
import json
import math
import resource
import shutil
import subprocess
import sys
from statistics import median
from pathlib import Path


BENCHMARK_FILES = [
    "typing/typeopt.ml",
    "typing/typecore.ml",
    "typing/ctype.ml",
    "typing/typedecl.ml",
    "typing/includemod.ml",
    "lambda/translcore.ml",
    "lambda/translmod.ml",
    "lambda/matching.ml",
]


T_CRITICAL_ONE_SIDED_99 = {
    1: 31.821,
    2: 6.965,
    3: 4.541,
    4: 3.747,
    5: 3.365,
    6: 3.143,
    7: 2.998,
    8: 2.896,
    9: 2.821,
    10: 2.764,
    11: 2.718,
    12: 2.681,
    13: 2.650,
    14: 2.624,
    15: 2.602,
    16: 2.583,
    17: 2.567,
    18: 2.552,
    19: 2.539,
    20: 2.528,
    21: 2.518,
    22: 2.508,
    23: 2.500,
    24: 2.492,
    25: 2.485,
    26: 2.479,
    27: 2.473,
    28: 2.467,
    29: 2.462,
    30: 2.457,
}


def t_critical_one_sided_99(samples: int) -> float:
    if samples < 2:
        return float("inf")
    degrees_of_freedom = samples - 1
    if degrees_of_freedom in T_CRITICAL_ONE_SIDED_99:
        return T_CRITICAL_ONE_SIDED_99[degrees_of_freedom]
    return 2.326


def benchmark_name(relative_path: Path) -> str:
    return str(relative_path.with_suffix("")).replace("/", "__")


def child_cpu_seconds() -> float:
    usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    return usage.ru_utime + usage.ru_stime


def compiler_libs_dir(compiler: Path) -> Path:
    return compiler.parent.parent / "lib" / "ocaml" / "compiler-libs"


def run_compile(
    compiler: Path,
    source: Path,
    run_dir: Path,
) -> float:
    if run_dir.exists():
        shutil.rmtree(run_dir)
    run_dir.mkdir(parents=True)

    compiler_libs = compiler_libs_dir(compiler)
    if not compiler_libs.exists():
        raise SystemExit(
            f"compiler-libs directory does not exist: {compiler_libs}"
        )

    before = child_cpu_seconds()
    proc = subprocess.run(
        [
            str(compiler),
            "-w",
            "-a",
            "-S",
            "-c",
            "-I",
            str(compiler_libs),
            str(source),
        ],
        cwd=run_dir,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    after = child_cpu_seconds()
    if proc.returncode != 0:
        sys.stderr.write(f"{compiler} failed on {source.name}\n")
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise SystemExit(proc.returncode)
    shutil.rmtree(run_dir)
    return after - before


def run_suite(
    *,
    compiler: Path,
    source_root: Path,
    relative_paths: list[Path],
    run_dir: Path,
) -> tuple[float, dict[str, float]]:
    per_file = {}
    for relative_path in relative_paths:
        path = source_root / relative_path
        time = run_compile(
            compiler,
            path,
            run_dir / benchmark_name(relative_path),
        )
        per_file[str(relative_path)] = time
    return sum(per_file.values()), per_file


def mean(values: list[float]) -> float:
    return sum(values) / len(values)


def sample_standard_deviation(values: list[float]) -> float:
    if len(values) < 2:
        return 0.0
    value_mean = mean(values)
    variance = sum((value - value_mean) ** 2 for value in values)
    variance /= len(values) - 1
    return math.sqrt(variance)


def corpus_statistics(runs: list[dict]) -> dict:
    log_ratios = [math.log(run["ratio"]) for run in runs]
    mean_log_ratio = mean(log_ratios)
    sd_log_ratio = sample_standard_deviation(log_ratios)
    t_critical = t_critical_one_sided_99(len(log_ratios))
    if math.isinf(t_critical):
        margin = 0.0
        lower_bound = runs[0]["ratio"]
    else:
        margin = t_critical * sd_log_ratio / math.sqrt(len(log_ratios))
        lower_bound = math.exp(mean_log_ratio - margin)
    return {
        "ratio": math.exp(mean_log_ratio),
        "lower_bound_99": lower_bound,
        "confidence_margin_log": margin,
        "mean_log_ratio": mean_log_ratio,
        "sd_log_ratio": sd_log_ratio,
        "t_critical_99": t_critical,
    }


def file_statistics(relative_paths: list[Path], runs: list[dict]) -> list[dict]:
    results = []
    for relative_path in relative_paths:
        path = str(relative_path)
        base_times = [run["base_files"][path] for run in runs]
        head_times = [run["head_files"][path] for run in runs]
        ratios = [
            head_time / base_time
            for base_time, head_time in zip(base_times, head_times)
        ]
        results.append(
            {
                "name": benchmark_name(relative_path),
                "path": path,
                "base_median": median(base_times),
                "head_median": median(head_times),
                "ratio_median": median(ratios),
                "ratios": ratios,
                "base_times": base_times,
                "head_times": head_times,
            }
        )
    return results


def load_build_results(path: Path | None) -> dict | None:
    if path is None:
        return None
    return json.loads(path.read_text())


def describe_change(ratio: float) -> str:
    percent = 100.0 * (ratio - 1.0)
    if abs(percent) < 0.05:
        return "within 0.05% of base"
    direction = "slower" if percent > 0.0 else "faster"
    return f"{abs(percent):.1f}% {direction}"


def evaluate_verdict(
    *,
    corpus: dict,
    files: list[dict],
    thresholds: dict,
    precision_gap: float,
    reached_max_runs: bool,
) -> dict:
    """Decide pass/fail from the measured statistics.

    The primary gate compares the point estimate of the corpus head/base
    ratio against the threshold. The confidence bound is still computed and
    reported for context, but it is not subtracted from the estimate here:
    subtracting it (the old lower-bound gate) let measurement noise protect
    real regressions, so a genuine slowdown could pass just by being noisy.

    Returns the list of failures, any warnings, and whether the result is
    low confidence (max runs reached without the margin achieving the target
    precision). Low confidence is a warning, not a failure: the point-estimate
    verdict stands.
    """
    failures = []
    warnings = []

    if corpus["ratio"] > thresholds["fail_corpus_ratio"]:
        failures.append(
            f"corpus ratio {corpus['ratio']:.3f} exceeds "
            f"{thresholds['fail_corpus_ratio']:.3f}"
        )

    for result in files:
        if result["ratio_median"] > thresholds["fail_any_file_median"]:
            failures.append(
                f"{result['name']} median ratio "
                f"{result['ratio_median']:.3f} exceeds "
                f"{thresholds['fail_any_file_median']:.3f}"
            )

    low_confidence = (
        reached_max_runs and corpus["confidence_margin_log"] > precision_gap
    )
    if low_confidence:
        warnings.append(
            "LOW CONFIDENCE: reached the maximum number of runs without the "
            "confidence margin reaching the target precision (log-space "
            f"margin {corpus['confidence_margin_log']:.4f} still exceeds the "
            f"target {precision_gap:.4f}). The point-estimate verdict above "
            "stands, but treat it as provisional."
        )

    return {
        "failures": failures,
        "warnings": warnings,
        "low_confidence": low_confidence,
    }


def write_markdown(
    path: Path,
    *,
    build_results: dict | None,
    corpus: dict,
    runs: list[dict],
    files: list[dict],
    thresholds: dict,
    verdict: dict,
) -> None:
    lines = [
        "# Compiler performance benchmark",
        "",
    ]
    for warning in verdict["warnings"]:
        lines.extend([f"> **:warning: {warning}**", ""])
    lines.extend([
        "## What this checks",
        "",
        (
            "The main check is the corpus CPU-time check. For run `i`, let "
            "`B_i` be total CPU seconds for the installed base compiler over "
            "the benchmark files from the base source tree, let `H_i` be "
            "total CPU seconds for the installed head compiler over the "
            "benchmark files from the head source tree, and let "
            "`r_i = H_i / B_i`."
        ),
        "",
        (
            "The corpus ratio is the point estimate "
            "`exp(mean(log(r_i)))`. The main check fails when this ratio is "
            f"greater than `{thresholds['fail_corpus_ratio']:.3f}`, i.e. when "
            "the head compiler is measured slower than the base compiler by "
            "more than the configured threshold."
        ),
        "",
        (
            "A 99% lower confidence bound "
            "`exp(mean(log(r_i)) - t_0.99,n-1 * sd(log(r_i)) / sqrt(n))` and "
            "its log-space margin are reported for context, but the gate uses "
            "the point estimate, not the bound: subtracting the confidence "
            "margin before comparing to the threshold let measurement noise "
            "hide real regressions."
        ),
        "",
        (
            "The benchmark starts with the minimum number of runs, then adds "
            "more runs if the confidence margin is too wide to distinguish "
            f"a `{thresholds['precision_target_slowdown']:.3f}` slowdown from "
            "the failure threshold. This spends extra time only on noisy "
            "measurements. If the maximum number of runs is reached without "
            "achieving that precision, the result is flagged **low "
            "confidence** but the point-estimate verdict still stands."
        ),
        "",
        (
            "The per-file check computes the median `H_i / B_i` for each "
            "benchmark file and catches large localized slowdowns. The build "
            "time below is reported for information only: it is a single "
            "(n=1) wall-clock measurement on a shared runner, so it does not "
            "gate."
        ),
        "",
    ])

    if build_results is not None:
        builds = build_results["builds"]
        lines.extend(
            [
                "## Compiler build time",
                "",
                "| Build | Elapsed s |",
                "| --- | ---: |",
            ]
        )
        for build in builds:
            lines.append(
                f"| {build['name']} | {build['elapsed_seconds']:.0f} |"
            )
        lines.extend(
            [
                "",
                f"Build head/base ratio: **{build_results['ratio']:.3f}**",
                "",
                "Measured build change: "
                f"**{describe_change(build_results['ratio'])}**",
                "",
            ]
        )

    lines.extend(
        [
            "## Compiler file benchmark",
            "",
            "| Run | Base total CPU s | Head total CPU s | Head/base |",
            "| ---: | ---: | ---: | ---: |",
        ]
    )
    for run in runs:
        lines.append(
            f"| {run['run']} | {run['base_total']:.3f} | "
            f"{run['head_total']:.3f} | {run['ratio']:.3f} |"
        )
    lines.extend(
        [
            "",
            f"Corpus head/base ratio: **{corpus['ratio']:.3f}**",
            "",
            f"Measured corpus change: **{describe_change(corpus['ratio'])}**",
            "",
            "99% lower confidence bound: "
            f"**{corpus['lower_bound_99']:.3f}**",
            "",
            "Log-space confidence margin: "
            f"**{corpus['confidence_margin_log']:.4f}**",
            "",
            "## File diagnostics",
            "",
            "| File | Base median CPU s | Head median CPU s | "
            "Median head/base | Median change |",
            "| --- | ---: | ---: | ---: | ---: |",
        ]
    )
    for result in files:
        lines.append(
            f"| `{result['path']}` | {result['base_median']:.3f} | "
            f"{result['head_median']:.3f} | "
            f"{result['ratio_median']:.3f} | "
            f"{describe_change(result['ratio_median'])} |"
        )
    lines.append("")
    path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-compiler", type=Path, required=True)
    parser.add_argument("--head-compiler", type=Path, required=True)
    parser.add_argument("--base-source-root", type=Path)
    parser.add_argument("--head-source-root", type=Path)
    parser.add_argument("--source-root", type=Path)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--runs", type=int, default=20)
    parser.add_argument("--max-runs", type=int, default=60)
    parser.add_argument("--run-batch-size", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--fail-corpus-ratio", type=float, default=1.015)
    parser.add_argument("--fail-any-file-median", type=float, default=1.25)
    parser.add_argument("--precision-target-slowdown", type=float, default=1.03)
    parser.add_argument("--build-results-json", type=Path)
    parser.add_argument("--json-output", type=Path)
    parser.add_argument("--markdown-output", type=Path)
    parser.add_argument("--keep-work-dir", action="store_true")
    args = parser.parse_args()

    if args.runs < 1:
        raise SystemExit("--runs must be at least 1")
    if args.warmups < 0:
        raise SystemExit("--warmups must not be negative")
    if args.max_runs < args.runs:
        raise SystemExit("--max-runs must be at least --runs")
    if args.run_batch_size < 1:
        raise SystemExit("--run-batch-size must be at least 1")
    if args.precision_target_slowdown <= args.fail_corpus_ratio:
        raise SystemExit(
            "--precision-target-slowdown must be greater than "
            "--fail-corpus-ratio"
        )

    for compiler in [args.base_compiler, args.head_compiler]:
        if not compiler.exists():
            raise SystemExit(f"compiler does not exist: {compiler}")

    if args.source_root is not None:
        if args.base_source_root is not None or args.head_source_root is not None:
            raise SystemExit(
                "--source-root cannot be combined with --base-source-root "
                "or --head-source-root"
            )
        args.base_source_root = args.source_root
        args.head_source_root = args.source_root
    if args.base_source_root is None:
        raise SystemExit("--base-source-root is required")
    if args.head_source_root is None:
        raise SystemExit("--head-source-root is required")

    for name, source_root in [
        ("base source root", args.base_source_root),
        ("head source root", args.head_source_root),
    ]:
        if not source_root.exists():
            raise SystemExit(f"{name} does not exist: {source_root}")

    if args.work_dir.exists():
        shutil.rmtree(args.work_dir)

    benchmark_files = [Path(path) for path in BENCHMARK_FILES]
    for relative_path in benchmark_files:
        for name, source_root in [
            ("base", args.base_source_root),
            ("head", args.head_source_root),
        ]:
            source = source_root / relative_path
            if not source.exists():
                raise SystemExit(
                    f"{name} benchmark source does not exist: {source}"
                )

    for i in range(args.warmups):
        run_suite(
            compiler=args.base_compiler,
            source_root=args.base_source_root,
            relative_paths=benchmark_files,
            run_dir=args.work_dir / "warmup" / "base" / str(i),
        )
        run_suite(
            compiler=args.head_compiler,
            source_root=args.head_source_root,
            relative_paths=benchmark_files,
            run_dir=args.work_dir / "warmup" / "head" / str(i),
        )

    precision_gap = (
        math.log(args.precision_target_slowdown)
        - math.log(args.fail_corpus_ratio)
    )

    runs = []
    while len(runs) < args.max_runs:
        i = len(runs)
        if i % 2 == 0:
            base_total, base_files = run_suite(
                compiler=args.base_compiler,
                source_root=args.base_source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "base",
            )
            head_total, head_files = run_suite(
                compiler=args.head_compiler,
                source_root=args.head_source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "head",
            )
        else:
            head_total, head_files = run_suite(
                compiler=args.head_compiler,
                source_root=args.head_source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "head",
            )
            base_total, base_files = run_suite(
                compiler=args.base_compiler,
                source_root=args.base_source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "base",
            )
        runs.append(
            {
                "run": i,
                "base_total": base_total,
                "head_total": head_total,
                "ratio": head_total / base_total,
                "base_files": base_files,
                "head_files": head_files,
            }
        )
        run_count = len(runs)
        if run_count < args.runs:
            continue
        should_check = (
            run_count == args.runs
            or run_count == args.max_runs
            or (run_count - args.runs) % args.run_batch_size == 0
        )
        if not should_check:
            continue
        corpus = corpus_statistics(runs)
        # Extend runs until the confidence margin is tight enough to
        # distinguish the precision target from the failure threshold. The
        # verdict itself uses the point estimate, not this bound; the margin
        # only decides when to stop measuring (and flags low confidence if we
        # never get there).
        if corpus["confidence_margin_log"] <= precision_gap:
            break

    reached_max_runs = len(runs) >= args.max_runs
    corpus = corpus_statistics(runs)
    files = file_statistics(benchmark_files, runs)
    build_results = load_build_results(args.build_results_json)

    thresholds = {
        "fail_corpus_ratio": args.fail_corpus_ratio,
        "fail_any_file_median": args.fail_any_file_median,
        "precision_target_slowdown": args.precision_target_slowdown,
        "min_runs": args.runs,
        "max_runs": args.max_runs,
        "run_batch_size": args.run_batch_size,
    }
    verdict = evaluate_verdict(
        corpus=corpus,
        files=files,
        thresholds=thresholds,
        precision_gap=precision_gap,
        reached_max_runs=reached_max_runs,
    )

    payload = {
        "build": build_results,
        "corpus": corpus,
        "runs": runs,
        "files": files,
        "thresholds": thresholds,
        "verdict": verdict,
    }
    if args.json_output is not None:
        args.json_output.parent.mkdir(parents=True, exist_ok=True)
        args.json_output.write_text(json.dumps(payload, indent=2) + "\n")
    if args.markdown_output is not None:
        args.markdown_output.parent.mkdir(parents=True, exist_ok=True)
        write_markdown(
            args.markdown_output,
            build_results=build_results,
            corpus=corpus,
            runs=runs,
            files=files,
            thresholds=thresholds,
            verdict=verdict,
        )

    print("Compiler performance benchmark")
    if build_results is not None:
        print(
            "build: "
            f"base={build_results['base_elapsed_seconds']:.0f}s "
            f"head={build_results['head_elapsed_seconds']:.0f}s "
            f"ratio={build_results['ratio']:.3f}"
        )
    for run in runs:
        print(
            f"run {run['run']}: base={run['base_total']:.3f}s "
            f"head={run['head_total']:.3f}s ratio={run['ratio']:.3f}"
        )
    print(f"corpus ratio: {corpus['ratio']:.3f}")
    print(f"99% lower confidence bound: {corpus['lower_bound_99']:.3f}")
    print(f"log-space confidence margin: {corpus['confidence_margin_log']:.4f}")
    for result in files:
        print(
            f"{result['path']}: base_median={result['base_median']:.3f}s "
            f"head_median={result['head_median']:.3f}s "
            f"median_ratio={result['ratio_median']:.3f}"
        )

    # The build ratio is a single, unreplicated wall-clock measurement (n=1)
    # on a shared runner, so it is reported but never gates.
    if build_results is not None:
        print(
            f"build ratio {build_results['ratio']:.3f} "
            "(reported only, not a gate)"
        )

    if not args.keep_work_dir:
        shutil.rmtree(args.work_dir, ignore_errors=True)

    for warning in verdict["warnings"]:
        print(f"::warning::{warning}")
    if verdict["failures"]:
        for failure in verdict["failures"]:
            print(f"::error::{failure}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
