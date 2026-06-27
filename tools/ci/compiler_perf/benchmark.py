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
        lower_bound = runs[0]["ratio"]
    else:
        margin = t_critical * sd_log_ratio / math.sqrt(len(log_ratios))
        lower_bound = math.exp(mean_log_ratio - margin)
    return {
        "ratio": math.exp(mean_log_ratio),
        "lower_bound_99": lower_bound,
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


def write_markdown(
    path: Path,
    *,
    build_results: dict | None,
    corpus: dict,
    runs: list[dict],
    files: list[dict],
    thresholds: dict,
) -> None:
    lines = [
        "# Compiler performance benchmark",
        "",
        "## What this checks",
        "",
        (
            "The main check is the corpus CPU-time check. For run `i`, let "
            "`B_i` be total CPU seconds for the base compiler over the "
            "benchmark files, let `H_i` be total CPU seconds for the head "
            "compiler over the same files, and let `r_i = H_i / B_i`."
        ),
        "",
        (
            "The script computes the mean and sample standard deviation of "
            "`log(r_i)`. If there are `n` runs, the reported 99% lower "
            "confidence bound is:"
        ),
        "",
        "`exp(mean(log(r_i)) - t_0.99,n-1 * sd(log(r_i)) / sqrt(n))`",
        "",
        (
            "The main check fails when this lower confidence bound is greater "
            f"than `{thresholds['fail_corpus_lower_bound']:.3f}`. In other "
            "words, the check only fails when even the conservative estimate "
            "says that the head compiler is slower than the base compiler by "
            "more than the configured threshold."
        ),
        "",
        (
            "There are two secondary checks. The build check compares elapsed "
            "time for building the base source tree with the head-built "
            "compiler against building it with the base compiler. The "
            "per-file check computes the median `H_i / B_i` for each "
            "benchmark file and catches large localized slowdowns."
        ),
        "",
    ]

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
            "99% lower confidence bound: "
            f"**{corpus['lower_bound_99']:.3f}**",
            "",
            "## File diagnostics",
            "",
            "| File | Base median CPU s | Head median CPU s | Median head/base |",
            "| --- | ---: | ---: | ---: |",
        ]
    )
    for result in files:
        lines.append(
            f"| `{result['path']}` | {result['base_median']:.3f} | "
            f"{result['head_median']:.3f} | "
            f"{result['ratio_median']:.3f} |"
        )
    lines.append("")
    path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-compiler", type=Path, required=True)
    parser.add_argument("--head-compiler", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--runs", type=int, default=20)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--fail-corpus-lower-bound", type=float, default=1.015)
    parser.add_argument("--fail-any-file-median", type=float, default=1.25)
    parser.add_argument("--fail-build-ratio", type=float, default=1.15)
    parser.add_argument("--build-results-json", type=Path)
    parser.add_argument("--json-output", type=Path)
    parser.add_argument("--markdown-output", type=Path)
    parser.add_argument("--keep-work-dir", action="store_true")
    args = parser.parse_args()

    if args.runs < 1:
        raise SystemExit("--runs must be at least 1")
    if args.warmups < 0:
        raise SystemExit("--warmups must not be negative")

    for compiler in [args.base_compiler, args.head_compiler]:
        if not compiler.exists():
            raise SystemExit(f"compiler does not exist: {compiler}")

    if not args.source_root.exists():
        raise SystemExit(f"source root does not exist: {args.source_root}")

    if args.work_dir.exists():
        shutil.rmtree(args.work_dir)

    benchmark_files = [Path(path) for path in BENCHMARK_FILES]
    for relative_path in benchmark_files:
        source = args.source_root / relative_path
        if not source.exists():
            raise SystemExit(f"benchmark source does not exist: {source}")

    for i in range(args.warmups):
        run_suite(
            compiler=args.base_compiler,
            source_root=args.source_root,
            relative_paths=benchmark_files,
            run_dir=args.work_dir / "warmup" / "base" / str(i),
        )
        run_suite(
            compiler=args.head_compiler,
            source_root=args.source_root,
            relative_paths=benchmark_files,
            run_dir=args.work_dir / "warmup" / "head" / str(i),
        )

    runs = []
    for i in range(args.runs):
        if i % 2 == 0:
            base_total, base_files = run_suite(
                compiler=args.base_compiler,
                source_root=args.source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "base",
            )
            head_total, head_files = run_suite(
                compiler=args.head_compiler,
                source_root=args.source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "head",
            )
        else:
            head_total, head_files = run_suite(
                compiler=args.head_compiler,
                source_root=args.source_root,
                relative_paths=benchmark_files,
                run_dir=args.work_dir / "runs" / str(i) / "head",
            )
            base_total, base_files = run_suite(
                compiler=args.base_compiler,
                source_root=args.source_root,
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

    corpus = corpus_statistics(runs)
    files = file_statistics(benchmark_files, runs)
    build_results = load_build_results(args.build_results_json)

    payload = {
        "build": build_results,
        "corpus": corpus,
        "runs": runs,
        "files": files,
        "thresholds": {
            "fail_corpus_lower_bound": args.fail_corpus_lower_bound,
            "fail_any_file_median": args.fail_any_file_median,
            "fail_build_ratio": args.fail_build_ratio,
        },
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
            thresholds=payload["thresholds"],
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
    for result in files:
        print(
            f"{result['path']}: base_median={result['base_median']:.3f}s "
            f"head_median={result['head_median']:.3f}s "
            f"median_ratio={result['ratio_median']:.3f}"
        )

    failures = []
    if (
        build_results is not None
        and build_results["ratio"] > args.fail_build_ratio
    ):
        failures.append(
            f"build ratio {build_results['ratio']:.3f} exceeds "
            f"{args.fail_build_ratio:.3f}"
        )
    if corpus["lower_bound_99"] > args.fail_corpus_lower_bound:
        failures.append(
            f"99% lower confidence bound {corpus['lower_bound_99']:.3f} "
            f"exceeds {args.fail_corpus_lower_bound:.3f}"
        )
    for result in files:
        if result["ratio_median"] > args.fail_any_file_median:
            failures.append(
                f"{result['name']} median ratio "
                f"{result['ratio_median']:.3f} exceeds "
                f"{args.fail_any_file_median:.3f}"
            )

    if not args.keep_work_dir:
        shutil.rmtree(args.work_dir, ignore_errors=True)

    if failures:
        for failure in failures:
            print(f"::error::{failure}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
