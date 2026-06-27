#!/usr/bin/env python3

import argparse
import json
import math
import resource
import shutil
import subprocess
import sys
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


def benchmark_one(
    *,
    base_compiler: Path,
    head_compiler: Path,
    base_source: Path,
    head_source: Path,
    relative_path: Path,
    runs: int,
    warmups: int,
    work_dir: Path,
) -> dict:
    name = benchmark_name(relative_path)
    for i in range(warmups):
        run_compile(
            base_compiler,
            base_source,
            work_dir / "warmup" / "base" / name / str(i),
        )
        run_compile(
            head_compiler,
            head_source,
            work_dir / "warmup" / "head" / name / str(i),
        )

    base_times = []
    head_times = []
    for i in range(runs):
        if i % 2 == 0:
            base_times.append(
                run_compile(
                    base_compiler,
                    base_source,
                    work_dir / "runs" / "base" / name / str(i),
                )
            )
            head_times.append(
                run_compile(
                    head_compiler,
                    head_source,
                    work_dir / "runs" / "head" / name / str(i),
                )
            )
        else:
            head_times.append(
                run_compile(
                    head_compiler,
                    head_source,
                    work_dir / "runs" / "head" / name / str(i),
                )
            )
            base_times.append(
                run_compile(
                    base_compiler,
                    base_source,
                    work_dir / "runs" / "base" / name / str(i),
                )
            )

    base_min = min(base_times)
    head_min = min(head_times)
    return {
        "name": name,
        "path": str(relative_path),
        "base_times": base_times,
        "head_times": head_times,
        "base_min": base_min,
        "head_min": head_min,
        "ratio": head_min / base_min,
    }


def geomean(values: list[float]) -> float:
    return math.exp(sum(math.log(value) for value in values) / len(values))


def write_markdown(
    path: Path,
    results: list[dict],
    ratio_geomean: float,
) -> None:
    lines = [
        "# Compiler performance benchmark",
        "",
        "| File | Base min CPU s | Head min CPU s | Head/base |",
        "| --- | ---: | ---: | ---: |",
    ]
    for result in results:
        lines.append(
            f"| `{result['path']}` | {result['base_min']:.3f} | "
            f"{result['head_min']:.3f} | {result['ratio']:.3f} |"
        )
    lines.extend(["", f"Geomean head/base: **{ratio_geomean:.3f}**", ""])
    path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-compiler", type=Path, required=True)
    parser.add_argument("--head-compiler", type=Path, required=True)
    parser.add_argument("--base-source-root", type=Path, required=True)
    parser.add_argument("--head-source-root", type=Path, required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--fail-geomean", type=float, default=1.20)
    parser.add_argument("--fail-any", type=float, default=1.50)
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

    for source_root in [args.base_source_root, args.head_source_root]:
        if not source_root.exists():
            raise SystemExit(f"source root does not exist: {source_root}")

    if args.work_dir.exists():
        shutil.rmtree(args.work_dir)

    benchmark_files = [Path(path) for path in BENCHMARK_FILES]
    for relative_path in benchmark_files:
        for source_root in [args.base_source_root, args.head_source_root]:
            source = source_root / relative_path
            if not source.exists():
                raise SystemExit(f"benchmark source does not exist: {source}")

    results = [
        benchmark_one(
            base_compiler=args.base_compiler,
            head_compiler=args.head_compiler,
            base_source=args.base_source_root / relative_path,
            head_source=args.head_source_root / relative_path,
            relative_path=relative_path,
            runs=args.runs,
            warmups=args.warmups,
            work_dir=args.work_dir,
        )
        for relative_path in benchmark_files
    ]
    ratio_geomean = geomean([result["ratio"] for result in results])

    payload = {
        "geomean": ratio_geomean,
        "fail_geomean": args.fail_geomean,
        "fail_any": args.fail_any,
        "results": results,
    }
    if args.json_output is not None:
        args.json_output.parent.mkdir(parents=True, exist_ok=True)
        args.json_output.write_text(json.dumps(payload, indent=2) + "\n")
    if args.markdown_output is not None:
        args.markdown_output.parent.mkdir(parents=True, exist_ok=True)
        write_markdown(args.markdown_output, results, ratio_geomean)

    print("Compiler performance benchmark")
    for result in results:
        print(
            f"{result['path']}: base={result['base_min']:.3f}s "
            f"head={result['head_min']:.3f}s ratio={result['ratio']:.3f}"
        )
    print(f"geomean: {ratio_geomean:.3f}")

    failures = []
    if ratio_geomean > args.fail_geomean:
        failures.append(
            f"geomean ratio {ratio_geomean:.3f} exceeds {args.fail_geomean:.3f}"
        )
    for result in results:
        if result["ratio"] > args.fail_any:
            failures.append(
                f"{result['name']} ratio {result['ratio']:.3f} exceeds "
                f"{args.fail_any:.3f}"
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
