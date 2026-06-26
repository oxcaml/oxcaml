#!/usr/bin/env python3

import argparse
import json
import math
import resource
import shutil
import subprocess
import sys
from pathlib import Path


def gen_pattern_matching(path: Path) -> None:
    constructors = [f"C{i:04d}" for i in range(700)]
    lines = ["type t ="]
    lines.extend(f"  | {constructor}" for constructor in constructors)
    lines.append("")
    lines.append("let classify = function")
    lines.extend(
        f"  | {constructor} -> {i % 97}" for i, constructor in enumerate(constructors)
    )
    lines.append("")
    lines.append("let all = [|")
    lines.extend(f"  {constructor};" for constructor in constructors)
    lines.append("|]")
    lines.append("")
    lines.append("let sum () =")
    lines.append("  let total = ref 0 in")
    lines.append("  for i = 0 to Array.length all - 1 do")
    lines.append("    total := !total + classify all.(i)")
    lines.append("  done;")
    lines.append("  !total")
    path.write_text("\n".join(lines) + "\n")


def gen_modules(path: Path) -> None:
    lines = ["module type S = sig"]
    lines.extend(f"  val v{i:03d} : int -> int" for i in range(220))
    lines.append("end")
    lines.append("")
    lines.append("module Base : S = struct")
    lines.extend(f"  let v{i:03d} x = x + {i}" for i in range(220))
    lines.append("end")
    lines.append("")
    for i in range(16):
        previous = "Base" if i == 0 else f"M{i - 1:02d}"
        lines.append(f"module F{i:02d} (X : S) : S = struct")
        lines.append(f"  module Y = {previous}")
        lines.extend(f"  let v{j:03d} x = X.v{j:03d} (Y.v{j:03d} x)" for j in range(220))
        lines.append("end")
        lines.append(f"module M{i:02d} = F{i:02d}({previous})")
        lines.append("")
    lines.append("module R = M15")
    lines.append("let run x =")
    lines.append("  x")
    lines.extend(f"  |> R.v{i:03d}" for i in range(220))
    path.write_text("\n".join(lines) + "\n")


def gen_closures(path: Path) -> None:
    lines = []
    for i in range(900):
        if i == 0:
            lines.append("let[@inline never] f000 x = x + 1")
        else:
            lines.append(f"let[@inline never] f{i:03d} x = f{i - 1:03d} (x + {i % 11})")
    lines.append("")
    lines.append("let run n =")
    lines.append("  let rec loop acc i =")
    lines.append("    if i = 0 then acc")
    lines.append("    else loop (f899 acc + i) (i - 1)")
    lines.append("  in")
    lines.append("  loop 0 n")
    path.write_text("\n".join(lines) + "\n")


def gen_gadts(path: Path) -> None:
    lines = [
        "type _ expr =",
        "  | Int : int -> int expr",
        "  | Bool : bool -> bool expr",
        "  | Pair : 'a expr * 'b expr -> ('a * 'b) expr",
        "  | Fun : ('a -> 'b) -> ('a -> 'b) expr",
        "  | App : ('a -> 'b) expr * 'a expr -> 'b expr",
        "",
        "let rec eval : type a. a expr -> a = function",
        "  | Int i -> i",
        "  | Bool b -> b",
        "  | Pair (a, b) -> eval a, eval b",
        "  | Fun f -> f",
        "  | App (f, x) -> eval f (eval x)",
        "",
    ]
    for i in range(280):
        lines.append(
            f"let e{i:03d} = Pair (Int {i}, App (Fun (fun x -> x + {i % 17}), Int {i + 1}))"
        )
    lines.append("")
    lines.append("let run () =")
    lines.append("  let total = ref 0 in")
    for i in range(280):
        lines.append(f"  let (a, b) = eval e{i:03d} in")
        lines.append("  total := !total + a + b;")
    lines.append("  !total")
    path.write_text("\n".join(lines) + "\n")


CORPUS = [
    ("pattern_matching", gen_pattern_matching),
    ("modules", gen_modules),
    ("closures", gen_closures),
    ("gadts", gen_gadts),
]


def generate_corpus(corpus_dir: Path) -> list[Path]:
    corpus_dir.mkdir(parents=True, exist_ok=True)
    sources = []
    for name, gen in CORPUS:
        path = corpus_dir / f"{name}.ml"
        gen(path)
        sources.append(path)
    return sources


def child_cpu_seconds() -> float:
    usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    return usage.ru_utime + usage.ru_stime


def run_compile(compiler: Path, source: Path, run_dir: Path) -> float:
    if run_dir.exists():
        shutil.rmtree(run_dir)
    run_dir.mkdir(parents=True)
    local_source = run_dir / source.name
    shutil.copy2(source, local_source)

    before = child_cpu_seconds()
    proc = subprocess.run(
        [
            str(compiler),
            "-w",
            "-a",
            "-S",
            "-c",
            local_source.name,
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
    source: Path,
    runs: int,
    warmups: int,
    work_dir: Path,
) -> dict:
    for i in range(warmups):
        run_compile(base_compiler, source, work_dir / "warmup" / "base" / source.stem / str(i))
        run_compile(head_compiler, source, work_dir / "warmup" / "head" / source.stem / str(i))

    base_times = []
    head_times = []
    for i in range(runs):
        if i % 2 == 0:
            base_times.append(
                run_compile(
                    base_compiler,
                    source,
                    work_dir / "runs" / "base" / source.stem / str(i),
                )
            )
            head_times.append(
                run_compile(
                    head_compiler,
                    source,
                    work_dir / "runs" / "head" / source.stem / str(i),
                )
            )
        else:
            head_times.append(
                run_compile(
                    head_compiler,
                    source,
                    work_dir / "runs" / "head" / source.stem / str(i),
                )
            )
            base_times.append(
                run_compile(
                    base_compiler,
                    source,
                    work_dir / "runs" / "base" / source.stem / str(i),
                )
            )

    base_min = min(base_times)
    head_min = min(head_times)
    return {
        "name": source.stem,
        "base_times": base_times,
        "head_times": head_times,
        "base_min": base_min,
        "head_min": head_min,
        "ratio": head_min / base_min,
    }


def geomean(values: list[float]) -> float:
    return math.exp(sum(math.log(value) for value in values) / len(values))


def write_markdown(path: Path, results: list[dict], ratio_geomean: float) -> None:
    lines = [
        "# Compiler performance smoke benchmark",
        "",
        "| Benchmark | Base min CPU s | Head min CPU s | Head/base |",
        "| --- | ---: | ---: | ---: |",
    ]
    for result in results:
        lines.append(
            f"| {result['name']} | {result['base_min']:.3f} | "
            f"{result['head_min']:.3f} | {result['ratio']:.3f} |"
        )
    lines.extend(["", f"Geomean head/base: **{ratio_geomean:.3f}**", ""])
    path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-compiler", type=Path, required=True)
    parser.add_argument("--head-compiler", type=Path, required=True)
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

    if args.work_dir.exists():
        shutil.rmtree(args.work_dir)
    corpus_dir = args.work_dir / "corpus"
    sources = generate_corpus(corpus_dir)

    results = [
        benchmark_one(
            base_compiler=args.base_compiler,
            head_compiler=args.head_compiler,
            source=source,
            runs=args.runs,
            warmups=args.warmups,
            work_dir=args.work_dir,
        )
        for source in sources
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

    print("Compiler performance smoke benchmark")
    for result in results:
        print(
            f"{result['name']}: base={result['base_min']:.3f}s "
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
