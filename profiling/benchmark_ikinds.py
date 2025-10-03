#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import statistics
import subprocess
import sys
import time
from pathlib import Path
from typing import Iterable, List

REPO_ROOT = Path(__file__).resolve().parents[1]
IKINDS_FILE = REPO_ROOT / "typing" / "ikinds" / "ikinds.ml"
PROFILING_DIR = REPO_ROOT / "profiling"
OCAMLC_DEFAULT = REPO_ROOT / "_build" / "_bootinstall" / "bin" / "ocamlc"
OCAMLLIB_DEFAULT = REPO_ROOT / "_build" / "runtime_stdlib_install" / "lib" / "ocaml_runtime_stdlib"

BASE_ARGS = ["-extension", "layouts_alpha"]

FORCE_LINE = "Clflags.ikinds := true"
COMMENTED_FORCE_LINE = "(* Clflags.ikinds := true *)"


class FileGuard:
    """Context manager that rewrites `typing/ikinds/ikinds.ml` and restores it afterwards."""

    def __init__(self, path: Path) -> None:
        self.path = path
        self.original: str | None = None

    def __enter__(self) -> None:
        self.original = self.path.read_text()

    def __exit__(self, exc_type, exc, tb) -> None:  # type: ignore[override]
        if self.original is not None:
            self.path.write_text(self.original)


def ensure_force_line_commented() -> None:
    content = IKINDS_FILE.read_text().splitlines()
    changed = False
    for idx, line in enumerate(content):
        stripped = line.strip()
        if FORCE_LINE in stripped and not stripped.startswith("(*"):
            content[idx] = line.replace(FORCE_LINE, COMMENTED_FORCE_LINE)
            changed = True
            break
    if not changed:
        raise SystemExit(
            "Could not locate an uncommented 'Clflags.ikinds := true' line in ikinds.ml"
        )
    IKINDS_FILE.write_text("\n".join(content) + "\n")


def run(cmd: List[str], *, cwd: Path | None = None, env: dict[str, str] | None = None) -> None:
    subprocess.run(cmd, cwd=cwd, env=env, check=True)


def run_make_boot() -> None:
    run(["make", "boot-compiler"], cwd=REPO_ROOT)


def compile_large(ocamlc: Path, ocamllib: Path, *, extra_args: Iterable[str]) -> float:
    env = os.environ.copy()
    env["OCAMLLIB"] = str(ocamllib)
    (PROFILING_DIR / "large_typecheck.cmi").unlink(missing_ok=True)
    (PROFILING_DIR / "large_typecheck.cmo").unlink(missing_ok=True)
    cmd = [str(ocamlc), *BASE_ARGS, *extra_args, "-c", "large_typecheck.ml"]
    start = time.perf_counter()
    run(cmd, cwd=PROFILING_DIR, env=env)
    end = time.perf_counter()
    return end - start


def describe(values: List[float]) -> tuple[float, float]:
    mean = statistics.mean(values)
    stddev = statistics.stdev(values) if len(values) > 1 else 0.0
    return mean, stddev


def format_seconds(seconds: float) -> str:
    return f"{seconds:.4f}"


def main() -> None:
    parser = argparse.ArgumentParser(description="Benchmark ocamlc with/without -ikinds")
    parser.add_argument("--iterations", type=int, default=10, help="Runs per configuration")
    parser.add_argument(
        "--ocamlc",
        type=Path,
        default=OCAMLC_DEFAULT,
        help="ocamlc executable to profile",
    )
    parser.add_argument(
        "--ocamllib",
        type=Path,
        default=OCAMLLIB_DEFAULT,
        help="OCAMLLIB directory to set for ocamlc",
    )
    args = parser.parse_args()

    if not args.ocamlc.is_file():
        parser.error(f"ocamlc not found: {args.ocamlc}")
    if not args.ocamllib.is_dir():
        parser.error(f"OCAMLLIB directory not found: {args.ocamllib}")

    with FileGuard(IKINDS_FILE):
        ensure_force_line_commented()
        run_make_boot()

        print(f"Running benchmarks with {args.ocamlc} (iterations={args.iterations})")
        with_flag: List[float] = []
        without_flag: List[float] = []

        print("\n## With -ikinds")
        print("  Warmup run (not recorded)...", end="", flush=True)
        compile_large(args.ocamlc, args.ocamllib, extra_args=["-ikinds"])
        print(" done")
        for i in range(1, args.iterations + 1):
            duration = compile_large(args.ocamlc, args.ocamllib, extra_args=["-ikinds"])
            with_flag.append(duration)
            print(f"  Run {i:2d}: {format_seconds(duration)} s")

        print("\n## Without -ikinds")
        print("  Warmup run (not recorded)...", end="", flush=True)
        compile_large(args.ocamlc, args.ocamllib, extra_args=[])
        print(" done")
        for i in range(1, args.iterations + 1):
            duration = compile_large(args.ocamlc, args.ocamllib, extra_args=[])
            without_flag.append(duration)
            print(f"  Run {i:2d}: {format_seconds(duration)} s")

        mean_with, std_with = describe(with_flag)
        mean_without, std_without = describe(without_flag)

        print("\n== Summary ==")
        print(f"  With -ikinds   : mean={format_seconds(mean_with)} s, stddev={format_seconds(std_with)} s")
        print(f"  Without -ikinds: mean={format_seconds(mean_without)} s, stddev={format_seconds(std_without)} s")

    # Restore original ikinds.ml and rebuild so repository returns to original state
    run_make_boot()


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as exc:
        sys.stderr.write(f"Command failed with exit code {exc.returncode}: {exc.cmd}\n")
        sys.exit(exc.returncode)
