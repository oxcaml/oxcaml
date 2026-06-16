#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys
from pathlib import Path


DEFAULT_FILES = [
    "uic-design.md",
    "typing/solver_intf.mli",
    "typing/solver.ml",
    "typing/mode.ml",
]


def run(repo: Path, args: list[str]) -> str:
    try:
        return subprocess.check_output(
            args,
            cwd=repo,
            stderr=subprocess.STDOUT,
            text=True,
        )
    except subprocess.CalledProcessError as ex:
        return ex.output


def fence(name: str, content: str) -> str:
    return f"\n\n## {name}\n\n```text\n{content.rstrip()}\n```\n"


def read_file(repo: Path, rel: str) -> str:
    path = repo / rel
    try:
        return path.read_text()
    except FileNotFoundError:
        return f"<missing file: {rel}>\n"


def copy_to_clipboard(text: str) -> None:
    try:
        subprocess.run(["pbcopy"], input=text, text=True, check=True)
    except (FileNotFoundError, subprocess.CalledProcessError) as ex:
        raise RuntimeError("failed to copy to clipboard with pbcopy") from ex


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Copy UIC design, solver context, and git diff to clipboard."
    )
    parser.add_argument(
        "--base",
        default="origin/main",
        help="base ref for branch diff; default: origin/main",
    )
    parser.add_argument(
        "--repo",
        default=Path(__file__).resolve().parents[1],
        type=Path,
        help="repository root; default: parent of scripts/",
    )
    parser.add_argument(
        "--print",
        action="store_true",
        help="print instead of copying to clipboard",
    )
    parser.add_argument(
        "files",
        nargs="*",
        help="files to include instead of the default UIC context set",
    )
    ns = parser.parse_args()

    repo = ns.repo.resolve()
    files = ns.files or DEFAULT_FILES

    parts = []
    parts.append("# UIC context for GPT-5.5 Pro\n")
    parts.append(f"Repository: {repo}\n")
    parts.append(f"Base ref: {ns.base}\n")

    for rel in files:
        parts.append(f"\n\n## {rel}\n\n")
        parts.append("```ocaml\n" if rel.endswith((".ml", ".mli")) else "```text\n")
        parts.append(read_file(repo, rel).rstrip())
        parts.append("\n```\n")

    parts.append(fence("git status", run(repo, ["git", "status", "--short", "--branch"])))
    parts.append(
        fence(
            f"git diff --stat {ns.base}...HEAD",
            run(repo, ["git", "diff", "--stat", f"{ns.base}...HEAD"]),
        )
    )
    parts.append(
        fence(
            f"git diff {ns.base}...HEAD",
            run(repo, ["git", "diff", f"{ns.base}...HEAD"]),
        )
    )
    parts.append(fence("git diff -- working tree", run(repo, ["git", "diff"])))

    text = "".join(parts)
    if ns.print:
        sys.stdout.write(text)
    else:
        copy_to_clipboard(text)
        print(f"Copied {len(text):,} characters to the clipboard.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
