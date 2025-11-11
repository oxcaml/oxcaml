#!/usr/bin/env python3
"""Script to collect file size metrics from _install directory."""

import argparse
import re
import sys
from datetime import datetime, timezone
from pathlib import Path


def collect_metrics(install_dir: str, output_dir: str, commit_hash: str,
                    commit_message: str, verbose: bool = False) -> None:
    """Collect file size metrics and write to CSV."""
    install_path = Path(install_dir)

    # Validate input directory exists
    if not install_path.is_dir():
        print(f"Error: Install directory '{install_dir}' does not exist",
              file=sys.stderr)
        sys.exit(1)

    # Extract PR number from commit message (use last match if multiple)
    pr_matches = re.findall(r'\(#(\d+)\)', commit_message)
    pr_number = pr_matches[-1] if pr_matches else "N/A"

    # Generate timestamp and compute output path
    now = datetime.now(timezone.utc)
    timestamp = now.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_str = now.strftime("%Y-%m-%d")
    short_hash = commit_hash[:8]

    # Create output directory and compute CSV filename
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    csv_path = output_path / f"metrics-{date_str}-{short_hash}.csv"

    # Extensions to track
    extensions = [
        "exe", "opt", "a", "cmxa", "cma", "cmi",
        "cmx", "cmo", "cms", "cmsi", "cmt", "cmti", "o"
    ]

    with csv_path.open("w") as csv_file:
        # Write CSV header
        csv_file.write("timestamp,commit_hash,pr_number,kind,name,value\n")

        # Collect metrics for each extension
        kind = "size_in_bytes"
        for ext in extensions:
            files = list(install_path.rglob(f"*.{ext}"))
            total_size = sum(file.stat().st_size for file in files
                           if file.is_file())

            # Write to CSV
            csv_file.write(f"{timestamp},{commit_hash},{pr_number},{kind},{ext},{total_size}\n")

    print(f"Generated metrics file: {csv_path}")
    print(f"Metrics collected for commit: {commit_hash}")

    # Print file contents if verbose
    if verbose:
        print("\nContents of generated metrics file:")
        with csv_path.open("r") as csv_file:
            print(csv_file.read())


def main() -> None:
    """Parse arguments and run metrics collection."""
    parser = argparse.ArgumentParser(
        description="Collect file size metrics from install directory"
    )
    parser.add_argument("install_directory", help="Path to install directory")
    parser.add_argument("output_directory", help="Output directory for CSV file")
    parser.add_argument("commit_hash", help="Git commit hash")
    parser.add_argument("commit_message", help="Git commit message")
    parser.add_argument("--verbose", action="store_true",
                        help="Print contents of generated CSV file")

    args = parser.parse_args()

    collect_metrics(
        args.install_directory,
        args.output_directory,
        args.commit_hash,
        args.commit_message,
        args.verbose
    )


if __name__ == "__main__":
    main()
