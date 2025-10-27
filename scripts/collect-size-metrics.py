#!/usr/bin/env python3
"""Script to collect file size metrics from _install directory."""

import argparse
import sys
from datetime import datetime, timezone
from pathlib import Path


def collect_metrics(install_dir: str, output_csv_file: str, commit_hash: str,
                    pr_number: str) -> None:
    """Collect file size metrics and write to CSV."""
    install_path = Path(install_dir)

    # Validate input directory exists
    if not install_path.is_dir():
        print(f"Error: Install directory '{install_dir}' does not exist",
              file=sys.stderr)
        sys.exit(1)

    # Generate timestamp
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # Extensions to track
    extensions = [
        "exe", "opt", "a", "cmxa", "cma", "cmi",
        "cmx", "cmo", "cms", "cmsi", "cmt", "cmti", "o"
    ]

    csv_path = Path(output_csv_file)

    with csv_path.open("w") as csv_file:
        # Write CSV header
        csv_file.write("timestamp,commit_hash,pr_number,extension,total_size_bytes\n")

        # Collect metrics for each extension
        for ext in extensions:
            files = list(install_path.rglob(f"*.{ext}"))
            total_size = sum(file.stat().st_size for file in files
                           if file.is_file())

            # Write to CSV
            csv_file.write(f"{timestamp},{commit_hash},{pr_number},{ext},{total_size}\n")

    print(f"Generated metrics file: {output_csv_file}")
    print(f"Metrics collected for commit: {commit_hash}")


def main() -> None:
    """Parse arguments and run metrics collection."""
    parser = argparse.ArgumentParser(
        description="Collect file size metrics from install directory"
    )
    parser.add_argument("install_directory", help="Path to install directory")
    parser.add_argument("output_csv_file", help="Output CSV file path")
    parser.add_argument("commit_hash", help="Git commit hash")
    parser.add_argument("pr_number", help="Pull request number")

    args = parser.parse_args()

    collect_metrics(
        args.install_directory,
        args.output_csv_file,
        args.commit_hash,
        args.pr_number
    )


if __name__ == "__main__":
    main()
