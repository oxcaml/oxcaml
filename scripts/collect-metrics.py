#!/usr/bin/env python3
"""Script to collect file size metrics from _install directory."""

import argparse
import re
import sys
import tarfile
from datetime import datetime, timezone
from pathlib import Path


def warn(message: str) -> None:
    """Print a warning message to stderr."""
    print(f"Warning: {message}", file=sys.stderr)


def fatal(message: str) -> None:
    """Print an error message to stderr and exit."""
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(1)


def archive_profile_csvs(profile_dir: str, output_dir: str, date_str: str,
                         short_hash: str, verbose: bool = False) -> None:
    """Find and archive profile CSV files."""
    profile_path = Path(profile_dir)

    # Find all profile CSV files in the profile directory
    profile_files = list(profile_path.glob("profile.*.csv"))

    if not profile_files:
        warn(f"No profile CSV files found in '{profile_path}', "
             "skipping profile archive")
        return

    # Create archive
    output_path = Path(output_dir)
    archive_path = output_path / f"profiles-{date_str}-{short_hash}.tar.gz"

    with tarfile.open(archive_path, "w:gz") as tar:
        for profile_file in profile_files:
            # Add with just the filename
            tar.add(profile_file, arcname=profile_file.name)

    print(f"Generated profile archive: {archive_path}")

    if verbose:
        print(f"\nProfile files included:")
        for profile_file in profile_files:
            print(f"  - {profile_file.name}")

    print(f"Collected {len(profile_files)} profile CSV file(s)")


def collect_metrics(install_dir: str, output_dir: str, commit_hash: str,
                    commit_message: str, profile_dir: str,
                    verbose: bool = False) -> None:
    """Collect file size metrics and write to CSV."""
    install_path = Path(install_dir)
    output_path = Path(output_dir)
    profile_path = Path(profile_dir)

    # Validate all directories exist
    if not install_path.is_dir():
        fatal(f"Install directory '{install_dir}' does not exist")
    if not output_path.is_dir():
        fatal(f"Output directory '{output_dir}' does not exist")
    if not profile_path.is_dir():
        fatal(f"Profile directory '{profile_dir}' does not exist")

    # Extract PR number from commit message (use last match if multiple)
    pr_matches = re.findall(r'\(#(\d+)\)', commit_message)
    pr_number = pr_matches[-1] if pr_matches else "N/A"

    # Generate timestamp and compute CSV filename
    now = datetime.now(timezone.utc)
    timestamp = now.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_str = now.strftime("%Y-%m-%d")
    short_hash = commit_hash[:8]
    csv_path = output_path / f"artifact-sizes-{date_str}-{short_hash}.csv"

    # Extensions to track
    extensions = [
        "exe", "opt", "a", "cmxa", "cma", "cmi",
        "cmx", "cmo", "cms", "cmsi", "cmt", "cmti", "o"
    ]

    with csv_path.open("w") as csv_file:
        # Write CSV header
        csv_file.write("timestamp,commit_hash,pr_number,kind,name,value\n")

        # Collect metrics for each artifact
        kind = "size_in_bytes"
        for ext in extensions:
            files = list(install_path.rglob(f"*.{ext}"))
            for file in files:
                if file.is_file():
                    size = file.stat().st_size
                    relative_path = file.relative_to(install_path)
                    csv_file.write(f"{timestamp},{commit_hash},{pr_number},{kind},{relative_path},{size}\n")

    print(f"Generated metrics file: {csv_path}")
    print(f"Metrics collected for commit: {commit_hash}")

    # Print file contents if verbose
    if verbose:
        print("\nContents of generated metrics file:")
        with csv_path.open("r") as csv_file:
            print(csv_file.read())

    # Archive profile CSV files
    archive_profile_csvs(profile_dir, output_dir, date_str, short_hash, verbose)


def main() -> None:
    """Parse arguments and run metrics collection."""
    parser = argparse.ArgumentParser(
        description="Collect file size metrics from install directory"
    )
    parser.add_argument("--install-dir", required=True,
                        help="Path to install directory")
    parser.add_argument("--output-dir", required=True,
                        help="Output directory for CSV file")
    parser.add_argument("--commit-hash", required=True,
                        help="Git commit hash")
    parser.add_argument("--commit-message", required=True,
                        help="Git commit message")
    parser.add_argument("--profile-dir", required=True,
                        help="Path to profile directory containing CSV files")
    parser.add_argument("--verbose", action="store_true",
                        help="Print contents of generated CSV file")

    args = parser.parse_args()

    collect_metrics(
        args.install_dir,
        args.output_dir,
        args.commit_hash,
        args.commit_message,
        args.profile_dir,
        args.verbose
    )


if __name__ == "__main__":
    main()
