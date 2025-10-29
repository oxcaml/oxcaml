#!/usr/bin/env python3
"""Script to collect compiler metrics (sizes, profile data, counters)."""

import argparse
import csv
import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import List, Dict, Any, Optional

# Type aliases for clarity
CsvRow = Dict[str, Any]  # column -> value
ProfileFile = List[CsvRow]
ProfileData = List[ProfileFile]
AggregatedMetrics = Dict[str, Dict[str, float]]  # kind -> (pass -> value)
Counters = Dict[str, int]  # name -> value

# Counter names we're interested in tracking
COUNTERS_OF_INTEREST = ["reload", "spill"]

# Special name for top-level pass (file=path/ without // separator)
TOP_LEVEL_PASS_NAME = "."


def print_warning(message: str) -> None:
    """Print a warning message to stderr."""
    print(f"Warning: {message}", file=sys.stderr)


def fatal(message: str) -> None:
    """Print an error message to stderr and exit with code 1."""
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(1)


def parse_pass_name(pass_name: str) -> Optional[str]:
    """Extract hierarchical pass name from 'file=path//pass/name' format.

    If the format is 'file=path/' (no '//' separator), this represents the
    top-level pass for that file, and we return TOP_LEVEL_PASS_NAME.
    """
    if not pass_name or not pass_name.startswith("file="):
        return None

    # Split on '//' to separate file path from pass hierarchy
    parts = pass_name.split(sep="//", maxsplit=1)
    if len(parts) < 2:
        # This is the top-level: file=path/
        # Check that it ends with '/' to validate the format
        if parts[0].endswith("/"):
            return TOP_LEVEL_PASS_NAME
        return None

    # Return the hierarchical pass name
    return parts[1]


def parse_value_with_unit(value_str: str) -> Optional[float]:
    """Parse value with unit (s, B, kB, MB, GB) and convert to base unit."""
    value_str = value_str.strip()

    if not value_str:
        return None

    # Time: always in seconds (e.g., "1.878s")
    if value_str.endswith("s"):
        try:
            return float(value_str[:-1])
        except ValueError:
            return None

    # Memory/heap: B, kB, MB, GB
    # Check longer suffixes first to avoid "GB" matching "B", etc.
    multipliers = [
        ("GB", 1024**3),
        ("MB", 1024**2),
        ("kB", 1024),
        ("B", 1),
    ]

    for unit, multiplier in multipliers:
        if value_str.endswith(unit):
            try:
                value = float(value_str[: -len(unit)])
                return value * multiplier
            except ValueError:
                return None

    return None


def parse_counters(counters_str: str) -> Counters:
    """Parse counter string format: [name = value; name = value; ...]"""
    counters: Counters = {}

    counters_str = counters_str.strip()

    if not counters_str:
        return counters

    # Remove brackets and split by semicolons
    if not (counters_str.startswith("[") and counters_str.endswith("]")):
        # Non-empty string without proper square brackets - invalid format
        return counters

    counters_str = counters_str[1:-1]

    # Parse each counter
    for part in counters_str.split(sep=";"):
        part = part.strip()
        if "=" in part:
            name, value = part.split(sep="=", maxsplit=1)
            name = name.strip()
            value = value.strip()
            try:
                counters[name] = int(value)
            except ValueError:
                # Skip if value is not an integer
                continue

    return counters


def aggregate_counters(profile_data_by_file: ProfileData) -> Counters:
    """Aggregate counters: keep latest per file, then sum across files."""
    total_counters = {counter: 0 for counter in COUNTERS_OF_INTEREST}

    # For each profile file
    for file_records in profile_data_by_file:
        # Get latest occurrence of each counter in this file
        file_counters: Counters = {}
        for record in file_records:
            counters_str = record.get("counters", "")
            counters = parse_counters(counters_str)

            # Update with latest values for counters of interest
            for counter_name in COUNTERS_OF_INTEREST:
                if counter_name in counters:
                    file_counters[counter_name] = counters[counter_name]

        # Sum the latest values from this file
        for counter_name, value in file_counters.items():
            total_counters[counter_name] += value

    return total_counters


def aggregate_profile_metrics(profile_data_by_file: ProfileData) -> AggregatedMetrics:
    """Aggregate time/memory/heap metrics by pass name, summing values."""
    # Metrics to aggregate: column name -> output kind name
    metric_columns = {
        "time": "time_in_seconds",
        "alloc": "alloc_in_bytes",
        "top-heap": "top_heap_in_bytes",
        "absolute-top-heap": "absolute_top_heap_in_bytes",
    }

    # Initialize aggregation dictionaries
    aggregated: AggregatedMetrics = {kind: {} for kind in metric_columns.values()}

    # Process each profile file
    for file_records in profile_data_by_file:
        for record in file_records:
            # Extract pass name
            raw_pass_name = record.get("pass name", "")
            pass_name = parse_pass_name(raw_pass_name)
            if pass_name is None:
                continue

            # Process each metric column
            for column, kind in metric_columns.items():
                value_str = record.get(column, "")
                value = parse_value_with_unit(value_str)

                if value is not None:
                    if pass_name not in aggregated[kind]:
                        aggregated[kind][pass_name] = 0.0
                    aggregated[kind][pass_name] += value

    return aggregated


def collect_metrics(
    install_dir: str,
    output_dir: str,
    commit_hash: str,
    commit_message: str,
    profile_data: ProfileData,
    verbose: bool = False,
) -> None:
    """Collect compiler metrics (sizes, profile data, counters) and write to CSV."""
    install_path = Path(install_dir)

    # Extract PR number from commit message (use last match if multiple)
    pr_matches = re.findall(r"\(#(\d+)\)", commit_message)
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
        "exe",
        "opt",
        "a",
        "cmxa",
        "cma",
        "cmi",
        "cmx",
        "cmo",
        "cms",
        "cmsi",
        "cmt",
        "cmti",
        "o",
    ]

    # Aggregate counter data from profile
    counter_metrics = aggregate_counters(profile_data)

    # Aggregate profile metrics (time, memory, heap)
    profile_metrics = aggregate_profile_metrics(profile_data)

    with csv_path.open("w") as csv_file:
        # Write CSV header
        csv_file.write("timestamp,commit_hash,pr_number,kind,name,value\n")

        # Collect size metrics for each extension
        for ext in extensions:
            files = list(install_path.rglob(f"*.{ext}"))
            total_size = sum(file.stat().st_size for file in files if file.is_file())

            # Write size metric to CSV
            csv_file.write(
                f"{timestamp},{commit_hash},{pr_number},"
                f"size_in_bytes,{ext},{total_size}\n"
            )

        # Write counter metrics to CSV
        for counter_name, counter_value in sorted(counter_metrics.items()):
            csv_file.write(
                f"{timestamp},{commit_hash},{pr_number},"
                f"counter,{counter_name},{counter_value}\n"
            )

        # Write profile metrics to CSV
        for kind in sorted(profile_metrics.keys()):
            for pass_name, value in sorted(profile_metrics[kind].items()):
                csv_file.write(
                    f"{timestamp},{commit_hash},{pr_number},"
                    f"{kind},{pass_name},{value}\n"
                )

    print(f"Generated metrics file: {csv_path}")
    print(f"Metrics collected for commit: {commit_hash}")

    # Print file contents if verbose
    if verbose:
        print("\nContents of generated metrics file:")
        with csv_path.open("r") as csv_file:
            print(csv_file.read())


def find_profile_csv_files(build_dir: str) -> List[Path]:
    """Find all profile CSV files under _build directory."""
    build_path = Path(build_dir)

    # Find all files matching the pattern */_profile_csv/profile.*.csv
    profile_files = list(build_path.rglob("_profile_csv/profile.*.csv"))

    return profile_files


def parse_profile_csv(profile_path: Path) -> ProfileFile:
    """Parse a single profile CSV file and return list of records."""
    records: ProfileFile = []

    try:
        with profile_path.open("r") as csv_file:
            reader = csv.DictReader(csv_file)
            for row in reader:
                records.append(row)
    except Exception as e:
        print_warning(f"Failed to parse {profile_path}: {e}")

    return records


def load_profile_data(build_dir: str, verbose: bool = False) -> ProfileData:
    """Load all profile CSV data, grouped by file."""
    profile_files = find_profile_csv_files(build_dir)

    if verbose:
        print(f"\nFound {len(profile_files)} profile CSV files:")
        for pf in profile_files:
            print(f"  {pf}")

    profile_data_by_file: ProfileData = []
    total_records = 0
    for profile_file in profile_files:
        records = parse_profile_csv(profile_file)
        profile_data_by_file.append(records)
        total_records += len(records)

    if verbose:
        print(f"\nLoaded {total_records} profile records total")

    return profile_data_by_file


def main() -> None:
    """Parse arguments and run metrics collection."""
    parser = argparse.ArgumentParser(
        description="Collect compiler metrics (sizes, profile data, counters)"
    )
    parser.add_argument(
        "--install-directory", required=True, help="Path to install directory"
    )
    parser.add_argument(
        "--output-directory", required=True, help="Output directory for CSV file"
    )
    parser.add_argument("--commit-hash", required=True, help="Git commit hash")
    parser.add_argument("--commit-message", required=True, help="Git commit message")
    parser.add_argument(
        "--build-directory", required=True, help="Path to build directory"
    )
    parser.add_argument(
        "--verbose", action="store_true", help="Print contents of generated CSV file"
    )

    args = parser.parse_args()

    # Validate that required directories exist
    if not Path(args.install_directory).is_dir():
        fatal(f"Install directory '{args.install_directory}' does not exist")

    if not Path(args.build_directory).is_dir():
        fatal(f"Build directory '{args.build_directory}' does not exist")

    # Load profile data
    profile_data = load_profile_data(args.build_directory, args.verbose)

    collect_metrics(
        args.install_directory,
        args.output_directory,
        args.commit_hash,
        args.commit_message,
        profile_data,
        args.verbose,
    )


if __name__ == "__main__":
    main()
