#!/usr/bin/env python3
"""Extract callee_regs_savings and callee_regs_live counters from profile CSVs.

Usage: python3 extract_callee_regs_counters.py <directory>

Scans all *.csv files in the given directory.  For each file, reads the row
whose pass name ends with "/save_cfg" (accumulated once per function, so
there is no double-counting) and sums the two counters of interest.
"""

import csv
import re
import sys
from pathlib import Path
from typing import Dict, Tuple


def parse_counters(cell: str) -> Dict[str, int]:
    """Parse '[k1 = v1; k2 = v2; ...]' into {k1: v1, k2: v2, ...}."""
    cell = cell.strip()
    if not cell or cell == "[]":
        return {}
    # Strip surrounding brackets
    if cell.startswith("["):
        cell = cell[1:]
    if cell.endswith("]"):
        cell = cell[:-1]
    result = {}
    for entry in cell.split(";"):
        entry = entry.strip()
        if not entry:
            continue
        m = re.fullmatch(r"(\S+)\s*=\s*(-?\d+)", entry)
        if m:
            result[m.group(1)] = int(m.group(2))
    return result


def process_file(path: Path) -> Tuple[int, int]:
    """Return (callee_regs_live, callee_regs_savings) summed over save_cfg rows."""
    live = 0
    savings = 0
    with path.open(newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            pass_name = row.get("pass name", "")
            if not pass_name.endswith("/save_cfg"):
                continue
            counters = parse_counters(row.get("counters", ""))
            live += counters.get("callee_regs_live", 0)
            savings += counters.get("callee_regs_savings", 0)
    return live, savings


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: {} <directory>".format(sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    directory = Path(sys.argv[1])
    if not directory.is_dir():
        print("Error: '{}' is not a directory".format(directory), file=sys.stderr)
        sys.exit(1)

    csv_files = sorted(directory.glob("*.csv"))
    if not csv_files:
        print("No CSV files found in '{}'".format(directory), file=sys.stderr)
        sys.exit(1)

    total_live = 0
    total_savings = 0

    for path in csv_files:
        live, savings = process_file(path)
        ratio = savings / live if live > 0 else float("nan")
        print("{}: live={}, savings={}, ratio={:.3f}".format(
            path.name, live, savings, ratio))
        total_live += live
        total_savings += savings

    total_ratio = total_savings / total_live if total_live > 0 else float("nan")
    print()
    print("Total ({} file(s)): live={}, savings={}, ratio={:.3f}".format(
        len(csv_files), total_live, total_savings, total_ratio))


if __name__ == "__main__":
    main()
