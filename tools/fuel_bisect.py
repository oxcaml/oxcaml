#!/usr/bin/env python3
"""Optimization fuel bisect script.

Subcommands:
  record              Clean build to record step counts
  bisect <test_cmd>   Binary search for the critical global step
  fail <threshold>    Rebuild at threshold with OPT_FUEL_FAIL to identify step

The compiler reads OPT_FUEL_RECORD (path to record file),
OPT_FUEL_THRESHOLD (global step limit), and OPT_FUEL_FAIL (global step
at which to fail with a diagnostic). All builds are clean.

Example workflow:
  python3 tools/fuel_bisect.py record
  python3 tools/fuel_bisect.py bisect /tmp/test_cmd
  python3 tools/fuel_bisect.py fail 42
"""

import os
import subprocess
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RECORD_FILE = os.path.join(REPO, "opt_fuel_record.txt")


def run(cmd, env=None, check=True, timeout=None):
    """Run a shell command in the repo root."""
    merged = dict(os.environ)
    if env:
        merged.update(env)
    env_prefix = ""
    if env:
        env_prefix = " ".join(f"{k}={v}" for k, v in env.items()) + " "
    print(f"  $ {env_prefix}{cmd}", flush=True)
    result = subprocess.run(
        cmd, shell=True, cwd=REPO, env=merged, timeout=timeout,
    )
    if check and result.returncode != 0:
        print(f"  -> exit {result.returncode}")
    return result.returncode


def clean_build(env=None, target="install"):
    """Clean and rebuild."""
    run("make -s clean", check=False, env=env)
    return run(f"make -s {target}", env=env)


def load_record():
    """Load record file. Returns list of (file, count) and total."""
    entries = []
    with open(RECORD_FILE) as f:
        for line in f:
            parts = line.strip().rsplit(" ", 1)
            assert len(parts) == 2
            entries.append((parts[0], int(parts[1])))
    total = sum(c for _, c in entries)
    return entries, total


def find_file_at_threshold(entries, threshold):
    """Find which file contains the given global step."""
    offset = 0
    for file_path, count in entries:
        if offset < threshold <= offset + count:
            return file_path, threshold - offset, count
        offset += count
    return None, 0, 0


def cmd_record():
    """Record step counts per compiled file."""
    print("=== Recording optimization steps ===")
    if os.path.exists(RECORD_FILE):
        os.remove(RECORD_FILE)

    clean_build(env={"OPT_FUEL_RECORD": RECORD_FILE})
    if not os.path.exists(RECORD_FILE):
        print("ERROR: no record file produced")
        sys.exit(1)

    entries, total = load_record()
    print(f"Record: {RECORD_FILE}")
    print(f"  {len(entries)} files, {total} total steps")
    for path, count in entries[:10]:
        print(f"  {count:4d} steps: {os.path.basename(path)}")
    if len(entries) > 10:
        print(f"  ... ({len(entries) - 10} more)")


def test_threshold(threshold, test_cmd):
    """Clean build at threshold, then run test. Returns True if pass."""
    print(f"\n--- threshold={threshold} ---")
    env = {
        "OPT_FUEL_RECORD": RECORD_FILE,
        "OPT_FUEL_THRESHOLD": str(threshold),
    }
    rc = clean_build(env=env)
    if rc != 0:
        print(f"  Build FAILED at threshold={threshold}")
        return False

    rc = run(test_cmd, env=env)
    passed = rc == 0
    print(f"  -> {'PASS' if passed else 'FAIL'}")
    return passed


def cmd_bisect(test_cmd):
    """Binary search for the critical threshold."""
    if not os.path.exists(RECORD_FILE):
        print("ERROR: record file not found. Run 'record' first.")
        sys.exit(1)

    entries, total = load_record()
    print(f"=== Bisecting {total} steps across {len(entries)} files ===")
    print(f"Test: {test_cmd}")

    # Verify endpoints
    if not test_threshold(0, test_cmd):
        print("ERROR: fails with no optimization. Bug is elsewhere.")
        sys.exit(1)

    if test_threshold(total, test_cmd):
        print("Passes with full optimization. Nothing to bisect.")
        sys.exit(0)

    # Binary search: lo passes, hi fails
    lo, hi = 0, total

    while hi - lo > 1:
        print(f"=== Bisecting: step range {lo} - {hi} ===")
        mid = (lo + hi) // 2
        if test_threshold(mid, test_cmd):
            lo = mid
        else:
            hi = mid

    print(f"\n=== RESULT ===")
    print(f"Critical threshold: {hi}")
    print(f"  threshold={lo} PASS, threshold={hi} FAIL")

    file_path, local_step, file_total = find_file_at_threshold(
        entries, hi
    )
    if file_path:
        print(f"  File: {file_path}")
        print(f"  Local step: {local_step} of {file_total}")

    print(f"\nTo identify: python3 {sys.argv[0]} fail {hi}")
    return hi


def cmd_fail(threshold):
    """Rebuild with OPT_FUEL_FAIL to get exact location."""
    print(f"=== Identifying critical step at threshold={threshold} ===")
    env = {
        "OPT_FUEL_RECORD": RECORD_FILE,
        "OPT_FUEL_FAIL": str(threshold),
    }
    rc = clean_build(env=env)
    if rc == 0:
        print(
            "Build succeeded. The critical step doesn't cause a build "
            "failure."
        )


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    cmd = sys.argv[1]
    if cmd == "record":
        cmd_record()
    elif cmd == "bisect":
        test_cmd = sys.argv[2] if len(sys.argv) > 2 else None
        if test_cmd is None:
            print("Usage: fuel_bisect.py bisect <test_cmd>")
            sys.exit(1)
        cmd_bisect(test_cmd)
    elif cmd == "fail":
        if len(sys.argv) < 3:
            print("Usage: fuel_bisect.py fail <threshold>")
            sys.exit(1)
        cmd_fail(int(sys.argv[2]))
    else:
        print(f"Unknown command: {cmd}")
        sys.exit(1)


if __name__ == "__main__":
    main()
