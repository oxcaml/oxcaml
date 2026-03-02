#!/usr/bin/env python3
"""
Analyze a merge-conflict-resolution with patdiff4.

For each file that differs between base and resolved, reconstructs
the four versions (base, oxcaml, incoming, resolution) from the diff3
conflict markers and runs patdiff4.

patdiff4 diamond:   base(b1)  oxcaml(f1)  incoming(b2)  resolution(f2)

Usage:  python3 analyze.py -commit COMMIT
        python3 analyze.py -base BASE -resolved RESOLVED
"""

import signal
import subprocess
import sys
import os
import tempfile


# ── Git helpers ───────────────────────────────────────────────────────────

def git_file(commit, filepath):
    """Return lines of a file at a given commit."""
    return subprocess.check_output(
        ["git", "show", "{0}:{1}".format(commit, filepath)],
        universal_newlines=True
    ).splitlines()


def changed_files(commit_a, commit_b):
    raw = subprocess.check_output(
        ["git", "diff", "--name-only", commit_a, commit_b],
        universal_newlines=True
    )
    return [f for f in raw.strip().split("\n") if f]


# ── Conflict parsing ─────────────────────────────────────────────────────

def resolve_conflicts(lines):
    """Resolve all diff3 conflicts in *lines*, returning all three sides.

    Returns (oxcaml, base, incoming).
    """
    oxcaml = []
    base = []
    incoming = []
    in_oxcaml = True
    in_base = True
    in_incoming = True

    in_conflict = False

    for i, line in enumerate(lines, 1):
        if line.startswith("<<<<<<< oxcaml"):
            in_conflict = True
            in_oxcaml = True
            in_base = False
            in_incoming = False
            continue

        if in_conflict and line.startswith("||||||| upstream-base"):
            in_oxcaml = False
            in_base = True
            in_incoming = False
            continue

        if in_conflict and line == "=======":
            in_oxcaml = False
            in_base = False
            in_incoming = True
            continue

        if in_conflict and line.startswith(">>>>>>> upstream-incoming"):
            in_conflict = False
            in_oxcaml = True
            in_base = True
            in_incoming = True
            continue

        if not in_oxcaml and not in_base and not in_incoming:
            sys.exit("Line {0}: no side is active, "
                     "something went wrong".format(i))

        if in_oxcaml:
            oxcaml.append(line)
        if in_base:
            base.append(line)
        if in_incoming:
            incoming.append(line)

    return oxcaml, base, incoming


# ── Main ──────────────────────────────────────────────────────────────────

def parse_args():
    usage = ("Usage: {0} -commit COMMIT\n"
             "       {0} -base BASE -resolved RESOLVED"
             ).format(sys.argv[0])
    args = sys.argv[1:]
    commit = None
    base = None
    resolved = None
    i = 0
    while i < len(args):
        if args[i] == "-commit" and i + 1 < len(args):
            commit = args[i + 1]
            i += 2
        elif args[i] == "-base" and i + 1 < len(args):
            base = args[i + 1]
            i += 2
        elif args[i] == "-resolved" and i + 1 < len(args):
            resolved = args[i + 1]
            i += 2
        else:
            print(usage, file=sys.stderr)
            sys.exit(1)

    if commit and not base and not resolved:
        return commit + "~1", commit
    elif base and resolved and not commit:
        return base, resolved
    else:
        print(usage, file=sys.stderr)
        sys.exit(1)


def main():
    base_commit, resolved_commit = parse_args()

    files = changed_files(base_commit, resolved_commit)
    tmpdir = tempfile.mkdtemp(prefix="conflict_pd4_")

    for filepath in files:
        pre_lines = git_file(base_commit, filepath)
        post_lines = git_file(resolved_commit, filepath)

        oxcaml, base, incoming = resolve_conflicts(pre_lines)

        safe = filepath.replace("/", "__")
        # patdiff4 diamond: (b1 -> f1) vs (b2 -> f2)
        #   left  = base -> incoming   (what upstream changed)
        #   right = oxcaml -> resolution (how oxcaml was adapted)
        b1 = os.path.join(tmpdir, safe + ".base")
        f1 = os.path.join(tmpdir, safe + ".incoming")
        b2 = os.path.join(tmpdir, safe + ".oxcaml")
        f2 = os.path.join(tmpdir, safe + ".resolution")

        for path, lines in [(b1, base), (f1, incoming),
                             (b2, oxcaml), (f2, post_lines)]:
            with open(path, "w") as f:
                f.write("\n".join(lines) + "\n")

        print("\n" + "=" * 78)
        print("  " + filepath)
        print("=" * 78)
        sys.stdout.flush()

        ret = subprocess.call(
            ["patdiff4", "-double-column", b1, f1, b2, f2])
        if ret == -signal.SIGPIPE:
            sys.exit(0)
        elif ret != 0:
            sys.exit("patdiff4 exited with code {0}".format(ret))


if __name__ == "__main__":
    # Let SIGPIPE kill us quietly (default UNIX behavior), so piping into
    # `less` and closing it doesn't produce a Python traceback.
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    main()
