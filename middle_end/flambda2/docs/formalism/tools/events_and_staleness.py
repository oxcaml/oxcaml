#!/usr/bin/env python3
# Written by Claude.
#
# events_and_staleness.py — evidence-event validation and staleness
# computation for the Flambda 2 formalism regen pipeline. Implements the
# event-line obligations of CORRESPONDENCE.md record 76 ("the solidity
# schema"): EVIDENCE is always recomputed from fence event lines plus git,
# never trusted from disk.
#
# What it checks, per ```rule fence:
#   1. Event-line syntax:
#        CHECKED @ <commit>              code-reading verification
#        VERIFIED <study> @ <commit>     validation case study
#        CAVEAT <kind>: <text>           kind in {known-false, compiler-bug,
#                                        pending-upstream, watch(W-nn),
#                                        disclosure}
#      A commit-less `VERIFIED <study>` (the pre-migration form) is recorded
#      as a failure unless the caller passes legacy_verified_ok=True.
#   2. Commit keys: well-formed (7-40 lowercase hex), resolvable in the
#      repository, and an ancestor of the current base (evidence must have
#      been taken on history the base contains).
#   3. REVERSE-PROVISO: a pending-upstream caveat must be keyed to a branch
#      (backticked in its text) and/or a commit; if any keyed branch tip or
#      commit is reachable from the current base, the proviso is flagged
#      lift-ready — the change landed but the text has not lifted. First
#      live instances: the string-load provisos in chapters 06/10, keyed to
#      `flambda2-string-load-fold` / 9712d270eb.
#   4. Staleness: an evidence event (CHECKED, or VERIFIED with a commit) is
#      stale when any file named by the rule's CODE anchors changed between
#      the evidence commit and the current base (an anchored file that
#      disappeared by the base counts as changed).
#
# All problems are returned as failure records, never raised: the pipeline
# records failures into the index's consistency section and decides the
# exit code itself (see HARD_CATEGORIES for the recommended split).
#
# Python 3 stdlib only. Git is consulted strictly read-only (rev-parse,
# merge-base --is-ancestor, diff --name-only, cat-file -e, for-each-ref).
# Importable from regen_rule_index.py; also runnable stand-alone:
#
#   python3 events_and_staleness.py            # analyze the real tree
#   python3 events_and_staleness.py --selftest # synthetic-fixture self-test

import argparse
import glob
import os
import re
import subprocess
import sys

# --- Locate the tree relative to this script (matches regen_rule_index) ----
TOOLS_DIR = os.path.dirname(os.path.realpath(__file__))
FORMALISM_DIR = os.path.dirname(TOOLS_DIR)
FLAMBDA2_DIR = os.path.dirname(os.path.dirname(FORMALISM_DIR))
REPO_ROOT = os.path.dirname(os.path.dirname(FLAMBDA2_DIR))

# Anchor paths are written relative to middle_end/flambda2/ except a few
# already relative to the repo root (same convention as regen_rule_index's
# ANCHOR_ROOTS, expressed as repo-relative prefixes so we can resolve them
# at a *commit* rather than in the working tree).
DEFAULT_ANCHOR_PREFIXES = ("", "middle_end/flambda2/")

# --- Syntax ------------------------------------------------------------------

COMMIT_RE = re.compile(r"^[0-9a-f]{7,40}$")
EVENT_KEYWORD_RE = re.compile(r"^(CHECKED|VERIFIED|CAVEAT)\b(.*)$")
CHECKED_RE = re.compile(r"^CHECKED\s+@\s+(\S+)\s*$")
VERIFIED_RE = re.compile(r"^VERIFIED\s+(\S+)(?:\s+@\s+(\S+))?\s*$")
CAVEAT_RE = re.compile(r"^CAVEAT\s+([^:]+?)\s*:\s*(.*)$")
WATCH_KIND_RE = re.compile(r"^watch\((W-\d+)\)$")
CAVEAT_KINDS = frozenset(
    ["known-false", "compiler-bug", "pending-upstream", "watch", "disclosure"]
)
# Keys inside a pending-upstream caveat's text: standalone lowercase-hex
# tokens are commit keys; backticked non-hex tokens are branch keys.
HEX_TOKEN_RE = re.compile(r"\b[0-9a-f]{7,40}\b")
BACKTICK_RE = re.compile(r"`([^`]+)`")

# Failure categories. The pipeline records all of them; only the first group
# should normally be hard (non-zero exit). "proviso-lift-ready" is an action
# item, not a defect, but record 76 says it is *flagged*, so it defaults hard.
HARD_CATEGORIES = frozenset(
    [
        "event-syntax",
        "commit-malformed",
        "commit-unknown",
        "commit-not-ancestor",
        "verified-missing-commit",
        "caveat-unknown-kind",
        "caveat-watch-missing-id",
        "caveat-empty-text",
        "proviso-unkeyed",
        "proviso-lift-ready",
        "git-error",
    ]
)
SOFT_CATEGORIES = frozenset(["proviso-key-unresolved", "anchor-unresolved"])
ALL_CATEGORIES = HARD_CATEGORIES | SOFT_CATEGORIES


def _failure(category, rule, chapter, line, detail):
    return dict(
        category=category, rule=rule, chapter=chapter, line=line, detail=detail
    )


# --- Event-line parsing ------------------------------------------------------


def parse_event_line(text, chapter, lineno, rid, legacy_verified_ok=False):
    """Parse one fence line. Returns (event | None, failures).

    event is None when the line is not an event line at all. A recognized
    keyword with bad syntax yields an event with missing fields plus a
    failure, so the pipeline can still show the line in context."""
    failures = []
    stripped = text.rstrip("\n")
    m = EVENT_KEYWORD_RE.match(stripped)
    if not m:
        return None, failures
    keyword = m.group(1)
    event = dict(
        kind=keyword,
        chapter=chapter,
        line=lineno,
        raw=stripped,
        commit=None,
        study=None,
        caveat_kind=None,
        watch_id=None,
        text=None,
        branches=[],
        commits=[],
    )

    def fail(category, detail):
        failures.append(_failure(category, rid, chapter, lineno, detail))

    if keyword == "CHECKED":
        cm = CHECKED_RE.match(stripped)
        if not cm:
            fail("event-syntax", f"expected `CHECKED @ <commit>`: {stripped!r}")
        elif not COMMIT_RE.match(cm.group(1)):
            fail(
                "commit-malformed",
                f"CHECKED commit key {cm.group(1)!r} is not 7-40 lowercase hex",
            )
        else:
            event["commit"] = cm.group(1)
    elif keyword == "VERIFIED":
        vm = VERIFIED_RE.match(stripped)
        if not vm:
            fail(
                "event-syntax",
                f"expected `VERIFIED <study> @ <commit>`: {stripped!r}",
            )
        else:
            event["study"] = vm.group(1)
            commit = vm.group(2)
            if commit is None:
                if not legacy_verified_ok:
                    fail(
                        "verified-missing-commit",
                        f"VERIFIED {vm.group(1)} carries no `@ <commit>` key",
                    )
            elif not COMMIT_RE.match(commit):
                fail(
                    "commit-malformed",
                    f"VERIFIED commit key {commit!r} is not 7-40 lowercase hex",
                )
            else:
                event["commit"] = commit
    else:  # CAVEAT
        cm = CAVEAT_RE.match(stripped)
        if not cm:
            fail("event-syntax", f"expected `CAVEAT <kind>: <text>`: {stripped!r}")
            return event, failures
        kind, body = cm.group(1), cm.group(2).strip()
        wm = WATCH_KIND_RE.match(kind)
        if wm:
            event["caveat_kind"] = "watch"
            event["watch_id"] = wm.group(1)
        elif kind == "watch":
            event["caveat_kind"] = "watch"
            fail(
                "caveat-watch-missing-id",
                "CAVEAT watch must be keyed `watch(W-nn)`",
            )
        elif kind in CAVEAT_KINDS:
            event["caveat_kind"] = kind
        else:
            fail(
                "caveat-unknown-kind",
                f"unknown CAVEAT kind {kind!r} (expected one of "
                f"{sorted(CAVEAT_KINDS)} with watch keyed as watch(W-nn))",
            )
        event["text"] = body
        if not body:
            fail("caveat-empty-text", "CAVEAT has an empty <text>")
        if event["caveat_kind"] == "pending-upstream":
            commits = HEX_TOKEN_RE.findall(body)
            branches = [
                b
                for b in BACKTICK_RE.findall(body)
                if not COMMIT_RE.match(b)
            ]
            event["commits"] = commits
            event["branches"] = branches
            if not commits and not branches:
                fail(
                    "proviso-unkeyed",
                    "pending-upstream CAVEAT names no branch (backticked) "
                    "and no commit key, so the reverse-proviso check cannot "
                    "watch for its lift",
                )
    return event, failures


def extract_rule_events(formalism_dir=FORMALISM_DIR, legacy_verified_ok=False):
    """Scan chapters NN-*.md for ```rule fences; return (rules, failures).

    Each rule dict carries: id, chapter, line, codes (CODE anchors, as
    written) and events (parsed CHECKED/VERIFIED/CAVEAT lines from anywhere
    in the fence — the phantom-keyword prose convention means any line
    beginning with an event keyword is claimed by the schema)."""
    failures = []
    rules = []
    files = sorted(glob.glob(os.path.join(formalism_dir, "[0-9][0-9]-*.md")))
    for path in files:
        chapter = os.path.basename(path)
        with open(path) as fh:
            lines = fh.readlines()
        i, n = 0, len(lines)
        while i < n:
            if re.match(r"^```rule\s*$", lines[i]):
                start = i
                i += 1
                block = []
                while i < n and not re.match(r"^```\s*$", lines[i]):
                    block.append((i + 1, lines[i]))
                    i += 1
                rid = None
                codes, events = [], []
                for lineno, bl in block:
                    m = re.match(r"^RULE\s+(\S+)", bl)
                    if m:
                        rid = m.group(1)
                for lineno, bl in block:
                    m = re.match(r"^CODE\s+(\S+)", bl)
                    if m:
                        codes.append(m.group(1))
                        continue
                    event, fs = parse_event_line(
                        bl, chapter, lineno, rid, legacy_verified_ok
                    )
                    failures.extend(fs)
                    if event is not None:
                        events.append(event)
                rules.append(
                    dict(
                        id=rid,
                        chapter=chapter,
                        line=start + 1,
                        codes=codes,
                        events=events,
                    )
                )
            i += 1
    return rules, failures


# --- Read-only git access ----------------------------------------------------


class Git:
    """Thin read-only wrapper around the git CLI for one repository."""

    def __init__(self, repo_root=REPO_ROOT):
        self.repo_root = repo_root

    def _run(self, *args):
        # stdout/stderr=PIPE + universal_newlines rather than
        # capture_output/text: keeps the module runnable on Python 3.6.
        return subprocess.run(
            ["git", "-C", self.repo_root] + list(args),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
        )

    def rev_parse(self, rev):
        """Full commit sha for rev, or None if it does not resolve."""
        p = self._run("rev-parse", "--verify", "--quiet", rev + "^{commit}")
        return p.stdout.strip() if p.returncode == 0 else None

    def is_ancestor(self, commit, base):
        return (
            self._run("merge-base", "--is-ancestor", commit, base).returncode
            == 0
        )

    def branch_tips(self, branch):
        """Tips of local and remote-tracking branches with this name."""
        tips = []
        local = self.rev_parse(branch)
        if local:
            tips.append(local)
        p = self._run(
            "for-each-ref",
            "--format=%(objectname)",
            f"refs/remotes/*/{branch}",
        )
        if p.returncode == 0:
            tips.extend(t for t in p.stdout.split() if t not in tips)
        return tips

    def path_exists_at(self, rev, path):
        return self._run("cat-file", "-e", f"{rev}:{path}").returncode == 0

    def files_changed(self, since, until, paths):
        """Repo-relative files under paths that differ between the two
        commits, or None on git error."""
        if not paths:
            return []
        p = self._run("diff", "--name-only", since, until, "--", *paths)
        if p.returncode != 0:
            return None
        return [ln for ln in p.stdout.splitlines() if ln.strip()]


# --- Checks: commits, reverse-proviso, staleness -----------------------------


def _resolve_anchor_files(git, rule, base_commit, evidence_commit, prefixes):
    """Resolve the rule's CODE anchor paths to repo-relative files.

    Returns (present_at_base, gone_since_evidence, unresolved): files to
    diff, files that existed at the evidence commit but not at the base
    (already a change), and anchors resolvable at neither commit."""
    present, gone, unresolved = [], [], []
    for anchor in rule["codes"]:
        apath = anchor.split("#", 1)[0]
        candidates = [
            os.path.normpath(os.path.join(pfx, apath)) for pfx in prefixes
        ]
        at_base = next(
            (c for c in candidates if git.path_exists_at(base_commit, c)),
            None,
        )
        if at_base is not None:
            present.append(at_base)
            continue
        at_evidence = next(
            (
                c
                for c in candidates
                if git.path_exists_at(evidence_commit, c)
            ),
            None,
        )
        if at_evidence is not None:
            gone.append(at_evidence)
        else:
            unresolved.append(apath)
    return present, gone, unresolved


def _check_evidence_event(git, base_commit, rule, event, failures, prefixes):
    rid, chapter, lineno = rule["id"], event["chapter"], event["line"]
    full = git.rev_parse(event["commit"])
    if full is None:
        failures.append(
            _failure(
                "commit-unknown",
                rid,
                chapter,
                lineno,
                f"{event['kind']} commit {event['commit']} does not resolve "
                "in this repository",
            )
        )
        return
    event["commit_resolved"] = full
    if not git.is_ancestor(full, base_commit):
        failures.append(
            _failure(
                "commit-not-ancestor",
                rid,
                chapter,
                lineno,
                f"{event['kind']} commit {event['commit']} is not an "
                "ancestor of the current base — evidence taken on unlanded "
                "history",
            )
        )
        return
    event["ancestor"] = True
    present, gone, unresolved = _resolve_anchor_files(
        git, rule, base_commit, full, prefixes
    )
    for apath in unresolved:
        failures.append(
            _failure(
                "anchor-unresolved",
                rid,
                chapter,
                lineno,
                f"CODE anchor path {apath} resolves at neither the base nor "
                f"the evidence commit {event['commit']}; treating the "
                "evidence as stale",
            )
        )
    changed = git.files_changed(full, base_commit, present)
    if changed is None:
        failures.append(
            _failure(
                "git-error",
                rid,
                chapter,
                lineno,
                f"git diff {event['commit']}..base failed for the rule's "
                "CODE anchors",
            )
        )
        changed = []
    stale_files = sorted(set(changed) | set(gone) | set(unresolved))
    event["stale"] = bool(stale_files)
    event["stale_files"] = stale_files


def _check_proviso_event(git, base_commit, rule, event, failures):
    rid, chapter, lineno = rule["id"], event["chapter"], event["line"]
    keyed = []  # (display_key, full_sha)
    for c in event["commits"]:
        full = git.rev_parse(c)
        if full is not None:
            keyed.append((c, full))
    for b in event["branches"]:
        for tip in git.branch_tips(b):
            keyed.append((b, tip))
    if not keyed:
        if event["commits"] or event["branches"]:
            failures.append(
                _failure(
                    "proviso-key-unresolved",
                    rid,
                    chapter,
                    lineno,
                    "pending-upstream keys "
                    f"{event['branches'] + event['commits']} resolve to no "
                    "commit in this repository; cannot run the "
                    "reverse-proviso check",
                )
            )
        # An unkeyed proviso already failed at parse time.
        event["lift_ready"] = False
        return
    landed = [key for key, sha in keyed if git.is_ancestor(sha, base_commit)]
    event["lift_ready"] = bool(landed)
    if landed:
        failures.append(
            _failure(
                "proviso-lift-ready",
                rid,
                chapter,
                lineno,
                f"pending-upstream key(s) {sorted(set(landed))} are "
                "reachable from the current base: the change landed but the "
                "proviso text has not lifted",
            )
        )


def check_rules(
    rules,
    git=None,
    base="HEAD",
    anchor_prefixes=DEFAULT_ANCHOR_PREFIXES,
):
    """Run commit, reverse-proviso, and staleness checks over parsed rules.

    Annotates each event in place (commit_resolved, ancestor, stale,
    stale_files, lift_ready) and each rule with `evidence_stale`:
      None  — the rule has no commit-keyed evidence events;
      False — at least one evidence event is valid and fresh;
      True  — evidence exists but every valid piece of it is stale (or none
              of it survived the commit checks).
    Returns the failure list."""
    git = git or Git()
    failures = []
    base_commit = git.rev_parse(base)
    if base_commit is None:
        failures.append(
            _failure(
                "git-error",
                None,
                None,
                None,
                f"current base {base!r} does not resolve to a commit",
            )
        )
        return failures
    for rule in rules:
        evidence_events = []
        for event in rule["events"]:
            if event["kind"] in ("CHECKED", "VERIFIED"):
                if event["commit"] is not None:
                    evidence_events.append(event)
                    _check_evidence_event(
                        git, base_commit, rule, event, failures, anchor_prefixes
                    )
            elif (
                event["kind"] == "CAVEAT"
                and event["caveat_kind"] == "pending-upstream"
            ):
                _check_proviso_event(git, base_commit, rule, event, failures)
        if not evidence_events:
            rule["evidence_stale"] = None
        else:
            rule["evidence_stale"] = not any(
                ev.get("stale") is False for ev in evidence_events
            )
    return failures


# --- Top-level entry points --------------------------------------------------


def analyze(
    formalism_dir=FORMALISM_DIR,
    repo_root=REPO_ROOT,
    base="HEAD",
    legacy_verified_ok=False,
    anchor_prefixes=DEFAULT_ANCHOR_PREFIXES,
):
    """Scan chapters, validate events, run git-backed checks.

    Returns (rules, failures): rules as from extract_rule_events with
    events/staleness annotations, failures from both phases combined."""
    rules, failures = extract_rule_events(formalism_dir, legacy_verified_ok)
    failures.extend(
        check_rules(rules, Git(repo_root), base, anchor_prefixes)
    )
    return rules, failures


def staleness_by_rule(rules):
    """id -> evidence_stale (True/False/None), for joining into the index."""
    return {r["id"]: r.get("evidence_stale") for r in rules if r["id"]}


def hard_failures(failures):
    return [f for f in failures if f["category"] in HARD_CATEGORIES]


def render_failures(failures):
    """Markdown bullet lines for the index's consistency-check section,
    grouped by category, in the style of regen_rule_index's sections."""
    lines = []
    by_cat = {}
    for f in failures:
        by_cat.setdefault(f["category"], []).append(f)
    for cat in sorted(by_cat):
        hardness = "hard" if cat in HARD_CATEGORIES else "soft"
        lines.append(f"- **{cat}** ({hardness}): {len(by_cat[cat])} found")
        for f in by_cat[cat]:
            where = f"{f['chapter']}:{f['line']}" if f["chapter"] else "(repo)"
            rid = f"`{f['rule']}`" if f["rule"] else "(no rule id)"
            lines.append(f"  - {rid} ({where}): {f['detail']}")
    if not lines:
        lines.append(
            "- Event and staleness checks: all evidence events well-formed, "
            "commit keys resolve as ancestors of the base, no lift-ready "
            "provisos, no stale evidence."
        )
    return lines


# --- Self-test over synthetic fixtures ---------------------------------------


def _selftest():  # noqa: C901
    import shutil
    import tempfile

    tmp = tempfile.mkdtemp(prefix="evs-selftest-")
    checks = []

    def check(name, cond):
        checks.append((name, cond))
        print(("PASS" if cond else "FAIL") + f"  {name}")

    def git(*args):
        p = subprocess.run(
            ["git", "-C", tmp, "-c", "user.name=t", "-c", "user.email=t@t"]
            + list(args),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
        )
        assert p.returncode == 0, (args, p.stderr)
        return p.stdout.strip()

    def write(rel, text):
        path = os.path.join(tmp, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as fh:
            fh.write(text)

    try:
        # Repo: c1 adds the anchored file; c2 touches something unrelated;
        # side branch `flambda2-string-load-fold` (unmerged) adds the fold;
        # branch `landed-branch` is merged into main; c3 edits the anchored
        # file (making c1/c2 evidence stale).
        git("init", "-b", "main")
        anchored = "middle_end/flambda2/simplify/simplify_binary_primitive.ml"
        write(anchored, "let simplify_string_or_bigstring_load = stub\n")
        git("add", "-A")
        git("commit", "-m", "c1")
        c1 = git("rev-parse", "HEAD")
        write("unrelated.txt", "hi\n")
        git("add", "-A")
        git("commit", "-m", "c2")
        c2 = git("rev-parse", "HEAD")
        git("checkout", "-b", "flambda2-string-load-fold", c1)
        write(anchored, "let simplify_string_or_bigstring_load = fold\n")
        git("add", "-A")
        git("commit", "-m", "string-load fold")
        pending_tip = git("rev-parse", "HEAD")
        git("checkout", "-b", "landed-branch", c2)
        write("landed.txt", "landed\n")
        git("add", "-A")
        git("commit", "-m", "landed change")
        git("checkout", "main")
        git("merge", "--no-ff", "-m", "merge landed-branch", "landed-branch")
        write(anchored, "let simplify_string_or_bigstring_load = tweaked\n")
        git("add", "-A")
        git("commit", "-m", "c3 touches anchor")
        c3 = git("rev-parse", "HEAD")

        formalism = os.path.join(tmp, "docs")
        write(
            os.path.join(formalism, "06-fixture.md"),
            f"""# Fixture chapter

```rule
RULE T.Fixture.FreshEvidence
CLAIM normative
CODE simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
CHECKED @ {c3[:10]}
VERIFIED 14-validation/fixture_study.md @ {c3[:10]}
---
Fresh: evidence at the tip; anchor unchanged since.
```

```rule
RULE T.Fixture.StaleEvidence
CLAIM normative
CODE simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
CHECKED @ {c1[:10]}
---
Stale: c3 edited the anchored file after c1.
```

```rule
RULE T.Fixture.PendingNotLanded
CLAIM descriptive
CODE simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
CHECKED @ {c3[:10]}
CAVEAT pending-upstream: fold lives on the `flambda2-string-load-fold` branch, commit {pending_tip[:10]}, not yet in mainline
---
Proviso whose branch has NOT landed: no lift-ready flag.
```

```rule
RULE T.Fixture.PendingLanded
CLAIM descriptive
CODE simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
CHECKED @ {c3[:10]}
CAVEAT pending-upstream: change lives on the `landed-branch` branch, not yet lifted
---
Proviso whose branch HAS landed: lift-ready.
```

```rule
RULE T.Fixture.Failures
CLAIM normative
CODE simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
CODE simplify/missing_module.ml#gone
CHECKED @ {c1[:10]}
CHECKED @ ZZZNOTHEX
CHECKED {c3[:10]}
CHECKED @ deadbeefcafe
CHECKED @ {pending_tip[:10]}
VERIFIED 14-validation/legacy_study.md
CAVEAT frobnicate: not a kind
CAVEAT watch: missing its W-nn key
CAVEAT watch(W-33): cyclic-iota tripwire
CAVEAT pending-upstream: no key named here at all
CAVEAT disclosure:
---
One rule exercising every failure category.
```
""",
        )

        rules, failures = analyze(
            formalism_dir=formalism, repo_root=tmp, base="HEAD"
        )
        by_id = {r["id"]: r for r in rules}
        cats = {}
        for f in failures:
            cats.setdefault(f["category"], []).append(f)

        check("five rules parsed", len(rules) == 5)
        fresh = by_id["T.Fixture.FreshEvidence"]
        check("fresh rule not stale", fresh["evidence_stale"] is False)
        check(
            "fresh VERIFIED carries study + commit",
            any(
                e["kind"] == "VERIFIED"
                and e["study"] == "14-validation/fixture_study.md"
                and e.get("stale") is False
                for e in fresh["events"]
            ),
        )
        stale = by_id["T.Fixture.StaleEvidence"]
        check("stale rule flagged stale", stale["evidence_stale"] is True)
        check(
            "stale event names the anchored file",
            any(
                anchored in e.get("stale_files", [])
                for e in stale["events"]
            ),
        )
        not_landed = by_id["T.Fixture.PendingNotLanded"]
        check(
            "unlanded proviso is not lift-ready",
            all(
                e.get("lift_ready") is False
                for e in not_landed["events"]
                if e["kind"] == "CAVEAT"
            ),
        )
        landed = by_id["T.Fixture.PendingLanded"]
        check(
            "landed proviso is lift-ready",
            any(
                e.get("lift_ready") is True
                for e in landed["events"]
                if e["kind"] == "CAVEAT"
            ),
        )
        check(
            "lift-ready recorded as failure naming the branch",
            any(
                "landed-branch" in f["detail"]
                for f in cats.get("proviso-lift-ready", [])
            ),
        )
        check(
            "lift-ready is exactly one",
            len(cats.get("proviso-lift-ready", [])) == 1,
        )
        expected = {
            "commit-malformed": "CHECKED @ ZZZNOTHEX",
            "event-syntax": "CHECKED without @",
            "commit-unknown": "deadbeefcafe",
            "commit-not-ancestor": "unmerged side-branch commit",
            "verified-missing-commit": "legacy VERIFIED",
            "caveat-unknown-kind": "frobnicate",
            "caveat-watch-missing-id": "plain watch",
            "proviso-unkeyed": "keyless pending-upstream",
            "caveat-empty-text": "empty disclosure",
            "anchor-unresolved": "missing_module.ml",
        }
        for cat, why in expected.items():
            check(f"failure {cat} recorded ({why})", bool(cats.get(cat)))
        check(
            "well-keyed watch(W-33) accepted",
            any(
                e.get("watch_id") == "W-33"
                for e in by_id["T.Fixture.Failures"]["events"]
            ),
        )
        check(
            "all failure categories are known",
            all(c in ALL_CATEGORIES for c in cats),
        )
        check(
            "hard/soft split usable",
            len(hard_failures(failures)) < len(failures),
        )
        # Determinism (record 76's double-run obligation).
        rules2, failures2 = analyze(
            formalism_dir=formalism, repo_root=tmp, base="HEAD"
        )
        check("double-run determinism", failures == failures2)
        rendered = render_failures(failures)
        check(
            "render mentions lift-ready",
            any("proviso-lift-ready" in ln for ln in rendered),
        )
        # Staleness relative to an older base: at base c2, c1 evidence is
        # fresh (anchor unchanged between c1 and c2).
        rules3, _ = analyze(formalism_dir=formalism, repo_root=tmp, base=c2)
        by_id3 = {r["id"]: r for r in rules3}
        check(
            "stale is base-relative (fresh at base c2)",
            by_id3["T.Fixture.StaleEvidence"]["evidence_stale"] is False,
        )
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    failed = [name for name, ok in checks if not ok]
    print()
    if failed:
        print(f"SELF-TEST FAILED: {len(failed)} of {len(checks)} checks:")
        for name in failed:
            print(f"  {name}")
        sys.exit(1)
    print(f"Self-test passed: {len(checks)} checks.")


# --- Main --------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="Validate formalism evidence events and compute staleness."
    )
    parser.add_argument(
        "--selftest",
        action="store_true",
        help="run the synthetic-fixture self-test instead of scanning the tree",
    )
    parser.add_argument(
        "--base",
        default="HEAD",
        help="current base commit-ish for ancestry/staleness (default HEAD)",
    )
    parser.add_argument(
        "--legacy-verified-ok",
        action="store_true",
        help="do not fail commit-less `VERIFIED <study>` lines (mid-migration)",
    )
    args = parser.parse_args()
    if args.selftest:
        _selftest()
        return
    rules, failures = analyze(
        base=args.base, legacy_verified_ok=args.legacy_verified_ok
    )
    n_events = sum(len(r["events"]) for r in rules)
    n_stale = sum(1 for r in rules if r.get("evidence_stale") is True)
    print(f"Scanned {len(rules)} rules; {n_events} event lines.")
    print(f"Rules with only stale evidence: {n_stale}")
    print("\n".join(render_failures(failures)))
    if hard_failures(failures):
        sys.exit(1)


if __name__ == "__main__":
    main()

