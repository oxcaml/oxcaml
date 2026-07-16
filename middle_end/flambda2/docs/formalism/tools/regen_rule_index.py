#!/usr/bin/env python3
# Written by Claude.
#
# Regenerate the Flambda 2 formalism rule index and refresh the rule/case-study
# counts in the formalism README.
#
# What it does:
#   1. Scans chapters NN-*.md (02-13) for fenced ```rule blocks and parses their
#      RULE / STATUS / CODE / VERIFIED header lines.
#   2. Runs consistency checks:
#        - rule IDs are globally unique;
#        - every rule has >= 1 CODE anchor;
#        - every CODE anchor's path exists under the repo root or under
#          middle_end/flambda2/;
#        - the last '.'-separated component of each anchor's #name occurs as a
#          whole word (grep -w) in that file.
#   3. Rewrites rule-index.md: header, one table per namespace prefix, and a
#      "Consistency check results" section (which records any failures found).
#   4. Surgically updates the total/by-status rule counts and the case-study
#      count in README.md's "Status & validation" section.
#
# Hard failures (duplicate IDs, rules with no anchor, unresolvable anchors) are
# recorded in the index's consistency section AND cause a non-zero exit.
#
# Python 3 stdlib only. Runnable from any cwd: paths are resolved relative to
# this script's own location.
#
# Usage (canonical, via the Makefile target — also generates the duneconf/
# workspace files the alias needs):
#   make regen-flambda2-formalism-rule-index
#
# or via the Dune alias directly (regenerates, checks, and promotes the
# results into the tree in one step):
#   dune build --root=. --workspace=duneconf/main.ws \
#     @middle_end/flambda2/docs/formalism/regen-rule-index --auto-promote
#
# Direct invocation (updates rule-index.md and README.md in place):
#   python3 middle_end/flambda2/docs/formalism/tools/regen_rule_index.py
#
# Under Dune, the rule in ../dune passes --out-index/--out-readme so the
# generated files are separate build targets, which Dune then diffs against
# and promotes over the checked-in ones.

import argparse
import glob
import os
import re
import subprocess
import sys
from collections import Counter, OrderedDict

# --- Locate the tree relative to this script -------------------------------
# This script lives at <repo>/middle_end/flambda2/docs/formalism/tools/.
TOOLS_DIR = os.path.dirname(os.path.realpath(__file__))
FORMALISM_DIR = os.path.dirname(TOOLS_DIR)
FLAMBDA2_DIR = os.path.dirname(
    os.path.dirname(FORMALISM_DIR)
)  # .../middle_end/flambda2
REPO_ROOT = os.path.dirname(os.path.dirname(FLAMBDA2_DIR))  # repo root
FLAMBDA2_REL = os.path.relpath(FLAMBDA2_DIR, REPO_ROOT)  # middle_end/flambda2

INDEX_PATH = os.path.join(FORMALISM_DIR, "rule-index.md")
README_PATH = os.path.join(FORMALISM_DIR, "README.md")
VALIDATION_DIR = os.path.join(FORMALISM_DIR, "14-validation")

# Anchor paths in the chapters are written relative to middle_end/flambda2/,
# except a few already written relative to the repo root. Try both.
ANCHOR_ROOTS = [REPO_ROOT, FLAMBDA2_DIR]

# --- Rule-block extraction --------------------------------------------------


def extract_rules():
    """Return a list of rule dicts scanned from the chapter files, in file then
    source order."""
    files = sorted(glob.glob(os.path.join(FORMALISM_DIR, "[0-9][0-9]-*.md")))
    rules = []
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
                    block.append(lines[i])
                    i += 1
                rid = status = None
                codes, verified = [], []
                for bl in block:
                    m = re.match(r"^RULE\s+(\S+)", bl)
                    if m:
                        rid = m.group(1)
                        continue
                    m = re.match(r"^STATUS\s+(\S+)", bl)
                    if m:
                        status = m.group(1)
                        continue
                    m = re.match(r"^CODE\s+(\S+)", bl)
                    if m:
                        codes.append(m.group(1))
                        continue
                    m = re.match(r"^VERIFIED\s+(\S+)", bl)
                    if m:
                        verified.append(m.group(1))
                        continue
                    if re.match(r"^---", bl):
                        break
                rules.append(
                    dict(
                        id=rid,
                        status=status,
                        chapter=chapter,
                        codes=codes,
                        verified=verified,
                        line=start + 1,
                    )
                )
            i += 1
    return rules


# --- Consistency checks -----------------------------------------------------


def resolve_anchor_path(path):
    for root in ANCHOR_ROOTS:
        full = os.path.join(root, path)
        if os.path.exists(full):
            return full
    return None


def run_checks(rules):
    """Return (duplicates, no_anchor, missing_file, missing_name)."""
    ids = [r["id"] for r in rules]
    duplicates = sorted(i for i, c in Counter(ids).items() if c > 1)
    no_anchor = [r["id"] for r in rules if not r["codes"]]
    missing_file, missing_name = [], []
    for r in rules:
        for anchor in r["codes"]:
            if "#" in anchor:
                apath, name = anchor.split("#", 1)
            else:
                apath, name = anchor, None
            full = resolve_anchor_path(apath)
            if full is None:
                missing_file.append((r["id"], r["chapter"], anchor))
                continue
            if name:
                token = name.split(".")[-1]
                if subprocess.run(["grep", "-wq", token, full]).returncode != 0:
                    missing_name.append((r["id"], r["chapter"], anchor, token))
    return duplicates, no_anchor, missing_file, missing_name


def count_case_studies():
    """Number of case-study files in 14-validation/ (all *.md except README)."""
    files = glob.glob(os.path.join(VALIDATION_DIR, "*.md"))
    return sum(1 for f in files if os.path.basename(f).lower() != "readme.md")


# --- Index generation -------------------------------------------------------

NS_ORDER = ["WF", "OS", "P", "T", "S", "CM", "TC", "R", "INV", "OTHER"]
NS_TITLES = {
    "WF": "WF — Kinding and well-formedness (ch. 03)",
    "OS": "OS — Operational semantics (ch. 04)",
    "P": "P — Primitive denotations (ch. 05-06)",
    "T": "T — Abstract domain, meet/join, provers, reification (ch. 07-08)",
    "S": "S — Simplify structure, rewrites, inlining, unboxing (ch. 09-12)",
    "CM": "CM — Core Cmm operational semantics (ch. 15, 19)",
    "TC": "TC — to_cmm translation (ch. 16, 18)",
    "R": "R — Representation relation (ch. 17)",
    "INV": "INV — Global invariants and to_cmm soundness (ch. 13, 20)",
    "OTHER": "Other / unclassified",
}


def namespace(rid):
    p = rid.split(".")[0]
    return p if p in NS_TITLES else "OTHER"


def render_index(rules, duplicates, no_anchor, missing_file, missing_name):
    buckets = OrderedDict((k, []) for k in NS_ORDER)
    for r in rules:
        buckets[namespace(r["id"])].append(r)

    def fmt_codes(codes):
        return "<br>".join(f"`{c}`" for c in codes)

    def fmt_verified(v):
        return "<br>".join(v) if v else "—"

    lines = []
    A = lines.append
    A("# Rule index")
    A("")
    A("Machine-greppable index of every formal rule in the Flambda 2 formalism")
    A("(chapters 02-20; chapter 01 has none). Each")
    A("row corresponds to one ```` ```rule ```` block in a chapter. See")
    A('[`README.md`](README.md) ("Rule blocks" and "Rule ID namespaces") for the')
    A("block format and the meaning of each namespace prefix.")
    A("")
    A("## Columns")
    A("")
    A("- **Rule ID** — the rule's stable ID (`RULE` header). Globally unique; never")
    A("  renumbered or reused.")
    A("- **Status** — `normative` (code must satisfy it), `descriptive` (documents")
    A("  the current algorithm/heuristic; may change), or `conjectured` (believed but")
    A("  not yet verified against the code).")
    A("- **Chapter** — the chapter file containing the rule block.")
    A("- **Code anchors** — every `CODE` anchor (`path#name`), as written in the")
    A("  block. Paths are relative to `middle_end/flambda2/` unless they already begin")
    A("  with `middle_end/`. A leading component may need `middle_end/flambda2/`")
    A("  prepended to resolve from the repository root.")
    A("- **Verified** — validation case study/studies (`VERIFIED` header), or `—` if")
    A("  none yet.")
    A("")
    A("## How agents should use this index")
    A("")
    A("- To answer a question or synthesize a test, grep here for candidate rules,")
    A("  then read the owning chapter section. Trust `normative` rules; treat")
    A("  `descriptive` as current-behaviour documentation and `conjectured` as leads")
    A("  to verify against the code.")
    A("- When code under `middle_end/flambda2/` changes, grep the **Code anchors**")
    A("  column for the changed file, then follow the sync protocol in `README.md`.")
    A("")
    A("## Regenerating this index")
    A("")
    A("This file is generated. Do not edit it by hand — from the repository root,")
    A("run:")
    A("")
    A("```")
    A("make regen-flambda2-formalism-rule-index")
    A("```")
    A("")
    A("or build the Dune alias directly:")
    A("")
    A("```")
    A("dune build --root=. --workspace=duneconf/main.ws \\")
    A(f"  @{FLAMBDA2_REL}/docs/formalism/regen-rule-index --auto-promote")
    A("```")
    A("")
    A("The underlying script scans each `NN-*.md` for fenced")
    A("`rule` blocks, reads the `RULE` / `STATUS` / `CODE` / `VERIFIED` header")
    A("lines, buckets by namespace prefix (the text before the first `.` in the")
    A("ID), runs the consistency checks below, rewrites this file, and refreshes")
    A("the rule/case-study counts in [`README.md`](README.md). It exits non-zero")
    A("if any check fails (duplicate IDs, rules with no anchor, or unresolvable")
    A("anchors), recording the failures in the section below either way. The Dune")
    A("alias does not run as part of the default build; the `--auto-promote` flag")
    A("copies the regenerated files back into the source tree.")
    A("")

    for k in NS_ORDER:
        rs = buckets[k]
        if not rs:
            continue
        A(f"## {NS_TITLES[k]}")
        A("")
        A(f"{len(rs)} rules.")
        A("")
        A("| Rule ID | Status | Chapter | Code anchors | Verified |")
        A("|---|---|---|---|---|")
        for r in sorted(rs, key=lambda r: (r["chapter"], r["line"])):
            A(
                f"| `{r['id']}` | {r['status']} | {r['chapter']} | "
                f"{fmt_codes(r['codes'])} | {fmt_verified(r['verified'])} |"
            )
        A("")

    A("## Consistency check results")
    A("")
    A("_Generated by scanning chapters 02-20._")
    A("")
    total = len(rules)
    by_status = Counter(r["status"] for r in rules)
    by_chap = Counter(r["chapter"] for r in rules)
    A(f"- **Total rules:** {total}")
    A(
        f"- **By status:** normative {by_status['normative']}, "
        f"descriptive {by_status['descriptive']}, conjectured {by_status['conjectured']}"
    )
    A("- **By chapter:**")
    for c in sorted(by_chap):
        A(f"  - {c}: {by_chap[c]}")
    if duplicates:
        A(f"- **Duplicate rule IDs:** {len(duplicates)} found (see below).")
    else:
        A("- **Duplicate rule IDs:** none.")
    if no_anchor:
        A(f"- **Rules with no code anchor:** {len(no_anchor)} found (see below).")
    else:
        A("- **Rules with no code anchor:** none (all rules have ≥ 1 `CODE`).")
    A("")
    A("### Anchor-resolution check")
    A("")
    A("An anchor path is considered resolvable if it exists relative to the")
    A("repository root or relative to `middle_end/flambda2/`. Name resolution checks")
    A("that the last `.`-separated component of `#name` occurs as a whole-word token")
    A("(`grep -w`) in the file.")
    A("")
    if not missing_file and not missing_name:
        A("All anchors resolve: every `CODE` path exists (under one of those two")
        A("roots) and every anchored name occurs as a whole-word token in its file.")
        A("")
    else:
        A("The following anchors did not resolve and should be corrected in their")
        A("chapters during verification (this index does not modify chapters):")
        A("")
        A("Path not found (file does not exist under either root):")
        A("")
        for rid, chap, anchor in missing_file:
            A(f"- `{rid}` ({chap}): `{anchor}`")
        if not missing_file:
            A("- none")
        A("")
        A(
            "Name not found (file exists, but the anchored name is not a whole-word token):"
        )
        A("")
        for rid, chap, anchor, token in missing_name:
            A(f"- `{rid}` ({chap}): `{anchor}` — token `{token}` absent")
        if not missing_name:
            A("- none")
        A("")
    A("### Duplicate-ID check")
    A("")
    if duplicates:
        A("Duplicate rule IDs found (each ID must be globally unique):")
        A("")
        for rid in duplicates:
            locs = [f"{r['chapter']}:{r['line']}" for r in rules if r["id"] == rid]
            A(f"- `{rid}`: {', '.join(locs)}")
    else:
        A(
            "No duplicate rule IDs across chapters 02-13. In particular the `P.Effects.*`"
        )
        A("family is cleanly partitioned: chapter 05 defines `P.Effects.PureScalars`,")
        A(
            "`P.Effects.FloatRoundingMode`, `P.Effects.BoxNumber`; chapter 06 defines the"
        )
        A("remaining `P.Effects.*` rules. No collision between the two chapters.")

    if no_anchor:
        A("")
        A("### Missing-anchor check")
        A("")
        A("Rules with no `CODE` anchor (every rule must have at least one):")
        A("")
        for rid in no_anchor:
            A(f"- `{rid}`")

    return "\n".join(lines) + "\n"


# --- README count update ----------------------------------------------------


def update_readme(rules, case_studies, out_path):
    """Surgically replace the total/by-status rule counts and the case-study
    count in README.md, writing the result to out_path. Idempotent: reruns
    produce no further change."""
    by_status = Counter(r["status"] for r in rules)
    total = len(rules)
    with open(README_PATH) as fh:
        text = fh.read()

    subs = [
        (r"\*\*\d+ rules\*\* across chapters", f"**{total} rules** across chapters"),
        (r"\*\*\d+ normative\*\*", f"**{by_status['normative']} normative**"),
        (r"\*\*\d+ descriptive\*\*", f"**{by_status['descriptive']} descriptive**"),
        (r"\*\*\d+ conjectured\*\*", f"**{by_status['conjectured']} conjectured**"),
        (r"\*\*\d+ case studies\*\*", f"**{case_studies} case studies**"),
    ]
    unmatched = []
    for pat, repl in subs:
        text, n = re.subn(pat, repl, text)
        if n == 0:
            unmatched.append(pat)
    with open(out_path, "w") as fh:
        fh.write(text)
    return unmatched


# --- Main -------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="Regenerate the formalism rule index and README counts."
    )
    parser.add_argument(
        "--out-index",
        default=None,
        help="write the regenerated index here (relative to the current "
        "directory) instead of updating rule-index.md in place",
    )
    parser.add_argument(
        "--out-readme",
        default=None,
        help="write the count-updated README here instead of updating "
        "README.md in place",
    )
    args = parser.parse_args()
    out_index = os.path.abspath(args.out_index) if args.out_index else INDEX_PATH
    out_readme = os.path.abspath(args.out_readme) if args.out_readme else README_PATH

    rules = extract_rules()
    duplicates, no_anchor, missing_file, missing_name = run_checks(rules)
    case_studies = count_case_studies()

    index_text = render_index(rules, duplicates, no_anchor, missing_file, missing_name)
    with open(out_index, "w") as fh:
        fh.write(index_text)

    unmatched = update_readme(rules, case_studies, out_readme)

    by_status = Counter(r["status"] for r in rules)
    print(f"Scanned {len(rules)} rules across chapters 02-13.")
    print(
        f"  normative {by_status['normative']}, "
        f"descriptive {by_status['descriptive']}, "
        f"conjectured {by_status['conjectured']}"
    )
    print(f"  case studies: {case_studies}")
    print(f"Wrote {os.path.relpath(out_index)}")
    print(f"Updated {os.path.relpath(out_readme)}")

    hard_fail = False
    if duplicates:
        print(f"FAIL: {len(duplicates)} duplicate rule ID(s): {duplicates}")
        hard_fail = True
    if no_anchor:
        print(f"FAIL: {len(no_anchor)} rule(s) with no CODE anchor: {no_anchor}")
        hard_fail = True
    if missing_file:
        print(f"FAIL: {len(missing_file)} anchor path(s) not found:")
        for rid, chap, anchor in missing_file:
            print(f"  {rid} ({chap}): {anchor}")
        hard_fail = True
    if missing_name:
        print(f"FAIL: {len(missing_name)} anchor name(s) not found:")
        for rid, chap, anchor, token in missing_name:
            print(f"  {rid} ({chap}): {anchor} (token {token})")
        hard_fail = True
    if unmatched:
        print("FAIL: README.md count patterns did not match (format changed?):")
        for pat in unmatched:
            print(f"  {pat}")
        hard_fail = True

    if hard_fail:
        sys.exit(1)
    print("All consistency checks passed.")


if __name__ == "__main__":
    main()
