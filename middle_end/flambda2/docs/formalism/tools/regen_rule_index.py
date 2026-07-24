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
ROCQ_DIR = os.path.join(FORMALISM_DIR, "rocq")

# Solidity-schema tooling (record 76): fence extraction, .v artifact
# classification, and event/staleness checks live in sibling modules.
sys.path.insert(0, TOOLS_DIR)
import artifact_classifier  # noqa: E402
import events_and_staleness as evs  # noqa: E402
import fence_parser  # noqa: E402
import solidity_join  # noqa: E402

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


def render_index(rules, duplicates, no_anchor, missing_file, missing_name,
                 art_by_id=None, stale_by_id=None, grade_by_id=None,
                 scoreboard=None, extra_sections=None):
    art_by_id = art_by_id or {}
    stale_by_id = stale_by_id or {}
    grade_by_id = grade_by_id or {}
    extra_sections = extra_sections or []
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
    A("**_Agents: weigh a rule by its CLAIM kind × derived grade × CAVEAT"
      " lines, never by kind alone — see [§ How agents should use this"
      " index](#how-agents-should-use-this-index)._**")
    A("")
    if scoreboard is not None and grade_by_id:
        by_grade = Counter(row["grade"] for row in grade_by_id.values())
        by_status = Counter(r["status"] for r in rules)
        by_chap = Counter(r["chapter"] for r in rules)
        cc = scoreboard["caveat_counts"]
        A("## Solidity scoreboard")
        A("")
        A("_Every number below is derived by the generator at each run — never")
        A("hand-maintained (record 76)._")
        A("")
        A(f"**{len(rules)} rules** across chapters 02-20, validated against")
        A(f"**{scoreboard['case_studies']} case studies** in `14-validation/`.")
        A("")
        A("### Grades")
        A("")
        A("```")
        maxc = max(by_grade.values()) if by_grade else 1
        for g in ("A", "B", "C", "D", "DISPUTED"):
            c = by_grade.get(g, 0)
            bar = "█" * ((c * 40) // maxc)
            if c and not bar:
                bar = "▏"
            A(f"{g:<9}{c:>4}  {bar}".rstrip())
        A("```")
        A("")
        A("- **By claim:** normative "
          f"{by_status['normative']}, descriptive {by_status['descriptive']}, "
          f"interpretive {by_status['interpretive']}")
        disputed = sorted(
            (row for row in grade_by_id.values() if row["grade"] == "DISPUTED"),
            key=lambda r: r["id"],
        )
        if disputed:
            A(f"- **DISPUTED ({len(disputed)}):** each suspends its derived")
            A("  letter until the named FIDELITY.md finding(s) close:")
            for row in disputed:
                A(
                    f"  - `{row['id']}` — suspends {row['suspended_letter']}; "
                    f"open: {', '.join(row['disputed_by'])}"
                )
        else:
            A("- **DISPUTED:** none. DISPUTED is the live under-investigation")
            A("  state — an open FIDELITY.md finding naming a rule suspends its")
            A("  letter; zero means no rule is currently under an open finding.")
        if by_grade.get(None):
            A(f"- **(join failed):** {by_grade[None]} rule(s) carry no grade —")
            A("  see the Solidity join and grade check below (hard failure).")
        A("")
        A("### Trust signals")
        A("")
        A("- **Caveats by kind** (urgency order): "
          f"known-false {cc['known-false']}, "
          f"compiler-bug {cc['compiler-bug']}, "
          f"pending-upstream {cc['pending-upstream']}, "
          f"watch {cc['watch']}, disclosure {cc['disclosure']}")
        A(f"- **Validation events:** {scoreboard['verified_lines']} `VERIFIED` "
          f"lines ({scoreboard['verified_keyed']} commit-keyed) across "
          f"{scoreboard['verified_rules']} rules; "
          f"{scoreboard['checked_lines']} `CHECKED`.")
        A("- **Watched provisos** (pending-upstream; the reverse-proviso check")
        A("  watches these keys for their lift):")
        for rid, keys in scoreboard["provisos"]:
            A(f"  - `{rid}` — {keys}")
        A("- **HYBRID rules lacking clause inventories (warned, by design):** "
          f"{scoreboard['hybrid_warnings']}")
        A("")
        A("### By chapter")
        A("")
        cbc = scoreboard["caveats_by_chapter"]
        grades_by_chap = {}
        for r in rules:
            row = grade_by_id.get(r["id"])
            if row is not None:
                grades_by_chap.setdefault(r["chapter"], Counter())[
                    row["grade"]
                ] += 1
        for c in sorted(by_chap):
            gc = grades_by_chap.get(c, Counter())
            parts = [f"{by_chap[c]} rules"]
            parts += [
                f"{g} {gc[g]}"
                for g in ("A", "B", "C", "D", "DISPUTED")
                if gc[g]
            ]
            if gc.get(None):
                parts.append(f"(join failed) {gc[None]}")
            n = cbc.get(c, 0)
            parts.append(f"{n} caveat{'s' if n != 1 else ''}")
            A(f"- {c} — {' · '.join(parts)}")
        A("")
    A("## How agents should use this index")
    A("")
    A("- To answer a question or synthesize a test, grep here for candidate rules,")
    A("  then read the owning chapter section. Weigh a rule by its CLAIM kind plus")
    A("  its caveats and derived evidence; `descriptive` rules are")
    A("  current-behaviour documentation and may drift with the code.")
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
    A("`rule` blocks, reads the `RULE` / `CLAIM` / `CODE` / `VERIFIED` / `CAVEAT` header")
    A("lines, buckets by namespace prefix (the text before the first `.` in the")
    A("ID), runs the consistency checks below, rewrites this file, and refreshes")
    A("the rule/case-study counts in [`README.md`](README.md). It exits non-zero")
    A("if any check fails (duplicate IDs, rules with no anchor, or unresolvable")
    A("anchors), recording the failures in the section below either way. The Dune")
    A("alias does not run as part of the default build; the `--auto-promote` flag")
    A("copies the regenerated files back into the source tree.")
    A("")
    A("## Columns")
    A("")
    A("- **Rule ID** — the rule's stable ID (`RULE` header). Globally unique; never")
    A("  renumbered or reused.")
    A("- **Claim** — `normative` (the subject is something the code computes or")
    A("  the runtime does), `descriptive` (documents the current")
    A("  algorithm/heuristic; may change), or `interpretive` (formalism-side")
    A("  apparatus; falsity is remediable only by a doc/model edit). Evidence and")
    A("  grades are DERIVED by the tooling, never declared (record 76).")
    A("- **Grade** — the derived solidity grade (record 76), monotone on the two")
    A("  evidence axes: A = validated × mechanized (no demoting flag), B =")
    A("  validated alone or code-read × mechanized, C = one axis, D = neither;")
    A("  DISPUTED (with the open FIDELITY.md finding ids) suspends the letter.")
    A("  Flags in [brackets]: false-as-stated and pending-upstream demote A;")
    A("  compiler-bug, hybrid, and stale display without demoting. A")
    A("  (proved-in-model) badge marks a Qed — never a grade input. Derived by")
    A("  `tools/solidity_join.py` from the fence events, the .v artifact kind,")
    A("  and FIDELITY.md's stamps table and `RULES:` headers.")
    A("- **Artifact** — the rule's Rocq artifact kind in rocq/theories/ (defining")
    A("  clause, theorem-admitted/qed, documented-anchor, demoted-claim), with")
    A("  (hybrid) and [stale] markers where applicable.")
    A("- **Chapter** — the chapter file containing the rule block.")
    A("- **Code anchors** — every `CODE` anchor (`path#name`), as written in the")
    A("  block. Paths are relative to `middle_end/flambda2/` unless they already begin")
    A("  with `middle_end/`. A leading component may need `middle_end/flambda2/`")
    A("  prepended to resolve from the repository root.")
    A("- **Verified** — validation case study/studies (`VERIFIED` header), or `—` if")
    A("  none yet.")
    A("")

    for k in NS_ORDER:
        rs = buckets[k]
        if not rs:
            continue
        A(f"## {NS_TITLES[k]}")
        A("")
        A(f"{len(rs)} rules.")
        A("")
        A("| Rule ID | Claim | Grade | Chapter | Artifact | Code anchors | Verified |")
        A("|---|---|---|---|---|---|---|")
        for r in sorted(rs, key=lambda r: (r["chapter"], r["line"])):
            a = art_by_id.get(r["id"])
            art = a["artifact"] if a else "—"
            if a and "hybrid" in a.get("variants", ()):
                art += " (hybrid)"
            if stale_by_id.get(r["id"]) is True:
                art += " [stale]"
            g = grade_by_id.get(r["id"])
            grade = solidity_join.render_grade(g) if g else "—"
            A(
                f"| `{r['id']}` | {r['status']} | {grade} | {r['chapter']} | "
                f"{art} | {fmt_codes(r['codes'])} | {fmt_verified(r['verified'])} |"
            )
        A("")

    A("## Consistency check results")
    A("")
    A("_Generated by scanning chapters 02-20. Headline counts (rules, claims,")
    A("grades, chapters) live in the [Solidity")
    A("scoreboard](#solidity-scoreboard) above; this section holds the")
    A("mechanical checks._")
    A("")
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
            "No duplicate rule IDs across chapters 02-20. In particular the `P.Effects.*`"
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

    for title, failure_lines in extra_sections:
        A("")
        A(f"### {title}")
        A("")
        if failure_lines:
            for fl in failure_lines:
                A(fl if fl.lstrip().startswith("- ") else f"- {fl}")
        else:
            A("- clean.")

    return "\n".join(lines) + "\n"


# --- README count update ----------------------------------------------------


SCOREBOARD_BEGIN = "<!-- scoreboard:begin — generated by tools/regen_rule_index.py; do not edit -->"
SCOREBOARD_END = "<!-- scoreboard:end -->"


def readme_scoreboard_block(rules, grade_by_id, scoreboard):
    """The README's front-door scoreboard (abbreviated; the full one lives
    in rule-index.md#solidity-scoreboard). Derived, never declared.
    Wording per the fidelity reviewer's front-door pass: full caveat
    census in urgency order (a curated alarming-kinds-only subset would
    contradict the explicit-zero honesty signature), DISPUTED glossed at
    the moment it is most confusing, no derived ratios (denominators are
    arguments)."""
    by_grade = Counter(row["grade"] for row in grade_by_id.values())
    by_status = Counter(r["status"] for r in rules)
    cc = scoreboard["caveat_counts"]
    grades = " · ".join(
        f"{g} {by_grade.get(g, 0)}" for g in ("A", "B", "C", "D", "DISPUTED")
    )
    ndisp = by_grade.get("DISPUTED", 0)
    if ndisp:
        gloss = (
            f"({ndisp} grade{'s' if ndisp != 1 else ''} "
            "suspended by open findings)"
        )
    else:
        gloss = "(no open finding suspends a grade)"
    return "\n".join([
        f"> **Scoreboard** ({len(rules)} rules · "
        f"{scoreboard['case_studies']} case studies) —",
        f"> grades **{grades}** {gloss} ·",
        f"> claims **{by_status['normative']} normative / "
        f"{by_status['descriptive']} descriptive / "
        f"{by_status['interpretive']} interpretive** ·",
        f"> caveats **{cc['known-false']} known-false · "
        f"{cc['compiler-bug']} compiler-bug · "
        f"{cc['pending-upstream']} pending-upstream (watched) · "
        f"{cc['watch']} watch · "
        f"{cc['disclosure']} disclosure** ·",
        f"> **{scoreboard['verified_lines']}** validation events, "
        "each pinned to a commit.",
        "> Derived by the generator, never hand-maintained — full scoreboard:",
        "> [`rule-index.md` § Solidity"
        " scoreboard](rule-index.md#solidity-scoreboard).",
    ])


def update_readme(rules, case_studies, out_path, grade_by_id=None,
                  scoreboard=None):
    """Surgically replace the total/by-status rule counts, the case-study
    count, and the scoreboard block in README.md, writing the result to
    out_path. Idempotent: reruns produce no further change."""
    by_status = Counter(r["status"] for r in rules)
    total = len(rules)
    with open(README_PATH) as fh:
        text = fh.read()

    subs = [
        (r"\*\*\d+ rules\*\* across chapters", f"**{total} rules** across chapters"),
        (r"\*\*\d+ normative\*\*", f"**{by_status['normative']} normative**"),
        (r"\*\*\d+ descriptive\*\*", f"**{by_status['descriptive']} descriptive**"),
        (r"\*\*\d+ interpretive\*\*", f"**{by_status['interpretive']} interpretive**"),
        (r"\*\*\d+ case studies\*\*", f"**{case_studies} case studies**"),
    ]
    unmatched = []
    for pat, repl in subs:
        text, n = re.subn(pat, repl, text)
        if n == 0:
            unmatched.append(pat)
    if grade_by_id is not None and scoreboard is not None:
        block = (
            SCOREBOARD_BEGIN
            + "\n"
            + readme_scoreboard_block(rules, grade_by_id, scoreboard)
            + "\n"
            + SCOREBOARD_END
        )
        pat = re.escape(SCOREBOARD_BEGIN) + r".*?" + re.escape(SCOREBOARD_END)
        text, n = re.subn(pat, lambda _m: block, text, flags=re.DOTALL)
        if n == 0:
            unmatched.append("<scoreboard marker block>")
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

    # Fence extraction via the record-76 parser (replaces extract_rules;
    # its failures subsume the old duplicate computation but run_checks'
    # duplicate pass is kept as a cross-check).
    parse = fence_parser.parse_chapters(FORMALISM_DIR)
    rules = fence_parser.to_legacy_rules(parse.fences)
    parse_failure_lines = [
        fence_parser.format_failure(f) for f in parse.failures
    ]

    duplicates, no_anchor, missing_file, missing_name = run_checks(rules)
    case_studies = count_case_studies()

    # STRICT JOIN (record 76 ruling (8)): the migration's completion
    # event has occurred (STATUS count reached zero), so any STATUS
    # row anywhere is a hard phantom-class error from here on.
    retired_keyword = [
        f"{r['id']} ({r['chapter']}:{r['line']}): retired STATUS keyword"
        for r in rules if r.get("keyword") == "STATUS"
    ]

    # .v-side artifact classification + doc/.v join (record 76: the join
    # FAILS on any disagreement, never picks a side).
    artifacts, art_failures = artifact_classifier.classify(
        os.path.join(ROCQ_DIR, "theories"),
        os.path.join(ROCQ_DIR, "CORRESPONDENCE.md"),
    )
    art_by_id = {a["id"]: a for a in artifacts}
    join_failures = []
    doc_ids = {r["id"] for r in rules if r["id"]}
    for r in rules:
        a = art_by_id.get(r["id"])
        if a is None:
            join_failures.append(
                f"{r['id']} ({r['chapter']}:{r['line']}): no .v artifact"
            )
        elif (r.get("keyword"), r.get("status")) != (a["keyword"], a["kind"]):
            join_failures.append(
                f"{r['id']}: doc {r.get('keyword')} {r.get('status')} != "
                f".v {a['keyword']} {a['kind']} ({a['file']}:{a['line']})"
            )
    for a in artifacts:
        if a["id"] not in doc_ids:
            join_failures.append(
                f"{a['id']} ({a['file']}:{a['line']}): no doc fence"
            )
    art_failure_lines = [
        f"FAIL[{f.check}] {f.rule_id} ({f.where}): {f.detail}"
        for f in art_failures
    ]
    retired_keyword_v = [
        f"{a['id']} ({a['file']}:{a['line']}): retired STATUS keyword (.v)"
        for a in artifacts if a.get("keyword") == "STATUS"
    ]
    join_failures.extend(retired_keyword_v)

    # Event lines + staleness (git-backed). FLAG DAY (record 76
    # ruling (9)): the backfill is complete — every VERIFIED line
    # carries its @commit key — so legacy tolerance is OFF; a
    # commit-less VERIFIED is a hard failure from here on.
    ev_rules, ev_failures = evs.analyze(legacy_verified_ok=False)
    ev_failure_lines = evs.render_failures(ev_failures)
    stale_by_id = evs.staleness_by_rule(ev_rules)

    # Solidity join (record 76): derive per-rule evidence and GRADE. The
    # doc fences are the event system of record — the .v RULE comments
    # still carry pre-migration commit-less VERIFIED transcriptions, so
    # the .v side contributes no events and the join runs in union mode,
    # not event-agreement mode. Event commit reachability is already
    # enforced by evs.analyze above, so it is not re-checked here.
    art_kind_map = {
        artifact_classifier.DEFINING: "constructor",
        artifact_classifier.THEOREM_ADMITTED: "theorem",
        artifact_classifier.THEOREM_QED: "theorem",
        artifact_classifier.DOCUMENTED_ANCHOR: "documented-anchor",
        artifact_classifier.DEMOTED_CLAIM: "claim-definition",
    }
    doc_fences = [
        dict(
            id=f["id"],
            keyword=f["keyword"],
            value=f["value"],
            source=f"{f['chapter']}:{f['line']}",
            events=(
                [dict(kind="CHECKED", commit=c["commit"]) for c in f["checked"]]
                + [
                    dict(kind="VERIFIED", study=v["study"], commit=v["commit"])
                    for v in f["verified"]
                ]
                + [
                    dict(kind="CAVEAT", caveat_kind=c["caveat_kind"], text=c["text"])
                    for c in f["caveats"]
                ]
            ),
        )
        for f in parse.fences
        if f["id"]
    ]
    v_fences = [
        dict(
            id=a["id"],
            keyword=a["keyword"],
            value=a["kind"],
            source=f"{a['file']}:{a['line']}",
            events=[],
            artifact_kind=art_kind_map.get(a["artifact"]),
            qed=a["artifact"] == artifact_classifier.THEOREM_QED,
        )
        for a in artifacts
    ]
    fidelity_path = os.path.join(ROCQ_DIR, "FIDELITY.md")
    stamped_files = set()
    with open(fidelity_path) as fh:
        for ln in fh:
            m = re.match(r"STAMP:\s+(\S+\.v)\s+GREEN\b", ln)
            if m:
                stamped_files.add(m.group(1))
    stamped_ids = {
        a["id"] for a in artifacts if os.path.basename(a["file"]) in stamped_files
    }
    variants, _ = artifact_classifier.parse_traceability(
        os.path.join(ROCQ_DIR, "CORRESPONDENCE.md")
    )
    join_result = solidity_join.join_index(
        doc_fences,
        v_fences,
        fidelity_path,
        hybrid_ids=variants["hybrid"],
        stamped_ids=stamped_ids,
        stale_ids={rid for rid, s in stale_by_id.items() if s},
        require_event_agreement=False,
    )
    grade_by_id = {r["id"]: r for r in join_result["rules"]}

    def rel_where(where):
        if not where:
            return "-"
        return where.replace(FORMALISM_DIR + os.sep, "")

    grade_failure_lines = [
        f"{f['severity'].upper()}[{f['check']}] {f.get('id') or '-'} "
        f"({rel_where(f.get('where'))}): {f['detail']}"
        for f in join_result["failures"]
    ]

    # Scoreboard inputs (all derived here, never hand-maintained).
    caveat_counts = Counter()
    caveats_by_chapter = Counter()
    for f in parse.fences:
        caveats_by_chapter[f["chapter"]] += len(f["caveats"])
        for cv in f["caveats"]:
            k = cv["caveat_kind"]
            caveat_counts["watch" if k.startswith("watch(") else k] += 1
    verified_lines = sum(len(f["verified"]) for f in parse.fences)
    verified_keyed = sum(
        1 for f in parse.fences for v in f["verified"] if v["commit"]
    )
    verified_rules = sum(1 for f in parse.fences if f["verified"])
    checked_lines = sum(len(f["checked"]) for f in parse.fences)
    provisos = []
    for r in ev_rules:
        for ev in r["events"]:
            if ev.get("caveat_kind") == "pending-upstream":
                keys = [f"`{b}`" for b in ev.get("branches", [])]
                keys += [f"@ {c}" for c in ev.get("commits", [])]
                provisos.append((r["id"], " ".join(keys) or "(no key)"))
    provisos.sort()
    scoreboard = dict(
        caveat_counts=caveat_counts,
        caveats_by_chapter=caveats_by_chapter,
        verified_lines=verified_lines,
        verified_keyed=verified_keyed,
        verified_rules=verified_rules,
        checked_lines=checked_lines,
        provisos=provisos,
        hybrid_warnings=sum(
            1
            for f in join_result["failures"]
            if f["check"] == "hybrid-no-clauses"
        ),
        case_studies=case_studies,
    )

    index_text = render_index(
        rules, duplicates, no_anchor, missing_file, missing_name,
        art_by_id=art_by_id,
        stale_by_id=stale_by_id,
        grade_by_id=grade_by_id,
        scoreboard=scoreboard,
        extra_sections=[
            ("Fence-parse check", parse_failure_lines),
            ("Artifact-classifier check", art_failure_lines),
            ("Doc/.v join check", join_failures),
            ("Event and staleness check", ev_failure_lines),
            ("Solidity join and grade check", grade_failure_lines),
        ],
    )
    with open(out_index, "w") as fh:
        fh.write(index_text)

    unmatched = update_readme(
        rules, case_studies, out_readme,
        grade_by_id=grade_by_id, scoreboard=scoreboard,
    )

    by_status = Counter(r["status"] for r in rules)
    print(f"Scanned {len(rules)} rules across chapters 02-20.")
    print(
        f"  normative {by_status['normative']}, "
        f"descriptive {by_status['descriptive']}, "
        f"interpretive {by_status['interpretive']}"
    )
    by_grade = Counter(r["grade"] for r in join_result["rules"])
    print(
        "  grades: "
        + ", ".join(
            f"{'(join failed)' if g is None else g} {by_grade[g]}"
            for g in ("A", "B", "C", "D", "DISPUTED", None)
            if by_grade[g]
        )
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
    if parse_failure_lines:
        print(f"FAIL: {len(parse_failure_lines)} fence-parse failure(s):")
        for fl in parse_failure_lines:
            print(f"  {fl}")
        hard_fail = True
    if art_failure_lines:
        print(f"FAIL: {len(art_failure_lines)} artifact-classifier failure(s):")
        for fl in art_failure_lines:
            print(f"  {fl}")
        hard_fail = True
    if join_failures:
        print(f"FAIL: {len(join_failures)} doc/.v join failure(s):")
        for fl in join_failures:
            print(f"  {fl}")
        hard_fail = True
    if retired_keyword:
        print(f"FAIL: {len(retired_keyword)} retired-STATUS row(s) (strict join):")
        for fl in retired_keyword:
            print(f"  {fl}")
        hard_fail = True
    if evs.hard_failures(ev_failures):
        print("FAIL: event/staleness hard failure(s):")
        for fl in ev_failure_lines:
            print(f"  {fl}")
        hard_fail = True
    if not join_result["ok"]:
        errors = [
            f for f in join_result["failures"] if f["severity"] == "error"
        ]
        print(f"FAIL: {len(errors)} solidity join/grade failure(s):")
        for f in errors:
            print(
                f"  [{f['check']}] {f.get('id') or '-'} "
                f"({rel_where(f.get('where'))}): {f['detail']}"
            )
        hard_fail = True

    if hard_fail:
        sys.exit(1)
    print("All consistency checks passed.")


if __name__ == "__main__":
    main()
