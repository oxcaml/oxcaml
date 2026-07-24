#!/usr/bin/env python3
# Written by Claude.
#
# Fence parser for the Flambda 2 formalism regen pipeline (CORRESPONDENCE.md
# record 76, "The solidity schema").
#
# Parses every fenced ```rule block across the 19 chapter files NN-*.md
# (01-13, 15-20) in middle_end/flambda2/docs/formalism/ and returns, per
# fence: the rule id (RULE line), the claim-kind line (STATUS or CLAIM —
# mid-migration both keywords coexist across the corpus, but each fence must
# carry EXACTLY ONE), CODE anchors, and the evidence event lines
# (VERIFIED / CHECKED / CAVEAT).
#
# Checks enforced here (all reported as failure records, never exceptions —
# the pipeline records failures rather than crashing):
#   - exactly one keyword (STATUS xor CLAIM) line per fence;
#   - keyword values: STATUS in {normative, descriptive, conjectured},
#     CLAIM in {normative, descriptive, interpretive};
#   - duplicate rule ids, detected PRE-dedup (every occurrence is kept in the
#     fence list and in the emitted triples; the duplicate check runs over the
#     full occurrence list before any uniquing);
#   - event-line syntax: VERIFIED <study> [@ <commit>], CHECKED @ <commit>,
#     CAVEAT <kind>: <text> with kind in {known-false, compiler-bug,
#     pending-upstream, watch(W-nn), disclosure}; commit keys must be
#     7-40 hex chars (repo REACHABILITY is a separate, optional pass:
#     check_commits_reachable);
#   - phantom-keyword scan: an uppercase keyword (STATUS, CLAIM, CAVEAT,
#     CHECKED, VERIFIED) with a payload at the start of a line ANYWHERE other
#     than a rule-fence header — plain prose, non-rule fences, or rule-fence
#     prose after the `---` separator (the ch. 18 "STATUS conjectured —"
#     counting trap) — is flagged.
#
# Mid-migration census output: emit_triples() produces one "id|keyword|value"
# TRIPLE per fence occurrence, in document order, for the doc<->.v
# byte-identity comparison (STATUS + CLAIM = 453, disjoint, STATUS strictly
# shrinking).
#
# Python 3 stdlib only. Designed to be imported by regen_rule_index.py; also
# runnable directly:
#   python3 fence_parser.py                    # run the self-tests
#   python3 fence_parser.py <formalism-dir>    # parse a real tree, print report

import glob
import os
import re
import subprocess
from collections import Counter, namedtuple

# --- Vocabulary --------------------------------------------------------------

KIND_KEYWORDS = ("STATUS", "CLAIM")
PHANTOM_KEYWORDS = ("STATUS", "CLAIM", "CAVEAT", "CHECKED", "VERIFIED")

STATUS_VALUES = frozenset(["normative", "descriptive", "conjectured"])
CLAIM_VALUES = frozenset(["normative", "descriptive", "interpretive"])
KIND_VALUES = {"STATUS": STATUS_VALUES, "CLAIM": CLAIM_VALUES}

RE_FENCE_RULE_OPEN = re.compile(r"^```rule\s*$")
RE_FENCE_OPEN = re.compile(r"^```\S")  # any other opening fence, e.g. ```ocaml
RE_FENCE_CLOSE = re.compile(r"^```\s*$")

RE_RULE = re.compile(r"^RULE\s+(\S+)\s*$")
RE_KIND = re.compile(r"^(STATUS|CLAIM)\s+(\S+)\s*$")
RE_CODE = re.compile(r"^CODE\s+(\S+)\s*$")
RE_VERIFIED = re.compile(r"^VERIFIED\s+(\S+)(?:\s+@\s+(\S+))?\s*$")
RE_CHECKED = re.compile(r"^CHECKED\s+@\s+(\S+)\s*$")
RE_CAVEAT = re.compile(r"^CAVEAT\s+(\S+):\s*(\S.*)$")
RE_COMMIT = re.compile(r"^[0-9a-f]{7,40}$")
RE_CAVEAT_KIND = re.compile(
    r"^(known-false|compiler-bug|pending-upstream|disclosure|watch\(W-\d+\))$"
)
# Any header line starting with a keyword we own (used to distinguish
# "malformed known keyword" from "unrecognized header line").
RE_HEADER_KEYWORD = re.compile(r"^(RULE|STATUS|CLAIM|CODE|VERIFIED|CHECKED|CAVEAT)\b")
# Mid-line uppercase keyword mention (strand_scan only).
RE_KEYWORD_WORD = re.compile(r"\b(STATUS|CLAIM|CAVEAT|CHECKED|VERIFIED)\b")

ParseResult = namedtuple("ParseResult", ["fences", "triples", "failures"])


def _phantom_match(line):
    """Return the keyword if this line reads as an uppercase keyword plus a
    payload (the prose-convention violation), else None."""
    m = re.match(r"^(STATUS|CLAIM|CAVEAT|CHECKED|VERIFIED)\b(.*)$", line)
    if m and m.group(2).strip():
        return m.group(1)
    return None


def _failure(kind, path, line, message):
    """A failure record. `line` is 1-based; `file` is the basename (chapters
    are unique by basename within the formalism dir)."""
    return dict(kind=kind, file=os.path.basename(path), line=line, message=message)


def format_failure(f):
    return f"{f['file']}:{f['line']}: [{f['kind']}] {f['message']}"


# --- Per-file parse -----------------------------------------------------------


def parse_file(path):
    """Parse one chapter file. Returns (fences, failures).

    Each fence dict has keys:
      id            rule id string, or None (missing-rule-id also recorded)
      keyword       'STATUS' or 'CLAIM' when exactly one kind line was found,
                    else None
      value         the kind line's value, or None
      kind_lines    [(lineno, keyword, value)] — every kind line seen (for the
                    exactly-one check; callers can inspect violations)
      codes         [anchor string]
      verified      [{'study', 'commit' (or None), 'line'}]
      checked       [{'commit', 'line'}]
      caveats       [{'caveat_kind', 'text', 'line'}]
      chapter       basename of the file
      path          full path
      line          1-based line number of the ```rule opener
    """
    chapter = os.path.basename(path)
    failures = []
    try:
        with open(path) as fh:
            lines = fh.readlines()
    except OSError as e:
        return [], [_failure("io-error", path, 0, str(e))]

    fences = []
    i, n = 0, len(lines)
    while i < n:
        line = lines[i].rstrip("\n")
        if RE_FENCE_RULE_OPEN.match(line):
            fence, i = _parse_rule_fence(lines, i, path, chapter, failures)
            fences.append(fence)
            continue
        if RE_FENCE_OPEN.match(line):
            # Non-rule fence: scan its contents for phantoms, skip to close.
            i += 1
            while i < n and not RE_FENCE_CLOSE.match(lines[i]):
                kw = _phantom_match(lines[i].rstrip("\n"))
                if kw:
                    failures.append(
                        _failure(
                            "phantom-keyword",
                            path,
                            i + 1,
                            f"{kw} line inside a non-rule fence: "
                            f"{lines[i].rstrip()!r}",
                        )
                    )
                i += 1
            if i >= n:
                failures.append(
                    _failure("unclosed-fence", path, n, "fence open at EOF")
                )
            i += 1
            continue
        # Plain prose.
        kw = _phantom_match(line)
        if kw:
            failures.append(
                _failure(
                    "phantom-keyword",
                    path,
                    i + 1,
                    f"{kw} line in prose outside any fence: {line.rstrip()!r}",
                )
            )
        i += 1
    return fences, failures


def _parse_rule_fence(lines, start, path, chapter, failures):
    """Parse one ```rule fence starting at index `start` (the opener).
    Appends failures; returns (fence_dict, next_index)."""
    n = len(lines)
    fence = dict(
        id=None,
        keyword=None,
        value=None,
        kind_lines=[],
        codes=[],
        verified=[],
        checked=[],
        caveats=[],
        chapter=chapter,
        path=path,
        line=start + 1,
    )
    i = start + 1
    in_header = True
    closed = False
    while i < n:
        raw = lines[i].rstrip("\n")
        if RE_FENCE_CLOSE.match(raw):
            closed = True
            i += 1
            break
        lineno = i + 1
        if in_header and re.match(r"^---", raw):
            in_header = False
            i += 1
            continue
        if not in_header:
            # Fence prose (rule statement / NOTES): phantom scan only.
            kw = _phantom_match(raw)
            if kw:
                failures.append(
                    _failure(
                        "phantom-keyword",
                        path,
                        lineno,
                        f"{kw} line in rule-fence prose (after ---): {raw!r}",
                    )
                )
            i += 1
            continue
        # Header region.
        m = RE_RULE.match(raw)
        if m:
            if fence["id"] is not None:
                failures.append(
                    _failure(
                        "duplicate-rule-line",
                        path,
                        lineno,
                        f"second RULE line in one fence: {raw!r}",
                    )
                )
            else:
                fence["id"] = m.group(1)
            i += 1
            continue
        m = RE_KIND.match(raw)
        if m:
            kw, value = m.group(1), m.group(2)
            fence["kind_lines"].append((lineno, kw, value))
            if value not in KIND_VALUES[kw]:
                failures.append(
                    _failure(
                        "bad-keyword-value",
                        path,
                        lineno,
                        f"{kw} value {value!r} not in "
                        f"{{{', '.join(sorted(KIND_VALUES[kw]))}}}",
                    )
                )
            i += 1
            continue
        m = RE_CODE.match(raw)
        if m:
            fence["codes"].append(m.group(1))
            i += 1
            continue
        m = RE_CHECKED.match(raw)
        if m:
            commit = m.group(1)
            if not RE_COMMIT.match(commit):
                failures.append(
                    _failure(
                        "bad-commit-key",
                        path,
                        lineno,
                        f"CHECKED commit key {commit!r} is not 7-40 hex chars",
                    )
                )
            fence["checked"].append(dict(commit=commit, line=lineno))
            i += 1
            continue
        m = RE_VERIFIED.match(raw)
        if m:
            study, commit = m.group(1), m.group(2)
            if commit is not None and not RE_COMMIT.match(commit):
                failures.append(
                    _failure(
                        "bad-commit-key",
                        path,
                        lineno,
                        f"VERIFIED commit key {commit!r} is not 7-40 hex chars",
                    )
                )
            fence["verified"].append(dict(study=study, commit=commit, line=lineno))
            i += 1
            continue
        m = RE_CAVEAT.match(raw)
        if m:
            ckind, text = m.group(1), m.group(2)
            if not RE_CAVEAT_KIND.match(ckind):
                failures.append(
                    _failure(
                        "bad-caveat-kind",
                        path,
                        lineno,
                        f"CAVEAT kind {ckind!r} not in {{known-false, "
                        f"compiler-bug, pending-upstream, watch(W-nn), "
                        f"disclosure}}",
                    )
                )
            fence["caveats"].append(dict(caveat_kind=ckind, text=text, line=lineno))
            i += 1
            continue
        if RE_HEADER_KEYWORD.match(raw):
            failures.append(
                _failure(
                    "malformed-header-line",
                    path,
                    lineno,
                    f"keyword line does not parse: {raw!r}",
                )
            )
        else:
            failures.append(
                _failure(
                    "unrecognized-header-line",
                    path,
                    lineno,
                    f"non-keyword line before ---: {raw!r}",
                )
            )
        i += 1
    if not closed:
        failures.append(
            _failure(
                "unclosed-fence", path, fence["line"], "```rule fence never closed"
            )
        )
    if fence["id"] is None:
        failures.append(
            _failure("missing-rule-id", path, fence["line"], "fence has no RULE line")
        )
    rid = fence["id"] or f"<no-id at {chapter}:{fence['line']}>"
    kinds = fence["kind_lines"]
    if len(kinds) == 0:
        failures.append(
            _failure(
                "missing-keyword-line",
                path,
                fence["line"],
                f"rule {rid}: no STATUS or CLAIM line",
            )
        )
    elif len(kinds) > 1:
        locs = ", ".join(f"{kw} at line {ln}" for ln, kw, _ in kinds)
        failures.append(
            _failure(
                "multiple-keyword-lines",
                path,
                fence["line"],
                f"rule {rid}: {len(kinds)} keyword lines ({locs}); "
                "exactly one STATUS or CLAIM line is required",
            )
        )
    else:
        _, fence["keyword"], fence["value"] = kinds[0]
    return fence, i


# --- Corpus-level entry points ------------------------------------------------


def chapter_files(formalism_dir):
    """The chapter files NN-*.md, sorted (01-13, 15-20; 14-validation is a
    directory and is not matched)."""
    return sorted(glob.glob(os.path.join(formalism_dir, "[0-9][0-9]-*.md")))


def find_duplicate_ids(fences):
    """Duplicate-id check over the FULL occurrence list (pre-dedup: no fence
    has been dropped or uniqued when this runs). One failure per duplicated
    id, listing every occurrence."""
    failures = []
    ids = [f["id"] for f in fences if f["id"] is not None]
    for rid, count in sorted(Counter(ids).items()):
        if count > 1:
            occs = [f for f in fences if f["id"] == rid]
            locs = ", ".join(f"{f['chapter']}:{f['line']}" for f in occs)
            failures.append(
                _failure(
                    "duplicate-id",
                    occs[0]["path"],
                    occs[0]["line"],
                    f"rule id {rid} defined {count} times: {locs}",
                )
            )
    return failures


def emit_triples(fences):
    """Mid-migration census TRIPLES, one per fence occurrence (pre-dedup) that
    has an id and exactly one keyword line, in document order:
        id|keyword|value       keyword in {STATUS, CLAIM}
    Fences excluded here are already covered by missing-rule-id /
    missing-keyword-line / multiple-keyword-lines failures, and their absence
    shows up as a census-count mismatch downstream."""
    return [
        f"{f['id']}|{f['keyword']}|{f['value']}"
        for f in fences
        if f["id"] is not None and f["keyword"] is not None
    ]


def triples_text(triples, sort=False):
    """Canonical text form for the doc<->.v byte-identity comparison. Document
    order by default; pass sort=True for an order-insensitive canonical form."""
    return "\n".join(sorted(triples) if sort else triples) + ("\n" if triples else "")


def parse_chapters(formalism_dir):
    """Main entry point. Parses every chapter file under formalism_dir and
    returns ParseResult(fences, triples, failures). Never raises on content
    problems; failures accumulate in document order (per-fence and phantom
    failures first, then corpus-level duplicate-id failures)."""
    fences, failures = [], []
    for path in chapter_files(formalism_dir):
        fs, errs = parse_file(path)
        fences.extend(fs)
        failures.extend(errs)
    failures.extend(find_duplicate_ids(fences))
    return ParseResult(fences=fences, triples=emit_triples(fences), failures=failures)


def to_legacy_rules(fences):
    """Adapter to regen_rule_index.py's extract_rules() shape: a list of dicts
    with keys id, status, chapter, codes, verified (study strings), line.
    For CLAIM fences, 'status' carries the CLAIM value; 'keyword' is included
    so the renderer can distinguish."""
    return [
        dict(
            id=f["id"],
            status=f["value"],
            keyword=f["keyword"],
            chapter=f["chapter"],
            codes=list(f["codes"]),
            verified=[v["study"] for v in f["verified"]],
            line=f["line"],
        )
        for f in fences
    ]


def strand_scan(formalism_dir):
    """Softer, opt-in scan for the sweep's strand inventory (record 76): every
    MID-LINE uppercase keyword mention in prose, non-rule fences, or rule-fence
    bodies (e.g. 'NOTES: STATUS conjectured — ...'). These do not threaten
    fence-aware extraction (the hard phantom-keyword check covers line-anchored
    occurrences), but the migration sweep must retire them by rewording.
    Returns records shaped like failures, kind 'keyword-in-prose'. NOT part of
    parse_chapters' failure list; the sweep driver calls this separately and
    diffs against its expected residue."""
    records = []
    for path in chapter_files(formalism_dir):
        try:
            with open(path) as fh:
                lines = fh.readlines()
        except OSError as e:
            records.append(_failure("io-error", path, 0, str(e)))
            continue
        n = len(lines)
        in_fence = None  # None | 'rule' | 'other'
        in_header = False
        for i in range(n):
            raw = lines[i].rstrip("\n")
            if in_fence is None:
                if RE_FENCE_RULE_OPEN.match(raw):
                    in_fence, in_header = "rule", True
                    continue
                if RE_FENCE_OPEN.match(raw):
                    in_fence = "other"
                    continue
            else:
                if RE_FENCE_CLOSE.match(raw):
                    in_fence = None
                    continue
                if in_fence == "rule" and in_header:
                    if re.match(r"^---", raw):
                        in_header = False
                    continue  # header lines are legitimate keyword lines
            if _phantom_match(raw):
                continue  # already a hard phantom-keyword failure
            m = RE_KEYWORD_WORD.search(raw)
            if m:
                where = {
                    None: "prose",
                    "other": "non-rule fence",
                    "rule": "rule-fence body",
                }[in_fence]
                records.append(
                    _failure(
                        "keyword-in-prose",
                        path,
                        i + 1,
                        f"mid-line {m.group(1)} mention in {where}: {raw.strip()!r}",
                    )
                )
    return records


def check_commits_reachable(fences, repo_root):
    """Optional pass (needs git and a repo): flag CHECKED/VERIFIED commit keys
    that are well-formed but not reachable objects in repo_root. Returns a
    failure list; never raises."""
    failures = []
    seen = {}
    for f in fences:
        events = [(c["commit"], c["line"]) for c in f["checked"]]
        events += [(v["commit"], v["line"]) for v in f["verified"] if v["commit"]]
        for commit, lineno in events:
            if not RE_COMMIT.match(commit):
                continue  # already flagged as bad-commit-key
            if commit not in seen:
                try:
                    # stdout/stderr=PIPE rather than capture_output:
                    # keeps the module runnable on Python 3.6 (the
                    # box default), matching events_and_staleness.
                    proc = subprocess.run(
                        ["git", "-C", repo_root, "cat-file", "-e", commit + "^{commit}"],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                    )
                    seen[commit] = proc.returncode == 0
                except OSError:
                    seen[commit] = None  # git unavailable: report distinctly
            if seen[commit] is False:
                failures.append(
                    _failure(
                        "unreachable-commit",
                        f["path"],
                        lineno,
                        f"commit {commit} not found in {repo_root}",
                    )
                )
            elif seen[commit] is None:
                failures.append(
                    _failure(
                        "commit-check-unavailable",
                        f["path"],
                        lineno,
                        f"could not run git to check commit {commit}",
                    )
                )
    return failures


# --- Self-tests ----------------------------------------------------------------


def _selftest():
    import tempfile

    def write(d, name, text):
        p = os.path.join(d, name)
        with open(p, "w") as fh:
            fh.write(text)
        return p

    with tempfile.TemporaryDirectory() as d:
        # Chapter 02: two good fences (one STATUS, one CLAIM with full event
        # lines), plus a phantom in prose and a phantom in a non-rule fence.
        write(
            d,
            "02-good.md",
            "# Chapter\n"
            "\n"
            "```rule\n"
            "RULE T.Good.One\n"
            "STATUS normative\n"
            "CODE simplify/foo.ml#bar\n"
            "VERIFIED 14-validation/study.md\n"
            "---\n"
            "premise\n"
            "----\n"
            "conclusion\n"
            "NOTES: mentions the word status in lowercase, fine.\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Good.Two\n"
            "CLAIM interpretive\n"
            "CODE types/baz.ml#quux\n"
            "VERIFIED 14-validation/study2.md @ 0123456789abcdef0123456789abcdef01234567\n"
            "CHECKED @ deadbeef00\n"
            "CAVEAT watch(W-36): batch flags enumerate their files\n"
            "CAVEAT compiler-bug: rule true of the code, code diverges\n"
            "---\n"
            "body\n"
            "```\n"
            "\n"
            "VERIFIED by inspection, this sentence is a prose phantom.\n"
            "\n"
            "```ocaml\n"
            "(* CHECKED @ 0000000 inside a non-rule fence is a phantom *)\n"
            "CHECKED @ 0000000\n"
            "```\n",
        )
        # Chapter 03: every failure mode.
        write(
            d,
            "03-bad.md",
            "```rule\n"
            "RULE T.Bad.BothKinds\n"
            "STATUS normative\n"
            "CLAIM normative\n"
            "CODE a.ml#f\n"
            "---\n"
            "x\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Bad.NoKind\n"
            "CODE a.ml#f\n"
            "---\n"
            "x\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Bad.Values\n"
            "CLAIM conjectured\n"
            "CODE a.ml#f\n"
            "CHECKED deadbeef\n"
            "VERIFIED s.md @ nothex\n"
            "CAVEAT bogus-kind: text\n"
            "---\n"
            "STATUS conjectured — the ch. 18 counting trap, in fence prose\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Dup.Id\n"
            "STATUS descriptive\n"
            "CODE a.ml#f\n"
            "---\n"
            "x\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Dup.Id\n"
            "CLAIM descriptive\n"
            "CODE a.ml#g\n"
            "---\n"
            "x\n"
            "```\n"
            "\n"
            "```rule\n"
            "STATUS normative\n"
            "CODE a.ml#f\n"
            "---\n"
            "fence with no RULE line\n"
            "```\n"
            "\n"
            "```rule\n"
            "RULE T.Bad.Unclosed\n"
            "STATUS normative\n",
        )

        res = parse_chapters(d)
        kinds = Counter(f["kind"] for f in res.failures)
        by_kind = {}
        for f in res.failures:
            by_kind.setdefault(f["kind"], []).append(f)

        # Good fences parsed fully.
        good = {f["id"]: f for f in res.fences if f["chapter"] == "02-good.md"}
        assert good["T.Good.One"]["keyword"] == "STATUS"
        assert good["T.Good.One"]["value"] == "normative"
        assert good["T.Good.One"]["codes"] == ["simplify/foo.ml#bar"]
        assert good["T.Good.One"]["verified"][0]["study"] == "14-validation/study.md"
        assert good["T.Good.One"]["verified"][0]["commit"] is None
        two = good["T.Good.Two"]
        assert two["keyword"] == "CLAIM" and two["value"] == "interpretive"
        assert two["verified"][0]["commit"].startswith("0123456789abcdef")
        assert two["checked"][0]["commit"] == "deadbeef00"
        assert [c["caveat_kind"] for c in two["caveats"]] == [
            "watch(W-36)",
            "compiler-bug",
        ]

        # Triples: pre-dedup — both T.Dup.Id occurrences present.
        assert "T.Good.One|STATUS|normative" in res.triples
        assert "T.Good.Two|CLAIM|interpretive" in res.triples
        assert res.triples.count("T.Dup.Id|STATUS|descriptive") == 1
        assert res.triples.count("T.Dup.Id|CLAIM|descriptive") == 1
        # T.Bad.BothKinds and T.Bad.NoKind excluded (no single keyword).
        assert not any(t.startswith("T.Bad.BothKinds|") for t in res.triples)
        assert not any(t.startswith("T.Bad.NoKind|") for t in res.triples)
        assert triples_text(["b|STATUS|x", "a|CLAIM|y"], sort=True) == (
            "a|CLAIM|y\nb|STATUS|x\n"
        )

        # Failure inventory.
        assert kinds["multiple-keyword-lines"] == 1
        assert "T.Bad.BothKinds" in by_kind["multiple-keyword-lines"][0]["message"]
        assert kinds["missing-keyword-line"] == 1  # T.Bad.NoKind
        assert kinds["missing-rule-id"] == 1
        assert kinds["bad-keyword-value"] == 1  # CLAIM conjectured
        assert "conjectured" in by_kind["bad-keyword-value"][0]["message"]
        assert kinds["malformed-header-line"] == 1  # CHECKED without @
        assert kinds["bad-commit-key"] == 1  # VERIFIED s.md @ nothex
        assert kinds["bad-caveat-kind"] == 1
        assert kinds["duplicate-id"] == 1
        assert "T.Dup.Id" in by_kind["duplicate-id"][0]["message"]
        assert kinds["unclosed-fence"] == 1
        # Phantoms: prose VERIFIED, non-rule-fence CHECKED, and the
        # in-fence-prose STATUS counting trap.
        phantoms = by_kind["phantom-keyword"]
        assert kinds["phantom-keyword"] == 3, phantoms
        msgs = "\n".join(p["message"] for p in phantoms)
        assert "prose outside any fence" in msgs
        assert "non-rule fence" in msgs
        assert "rule-fence prose" in msgs and "counting trap" in msgs

        # Legacy adapter shape.
        legacy = to_legacy_rules([good["T.Good.Two"]])
        assert legacy[0]["status"] == "interpretive"
        assert legacy[0]["verified"] == ["14-validation/study2.md"]

        # Determinism: double run, identical output.
        res2 = parse_chapters(d)
        assert res2 == res

        # Strand scan (opt-in, mid-line mentions only): the 'NOTES: STATUS
        # conjectured' pattern and a prose mid-line mention are found; header
        # keyword lines and line-anchored phantoms are NOT double-reported.
        write(
            d,
            "04-strands.md",
            "Prose citing a rule's VERIFIED case study mid-line.\n"
            "\n"
            "```rule\n"
            "RULE T.Strand.One\n"
            "STATUS normative\n"
            "CODE a.ml#f\n"
            "---\n"
            "body\n"
            "NOTES: STATUS conjectured — mid-line strand, not a hard phantom.\n"
            "```\n",
        )
        strands = strand_scan(d)
        strands_04 = [s for s in strands if s["file"] == "04-strands.md"]
        assert len(strands_04) == 2, strands_04
        assert any("VERIFIED" in s["message"] and "prose" in s["message"]
                   for s in strands_04)
        assert any("STATUS" in s["message"] and "rule-fence body" in s["message"]
                   for s in strands_04)
        # The hard phantom in 03-bad.md's fence body is excluded from strands.
        assert not any(
            s["file"] == "03-bad.md" and "counting trap" in s["message"]
            for s in strands
        )

    print("fence_parser self-tests passed.")


def _report(formalism_dir):
    res = parse_chapters(formalism_dir)
    by_kw = Counter(t.split("|")[1] for t in res.triples)
    print(f"{len(res.fences)} fences, {len(res.triples)} triples "
          f"(STATUS {by_kw['STATUS']}, CLAIM {by_kw['CLAIM']}), "
          f"{len(res.failures)} failure(s)")
    for f in res.failures:
        print("  " + format_failure(f))
    strands = strand_scan(formalism_dir)
    print(f"strand scan (informational): {len(strands)} mid-line mention(s)")
    for s in strands:
        print("  " + format_failure(s))


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        _report(sys.argv[1])
    else:
        _selftest()

