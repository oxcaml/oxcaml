#!/usr/bin/env python3
# Written by Claude.
#
# Joiner-and-grader for the solidity schema (CORRESPONDENCE.md record 76,
# "The solidity schema").
#
# This is the join stage of the rule-index regen pipeline. Upstream
# extractors parse the doc-side rule fences (chapters NN-*.md) and the
# .v-side RULE comment fences (rocq/theories/*.v) into fence records, and
# the artifact-kind classifier tags each .v fence with its artifact kind
# (checked against CORRESPONDENCE.md's Traceability instance lists). This
# module then:
#
#   1. JOINS the two fence sets per rule id. The join FAILS on any doc/.v
#      disagreement (id present on one side only, keyword mismatch,
#      keyword-value byte mismatch, or — by default — event-line
#      disagreement). It never picks a side: a rule whose join fails gets
#      no evidence and no grade, only a recorded failure.
#
#   2. Derives per-rule EVIDENCE on TWO INCOMPARABLE AXES (always
#      recomputed from the fences, never trusted from disk):
#        - empirical: stated < code-read < validated.
#          `CHECKED @ <commit>` ⇒ code-read; `VERIFIED <study> @ <commit>`
#          ⇒ validated; no event ⇒ stated.
#        - formal: unmechanized < mechanized. "Mechanized" REQUIRES the
#          fidelity-review stamp and EXCLUDES documented anchors. A Qed is
#          a display badge ("proved-in-model") atop mechanized, never a
#          grade input.
#
#   3. Derives the GRADE (grades come from this tooling only;
#      hand-computed grades are forbidden), monotone on the two axes:
#        A = validated × mechanized, with no demoting flag;
#        B = validated alone, OR code-read × mechanized;
#        C = exactly one axis inhabited;
#        D = neither.
#      Display flags: false-as-stated (CAVEAT known-false), compiler-bug
#      (CAVEAT compiler-bug), pending-upstream (CAVEAT pending-upstream).
#      Per the drift filed in FIDELITY.md's review log against record 76's
#      letter ("no flags"), only false-as-stated and pending-upstream
#      demote A; compiler-bug displays without demoting (KF-040 polarity:
#      the rule is TRUE of the code). See DEMOTING_FLAGS.
#
#   4. DISPUTED — derived as "an open FIDELITY.md finding whose RULES:
#      header names this id" — SUSPENDS the letter entirely. The computed
#      letter is kept in `suspended_letter` for tooling; the displayed
#      grade is "DISPUTED".
#
#   5. HYBRID ids carry clause-granular evidence and grade at the weakest
#      clause, with an explicit "hybrid" flag; rule-level VERIFIED on a
#      HYBRID id is an ERROR (the event is discarded, never counted).
#
#   6. Parses FIDELITY.md's structured `RULES: <id>, ...` /
#      `RULES: none` headers (one at the top of each finding entry). A
#      finding entry without one, or one naming an unknown id, is an
#      error.
#
# Failures are RETURNED as data (severity "error" or "warning"), matching
# the regen pipeline's convention of recording failures in the generated
# index rather than crashing; the caller decides exit status (see
# regen_rule_index.py's hard_fail handling). Double-run determinism:
# every function here is a pure function of its inputs and all output
# lists are deterministically ordered.
#
# Python 3 stdlib only. Designed to be imported by regen_rule_index.py.

import re

# --- Schema constants -------------------------------------------------------

EMPIRICAL_RUNGS = {"stated": 0, "code-read": 1, "validated": 2}
FORMAL_RUNGS = {"unmechanized": 0, "mechanized": 1}

# Legal keyword-line values, per keyword (mid-migration both keywords are
# live; STATUS ⊎ CLAIM = 453 is the census module's check, not ours).
LEGAL_VALUES = {
    "CLAIM": frozenset({"normative", "descriptive", "interpretive"}),
    "STATUS": frozenset({"normative", "descriptive", "conjectured"}),
}

# CAVEAT kinds and the display flags they induce. watch(W-nn) and
# disclosure are recorded but induce no display flag.
CAVEAT_FLAG = {
    "known-false": "false-as-stated",
    "compiler-bug": "compiler-bug",
    "pending-upstream": "pending-upstream",
}
WATCH_KIND_RE = re.compile(r"^watch\(W-\d+\)$")
FLAGLESS_CAVEAT_KINDS = frozenset({"disclosure"})

# Flags that demote an otherwise-A rule (record 76's letter says "no
# flags"; the FIDELITY review-log ruling filed the precise fix: only
# false-as-stated and pending-upstream undermine current-truth;
# compiler-bug displays without demoting).
DEMOTING_FLAGS = frozenset({"false-as-stated", "pending-upstream"})

# Artifact kinds that can carry mechanization. Documented anchors are
# excluded by ruling (else the mechanized rung is uniformly true across
# all 453 and stops discriminating). "claim-definition" is the
# REFUTED-CONJECTURE variant's stated-not-asserted Prop: it asserts
# nothing, so it is not mechanization either.
MECHANIZABLE_ARTIFACT_KINDS = frozenset({"theorem", "constructor", "equation"})
ARTIFACT_KINDS = MECHANIZABLE_ARTIFACT_KINDS | frozenset(
    {"documented-anchor", "claim-definition"}
)

COMMIT_RE = re.compile(r"^[0-9a-f]{7,40}$")
RULE_ID_RE = re.compile(r"^[A-Z][A-Za-z0-9]*(?:\.[A-Za-z0-9][A-Za-z0-9-]*)+$")

GRADE_RANK = {"D": 0, "C": 1, "B": 2, "A": 3}
_RANK_GRADE = {v: k for k, v in GRADE_RANK.items()}

# --- Failure records --------------------------------------------------------


def _fail(failures, check, detail, rule_id=None, where=None, severity="error"):
    failures.append(
        dict(
            check=check,
            severity=severity,
            id=rule_id,
            where=where,
            detail=detail,
        )
    )


def _sorted_failures(failures):
    def key(f):
        return (
            0 if f["severity"] == "error" else 1,
            f["check"],
            f["id"] or "",
            f["where"] or "",
            f["detail"],
        )

    return sorted(failures, key=key)


# --- FIDELITY.md RULES: headers ---------------------------------------------

_FINDING_HEADING_RE = re.compile(r"^###\s+((?:KF-\d+|Finding\b).*?)\s*$")
# Headings that look like findings but fail the strict syntax (typo'd id,
# wrong # level, wrong case) would silently become plain section
# boundaries and drop out of the DISPUTED derivation; warn instead.
_FINDING_ISH_RE = re.compile(r"^#{1,3}\s+(?:KF-|Finding\b)", re.IGNORECASE)
_RULES_HEADER_RE = re.compile(r"^\s*(?:[-*]\s*)?RULES:\s*(.*?)\s*$")
_STATUS_LINE_RE = re.compile(r"^\s*[-*]\s*Status:\s*(\S+)")
_CLOSED_STATUS_WORDS = frozenset(
    {"resolved", "closed", "fixed", "retired", "withdrawn", "discharged", "superseded"}
)


def _blank_html_comments(lines):
    """Replace the content of <!-- --> blocks with empty lines, keeping
    line numbers stable. A line sharing text with a comment marker is
    blanked whole — fine for this file's block-comment usage (templates).
    Comments are not data: nothing inside one can open a finding entry,
    act as a RULES:/Status: header, or attach to the preceding entry.
    Returns (lines, blanked) where blanked is [(0-based index, original)]
    so the caller can still warn about entry-shaped commented content."""
    out = []
    blanked = []
    in_comment = False
    for i, line in enumerate(lines):
        opens = "<!--" in line
        if in_comment or opens:
            out.append("")
            blanked.append((i, line))
            if in_comment and "-->" in line:
                in_comment = False
            elif opens and "-->" not in line:
                in_comment = True
        else:
            out.append(line)
    return out, blanked


def parse_fidelity_rules_text(text, where="FIDELITY.md"):
    """Parse the finding entries of FIDELITY.md (given as a string) into
    (findings, failures). HTML comment blocks are ignored entirely.

    A finding entry is a `### KF-nnn — …` or `### Finding … — …` heading
    plus everything up to the next heading. Each entry must carry exactly
    one `RULES: <id>, …` or `RULES: none` header (record 76 makes these
    load-bearing for the DISPUTED derivation). Each finding dict has:
      finding_id : str  (e.g. "KF-031", or the full pre-em-dash title)
      title      : str  (heading text)
      line       : int  (1-based heading line)
      rules      : list of rule ids (empty for RULES: none or on error)
      has_rules_header : bool
      open       : bool (True unless a `- Status:` line's first word is a
                   closed-status word; the LAST Status line wins)
    """
    lines, commented = _blank_html_comments(text.split("\n"))
    findings = []
    failures = []
    # A commented-out REAL entry must not vanish silently: warn on any
    # strict finding heading inside a comment. Templates stay clean
    # because their placeholder ids (KF-NNN) fail the strict regex.
    for i, line in commented:
        if _FINDING_HEADING_RE.match(line):
            _fail(
                failures,
                "fidelity-heading-syntax",
                f"commented-out finding heading is ignored by the scan "
                f"(comments are not data): {line.strip()!r}",
                where=f"{where}:{i + 1}",
                severity="warning",
            )
    # Locate finding headings.
    heads = []
    for i, line in enumerate(lines):
        m = _FINDING_HEADING_RE.match(line)
        if m:
            heads.append((i, m.group(1)))
        elif re.match(r"^#{1,3}\s", line):
            if _FINDING_ISH_RE.match(line):
                _fail(
                    failures,
                    "fidelity-heading-syntax",
                    f"heading looks like a finding but fails the id syntax; "
                    f"treated as a plain section boundary, so it cannot "
                    f"drive DISPUTED: {line.strip()!r}",
                    where=f"{where}:{i + 1}",
                    severity="warning",
                )
            heads.append((i, None))  # non-finding heading: entry boundary
    heads.append((len(lines), None))

    for h, (i, title) in enumerate(heads[:-1]):
        if title is None:
            continue
        body = lines[i + 1 : heads[h + 1][0]]
        loc = f"{where}:{i + 1}"
        finding_id = title.split(" — ")[0].split(" -- ")[0].strip()
        rules = []
        has_header = False
        for bl in body:
            m = _RULES_HEADER_RE.match(bl)
            if not m:
                continue
            if has_header:
                _fail(
                    failures,
                    "fidelity-rules-header",
                    f"{finding_id}: multiple RULES: headers in one entry",
                    rule_id=None,
                    where=loc,
                )
                break
            has_header = True
            value = m.group(1)
            if value.strip().lower() == "none":
                rules = []
            else:
                for tok in value.split(","):
                    tok = tok.strip().rstrip(".")
                    if not tok:
                        continue
                    if not RULE_ID_RE.match(tok):
                        _fail(
                            failures,
                            "fidelity-rules-header",
                            f"{finding_id}: malformed rule id {tok!r} in RULES: header",
                            where=loc,
                        )
                        continue
                    rules.append(tok)
        if not has_header:
            _fail(
                failures,
                "fidelity-rules-header",
                f"{finding_id}: finding entry has no RULES: header "
                "(use 'RULES: none' for findings targeting no rule)",
                where=loc,
            )
        is_open = True
        saw_status = False
        for bl in body:
            m = _STATUS_LINE_RE.match(bl)
            if m:
                saw_status = True
                word = m.group(1).strip().rstrip(":;,.—-").lower()
                is_open = word not in _CLOSED_STATUS_WORDS
        if not saw_status:
            _fail(
                failures,
                "fidelity-open-status",
                f"{finding_id}: no '- Status:' line; treating the finding as OPEN",
                where=loc,
                severity="warning",
            )
        findings.append(
            dict(
                finding_id=finding_id,
                title=title,
                line=i + 1,
                rules=sorted(set(rules)),
                has_rules_header=has_header,
                open=is_open,
            )
        )
    return findings, failures


def parse_fidelity_rules_headers(path):
    """Read FIDELITY.md from `path` and parse it; see
    parse_fidelity_rules_text."""
    with open(path) as fh:
        text = fh.read()
    return parse_fidelity_rules_text(text, where=path)


# --- Event validation -------------------------------------------------------


def _event_key(ev):
    """Canonical comparison key for a fence event (side-independent)."""
    return tuple(
        "" if ev.get(k) is None else str(ev.get(k))
        for k in ("kind", "clause", "study", "caveat_kind", "text", "commit")
    )


def _validate_events(fence, failures, commit_reachable=None):
    """Syntax-check a fence's events. Returns the list of events that may
    feed evidence derivation; malformed events are discarded (evidence is
    derived conservatively) and recorded as failures."""
    ok = []
    rid = fence.get("id")
    where = fence.get("source")
    for ev in fence.get("events") or []:
        kind = ev.get("kind")
        commit = ev.get("commit")
        if commit is not None and not COMMIT_RE.match(commit):
            _fail(
                failures,
                "event-commit-syntax",
                f"malformed commit key {commit!r} on {kind} event",
                rule_id=rid,
                where=where,
            )
            continue
        if (
            commit is not None
            and commit_reachable is not None
            and not commit_reachable(commit)
        ):
            _fail(
                failures,
                "event-commit-unreachable",
                f"commit {commit} on {kind} event is not reachable in the repo",
                rule_id=rid,
                where=where,
            )
            continue
        if kind == "CHECKED":
            if commit is None:
                _fail(
                    failures,
                    "event-commit-syntax",
                    "CHECKED event without '@ <commit>'",
                    rule_id=rid,
                    where=where,
                )
                continue
        elif kind == "VERIFIED":
            if not ev.get("study"):
                _fail(
                    failures,
                    "event-syntax",
                    "VERIFIED event without a study",
                    rule_id=rid,
                    where=where,
                )
                continue
            if commit is None:
                # Legacy rows predate the '@ <commit>' suffix; count them
                # but flag for the migration sweep.
                _fail(
                    failures,
                    "event-commit-syntax",
                    f"VERIFIED {ev.get('study')} without '@ <commit>' (legacy row)",
                    rule_id=rid,
                    where=where,
                    severity="warning",
                )
        elif kind == "CAVEAT":
            ck = ev.get("caveat_kind")
            if not (
                ck in CAVEAT_FLAG
                or ck in FLAGLESS_CAVEAT_KINDS
                or (ck is not None and WATCH_KIND_RE.match(ck))
            ):
                _fail(
                    failures,
                    "event-syntax",
                    f"unknown CAVEAT kind {ck!r}",
                    rule_id=rid,
                    where=where,
                )
                continue
        else:
            _fail(
                failures,
                "event-syntax",
                f"unknown event kind {kind!r}",
                rule_id=rid,
                where=where,
            )
            continue
        ok.append(ev)
    return ok


# --- Evidence and grade derivation ------------------------------------------


def _empirical_of_events(events):
    rung = 0
    for ev in events:
        if ev.get("kind") == "VERIFIED":
            rung = max(rung, EMPIRICAL_RUNGS["validated"])
        elif ev.get("kind") == "CHECKED":
            rung = max(rung, EMPIRICAL_RUNGS["code-read"])
    for name, r in EMPIRICAL_RUNGS.items():
        if r == rung:
            return name
    raise AssertionError("unreachable")


def _formal_of_artifact(artifact_kind, stamped, failures, rid, where):
    """The formal axis: mechanized iff the artifact is a real statement
    (not a documented anchor / claim-definition) AND the fidelity-review
    stamp is present (unreviewed Admitted certifies falsities)."""
    if artifact_kind is None:
        _fail(
            failures,
            "artifact-kind",
            "no artifact kind from the classifier; treating as unmechanized",
            rule_id=rid,
            where=where,
        )
        return "unmechanized"
    if artifact_kind not in ARTIFACT_KINDS:
        _fail(
            failures,
            "artifact-kind",
            f"unknown artifact kind {artifact_kind!r}; treating as unmechanized",
            rule_id=rid,
            where=where,
        )
        return "unmechanized"
    if artifact_kind in MECHANIZABLE_ARTIFACT_KINDS and stamped:
        return "mechanized"
    return "unmechanized"


def derive_grade(empirical, formal, flags):
    """A/B/C/D from the two axes, monotone; demoting flags gate A only."""
    validated = EMPIRICAL_RUNGS[empirical] >= EMPIRICAL_RUNGS["validated"]
    code_read = EMPIRICAL_RUNGS[empirical] >= EMPIRICAL_RUNGS["code-read"]
    mechanized = FORMAL_RUNGS[formal] >= FORMAL_RUNGS["mechanized"]
    if validated and mechanized and not (DEMOTING_FLAGS & set(flags)):
        return "A"
    if validated or (code_read and mechanized):
        return "B"
    if code_read or mechanized:
        return "C"
    return "D"


# --- The join ----------------------------------------------------------------


def _index_by_id(fences, side, failures):
    """Index fences by id; duplicate ids (an extractor pre-dedup
    obligation we defend against) poison the id for this side."""
    by_id = {}
    poisoned = set()
    for f in fences:
        rid = f.get("id")
        if rid is None:
            _fail(
                failures,
                "fence-shape",
                f"{side}-side fence with no id",
                where=f.get("source"),
            )
            continue
        if rid in by_id:
            _fail(
                failures,
                "duplicate-id",
                f"duplicate {side}-side fence for {rid} "
                f"(at {by_id[rid].get('source')} and {f.get('source')}); "
                "extractor should have pre-deduped",
                rule_id=rid,
                where=f.get("source"),
            )
            poisoned.add(rid)
            continue
        by_id[rid] = f
    return by_id, poisoned


def _join_failed_row(rid, join_error):
    return dict(
        id=rid,
        keyword=None,
        claim=None,
        doc_source=None,
        v_source=None,
        artifact_kind=None,
        qed=False,
        empirical=None,
        formal=None,
        badges=[],
        flags=[],
        clause_evidence=None,
        disputed_by=[],
        grade=None,
        suspended_letter=None,
        join_error=join_error,
    )


def join_and_grade(
    doc_fences,
    v_fences,
    findings,
    known_ids=None,
    hybrid_ids=None,
    stamped_ids=(),
    stale_ids=(),
    require_event_agreement=True,
    commit_reachable=None,
):
    """Join doc-side and .v-side fences, derive evidence and grades.

    Arguments:
      doc_fences, v_fences : lists of fence dicts. Each fence:
          id       : rule id
          keyword  : "CLAIM" or "STATUS" (exactly one keyword line per
                     fence — an extractor obligation)
          value    : the keyword's value, byte-exact
          source   : "file:line" provenance for failure reports
          events   : list of event dicts, each with
                     kind ∈ {CHECKED, VERIFIED, CAVEAT},
                     and (per kind) commit, study, caveat_kind, text,
                     clause (clause label, HYBRID rules only)
        v-side fences additionally carry (from the artifact-kind
        classifier):
          artifact_kind : "theorem" | "constructor" | "equation" |
                          "documented-anchor" | "claim-definition"
          qed      : bool (display badge input only)
          clauses  : optional {clause: {"artifact_kind":…, "qed":…}} for
                     HYBRID rules whose clauses map to distinct artifacts
      findings : output of parse_fidelity_rules_headers (drives DISPUTED)
      known_ids : optional iterable of all legal rule ids (defaults to
        the ids seen in the fences) — RULES: headers naming ids outside
        this set are errors
      hybrid_ids : the HYBRID-listed ids (Traceability ground truth) —
        either an iterable of ids, or a dict id → iterable of clause
        names (clause names let clauses with no evidence yet drag the
        grade down, as "grade at the weakest clause" requires)
      stamped_ids : ids carrying the fidelity-review stamp ("mechanized"
        requires it)
      stale_ids : ids whose evidence is stale (a CODE-anchored file
        changed since the evidence commit — computed upstream); adds a
        "stale" flag, display-only
      require_event_agreement : when True (default) the join fails on any
        doc/.v event-line disagreement; when False the union of both
        sides' events feeds derivation
      commit_reachable : optional callable commit → bool for repo
        reachability of event commit keys (syntax is always checked here;
        reachability only when this is supplied)

    Returns dict(rules=[row…], failures=[…], ok=bool). Rows are sorted by
    id; `ok` is False iff any failure has severity "error". Rows for
    join-failed ids carry join_error and grade None (the join never picks
    a side).
    """
    failures = []
    if hybrid_ids is None:
        hybrid_map = {}
    elif isinstance(hybrid_ids, dict):
        hybrid_map = {k: tuple(v) for k, v in hybrid_ids.items()}
    else:
        hybrid_map = {k: () for k in hybrid_ids}
    stamped = frozenset(stamped_ids)
    stale = frozenset(stale_ids)

    doc_by_id, doc_poisoned = _index_by_id(doc_fences, "doc", failures)
    v_by_id, v_poisoned = _index_by_id(v_fences, ".v", failures)
    all_ids = sorted(set(doc_by_id) | set(v_by_id) | doc_poisoned | v_poisoned)

    known = set(known_ids) if known_ids is not None else set(all_ids)
    known |= set(all_ids)

    # DISPUTED derivation: open findings' RULES: headers, unknown ids are
    # errors (the headers are load-bearing for grading).
    disputed_by = {}
    for finding in findings:
        for rid in finding.get("rules") or []:
            if rid not in known:
                _fail(
                    failures,
                    "fidelity-unknown-rule",
                    f"{finding.get('finding_id')}: RULES: header names unknown "
                    f"rule id {rid}",
                    rule_id=rid,
                    where=f"FIDELITY.md:{finding.get('line')}",
                )
                continue
            if finding.get("open"):
                disputed_by.setdefault(rid, []).append(finding.get("finding_id"))

    rows = []
    for rid in all_ids:
        # -- Join checks: never pick a side. --------------------------------
        if rid in doc_poisoned or rid in v_poisoned:
            rows.append(_join_failed_row(rid, "duplicate fences (see failures)"))
            continue
        doc = doc_by_id.get(rid)
        v = v_by_id.get(rid)
        if doc is None or v is None:
            side = ".v" if doc is None else "doc"
            other = "doc" if doc is None else ".v"
            src = (v or doc).get("source")
            _fail(
                failures,
                "join-coverage",
                f"rule present on the {side} side only (no {other}-side fence)",
                rule_id=rid,
                where=src,
            )
            rows.append(_join_failed_row(rid, f"present on {side} side only"))
            continue
        if doc.get("keyword") != v.get("keyword") or doc.get("value") != v.get(
            "value"
        ):
            _fail(
                failures,
                "join-keyword-mismatch",
                f"doc says {doc.get('keyword')} {doc.get('value')!r} "
                f"({doc.get('source')}); .v says {v.get('keyword')} "
                f"{v.get('value')!r} ({v.get('source')}) — the join never "
                "picks a side",
                rule_id=rid,
                where=doc.get("source"),
            )
            rows.append(_join_failed_row(rid, "doc/.v keyword-line disagreement"))
            continue
        keyword, value = doc.get("keyword"), doc.get("value")
        if keyword not in LEGAL_VALUES:
            _fail(
                failures,
                "keyword-syntax",
                f"unknown keyword {keyword!r} (expected CLAIM or STATUS)",
                rule_id=rid,
                where=doc.get("source"),
            )
        elif value not in LEGAL_VALUES[keyword]:
            _fail(
                failures,
                "keyword-syntax",
                f"illegal {keyword} value {value!r} (legal: "
                f"{', '.join(sorted(LEGAL_VALUES[keyword]))})",
                rule_id=rid,
                where=doc.get("source"),
            )

        doc_events = _validate_events(doc, failures, commit_reachable)
        v_events = _validate_events(v, failures, commit_reachable)
        doc_keys = sorted(_event_key(ev) for ev in doc_events)
        v_keys = sorted(_event_key(ev) for ev in v_events)
        if doc_keys != v_keys:
            if require_event_agreement:
                only_doc = [k for k in doc_keys if k not in v_keys]
                only_v = [k for k in v_keys if k not in doc_keys]
                _fail(
                    failures,
                    "join-event-mismatch",
                    f"doc/.v event lines disagree (doc-only: {only_doc}; "
                    f".v-only: {only_v}) — the join never picks a side",
                    rule_id=rid,
                    where=doc.get("source"),
                )
                rows.append(_join_failed_row(rid, "doc/.v event-line disagreement"))
                continue
            # Union mode (explicitly requested): dedup by canonical key.
            seen = set()
            events = []
            for ev in doc_events + v_events:
                k = _event_key(ev)
                if k not in seen:
                    seen.add(k)
                    events.append(ev)
        else:
            events = doc_events

        # -- HYBRID discipline. ----------------------------------------------
        is_hybrid = rid in hybrid_map
        usable = []
        for ev in events:
            if is_hybrid and ev.get("kind") == "VERIFIED" and ev.get("clause") is None:
                _fail(
                    failures,
                    "hybrid-rule-level-verified",
                    f"rule-level VERIFIED {ev.get('study')} on HYBRID id "
                    f"{rid}: HYBRID ids demand clause-granular evidence; "
                    "event discarded",
                    rule_id=rid,
                    where=doc.get("source"),
                )
                continue
            if is_hybrid and ev.get("kind") == "CHECKED" and ev.get("clause") is None:
                _fail(
                    failures,
                    "hybrid-rule-level-checked",
                    f"rule-level CHECKED on HYBRID id {rid}: clause-granular "
                    "evidence expected; event not counted",
                    rule_id=rid,
                    where=doc.get("source"),
                    severity="warning",
                )
                continue
            if not is_hybrid and ev.get("clause") is not None:
                _fail(
                    failures,
                    "clause-on-non-hybrid",
                    f"clause-labeled {ev.get('kind')} event on non-HYBRID id "
                    f"{rid}; clause label ignored",
                    rule_id=rid,
                    where=doc.get("source"),
                    severity="warning",
                )
            usable.append(ev)

        # -- Flags (rule-level; caveats flag the rule whatever their clause).
        flags = set()
        for ev in usable:
            if ev.get("kind") == "CAVEAT":
                flag = CAVEAT_FLAG.get(ev.get("caveat_kind"))
                if flag:
                    flags.add(flag)
        if is_hybrid:
            flags.add("hybrid")
        if rid in stale:
            flags.add("stale")

        artifact_kind = v.get("artifact_kind")
        qed = bool(v.get("qed"))
        badges = []

        # -- Evidence + grade. -----------------------------------------------
        if is_hybrid:
            clause_names = set(hybrid_map[rid])
            clause_names |= {
                ev.get("clause") for ev in usable if ev.get("clause") is not None
            }
            v_clauses = v.get("clauses") or {}
            clause_names |= set(v_clauses)
            if not clause_names:
                _fail(
                    failures,
                    "hybrid-no-clauses",
                    f"HYBRID id {rid} with no clause inventory and no "
                    "clause-labeled evidence; grading at a single implicit "
                    "clause with no evidence",
                    rule_id=rid,
                    where=doc.get("source"),
                    severity="warning",
                )
                clause_names = {"(whole rule)"}
            clause_evidence = {}
            worst = None
            for cl in sorted(clause_names):
                cl_events = [ev for ev in usable if ev.get("clause") == cl]
                emp = _empirical_of_events(cl_events)
                cl_art = v_clauses.get(cl, {}).get("artifact_kind", artifact_kind)
                frm = _formal_of_artifact(
                    cl_art, rid in stamped, failures, rid, v.get("source")
                )
                g = derive_grade(emp, frm, flags)
                clause_evidence[cl] = dict(empirical=emp, formal=frm, grade=g)
                if worst is None or GRADE_RANK[g] < GRADE_RANK[worst[0]]:
                    worst = (g, emp, frm)
            letter, empirical, formal = worst
            if qed and any(
                ce["formal"] == "mechanized" for ce in clause_evidence.values()
            ):
                badges.append("proved-in-model (envelope)")
        else:
            clause_evidence = None
            empirical = _empirical_of_events(usable)
            formal = _formal_of_artifact(
                artifact_kind, rid in stamped, failures, rid, v.get("source")
            )
            letter = derive_grade(empirical, formal, flags)
            if qed and formal == "mechanized":
                badges.append("proved-in-model")

        rid_disputes = sorted(set(disputed_by.get(rid, [])))
        if rid_disputes:
            grade = "DISPUTED"
            suspended_letter = letter
        else:
            grade = letter
            suspended_letter = None

        rows.append(
            dict(
                id=rid,
                keyword=keyword,
                claim=value,
                doc_source=doc.get("source"),
                v_source=v.get("source"),
                artifact_kind=artifact_kind,
                qed=qed,
                empirical=empirical,
                formal=formal,
                badges=badges,
                flags=sorted(flags),
                clause_evidence=clause_evidence,
                disputed_by=rid_disputes,
                grade=grade,
                suspended_letter=suspended_letter,
                join_error=None,
            )
        )

    failures = _sorted_failures(failures)
    ok = not any(f["severity"] == "error" for f in failures)
    return dict(rules=rows, failures=failures, ok=ok)


def join_index(doc_fences, v_fences, fidelity_path, **kwargs):
    """Convenience wrapper: parse FIDELITY.md from `fidelity_path`, then
    join_and_grade. Merges the FIDELITY parse failures into the result's
    failure list."""
    findings, fid_failures = parse_fidelity_rules_headers(fidelity_path)
    result = join_and_grade(doc_fences, v_fences, findings, **kwargs)
    result["failures"] = _sorted_failures(fid_failures + result["failures"])
    result["ok"] = not any(f["severity"] == "error" for f in result["failures"])
    return result


# --- Display helpers (for regen_rule_index.py's table renderer) --------------


def render_grade(row):
    """One-cell rendering of a row's grade + flags + badges, e.g.
    'A [compiler-bug] (proved-in-model)' or 'DISPUTED (KF-051)' or
    '(join failed)'."""
    if row.get("join_error"):
        return "(join failed)"
    parts = [row["grade"]]
    if row["grade"] == "DISPUTED" and row.get("disputed_by"):
        parts.append("(" + ", ".join(row["disputed_by"]) + ")")
    if row.get("flags"):
        parts.append("[" + ", ".join(row["flags"]) + "]")
    for b in row.get("badges") or []:
        parts.append(f"({b})")
    return " ".join(parts)


def render_evidence(row):
    """One-cell rendering of the two axes, e.g. 'validated / mechanized';
    HYBRID rows show the weakest clause's pair (details in
    clause_evidence)."""
    if row.get("join_error"):
        return "—"
    return f"{row['empirical']} / {row['formal']}"


# --- Self-test ----------------------------------------------------------------

if __name__ == "__main__":
    import json

    CHECKS = [0]

    def check(cond, label):
        assert cond, label
        CHECKS[0] += 1

    def fence(
        rid,
        side,
        keyword="CLAIM",
        value="normative",
        events=None,
        artifact_kind=None,
        qed=False,
        clauses=None,
        source=None,
    ):
        f = dict(
            id=rid,
            keyword=keyword,
            value=value,
            source=source or f"{side}:{rid}",
            events=list(events or []),
        )
        if side == "v":
            f["artifact_kind"] = artifact_kind
            f["qed"] = qed
            if clauses is not None:
                f["clauses"] = clauses
        return f

    def pair(rid, events=None, artifact_kind="theorem", qed=False, clauses=None, **kw):
        return (
            fence(rid, "doc", events=events, **kw),
            fence(
                rid,
                "v",
                events=events,
                artifact_kind=artifact_kind,
                qed=qed,
                clauses=clauses,
                **kw,
            ),
        )

    def run(pairs, findings=(), **kw):
        docs = [d for d, _ in pairs]
        vs = [v for _, v in pairs]
        return join_and_grade(docs, vs, list(findings), **kw)

    def row(res, rid):
        (r,) = [r for r in res["rules"] if r["id"] == rid]
        return r

    def errs(res, chk=None):
        return [
            f
            for f in res["failures"]
            if f["severity"] == "error" and (chk is None or f["check"] == chk)
        ]

    COMMIT = "0123abc"
    CHECKED = dict(kind="CHECKED", commit=COMMIT)
    VERIFIED = dict(kind="VERIFIED", study="14-validation/x.md", commit=COMMIT)

    # 1. Grade A: validated x mechanized (stamped theorem), no flags.
    res = run([pair("A.Rule.One", events=[VERIFIED])], stamped_ids={"A.Rule.One"})
    r = row(res, "A.Rule.One")
    check(res["ok"], "A: no errors")
    check(r["grade"] == "A", "A: grade")
    check(r["empirical"] == "validated" and r["formal"] == "mechanized", "A: axes")
    check(r["badges"] == [], "A: no badge without Qed")

    # 2. Qed is a display badge only: stamped+Qed adds the badge, no grade
    #    change; unstamped Qed theorem is NOT mechanized (no badge, B via
    #    validated alone).
    res = run(
        [pair("A.Rule.Qed", events=[VERIFIED], qed=True)], stamped_ids={"A.Rule.Qed"}
    )
    r = row(res, "A.Rule.Qed")
    check(r["grade"] == "A" and r["badges"] == ["proved-in-model"], "Qed badge")
    res = run([pair("A.Rule.UnstampedQed", events=[VERIFIED], qed=True)])
    r = row(res, "A.Rule.UnstampedQed")
    check(
        r["formal"] == "unmechanized" and r["grade"] == "B" and r["badges"] == [],
        "unstamped Qed: unreviewed mechanization does not count",
    )

    # 3. B both ways; anchors are excluded from mechanization.
    res = run(
        [pair("B.Rule.ValOnly", events=[VERIFIED], artifact_kind="documented-anchor")]
    )
    check(row(res, "B.Rule.ValOnly")["grade"] == "B", "B: validated alone")
    check(
        row(res, "B.Rule.ValOnly")["formal"] == "unmechanized",
        "anchor excluded from mechanized",
    )
    res = run([pair("B.Rule.CodeMech", events=[CHECKED])], stamped_ids={"B.Rule.CodeMech"})
    check(row(res, "B.Rule.CodeMech")["grade"] == "B", "B: code-read x mechanized")

    # 4. C: exactly one axis.
    res = run([pair("C.Rule.CodeOnly", events=[CHECKED], artifact_kind="documented-anchor")])
    check(row(res, "C.Rule.CodeOnly")["grade"] == "C", "C: code-read only")
    res = run([pair("C.Rule.MechOnly", artifact_kind="constructor")],
              stamped_ids={"C.Rule.MechOnly"})
    r = row(res, "C.Rule.MechOnly")
    check(r["grade"] == "C" and r["empirical"] == "stated", "C: stated x mechanized")

    # 5. D: neither. Claim-definition (REFUTED-CONJECTURE) asserts nothing.
    res = run([pair("D.Rule.Bare", artifact_kind="claim-definition")],
              stamped_ids={"D.Rule.Bare"})
    check(row(res, "D.Rule.Bare")["grade"] == "D", "D: neither axis")

    # 6. Flags: known-false / pending-upstream demote A; compiler-bug
    #    displays without demoting.
    KF = dict(kind="CAVEAT", caveat_kind="known-false", text="t")
    CB = dict(kind="CAVEAT", caveat_kind="compiler-bug", text="t")
    PU = dict(kind="CAVEAT", caveat_kind="pending-upstream", text="t")
    W = dict(kind="CAVEAT", caveat_kind="watch(W-35)", text="t")
    res = run([pair("F.Rule.KF", events=[VERIFIED, KF])], stamped_ids={"F.Rule.KF"})
    r = row(res, "F.Rule.KF")
    check(r["grade"] == "B" and "false-as-stated" in r["flags"], "known-false demotes A")
    res = run([pair("F.Rule.CB", events=[VERIFIED, CB])], stamped_ids={"F.Rule.CB"})
    r = row(res, "F.Rule.CB")
    check(r["grade"] == "A" and "compiler-bug" in r["flags"],
          "compiler-bug displays without demoting")
    res = run([pair("F.Rule.PU", events=[VERIFIED, PU])], stamped_ids={"F.Rule.PU"})
    check(row(res, "F.Rule.PU")["grade"] == "B", "pending-upstream demotes A")
    res = run([pair("F.Rule.W", events=[VERIFIED, W])], stamped_ids={"F.Rule.W"})
    r = row(res, "F.Rule.W")
    check(res["ok"] and r["grade"] == "A" and r["flags"] == [],
          "watch caveat: recorded, no flag, no error")

    # 7. Unknown caveat kind is an error; the event is discarded.
    BAD = dict(kind="CAVEAT", caveat_kind="mystery", text="t")
    res = run([pair("F.Rule.Bad", events=[VERIFIED, BAD])], stamped_ids={"F.Rule.Bad"})
    check(errs(res, "event-syntax"), "unknown caveat kind is an error")
    check(row(res, "F.Rule.Bad")["grade"] == "A", "bad caveat discarded, not flagged")

    # 8. DISPUTED: open finding naming the id suspends the letter; closed
    #    findings do not.
    open_f = dict(finding_id="KF-900", title="KF-900 — t", line=1,
                  rules=["A.Rule.One"], has_rules_header=True, open=True)
    closed_f = dict(finding_id="KF-901", title="KF-901 — t", line=2,
                    rules=["A.Rule.One"], has_rules_header=True, open=False)
    res = run([pair("A.Rule.One", events=[VERIFIED])], findings=[open_f, closed_f],
              stamped_ids={"A.Rule.One"})
    r = row(res, "A.Rule.One")
    check(r["grade"] == "DISPUTED" and r["suspended_letter"] == "A"
          and r["disputed_by"] == ["KF-900"], "DISPUTED suspends the letter")
    res = run([pair("A.Rule.One", events=[VERIFIED])], findings=[closed_f],
              stamped_ids={"A.Rule.One"})
    check(row(res, "A.Rule.One")["grade"] == "A", "closed finding does not dispute")

    # 9. Unknown id in a RULES: header is an error.
    ghost = dict(finding_id="KF-902", title="KF-902 — t", line=3,
                 rules=["No.Such.Rule"], has_rules_header=True, open=True)
    res = run([pair("A.Rule.One", events=[VERIFIED])], findings=[ghost])
    check(errs(res, "fidelity-unknown-rule"), "unknown id in RULES: header")

    # 10. HYBRID: clause-granular evidence, graded at the weakest clause,
    #     explicit hybrid flag; rule-level VERIFIED is an ERROR.
    CH1 = dict(kind="CHECKED", commit=COMMIT, clause="c1")
    V2 = dict(kind="VERIFIED", study="14-validation/y.md", commit=COMMIT, clause="c2")
    hyb = {"H.Rule.Mix": ["c1", "c2"]}
    res = run(
        [pair("H.Rule.Mix", events=[CH1, V2],
              clauses={"c1": dict(artifact_kind="theorem"),
                       "c2": dict(artifact_kind="documented-anchor")})],
        hybrid_ids=hyb, stamped_ids={"H.Rule.Mix"})
    r = row(res, "H.Rule.Mix")
    check(r["grade"] == "B" and "hybrid" in r["flags"], "hybrid: weakest clause B")
    check(r["clause_evidence"]["c1"]["grade"] == "B"
          and r["clause_evidence"]["c2"]["grade"] == "B", "hybrid: per-clause grades")
    # An evidence-free clause drags the rule to its grade.
    res = run(
        [pair("H.Rule.Mix", events=[CH1],
              clauses={"c1": dict(artifact_kind="theorem"),
                       "c2": dict(artifact_kind="documented-anchor")})],
        hybrid_ids=hyb, stamped_ids={"H.Rule.Mix"})
    r = row(res, "H.Rule.Mix")
    check(r["grade"] == "D" and r["clause_evidence"]["c2"]["grade"] == "D",
          "hybrid: evidence-free clause is the weakest")
    res = run([pair("H.Rule.Mix", events=[VERIFIED])], hybrid_ids=hyb,
              stamped_ids={"H.Rule.Mix"})
    check(errs(res, "hybrid-rule-level-verified"),
          "rule-level VERIFIED on a HYBRID id is an ERROR")
    check(row(res, "H.Rule.Mix")["empirical"] == "stated",
          "the discarded rule-level VERIFIED is never counted")

    # 11. Join failures: the join never picks a side.
    d, v = pair("J.Rule.Val")
    v["value"] = "descriptive"
    res = run([(d, v)])
    r = row(res, "J.Rule.Val")
    check(errs(res, "join-keyword-mismatch") and r["grade"] is None
          and r["claim"] is None, "value mismatch fails the join")
    d, v = pair("J.Rule.Kw")
    v["keyword"], v["value"] = "STATUS", "normative"
    res = run([(d, v)])
    check(errs(res, "join-keyword-mismatch"), "keyword mismatch fails the join")
    d, _ = pair("J.Rule.DocOnly")
    res = join_and_grade([d], [], [])
    check(errs(res, "join-coverage")
          and row(res, "J.Rule.DocOnly")["join_error"] is not None,
          "doc-only rule fails the join")
    d, v = pair("J.Rule.Ev", events=[VERIFIED])
    d["events"] = [VERIFIED, CHECKED]
    res = run([(d, v)])
    check(errs(res, "join-event-mismatch"), "event mismatch fails the join")
    res = run([(d, v)], require_event_agreement=False, stamped_ids={"J.Rule.Ev"})
    check(row(res, "J.Rule.Ev")["grade"] == "A",
          "union mode (opt-in) merges events instead")

    # 12. Duplicate fences poison the id.
    d1, v1 = pair("J.Rule.Dup", events=[VERIFIED])
    d2, _ = pair("J.Rule.Dup", events=[VERIFIED])
    res = join_and_grade([d1, d2], [v1], [])
    check(errs(res, "duplicate-id")
          and row(res, "J.Rule.Dup")["grade"] is None, "duplicate id poisons the rule")

    # 13. Commit-key hygiene: malformed commit is an error and the event is
    #     discarded; VERIFIED without a commit is a legacy-row warning that
    #     still counts; reachability hook.
    BADC = dict(kind="CHECKED", commit="not-a-sha")
    res = run([pair("K.Rule.BadCommit", events=[BADC])])
    check(errs(res, "event-commit-syntax"), "malformed commit is an error")
    check(row(res, "K.Rule.BadCommit")["empirical"] == "stated",
          "malformed event does not feed evidence")
    LEGACY = dict(kind="VERIFIED", study="14-validation/x.md")
    res = run([pair("K.Rule.Legacy", events=[LEGACY])])
    check(res["ok"] and row(res, "K.Rule.Legacy")["empirical"] == "validated",
          "legacy VERIFIED counts with a warning")
    check(any(f["check"] == "event-commit-syntax" and f["severity"] == "warning"
              for f in res["failures"]), "legacy VERIFIED warns")
    res = run([pair("K.Rule.Unreach", events=[CHECKED])],
              commit_reachable=lambda c: False)
    check(errs(res, "event-commit-unreachable"), "unreachable commit is an error")

    # 14. Illegal keyword values.
    res = run([pair("L.Rule.BadClaim", value="conjectured")])
    check(errs(res, "keyword-syntax"),
          "conjectured is not a legal CLAIM value (it dissolved)")
    res = run([pair("L.Rule.OldStatus", keyword="STATUS", value="conjectured")])
    check(not errs(res, "keyword-syntax"),
          "STATUS conjectured stays legal mid-migration")

    # 15. Stale flag is display-only.
    res = run([pair("M.Rule.Stale", events=[VERIFIED])],
              stamped_ids={"M.Rule.Stale"}, stale_ids={"M.Rule.Stale"})
    r = row(res, "M.Rule.Stale")
    check(r["grade"] == "A" and "stale" in r["flags"], "stale flag display-only")

    # 16. FIDELITY.md parsing over a synthetic fixture, including the
    #     failure cases.
    FIDELITY_FIXTURE = """# Fidelity review log

## Findings

### KF-101 — a finding with rule targets (high)
- RULES: A.Rule.One, H.Rule.Mix
- Chapter: 04-opsem.md:1.
- What differs: something.

### KF-102 — a resolved finding (low)
- RULES: A.Rule.One
- Chapter: 04-opsem.md:2.
- Status: resolved — fixed upstream.

### Finding #16 verification — a doc-level finding
- RULES: none
- Chapter: 10-simplify-rewrites.md:3.

### KF-103 — missing its RULES header (medium)
- Chapter: 04-opsem.md:4.

### KF-104 — malformed id in header (low)
- RULES: lowercase.bad.id
- Status: open — under review.

## Watch items (not findings)

- W-33: not an entry.
"""
    findings, ffail = parse_fidelity_rules_text(FIDELITY_FIXTURE, where="fix.md")
    by_fid = {f["finding_id"]: f for f in findings}
    check(len(findings) == 5, "fidelity: five entries found")
    check(by_fid["KF-101"]["rules"] == ["A.Rule.One", "H.Rule.Mix"]
          and by_fid["KF-101"]["open"], "fidelity: rules parsed, open by default")
    check(not by_fid["KF-102"]["open"], "fidelity: Status resolved closes")
    check(by_fid["Finding #16 verification"]["rules"] == []
          and by_fid["Finding #16 verification"]["has_rules_header"],
          "fidelity: RULES: none")
    check(any(f["check"] == "fidelity-rules-header" and "KF-103" in f["detail"]
              for f in ffail), "fidelity: missing RULES header is an error")
    check(any(f["check"] == "fidelity-rules-header" and "KF-104" in f["detail"]
              for f in ffail), "fidelity: malformed id is an error")
    # The open finding disputes; the resolved one does not.
    res = run([pair("A.Rule.One", events=[VERIFIED]),
               pair("H.Rule.Mix", events=[VERIFIED])],
              findings=findings, stamped_ids={"A.Rule.One"})
    check(row(res, "A.Rule.One")["disputed_by"] == ["KF-101"],
          "fidelity fixture drives DISPUTED")
    # A finding-ish heading with a bad id must warn, not silently become
    # a plain boundary (it still parses as no finding).
    f2, ff2 = parse_fidelity_rules_text(
        "### KF-abc — typo'd id\nRULES: A.Rule.One\n- Status: open.\n",
        where="fix2.md",
    )
    check(len(f2) == 0, "fidelity: finding-ish heading yields no finding")
    check(any(f["check"] == "fidelity-heading-syntax"
              and f["severity"] == "warning" for f in ff2),
          "fidelity: finding-ish heading warns")
    # HTML comments are not data: a template inside <!-- --> must
    # neither open a finding, nor warn, nor attach its RULES:/Status:
    # lines to the preceding entry.
    f3, ff3 = parse_fidelity_rules_text(
        "### KF-105 — real (low)\n"
        "RULES: none\n"
        "- Status: resolved — done.\n"
        "\n"
        "<!-- Finding template:\n"
        "### KF-NNN — <RULE id> (<severity>)\n"
        "RULES: <ids or none>\n"
        "- Status: open — <disposition>\n"
        "-->\n",
        where="fix3.md",
    )
    check(len(f3) == 1 and not f3[0]["open"] and ff3 == [],
          "fidelity: comment blocks are ignored entirely")
    # But a commented-out REAL entry (strict heading) warns rather than
    # silently vanishing from the scan.
    f4, ff4 = parse_fidelity_rules_text(
        "### KF-106 — real (low)\n"
        "RULES: none\n"
        "- Status: resolved — done.\n"
        "\n"
        "<!--\n"
        "### KF-107 — commented-out entry (high)\n"
        "RULES: A.Rule.One\n"
        "- Status: open — hidden.\n"
        "-->\n",
        where="fix4.md",
    )
    check(len(f4) == 1 and f4[0]["finding_id"] == "KF-106"
          and any(f["check"] == "fidelity-heading-syntax"
                  and "KF-107" in f["detail"] for f in ff4),
          "fidelity: commented-out real entry warns, does not vanish")

    # 17. Double-run determinism (an explicit record-76 obligation).
    def big_run():
        pairs = [
            pair("A.Rule.One", events=[VERIFIED]),
            pair("H.Rule.Mix", events=[CH1, V2],
                 clauses={"c1": dict(artifact_kind="theorem"),
                          "c2": dict(artifact_kind="documented-anchor")}),
            pair("F.Rule.CB", events=[VERIFIED, CB]),
            pair("D.Rule.Bare", artifact_kind="documented-anchor"),
        ]
        return run(pairs, findings=findings, hybrid_ids=hyb,
                   stamped_ids={"A.Rule.One", "H.Rule.Mix"},
                   stale_ids={"F.Rule.CB"})

    r1, r2 = big_run(), big_run()
    check(json.dumps(r1, sort_keys=True) == json.dumps(r2, sort_keys=True),
          "double-run determinism")

    # 18. Display helpers.
    res = big_run()
    check(render_grade(row(res, "A.Rule.One")) == "DISPUTED (KF-101)",
          "render_grade: disputed")
    check(render_evidence(row(res, "F.Rule.CB")) == "validated / unmechanized",
          "render_evidence")
    check(render_grade(row(res, "F.Rule.CB")) == "B [compiler-bug, stale]",
          "render_grade: flags")

    print(f"solidity_join self-test: {CHECKS[0]} checks passed.")
