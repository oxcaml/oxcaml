#!/usr/bin/env python3
# Written by Claude.
#
# artifact_classifier: classify the Rocq artifact kind of every RULE id in the
# flambda2 formalism mechanization (rocq/theories/*.v) and cross-check the
# classification against the Traceability instance lists in
# rocq/CORRESPONDENCE.md.
#
# Per record 76 of the correspondence catalog, the Traceability instance lists
# (EXCEPTION / HYBRID / ENVELOPE-QED / DESCRIPTIVE-AS-DEFINING-CLAUSE /
# REFUTED-CONJECTURE) are ground truth: any classifier-vs-list mismatch is an
# error, in both directions (parser and catalog mutually checking).
#
# Artifact kinds (one per RULE comment, from the declaration that follows it):
#   defining          -- constructor of an Inductive (including the first,
#                        un-piped one and `with` clauses of mutual blocks), or
#                        an Inductive/Record/Fixpoint/Definition equation
#   theorem-admitted  -- Theorem/Lemma/... whose proof ends in Admitted.
#   theorem-qed       -- Theorem/Lemma/... whose proof ends in Qed. (or
#                        Defined.)
#   documented-anchor -- Definition <id>_documented : Prop := True.
#   demoted-claim     -- Definition <id>_claim : Prop := ...  (unasserted;
#                        the REFUTED-CONJECTURE variant)
#   unknown           -- anything else (always accompanied by a failure)
#
# Headerless obligation kernels (Theorems/Lemmas with no RULE comment, e.g.
# TC_Switch_TestByValue) never appear in the output: classification keys off
# RULE comments only, and a kernel between a RULE artifact and the next RULE
# comment is never mistaken for the artifact (the artifact is always the
# first declaration after its comment).
#
# Designed to be imported by regen_rule_index.py. Data problems are returned
# as Failure records (the pipeline records failures in the index rather than
# crashing); this module raises only on I/O errors (missing directory etc.).
#
# Python 3 stdlib only. No paths are hardcoded: callers pass theories_dir and
# correspondence_path (regen_rule_index.py derives them from its own
# location).
#
# Entry points:
#   classify(theories_dir, correspondence_path) -> (artifacts, failures)
#   scan_theories(theories_dir)                 -> (artifacts, failures)
#   parse_traceability(correspondence_path)     -> (variants, failures)
#   crosscheck(artifacts, variants)             -> failures
#
# An artifact is a dict:
#   {"id": "T.Meet.Sound", "file": "MeetJoin.v", "line": 889,
#    "keyword": "STATUS" | "CLAIM" | None,
#    "kind": "normative" | "descriptive" | "conjectured" | "interpretive"
#            | None,
#    "artifact": one of the kinds above, "decl": raw following token,
#    "decl_name": declared Rocq name or None,
#    "variants": sorted list of sanctioned-variant tags claiming this id}
#
# A Failure is a namedtuple (check, rule_id, where, detail); rule_id may be
# None for file- or catalog-level problems.

import fnmatch
import glob
import os
import re
from collections import Counter, namedtuple

Failure = namedtuple("Failure", ["check", "rule_id", "where", "detail"])

# Artifact kinds.
DEFINING = "defining"
THEOREM_ADMITTED = "theorem-admitted"
THEOREM_QED = "theorem-qed"
DOCUMENTED_ANCHOR = "documented-anchor"
DEMOTED_CLAIM = "demoted-claim"
UNKNOWN = "unknown"

# Variant tags (keys of the dict returned by parse_traceability).
V_EXCEPTION = "exception"
V_HYBRID = "hybrid"
V_ENVELOPE_QED = "envelope-qed"
V_DESCRIPTIVE_AS_DEFINING = "descriptive-as-defining"
V_REFUTED_CONJECTURE = "refuted-conjecture"
V_PROVIDED_PROP = "provided-prop"
V_REFLEXIVITY_QED = "reflexivity-qed"
ALL_VARIANTS = [
    V_EXCEPTION,
    V_HYBRID,
    V_ENVELOPE_QED,
    V_DESCRIPTIVE_AS_DEFINING,
    V_REFUTED_CONJECTURE,
    V_PROVIDED_PROP,
    V_REFLEXIVITY_QED,
]

# Claim-kind vocabulary per keyword (mid-migration: STATUS is the retired
# keyword, CLAIM the record-76 one; both are legal until the sweep finishes).
KINDS_BY_KEYWORD = {
    "STATUS": {"normative", "descriptive", "conjectured"},
    "CLAIM": {"normative", "descriptive", "interpretive"},
}

_THEOREM_KEYWORDS = {
    "Theorem",
    "Lemma",
    "Corollary",
    "Proposition",
    "Fact",
    "Remark",
}
_DEFINING_KEYWORDS = {
    "Inductive",
    "CoInductive",
    "Variant",
    "Record",
    "Structure",
    "Fixpoint",
    "CoFixpoint",
    "with",
}
_AXIOM_KEYWORDS = {"Parameter", "Parameters", "Axiom", "Axioms", "Conjecture", "Hypothesis"}

_RULE_COMMENT_RE = re.compile(r"\(\*\*\s*RULE\s+([A-Za-z0-9.]+)")
_HEADER_RE = re.compile(
    r"\(\*\*\s*RULE\s+[A-Za-z0-9.]+\s*\(\s*(STATUS|CLAIM)\s+([a-z-]+)\s*\)"
)
_TERMINATOR_RE = re.compile(r"\b(Admitted|Qed|Defined|Abort)\s*\.")
_ANCHOR_BODY_RE = re.compile(r"^[^.]*?:\s*Prop\s*:=\s*True\s*\.", re.S)

_NUMBER_WORDS = {
    "one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6,
    "seven": 7, "eight": 8, "nine": 9, "ten": 10, "eleven": 11, "twelve": 12,
}


def rocq_name(rule_id):
    """Rule id -> Rocq identifier (dots to underscores)."""
    return rule_id.replace(".", "_")


# --- Rocq comment/declaration lexing ----------------------------------------


def _comment_end(text, i):
    """text[i:i+2] == '(*'. Return the index just past the matching '*)'
    (nesting-aware), or len(text) if unterminated."""
    depth = 0
    n = len(text)
    while i < n:
        if text.startswith("(*", i):
            depth += 1
            i += 2
        elif text.startswith("*)", i):
            depth -= 1
            i += 2
            if depth == 0:
                return i
        else:
            i += 1
    return n


def _next_decl(text, i):
    """Skip whitespace and non-RULE comments from index i. Return
    (decl_index, None) at the first declaration character, or (None, reason)
    if another RULE comment or EOF intervenes before any declaration."""
    n = len(text)
    while i < n:
        while i < n and text[i] in " \t\r\n":
            i += 1
        if i >= n:
            return None, "end of file"
        if text.startswith("(*", i):
            if _RULE_COMMENT_RE.match(text, i):
                return None, "another RULE comment (no declaration between)"
            i = _comment_end(text, i)
            continue
        return i, None
    return None, "end of file"


def _classify_decl(text, decl_start, bound, fname, line, rule_id, failures):
    """Classify the declaration at text[decl_start:bound]. Returns
    (artifact, decl_token, decl_name)."""
    seg = text[decl_start:bound]
    if seg.startswith("|"):
        return DEFINING, "|", None
    m = re.match(r"[A-Za-z_][A-Za-z0-9_']*", seg)
    if not m:
        failures.append(
            Failure(
                "artifact", rule_id, "%s:%d" % (fname, line),
                "unparsable text after RULE comment: %r" % seg[:40],
            )
        )
        return UNKNOWN, None, None
    tok = m.group(0)
    rest = seg[m.end():]
    if tok in _THEOREM_KEYWORDS:
        nm = re.match(r"\s+([A-Za-z_][A-Za-z0-9_']*)", rest)
        name = nm.group(1) if nm else None
        t = _TERMINATOR_RE.search(seg)
        if t is None or t.group(1) == "Abort":
            failures.append(
                Failure(
                    "artifact", rule_id, "%s:%d" % (fname, line),
                    "%s %s has no Admitted/Qed terminator before the next "
                    "RULE comment" % (tok, name),
                )
            )
            return UNKNOWN, tok, name
        art = THEOREM_ADMITTED if t.group(1) == "Admitted" else THEOREM_QED
        expect = rocq_name(rule_id)
        if name != expect:
            failures.append(
                Failure(
                    "decl-name", rule_id, "%s:%d" % (fname, line),
                    "%s named %s, expected %s" % (tok, name, expect),
                )
            )
        return art, tok, name
    if tok in ("Definition", "Example"):
        nm = re.match(r"\s+([A-Za-z_][A-Za-z0-9_']*)", rest)
        name = nm.group(1) if nm else None
        if name and name.endswith("_documented"):
            expect = rocq_name(rule_id) + "_documented"
            if name != expect:
                failures.append(
                    Failure(
                        "decl-name", rule_id, "%s:%d" % (fname, line),
                        "anchor named %s, expected %s" % (name, expect),
                    )
                )
            body = rest[nm.end():]
            if not _ANCHOR_BODY_RE.match(body):
                failures.append(
                    Failure(
                        "anchor-body", rule_id, "%s:%d" % (fname, line),
                        "documented anchor %s does not have body "
                        "': Prop := True.'" % name,
                    )
                )
            return DOCUMENTED_ANCHOR, tok, name
        if name and name.endswith("_claim"):
            expect = rocq_name(rule_id) + "_claim"
            if name != expect:
                failures.append(
                    Failure(
                        "decl-name", rule_id, "%s:%d" % (fname, line),
                        "claim named %s, expected %s" % (name, expect),
                    )
                )
            return DEMOTED_CLAIM, tok, name
        return DEFINING, tok, name
    if tok in _DEFINING_KEYWORDS:
        nm = re.match(r"\s+([A-Za-z_][A-Za-z0-9_']*)", rest)
        return DEFINING, tok, nm.group(1) if nm else None
    if tok in _AXIOM_KEYWORDS:
        failures.append(
            Failure(
                "artifact", rule_id, "%s:%d" % (fname, line),
                "RULE comment followed by %s (axiom footprint is not a "
                "sanctioned artifact kind)" % tok,
            )
        )
        return UNKNOWN, tok, None
    # A bare identifier: the first constructor of an Inductive, written
    # without a leading '|' (e.g. No_effects in Syntax.v).
    return DEFINING, tok, tok


def scan_theories(theories_dir):
    """Scan theories_dir/*.v for RULE comments and classify the artifact that
    follows each. Returns (artifacts, failures); artifacts in file-then-source
    order. Headerless declarations (no RULE comment) are never emitted."""
    artifacts, failures = [], []
    paths = sorted(glob.glob(os.path.join(theories_dir, "*.v")))
    if not paths:
        failures.append(
            Failure("theories-scan", None, theories_dir, "no .v files found")
        )
    for path in paths:
        fname = os.path.basename(path)
        with open(path) as fh:
            text = fh.read()
        matches = list(_RULE_COMMENT_RE.finditer(text))
        # Precompute each RULE comment's start so that theorem-terminator
        # searches never run past the next rule's territory.
        starts = [m.start() for m in matches] + [len(text)]
        for idx, m in enumerate(matches):
            rule_id = m.group(1)
            line = text.count("\n", 0, m.start()) + 1
            where = "%s:%d" % (fname, line)
            hm = _HEADER_RE.match(text, m.start())
            keyword = kind = None
            if hm:
                keyword, kind = hm.group(1), hm.group(2)
                if kind not in KINDS_BY_KEYWORD[keyword]:
                    failures.append(
                        Failure(
                            "rule-header", rule_id, where,
                            "kind %r is not in the %s vocabulary %s"
                            % (kind, keyword, sorted(KINDS_BY_KEYWORD[keyword])),
                        )
                    )
            else:
                failures.append(
                    Failure(
                        "rule-header", rule_id, where,
                        "RULE comment lacks a well-formed "
                        "(STATUS <kind>) / (CLAIM <kind>) header",
                    )
                )
            comment_end = _comment_end(text, m.start())
            decl_start, reason = _next_decl(text, comment_end)
            if decl_start is None:
                failures.append(
                    Failure(
                        "artifact", rule_id, where,
                        "no declaration follows the RULE comment (%s)" % reason,
                    )
                )
                artifact, decl, decl_name = UNKNOWN, None, None
            else:
                artifact, decl, decl_name = _classify_decl(
                    text, decl_start, starts[idx + 1], fname, line, rule_id,
                    failures,
                )
            artifacts.append(
                dict(
                    id=rule_id,
                    file=fname,
                    line=line,
                    keyword=keyword,
                    kind=kind,
                    artifact=artifact,
                    decl=decl,
                    decl_name=decl_name,
                    variants=[],
                )
            )
    dupes = sorted(
        i for i, c in Counter(a["id"] for a in artifacts).items() if c > 1
    )
    for rid in dupes:
        locs = ", ".join(
            "%s:%d" % (a["file"], a["line"]) for a in artifacts if a["id"] == rid
        )
        failures.append(
            Failure("duplicate-id", rid, locs, "RULE id appears more than once")
        )
    return artifacts, failures


# --- Traceability instance-list parsing --------------------------------------

_VARIANT_STARTS = [
    (V_EXCEPTION, re.compile(r"^\s*-\s*EXCEPTION\b")),
    (V_HYBRID, re.compile(r"^\s*-\s*HYBRID variant\b")),
    (
        V_DESCRIPTIVE_AS_DEFINING,
        re.compile(r"^\s*-\s*DESCRIPTIVE-AS-DEFINING-CLAUSE variant\b"),
    ),
    (V_ENVELOPE_QED, re.compile(r"^\s*-\s*ENVELOPE-QED variant\b")),
    (V_REFUTED_CONJECTURE, re.compile(r"^\s*-\s*REFUTED-CONJECTURE variant\b")),
    (V_PROVIDED_PROP, re.compile(r"^\s*-\s*PROVIDED-PROP variant\b")),
    (V_REFLEXIVITY_QED, re.compile(r"^\s*-\s*REFLEXIVITY-QED variant\b")),
]
# Structural guard (auditor finding, join-certification round): any bullet
# that LOOKS variant-shaped (leading all-caps token) and carries an
# Instance(s): clause but matches none of the known starts is a hard
# failure — every future catalog variant addition must force a tool update.
_VARIANT_SHAPED_RE = re.compile(r"^\s*-\s*([A-Z][A-Z-]{2,})\b")
_BULLET_RE = re.compile(r"^\s{0,3}-\s")
_INSTANCES_RE = re.compile(r"\b(?:Current\s+instances|Instances|Instance)\s*:")
_ID_TOKEN_RE = re.compile(r"[A-Za-z0-9]+(?:\.[A-Za-z0-9]+)+$")
_ABBREV_TOKEN_RE = re.compile(r"\.[A-Za-z0-9]+$")


def _split_semicolons(s):
    """Split on ';' at parenthesis depth 0 (the catalog nests ';' inside
    parentheticals, e.g. '...; entry 70)')."""
    out, depth, cur = [], 0, []
    for ch in s:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth = max(0, depth - 1)
        if ch == ";" and depth == 0:
            out.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    out.append("".join(cur))
    return out


def _segment_tokens(segment):
    """Yield (kind, value) for each backticked token in a catalog segment.
    kind is 'id', 'file', 'glob', or 'skip'. Abbreviated ids ('.Foo') are
    expanded against the previous full id's prefix. Ids preceded by the word
    'witness' are skipped (they name refutation witnesses, not instances)."""
    prev_id = None
    for m in re.finditer(r"`([^`\s]+)`", segment):
        tok = m.group(1)
        lead = segment[max(0, m.start() - 16):m.start()]
        if re.search(r"\bwitness\s*$", lead):
            yield "skip", tok
            continue
        if tok.endswith(".v") or tok.endswith(".md") or tok.endswith(".ml") \
                or tok.endswith(".mli"):
            yield "file", tok
        elif "*" in tok:
            yield "glob", tok
        elif "_" in tok or "/" in tok:
            yield "skip", tok  # a Rocq identifier or path, not a rule id
        elif _ABBREV_TOKEN_RE.fullmatch(tok):
            if prev_id is None:
                yield "skip", tok
            else:
                full = prev_id.rsplit(".", 1)[0] + tok
                prev_id = full
                yield "id", full
        elif _ID_TOKEN_RE.fullmatch(tok):
            prev_id = tok
            yield "id", tok
        else:
            yield "skip", tok


def _segment_count_word(segment):
    """An instance count is only stated in the catalog's 'N ch. NN ...'
    phrasing ('seven ch. 13 ...'); anything else numeric ('inc 4',
    'entry 70') is prose and must not be read as a count."""
    m = re.search(r"\b([a-z]+|\d+)\s+ch\.", segment)
    if m:
        w = m.group(1)
        if w.isdigit():
            return int(w)
        if w in _NUMBER_WORDS:
            return _NUMBER_WORDS[w]
    return None


def parse_traceability(correspondence_path):
    """Parse the five sanctioned-variant instance lists out of the
    Traceability section of CORRESPONDENCE.md.

    Returns (variants, failures) where variants is:
      {"exception": {"ids": set, "groups": [ {"file": str,
                                              "glob": str or None,
                                              "count": int or None} ]},
       "hybrid": set, "envelope-qed": {id: {"clause_level": bool}},
       "descriptive-as-defining": set, "refuted-conjecture": set}

    Only the Traceability section is consumed (record 76's phantom-scan
    scope: the numbered catalog records are historical documents and are
    never parsed here)."""
    failures = []
    variants = {
        V_EXCEPTION: {"ids": set(), "groups": []},
        V_HYBRID: set(),
        V_ENVELOPE_QED: {},
        V_DESCRIPTIVE_AS_DEFINING: set(),
        V_REFUTED_CONJECTURE: set(),
        V_PROVIDED_PROP: set(),
        V_REFLEXIVITY_QED: set(),
    }
    with open(correspondence_path) as fh:
        lines = fh.read().splitlines()

    # Isolate the Traceability section.
    sec, in_sec = [], False
    for ln in lines:
        if re.match(r"^##\s+Traceability\s*$", ln):
            in_sec = True
            continue
        if in_sec and re.match(r"^##\s", ln):
            break
        if in_sec:
            sec.append(ln)
    if not sec:
        failures.append(
            Failure(
                "traceability-parse", None, correspondence_path,
                "no '## Traceability' section found",
            )
        )
        return variants, failures

    # Slice out each variant's bullet (from its start line to the next
    # bullet at the same shallow indent, or end of section).
    bullets = {}
    i = 0
    while i < len(sec):
        matched = False
        for name, start_re in _VARIANT_STARTS:
            if start_re.match(sec[i]):
                j = i + 1
                while j < len(sec) and not _BULLET_RE.match(sec[j]):
                    j += 1
                bullets[name] = "\n".join(sec[i:j])
                i = j - 1
                matched = True
                break
        if not matched:
            vm = _VARIANT_SHAPED_RE.match(sec[i])
            if vm:
                j = i + 1
                while j < len(sec) and not _BULLET_RE.match(sec[j]):
                    j += 1
                body = "\n".join(sec[i:j])
                if _INSTANCES_RE.search(body):
                    failures.append(
                        Failure(
                            "unknown-variant", None, correspondence_path,
                            "variant-shaped bullet %r carries an "
                            "Instance(s): clause but matches no known "
                            "variant pattern — add it to the classifier"
                            % vm.group(1),
                        )
                    )
                i = j - 1
        i += 1
    for name in ALL_VARIANTS:
        if name not in bullets:
            failures.append(
                Failure(
                    "traceability-parse", None, correspondence_path,
                    "variant bullet %r not found in the Traceability section"
                    % name,
                )
            )

    for name, bullet in bullets.items():
        im = _INSTANCES_RE.search(bullet)
        if not im:
            failures.append(
                Failure(
                    "traceability-parse", None, correspondence_path,
                    "variant %r has no 'Current instances:'/'Instance(s):' "
                    "marker" % name,
                )
            )
            continue
        tail = bullet[im.end():]
        for segment in _split_semicolons(tail):
            toks = list(_segment_tokens(segment))
            ids = [v for k, v in toks if k == "id"]
            globs = [v for k, v in toks if k == "glob"]
            files = [v for k, v in toks if k == "file" and v.endswith(".v")]
            if ids:
                count = _segment_count_word(segment)
                if count is not None and count != len(ids):
                    failures.append(
                        Failure(
                            "traceability-parse", None, correspondence_path,
                            "variant %r segment says %d instances but "
                            "enumerates %d: %r"
                            % (name, count, len(ids), segment.strip()[:70]),
                        )
                    )
                if name == V_EXCEPTION:
                    variants[name]["ids"].update(ids)
                elif name == V_ENVELOPE_QED:
                    clause = "within its hybrid" in segment
                    for rid in ids:
                        variants[name][rid] = {"clause_level": clause}
                else:
                    variants[name].update(ids)
            elif name == V_EXCEPTION and "anchor set" in segment and files:
                variants[name]["groups"].append(
                    dict(file=files[0], glob=None, count=None)
                )
            elif name == V_EXCEPTION and globs and files:
                variants[name]["groups"].append(
                    dict(
                        file=files[0],
                        glob=globs[0],
                        count=_segment_count_word(segment),
                    )
                )
            elif not segment.strip() or not re.search(r"`", segment):
                continue  # prose-only remainder (e.g. '... entry 70).')
            elif files and not globs:
                continue  # a file-only parenthetical continuation
            else:
                failures.append(
                    Failure(
                        "traceability-parse", None, correspondence_path,
                        "variant %r: cannot interpret instance segment %r"
                        % (name, segment.strip()[:70]),
                    )
                )
    return variants, failures


# --- Classifier-vs-catalog cross-checks ---------------------------------------


def _exception_covers(artifact, variants):
    """True if a documented anchor is sanctioned by the EXCEPTION list,
    either explicitly or through one of its file-scoped groups."""
    if artifact["id"] in variants[V_EXCEPTION]["ids"]:
        return True
    for g in variants[V_EXCEPTION]["groups"]:
        if artifact["file"] != g["file"]:
            continue
        if g["glob"] is None or fnmatch.fnmatchcase(artifact["id"], g["glob"]):
            return True
    return False


def crosscheck(artifacts, variants):
    """Cross-check the classification against the Traceability lists (ground
    truth, both directions). Also annotates each artifact's 'variants' list.
    Returns failures."""
    failures = []
    by_id = {}
    for a in artifacts:
        by_id.setdefault(a["id"], a)  # dups already reported by scan_theories

    def listed(name):
        if name == V_EXCEPTION:
            return sorted(variants[name]["ids"])
        if name == V_ENVELOPE_QED:
            return sorted(variants[name])
        return sorted(variants[name])

    # Every listed id must exist in the scan; tag the ones that do.
    for name in ALL_VARIANTS:
        for rid in listed(name):
            a = by_id.get(rid)
            if a is None:
                failures.append(
                    Failure(
                        "variant-missing-id", rid, "CORRESPONDENCE.md",
                        "listed under the %s variant but no RULE comment "
                        "with this id exists in theories/" % name,
                    )
                )
            else:
                a["variants"].append(name)
    for a in artifacts:
        a["variants"] = sorted(set(a["variants"]))

    def mismatch(rid, where, detail):
        failures.append(Failure("variant-mismatch", rid, where, detail))

    # Forward: each list's members must classify as the variant demands.
    for rid in listed(V_EXCEPTION):
        a = by_id.get(rid)
        if a and a["artifact"] != DOCUMENTED_ANCHOR:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "EXCEPTION-listed but classified %s (expected "
                "documented-anchor)" % a["artifact"],
            )
    for g in variants[V_EXCEPTION]["groups"]:
        if g["count"] is None:
            continue
        n = sum(
            1
            for a in artifacts
            if a["file"] == g["file"]
            and a["artifact"] == DOCUMENTED_ANCHOR
            and a["kind"] != "descriptive"
            and (g["glob"] is None or fnmatch.fnmatchcase(a["id"], g["glob"]))
        )
        if n != g["count"]:
            mismatch(
                None, g["file"],
                "EXCEPTION group %s in %s: catalog says %d instances, "
                "classifier finds %d non-descriptive documented anchors"
                % (g["glob"] or "(anchor set)", g["file"], g["count"], n),
            )
    for rid in listed(V_HYBRID):
        a = by_id.get(rid)
        if a and a["artifact"] not in (
            THEOREM_ADMITTED,
            THEOREM_QED,
            DOCUMENTED_ANCHOR,
        ):
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "HYBRID-listed but classified %s (expected a Theorem or a "
                "documented anchor)" % a["artifact"],
            )
    for rid in listed(V_ENVELOPE_QED):
        a = by_id.get(rid)
        if a is None:
            continue
        if variants[V_ENVELOPE_QED][rid]["clause_level"]:
            if rid not in variants[V_HYBRID]:
                mismatch(
                    rid, "CORRESPONDENCE.md",
                    "ENVELOPE-QED lists clauses 'within its hybrid' but the "
                    "id is not in the HYBRID list",
                )
        elif a["artifact"] != THEOREM_QED:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "ENVELOPE-QED-listed but classified %s (expected "
                "theorem-qed)" % a["artifact"],
            )
    for rid in listed(V_DESCRIPTIVE_AS_DEFINING):
        a = by_id.get(rid)
        if a is None:
            continue
        if a["artifact"] != DEFINING:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "DESCRIPTIVE-AS-DEFINING-listed but classified %s (expected "
                "defining)" % a["artifact"],
            )
        if a["kind"] != "descriptive":
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "DESCRIPTIVE-AS-DEFINING-listed but claim kind is %s"
                % a["kind"],
            )
    for rid in listed(V_REFLEXIVITY_QED):
        a = by_id.get(rid)
        if a and a["artifact"] != THEOREM_QED:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "REFLEXIVITY-QED-listed but classified %s (expected "
                "theorem-qed)" % a["artifact"],
            )
    for rid in listed(V_PROVIDED_PROP):
        a = by_id.get(rid)
        if a and a["artifact"] != DOCUMENTED_ANCHOR:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "PROVIDED-PROP-listed but classified %s (expected "
                "documented-anchor with an adjacent provided Prop)"
                % a["artifact"],
            )
    for rid in listed(V_REFUTED_CONJECTURE):
        a = by_id.get(rid)
        if a and a["artifact"] != DEMOTED_CLAIM:
            mismatch(
                rid, "%s:%d" % (a["file"], a["line"]),
                "REFUTED-CONJECTURE-listed but classified %s (expected "
                "demoted-claim)" % a["artifact"],
            )

    # Reverse: every classified artifact that needs a sanction must have one.
    for a in artifacts:
        where = "%s:%d" % (a["file"], a["line"])
        if (
            a["artifact"] == DEMOTED_CLAIM
            and a["id"] not in variants[V_REFUTED_CONJECTURE]
        ):
            mismatch(
                a["id"], where,
                "demoted _claim Definition not listed under the "
                "REFUTED-CONJECTURE variant",
            )
        if (
            a["artifact"] == DOCUMENTED_ANCHOR
            and a["kind"] not in (None, "descriptive")
            and not _exception_covers(a, variants)
            and a["id"] not in variants[V_HYBRID]
        ):
            mismatch(
                a["id"], where,
                "non-descriptive documented anchor (kind %s) sanctioned by "
                "neither the EXCEPTION list/groups nor the HYBRID list"
                % a["kind"],
            )
        if (
            a["artifact"] == DEFINING
            and a["kind"] == "descriptive"
            and a["id"] not in variants[V_DESCRIPTIVE_AS_DEFINING]
        ):
            mismatch(
                a["id"], where,
                "descriptive rule encoded as a defining clause but not "
                "listed under the DESCRIPTIVE-AS-DEFINING-CLAUSE variant",
            )
        if (
            a["artifact"] in (THEOREM_ADMITTED, THEOREM_QED)
            and a["kind"] == "descriptive"
            # Post-migration (batch C re-key): the ENVELOPE-QED variant
            # sanctions a Qed under ANY claim kind ("true CLAIM
            # preserved"), so envelope-listed descriptive Theorems are
            # legal.  Anything else descriptive-and-Theorem stays an
            # error.
            and V_ENVELOPE_QED not in a["variants"]
            and V_REFLEXIVITY_QED not in a["variants"]
            and V_HYBRID not in a["variants"]
        ):
            mismatch(
                a["id"], where,
                "descriptive rule encoded as a Theorem (no sanctioned "
                "variant maps descriptive to a Theorem)",
            )
    return failures


# --- Top-level entry point ----------------------------------------------------


def classify(theories_dir, correspondence_path):
    """Classify every RULE id's Rocq artifact kind and cross-check against the
    Traceability lists. Returns (artifacts, failures); artifacts in
    file-then-source order, failures sorted deterministically. Never raises on
    data problems (only on I/O errors)."""
    artifacts, failures = scan_theories(theories_dir)
    variants, vfailures = parse_traceability(correspondence_path)
    failures += vfailures
    failures += crosscheck(artifacts, variants)
    failures.sort(key=lambda f: (f.check, f.rule_id or "", f.where or "", f.detail))
    return artifacts, failures


if __name__ == "__main__":
    # Ad-hoc CLI for eyeballing; the canonical consumer is
    # regen_rule_index.py.
    import argparse
    import json

    p = argparse.ArgumentParser(
        description="Classify Rocq artifact kinds for formalism RULE ids."
    )
    p.add_argument("theories_dir")
    p.add_argument("correspondence_path")
    args = p.parse_args()
    arts, fails = classify(args.theories_dir, args.correspondence_path)
    print(json.dumps(arts, indent=1))
    for f in fails:
        print("FAIL[%s] %s (%s): %s" % (f.check, f.rule_id, f.where, f.detail))
    print(
        "%d artifacts, %d failures, kinds: %s"
        % (
            len(arts),
            len(fails),
            dict(Counter(a["artifact"] for a in arts)),
        )
    )

