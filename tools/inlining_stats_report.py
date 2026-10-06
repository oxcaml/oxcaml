#!/usr/bin/env python3
"""Aggregate and compare the statistics printed by -dinlining-stats.

Each result set is a directory (or a file) containing the output of
compilations run with -dinlining-stats: build logs, per-unit files, any mix.
A unit's block starts with a line "inlining stats for <unit>" followed by
lines "  <value> <key>". Counts and sums are added across units; the keys
ending in "max_copies" or "max_identical" are combined with the maximum.

  tools/inlining_stats_report.py current=DIR 2026=DIR [...] --pdf out.pdf

The first set is the baseline the others are compared against. A summary is
printed, and the PDF has one figure per page followed by a per-unit table.
Sizes are shown for the architecture given by --arch (default: the host's).
"""
import argparse
import os
import platform
import re
import sys
from collections import OrderedDict, defaultdict

HEADER = re.compile(r"^inlining stats for (\S+)\s*$")
LINE = re.compile(r"^  (-?[0-9.]+) (\S+)\s*$")


# --------------------------------------------------------------------------
# loading
# --------------------------------------------------------------------------

def parse_blocks(text):
    """unit -> {key: number} for every block in the text."""
    units = {}
    cur = None
    for line in text.splitlines():
        m = HEADER.match(line)
        if m:
            cur = {}
            units[m.group(1)] = cur
            continue
        if cur is None:
            continue
        m = LINE.match(line)
        if m:
            v, k = m.group(1), m.group(2)
            cur[k] = float(v) if "." in v else int(v)
        elif line and not line.startswith("  "):
            cur = None
    return units


def load_meta(path):
    """Build timings recorded by inlining_comparison_report.py, if the set
    lives in (or is a file of) one of its configuration directories."""
    import json
    d = path if os.path.isdir(path) else os.path.dirname(os.path.abspath(path))
    try:
        with open(os.path.join(d, "meta.json")) as f:
            m = json.load(f)
        return {k: m.get(k) for k in ("wall_seconds", "cpu_seconds")}
    except (OSError, ValueError):
        return {}


def text_size(path):
    """Size of the text segment of an object file, via the `size` tool (GNU
    or macOS output), or None."""
    import subprocess
    try:
        out = subprocess.run(["size", path], capture_output=True, text=True).stdout
    except OSError:
        return None
    for line in out.splitlines()[1:]:
        parts = line.split()
        if parts and parts[0].isdigit():
            return int(parts[0])
    return None


def load_objects(path):
    """(number of object files, total text bytes): from objects.csv written
    by inlining_comparison_report.py next to the set, else by measuring the
    .o files found under the set's directory."""
    import csv
    d = path if os.path.isdir(path) else os.path.dirname(os.path.abspath(path))
    objs = os.path.join(d, "objects.csv")
    if os.path.isfile(objs):
        n = total = 0
        with open(objs) as f:
            for row in csv.DictReader(f):
                n += 1
                total += int(row.get("text_bytes") or 0)
        return n, total
    if os.path.isdir(path):
        n = total = 0
        for root, _dirs, names in os.walk(path):
            for name in names:
                if name.endswith(".o"):
                    t = text_size(os.path.join(root, name))
                    if t is not None:
                        n += 1
                        total += t
        if n:
            return n, total
    return None, None


def load_set(path):
    units = {}
    files = []
    if os.path.isdir(path):
        for root, _dirs, names in os.walk(path):
            for n in names:
                if n.endswith((".txt", ".log", ".stats")):
                    files.append(os.path.join(root, n))
    else:
        files = [path]
    for f in sorted(files):
        try:
            with open(f, errors="replace") as fh:
                units.update(parse_blocks(fh.read()))
        except OSError:
            pass
    return units


def is_max_key(key):
    return key.endswith("max_copies") or key.endswith("max_identical")


def aggregate(units):
    tot = defaultdict(float)
    for d in units.values():
        for k, v in d.items():
            if is_max_key(k):
                tot[k] = max(tot[k], v)
            else:
                tot[k] += v
    return dict(tot)


def get(d, key, default=0):
    return d.get(key, default)


def sum_keys(d, prefix, suffix=""):
    return sum(v for k, v in d.items() if k.startswith(prefix) and k.endswith(suffix))


# --------------------------------------------------------------------------
# the quantities shown
# --------------------------------------------------------------------------

def summary_rows(arch):
    """(label, function of totals) in display order."""
    S = f".{arch}"
    both = lambda d, k: get(d, k + ".function") + get(d, k + ".functor")
    return [
        ("build wall time (s)", lambda d: d.get("build.wall_seconds")),
        ("build CPU time (s)", lambda d: d.get("build.cpu_seconds")),
        ("object files measured", lambda d: d.get("objects.count")),
        ("object text bytes (sum over the object files)", lambda d: d.get("objects.text_bytes")),
        ("functions in the output", lambda d: get(d, "code.functions")),
        ("  of which copies of another (extra copies)", lambda d: get(d, "code.specialisation.extra_copies")),
        ("  most copies of one function", lambda d: get(d, "code.specialisation.max_copies")),
        ("estimated code size (v2 instructions)", lambda d: get(d, "code.size.v2" + S)),
        ("  in extra copies", lambda d: get(d, "code.specialisation.extra_copies_size" + S)),
        ("direct applies left", lambda d: get(d, "code.apply.direct")),
        ("indirect applies left", lambda d: get(d, "code.apply.indirect_unknown_arity") + get(d, "code.apply.indirect_known_arity")),
        ("closures allocated dynamically (sets)", lambda d: get(d, "code.set_of_closures.dynamic")),
        ("call sites with a known callee", lambda d: get(d, "call_site.function.count") + get(d, "call_site.functor.count")),
        ("  inlined", lambda d: get(d, "call_site.function.inlined") + get(d, "call_site.functor.inlined")),
        ("    because the definition says so (small, attribute)", lambda d: get(d, "call_site.function.definition_says_inline") + get(d, "call_site.function.definition_says_inline_attribute") + get(d, "call_site.functor.definition_says_inline") + get(d, "call_site.functor.definition_says_inline_attribute")),
        ("    after speculation", lambda d: get(d, "call_site.function.speculation_inline") + get(d, "call_site.functor.speculation_inline")),
        ("  refused after speculation", lambda d: get(d, "call_site.function.speculation_not_inline") + get(d, "call_site.functor.speculation_not_inline")),
        ("  speculation aborted (budget)", lambda d: get(d, "call_site.function.speculation_aborted") + get(d, "call_site.functor.speculation_aborted")),
        ("  refused by the budget pre-check", lambda d: get(d, "call_site.function.speculation_budget_exhausted") + get(d, "call_site.functor.speculation_budget_exhausted")),
        ("  definition says never (large, attribute)", lambda d: get(d, "call_site.function.definition_says_not_to_inline") + get(d, "call_site.function.never_inlined_attribute") + get(d, "call_site.functor.definition_says_not_to_inline") + get(d, "call_site.functor.never_inlined_attribute")),
        ("  argument types not useful", lambda d: get(d, "call_site.function.argument_types_not_useful")),
        ("  depth limits (recursion, inlining depth, unrolling)", lambda d: get(d, "call_site.function.recursion_depth_exceeded") + get(d, "call_site.function.max_inlining_depth_exceeded") + get(d, "call_site.function.unrolling_depth_exceeded")),
        ("functor applications inlined", lambda d: get(d, "call_site.functor.inlined")),
        ("call sites with an unknown callee", lambda d: get(d, "call_site.unknown_callee.count")),
        ("speculations at the outermost level", lambda d: sum_keys(d, "speculation.outermost.", ".count") - sum_keys(d, "speculation.outermost.", ".functor.count")),
        ("  original size of those inlined", lambda d: get(d, "speculation.outermost.inline.original_size" + S)),
        ("  size of their inlined copies", lambda d: get(d, "speculation.outermost.inline.simplified_size" + S)),
        ("  original size of those refused", lambda d: get(d, "speculation.outermost.not_inline.original_size" + S)),
        ("  original size of those aborted", lambda d: get(d, "speculation.outermost.aborted.original_size" + S)),
        ("speculations inside inlined bodies (regions)", lambda d: sum_keys(d, "speculation.in_region.", ".count") - sum_keys(d, "speculation.in_region.", ".functor.count")),
        ("seconds spent speculating", lambda d: get(d, "speculation.outermost.seconds") + get(d, "speculation.nested.seconds")),
        ("budget regions opened / exhausted", lambda d: (get(d, "budget.region.opened"), get(d, "budget.region.exhausted"))),
        ("function definitions", lambda d: get(d, "definition.count")),
        ("  classed small (always inlined)", lambda d: get(d, "definition.small_function.count") + get(d, "definition.small_functor.count")),
        ("  classed speculatively inlinable", lambda d: get(d, "definition.speculatively_inlinable.count") + get(d, "definition.speculatively_inlinable_functor.count")),
        ("  classed too large", lambda d: get(d, "definition.function_body_too_large.count") + get(d, "definition.functor_body_too_large.count")),
        ("decisions taken inside speculations", lambda d: get(d, "in_speculation.call_site.function.count")),
    ]


def fmt(v):
    if v is None:
        return "n/a"
    if isinstance(v, tuple):
        return " / ".join(fmt(x) for x in v)
    if isinstance(v, float) and not v.is_integer():
        return f"{v:,.1f}"
    return f"{int(v):,}"


def pct_change(base, v):
    """Signed percentage change from [base] to [v], or "" where it does not
    apply (missing values, tuples, a zero baseline)."""
    if isinstance(base, tuple) or isinstance(v, tuple) or base is None or v is None or not base:
        return ""
    return f"{100.0 * (v - base) / base:+.1f}%"


def summary_columns(names):
    """Column headings: the baseline, then each other set with its change."""
    cols = [names[0]]
    for n in names[1:]:
        cols += [n, "change"]
    return cols


def summary_cells(names, totals, f):
    base = f(totals[names[0]])
    cells = [fmt(base)]
    for n in names[1:]:
        v = f(totals[n])
        cells += [fmt(v), pct_change(base, v)]
    return cells


def print_summary(names, totals, arch):
    cols = summary_columns(names)
    w = max(12, max(len(c) for c in cols) + 2)
    print(f"{'':<52}" + "".join(f"{c:>{w}}" for c in cols))
    for label, f in summary_rows(arch):
        print(f"{label:<52}" + "".join(f"{c:>{w}}" for c in summary_cells(names, totals, f)))


# --------------------------------------------------------------------------
# figures
# --------------------------------------------------------------------------

def style(ax):
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.grid(True, which="major", alpha=0.25)
    ax.set_axisbelow(True)


def per_unit_scatter(pdf, plt, names, sets, key, title, xlabel, log=True):
    base = names[0]
    fig, ax = plt.subplots(figsize=(8.5, 7))
    colours = plt.get_cmap("tab10").colors
    lo, hi = float("inf"), 0.0
    for i, n in enumerate(names[1:], 1):
        xs, ys, labels = [], [], []
        for unit in sorted(set(sets[base]) & set(sets[n])):
            x, y = get(sets[base][unit], key), get(sets[n][unit], key)
            if log:
                x, y = max(x, 1), max(y, 1)
            xs.append(x); ys.append(y); labels.append(unit)
            lo, hi = min(lo, x, y), max(hi, x, y)
        ax.scatter(xs, ys, s=22, color=colours[i], alpha=0.8, label=f"{n} (y) against {base} (x)")
        if i == 1:
            # label the largest units
            for j, (x, y, l) in enumerate(sorted(zip(xs, ys, labels), reverse=True)[:6]):
                ax.annotate(l.replace("caml", "", 1), (x, y), fontsize=7,
                            xytext=(4, -9 if j % 2 else 4), textcoords="offset points")
    if lo < hi:
        ax.plot([lo, hi], [lo, hi], color="grey", lw=0.8, ls="--", label="no change")
    if log:
        ax.set_xscale("log"); ax.set_yscale("log")
    ax.set_xlabel(f"{xlabel}, {base}" + (" (0 drawn as 1)" if log else ""))
    ax.set_ylabel(f"{xlabel}, other configuration")
    ax.set_title(title)
    ax.legend(loc="upper left", fontsize=8)
    style(ax)
    pdf.savefig(fig); plt.close(fig)


def grouped_bars(pdf, plt, names, totals, items, title, ylabel, horizontal=True):
    """items: list of (label, function of totals)."""
    import numpy as np
    items = [(l, f) for l, f in items if any(f(totals[n]) for n in names)]
    for n in names:
        totals[n] = {k: (0 if v is None else v) for k, v in totals[n].items()}
    if not items:
        return
    fig, ax = plt.subplots(figsize=(8.5, 0.45 * len(items) + 2.5 if horizontal else 7))
    colours = plt.get_cmap("tab10").colors
    k = len(names)
    pos = np.arange(len(items))
    width = 0.8 / k
    for i, n in enumerate(names):
        vals = [f(totals[n]) for _, f in items]
        off = (i - (k - 1) / 2) * width
        if horizontal:
            ax.barh(pos + off, vals, height=width, color=colours[i], label=n)
        else:
            ax.bar(pos + off, vals, width=width, color=colours[i], label=n)
    labels = [l for l, _ in items]
    if horizontal:
        ax.set_yticks(pos); ax.set_yticklabels(labels, fontsize=8); ax.invert_yaxis()
        ax.set_xlabel(ylabel)
    else:
        ax.set_xticks(pos); ax.set_xticklabels(labels, fontsize=8, rotation=30, ha="right")
        ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.legend(fontsize=8)
    style(ax)
    fig.tight_layout()
    pdf.savefig(fig); plt.close(fig)


def ratio_bars(pdf, plt, names, totals, metrics, title, reference):
    """For each set after the first, the ratio of each metric to the
    baseline's, on a log axis, with a dashed line at the ratio given by
    [reference] (e.g. the change in code size)."""
    import numpy as np
    base = names[0]
    metrics = [(l, f) for l, f in metrics if f(totals[base])]
    fig, ax = plt.subplots(figsize=(8.5, 0.4 * len(metrics) + 2.5))
    colours = plt.get_cmap("tab10").colors
    pos = np.arange(len(metrics))
    k = max(len(names) - 1, 1)
    width = 0.8 / k
    for i, n in enumerate(names[1:]):
        ratios = [f(totals[n]) / f(totals[base]) for _, f in metrics]
        off = (i - (k - 1) / 2) * width
        ax.barh(pos + off, ratios, height=width, color=colours[i + 1], label=f"{n} / {base}")
        r = reference(totals[n]) / reference(totals[base])
        ax.axvline(r, color=colours[i + 1], ls="--", lw=0.9, label=f"change in estimated code size ({r:.2f})")
    ax.axvline(1.0, color="grey", lw=0.8)
    ax.set_xscale("log")
    ax.set_yticks(pos); ax.set_yticklabels([l for l, _ in metrics], fontsize=8); ax.invert_yaxis()
    ax.set_xlabel("ratio to the baseline (log scale)")
    ax.set_title(title)
    ax.legend(fontsize=8, loc="lower right")
    style(ax)
    fig.tight_layout()
    pdf.savefig(fig); plt.close(fig)


def unit_table(pdf, plt, names, sets, arch, top=28):
    S = f".{arch}"
    base = names[0]
    units = sorted(set.intersection(*(set(sets[n]) for n in names)),
                   key=lambda u: -get(sets[base][u], "code.size.v2" + S))[:top]
    cols = ["unit"] + [f"size\n{n}" for n in names] + [f"extra\ncopies {n}" for n in names] + [f"most\ncopies {n}" for n in names]
    rows = []
    for u in units:
        r = [u.replace("caml", "", 1)]
        r += [fmt(get(sets[n][u], "code.size.v2" + S)) for n in names]
        r += [fmt(get(sets[n][u], "code.specialisation.extra_copies")) for n in names]
        r += [fmt(get(sets[n][u], "code.specialisation.max_copies")) for n in names]
        rows.append(r)
    fig, ax = plt.subplots(figsize=(8.5, 0.28 * len(rows) + 1.5))
    ax.axis("off")
    t = ax.table(cellText=rows, colLabels=cols, loc="upper left", cellLoc="right", colLoc="right")
    t.auto_set_font_size(False); t.set_fontsize(6.5); t.scale(1, 1.15)
    for (r, c), cell in t.get_celld().items():
        if c == 0:
            cell.set_text_props(ha="left")
            cell.set_width(0.22)
        else:
            cell.set_width(0.78 / (len(cols) - 1))
        if r == 0:
            cell.set_text_props(weight="bold")
            cell.set_height(cell.get_height() * 2)
    ax.set_title(f"Largest units by estimated code size in {base} ({arch})", fontsize=10, loc="left")
    pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)


def summary_page(pdf, plt, names, totals, arch):
    cols = summary_columns(names)
    rows = [[label] + summary_cells(names, totals, f) for label, f in summary_rows(arch)]
    fig, ax = plt.subplots(figsize=(8.5, 0.27 * len(rows) + 1.2))
    ax.axis("off")
    t = ax.table(cellText=rows, colLabels=[""] + cols, loc="upper left", cellLoc="right", colLoc="right")
    t.auto_set_font_size(False); t.set_fontsize(7); t.scale(1, 1.15)
    change_cols = {i + 1 for i, c in enumerate(cols) if c == "change"}
    value_width = 0.5 / (len(cols) - len(change_cols) + 0.6 * len(change_cols))
    for (r, c), cell in t.get_celld().items():
        if c == 0:
            cell.set_text_props(ha="left")
            cell.set_width(0.5)
        else:
            cell.set_width(value_width * (0.6 if c in change_cols else 1.0))
        if r == 0:
            cell.set_text_props(weight="bold")
    ax.set_title(f"Totals over {len(set.intersection(*(set(s) for s in (sets_global[n] for n in names))))} units common to all sets (sizes for {arch})", fontsize=10, loc="left")
    pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)


sets_global = {}


def make_pdf(out, names, sets, totals, arch):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.backends.backend_pdf import PdfPages
    S = f".{arch}"
    both = lambda d, k: get(d, k + ".function") + get(d, k + ".functor")
    with PdfPages(out) as pdf:
        summary_page(pdf, plt, names, totals, arch)
        per_unit_scatter(pdf, plt, names, sets, "code.size.v2" + S,
                         "Estimated code size per unit", "v2 instructions")
        per_unit_scatter(pdf, plt, names, sets, "code.specialisation.extra_copies",
                         "Extra copies of functions per unit (copies made by inlining)", "extra copies")
        per_unit_scatter(pdf, plt, names, sets, "code.specialisation.max_copies",
                         "Most copies of a single function, per unit", "copies", log=True)
        code_size = lambda d: get(d, "code.size.v2" + S)
        known = lambda d: get(d, "call_site.function.count") + get(d, "call_site.unknown_callee.count")
        pct = lambda f, denom: (lambda d: 100.0 * f(d) / denom(d) if denom(d) else 0.0)
        per_k = lambda f: (lambda d: 1000.0 * f(d) / code_size(d) if code_size(d) else 0.0)
        ratio_bars(pdf, plt, names, totals, [
            ("object text bytes", lambda d: get(d, "objects.text_bytes")),
            ("estimated code size", code_size),
            ("functions in the output", lambda d: get(d, "code.functions")),
            ("extra copies of functions", lambda d: get(d, "code.specialisation.extra_copies")),
            ("most copies of one function", lambda d: get(d, "code.specialisation.max_copies")),
            ("direct applies left", lambda d: get(d, "code.apply.direct")),
            ("indirect applies left", lambda d: get(d, "code.apply.indirect_unknown_arity") + get(d, "code.apply.indirect_known_arity")),
            ("C calls left", lambda d: get(d, "code.apply.c_call")),
            ("sets of closures allocated dynamically", lambda d: get(d, "code.set_of_closures.dynamic")),
            ("call sites with a known callee", lambda d: get(d, "call_site.function.count")),
            ("call sites inlined", lambda d: get(d, "call_site.function.inlined")),
            ("inlined because the definition says so", lambda d: get(d, "call_site.function.definition_says_inline") + get(d, "call_site.function.definition_says_inline_attribute")),
            ("inlined after speculation", lambda d: get(d, "call_site.function.speculation_inline")),
            ("call sites with an unknown callee", lambda d: get(d, "call_site.unknown_callee.count")),
            ("function definitions", lambda d: get(d, "definition.count")),
            ("definitions classed small", lambda d: get(d, "definition.small_function.count")),
            ("seconds spent speculating", lambda d: get(d, "speculation.outermost.seconds") + get(d, "speculation.nested.seconds")),
            ("build CPU time", lambda d: get(d, "build.cpu_seconds")),
        ], "Change relative to the baseline (dashed line: change in estimated code size)", code_size)
        grouped_bars(pdf, plt, names, totals, [
            ("inlined: definition says so (small / attribute)", pct(lambda d: get(d, "call_site.function.definition_says_inline") + get(d, "call_site.function.definition_says_inline_attribute"), known)),
            ("inlined after speculation", pct(lambda d: get(d, "call_site.function.speculation_inline"), known)),
            ("refused after speculation", pct(lambda d: get(d, "call_site.function.speculation_not_inline"), known)),
            ("speculation aborted (budget)", pct(lambda d: get(d, "call_site.function.speculation_aborted"), known)),
            ("refused by budget pre-check", pct(lambda d: get(d, "call_site.function.speculation_budget_exhausted"), known)),
            ("definition says never (large / attribute)", pct(lambda d: get(d, "call_site.function.definition_says_not_to_inline") + get(d, "call_site.function.never_inlined_attribute"), known)),
            ("argument types not useful", pct(lambda d: get(d, "call_site.function.argument_types_not_useful"), known)),
            ("recursion depth exceeded", pct(lambda d: get(d, "call_site.function.recursion_depth_exceeded"), known)),
            ("max inlining depth exceeded", pct(lambda d: get(d, "call_site.function.max_inlining_depth_exceeded"), known)),
            ("in a stub", pct(lambda d: get(d, "call_site.function.in_a_stub"), known)),
            ("unknown callee", pct(lambda d: get(d, "call_site.unknown_callee.count"), known)),
        ], "Decisions at call sites, share of all call sites (functions)", "% of call sites")
        n_spec = lambda d: sum(get(d, f"speculation.{sc}.{o}.count") for sc in ("outermost", "in_region", "in_speculation") for o in ("inline", "not_inline", "aborted", "budget_exhausted"))
        grouped_bars(pdf, plt, names, totals, [
            (f"{scope}: {outcome}", pct((lambda s, o: lambda d: get(d, f"speculation.{s}.{o}.count"))(scope, outcome), n_spec))
            for scope in ("outermost", "in_region", "in_speculation")
            for outcome in ("inline", "not_inline", "aborted", "budget_exhausted")
        ], "Speculations by scope and outcome, as a share of all speculations", "% of speculations")
        per = lambda num, cnt: (lambda d: get(d, num) / get(d, cnt) if get(d, cnt) else 0.0)
        grouped_bars(pdf, plt, names, totals, [
            ("inlined: original size", per("speculation.outermost.inline.original_size" + S, "speculation.outermost.inline.count")),
            ("inlined: size of the copy", per("speculation.outermost.inline.simplified_size" + S, "speculation.outermost.inline.count")),
            ("refused: original size", per("speculation.outermost.not_inline.original_size" + S, "speculation.outermost.not_inline.count")),
            ("refused: size after simplification", per("speculation.outermost.not_inline.simplified_size" + S, "speculation.outermost.not_inline.count")),
            ("aborted: original size", per("speculation.outermost.aborted.original_size" + S, "speculation.outermost.aborted.count")),
        ], "Outermost speculations: mean sizes before and after, per speculation", f"v2 instructions ({arch})")
        grouped_bars(pdf, plt, names, totals, [
            ("direct applies", per_k(lambda d: get(d, "code.apply.direct"))),
            ("indirect applies, unknown arity", per_k(lambda d: get(d, "code.apply.indirect_unknown_arity"))),
            ("indirect applies, known arity", per_k(lambda d: get(d, "code.apply.indirect_known_arity"))),
            ("C calls", per_k(lambda d: get(d, "code.apply.c_call"))),
            ("sets of closures allocated dynamically", per_k(lambda d: get(d, "code.set_of_closures.dynamic"))),
            ("sets of closures static", per_k(lambda d: get(d, "code.set_of_closures.static"))),
            ("functions", per_k(lambda d: get(d, "code.functions"))),
            ("functions that are extra copies", per_k(lambda d: get(d, "code.specialisation.extra_copies"))),
        ], "Shape of the final code, per 1,000 instructions of estimated code", "per 1,000 instructions")
        n_def = lambda d: get(d, "definition.count")
        grouped_bars(pdf, plt, names, totals, [
            ("small (always inlined)", pct(lambda d: get(d, "definition.small_function.count"), n_def)),
            ("speculatively inlinable", pct(lambda d: get(d, "definition.speculatively_inlinable.count"), n_def)),
            ("too large", pct(lambda d: get(d, "definition.function_body_too_large.count"), n_def)),
            ("recursive", pct(lambda d: get(d, "definition.recursive.count"), n_def)),
            ("stub", pct(lambda d: get(d, "definition.stub.count"), n_def)),
            ("[@inline never]", pct(lambda d: get(d, "definition.never_inline_attribute.count"), n_def)),
            ("[@inline always]", pct(lambda d: get(d, "definition.attribute_inline.count"), n_def)),
            ("functors", pct(lambda d: get(d, "definition.functor.count"), n_def)),
        ], "Function definitions by inlining class, share of definitions", "% of definitions")
        per_unit_scatter(pdf, plt, names, sets, "speculation.outermost.seconds",
                         "Seconds spent in outermost speculations, per unit", "seconds")
        unit_table(pdf, plt, names, sets, arch)


# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("sets", nargs="+", help="name=path (the first is the baseline)")
    ap.add_argument("--pdf", help="write the figures and tables to this PDF")
    ap.add_argument("--arch", choices=["arm64", "x86_64"],
                    default="arm64" if platform.machine() in ("arm64", "aarch64") else "x86_64")
    args = ap.parse_args()
    names, sets = [], {}
    for s in args.sets:
        if "=" not in s:
            sys.exit(f"expected name=path, got {s}")
        n, p = s.split("=", 1)
        names.append(n)
        sets[n] = load_set(p)
        if not sets[n]:
            sys.exit(f"no inlining stats found under {p}")
    common = set.intersection(*(set(sets[n]) for n in names))
    for n in names:
        extra = set(sets[n]) - common
        if extra:
            print(f"note: {n} has {len(extra)} units absent from another set; totals use the {len(common)} common units", file=sys.stderr)
    totals = {n: aggregate({u: sets[n][u] for u in common}) for n in names}
    for s, n in zip(args.sets, names):
        meta = load_meta(s.split("=", 1)[1])
        totals[n]["build.wall_seconds"] = meta.get("wall_seconds")
        totals[n]["build.cpu_seconds"] = meta.get("cpu_seconds")
        count, text = load_objects(s.split("=", 1)[1])
        totals[n]["objects.count"] = count
        totals[n]["objects.text_bytes"] = text
    sets_global.update(sets)
    print_summary(names, totals, args.arch)
    if args.pdf:
        make_pdf(args.pdf, names, sets, totals, args.arch)
        print(f"\nwrote {args.pdf}")


if __name__ == "__main__":
    main()
