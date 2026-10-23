#!/usr/bin/env python3
# ******************************************************************************
# *                                  OxCaml                                    *
# *                       Mark Shinwell, Jane Street                           *
# * -------------------------------------------------------------------------- *
# *                               MIT License                                  *
# *                                                                            *
# * Copyright (c) 2026 Jane Street Group LLC                                   *
# * opensource-contacts@janestreet.com                                         *
# *                                                                            *
# * Permission is hereby granted, free of charge, to any person obtaining a    *
# * copy of this software and associated documentation files (the "Software"), *
# * to deal in the Software without restriction, including without limitation  *
# * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
# * and/or sell copies of the Software, and to permit persons to whom the      *
# * Software is furnished to do so, subject to the following conditions:       *
# *                                                                            *
# * The above copyright notice and this permission notice shall be included    *
# * in all copies or substantial portions of the Software.                     *
# *                                                                            *
# * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
# * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
# * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
# * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
# * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
# * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
# * DEALINGS IN THE SOFTWARE.                                                  *
# ******************************************************************************

"""Compare Flambda 2's code size estimates with the machine code actually
emitted.

Compile with -dcode-sizes (make code-size-report does so for the whole
compiler distribution, into _build/code_sizes/main, and then runs this script).
Each compilation unit then gets a <prefix>.code_sizes.csv file next to its
.o file, with one line per function giving its estimated size under each
code size model, one column per model (see Code_size_report in the
compiler).  This script disassembles the .o files with objdump, counts the
instructions of each function, matches them with the estimates by symbol
name, and prints, for each model, the distribution of log2(estimate /
emitted instructions) as a histogram together with summary statistics.  With
--plot, the histograms of the models are also drawn, overlaid, to an image
file (PDF, PNG, ... according to the extension).

A model whose estimates depend on the target has one column per
architecture, named with the suffix _x86_64 or _arm64; only the column for
the architecture of the object files is used.

A function inlined into many compilation units (such as the closures created
by Format.fprintf) appears once per unit.  By default each set of such
identical copies -- functions with the same name, once the compilation unit
and the numeric suffixes are removed, and the same estimated and emitted sizes
-- is counted only once, so that the results reflect distinct code;
--no-dedupe counts every copy.

The estimates are in instructions.  On arm64 every instruction is four bytes,
so the emitted instructions are counted directly (--unit instructions, the
default there).  On x86-64 instruction lengths vary (from one to fifteen
bytes), so by default (--unit bytes) the emitted code is measured in bytes and
converted to instruction equivalents by dividing by the typical instruction
length (or by --bytes-per-insn).  That length is the geometric mean over the
functions of their mean instruction lengths, so that the conversion does not
shift the histogram (the overall mean is dominated by large functions, whose
instructions tend to be longer).  The estimates are thus judged on how well
they predict the sizes of functions in bytes.  This also measures jump tables
stored in the text section correctly, which objdump disassembles as a few
spurious instructions.

Usage: code_size_histogram.py [options] ROOT [ROOT...]
  ROOT is a directory searched recursively for *.code_sizes.csv files (e.g.
  _build/code_sizes/main), or a single such file.
"""

import argparse
import collections
import csv
import math
import os
import platform
import re
import shutil
import subprocess
import sys

SYMBOL_RE = re.compile(r"^([0-9a-f]+) <(.+?)>:\s*$")
SECTION_RE = re.compile(r"^Disassembly of section ")
# A line of disassembly: address, raw bytes (pairs of hex digits on x86-64,
# one 32-bit word on arm64) and, after a tab, the instruction.  GNU objdump
# continues the bytes of long instructions on further lines without an
# instruction.
INSN_RE = re.compile(
    r"^\s*[0-9a-f]+:\s*((?:[0-9a-f]{2,8} )*[0-9a-f]{2,8}) *(?:\t(.*))?$"
)
# Alignment padding as printed by GNU objdump and llvm-objdump: plain and
# multi-byte nops (possibly with prefixes), [xchg %ax,%ax] and [int3].
PADDING_RE = re.compile(
    r"^(?:(?:data16|cs|ds)\s+)*(?:nop\w*(?:\s.*)?|xchgw?\s+%ax,\s*%ax|int3)\s*$"
)

ARCHES = ("x86_64", "arm64")

# How the models are described in the output, by column name.
DESCRIPTIONS = {"v1": "original model"}

# The colours of the histograms of the models, in column order.
COLOURS = ["#4C78A8", "#E45756", "#72B7B2", "#F58518", "#54A24B", "#B279A2"]


def find_objdump(requested):
    """The disassembler to use: the one requested, else the first of
    llvm-objdump and objdump found on the PATH."""
    if requested:
        return requested
    for candidate in ("llvm-objdump", "objdump"):
        if shutil.which(candidate):
            return candidate
    sys.exit(
        "error: neither llvm-objdump nor objdump found on the PATH;"
        " use --objdump"
    )


def demangle(symbol):
    """Undo the assembler-level mangling of characters that are not valid in
    symbols (e.g. the brackets and colons in the names of anonymous functions),
    which the emitter writes as $XX with XX the hexadecimal character code."""
    return re.sub(
        r"\$([0-9a-fA-F]{2})", lambda m: chr(int(m.group(1), 16)), symbol
    )


def disassemble(objdump, path):
    """Return (arch, {symbol: (instruction count, size in bytes)}) for an
    object file."""
    try:
        out = subprocess.run(
            # [-z] since runs of zero bytes would otherwise be elided.
            [objdump, "-d", "-z", path],
            check=True,
            capture_output=True,
            text=True,
            errors="replace",
        ).stdout
    except (OSError, subprocess.CalledProcessError) as exn:
        print(f"warning: cannot disassemble {path}: {exn}", file=sys.stderr)
        return None, {}
    arch = None
    counts = collections.OrderedDict()
    sizes = {}
    # Instructions and bytes of alignment padding at the end of the code seen
    # so far for each symbol.
    trailing = {}
    # The symbols at the address of the code being read.  Several symbols can
    # share an address (e.g. the first function of a unit and the unit's
    # code_begin symbol); llvm-objdump prints a line for each of them whereas
    # GNU objdump picks one, so the instructions are attributed to all of
    # them to get the same result either way.
    current = []
    current_address = None
    for line in out.split("\n"):
        if arch is None and "file format" in line:
            lowered = line.lower()
            if "arm64" in lowered or "aarch64" in lowered:
                arch = "arm64"
            elif "x86-64" in lowered or "x86_64" in lowered:
                arch = "x86_64"
        if SECTION_RE.match(line):
            # Addresses restart in each section.
            current = []
            current_address = None
            continue
        m = SYMBOL_RE.match(line)
        if m:
            address = m.group(1)
            symbol = demangle(m.group(2))
            if symbol.startswith("_caml"):  # Mach-O leading underscore
                symbol = symbol[1:]
            if address != current_address:
                current = []
                current_address = address
            current.append(symbol)
            counts.setdefault(symbol, 0)
            sizes.setdefault(symbol, 0)
            continue
        if not current:
            continue
        m = INSN_RE.match(line)
        if not m:
            continue
        num_bytes = len(m.group(1).replace(" ", "")) // 2
        insn = m.group(2)
        for symbol in current:
            sizes[symbol] += num_bytes
            nops, nop_bytes = trailing.get(symbol, (0, 0))
            if insn is None:
                # Continuation of the bytes of the previous instruction.
                if nops:
                    trailing[symbol] = (nops, nop_bytes + num_bytes)
                continue
            counts[symbol] += 1
            # Alignment padding after the last instruction of a function is
            # emitted as nops; do not count it (it is not estimated either).
            if PADDING_RE.match(insn):
                trailing[symbol] = (nops + 1, nop_bytes + num_bytes)
            else:
                trailing[symbol] = (0, 0)
    for symbol, (nops, nop_bytes) in trailing.items():
        counts[symbol] -= nops
        sizes[symbol] -= nop_bytes
    return arch, {symbol: (counts[symbol], sizes[symbol]) for symbol in counts}


def read_estimates(path):
    """Return (the names of the estimate columns, the rows) of a
    code_sizes.csv file.  The symbol and debuginfo fields may contain commas
    (anonymous functions are named after their source location), so each line
    is split from the right around the numeric fields rather than with the
    csv module."""
    rows = []
    with open(path) as f:
        header = f.readline().rstrip("\n").split(",")
        if header[:2] != ["symbol", "debuginfo"] or len(header) < 3:
            return [], rows
        columns = header[2:]
        for line in f:
            line = line.rstrip("\n")
            if not line:
                continue
            fields = line.rsplit(",", len(columns))
            if len(fields) != len(columns) + 1:
                continue
            try:
                symbol, debuginfo = fields[0].split(',"', 1)
                estimates = dict(zip(columns, map(int, fields[1:])))
            except ValueError:
                continue
            rows.append(
                {
                    "symbol": symbol.strip('"'),
                    "debuginfo": debuginfo.rstrip('"'),
                    "estimates": estimates,
                }
            )
    return columns, rows


def models_for(columns, arch):
    """The (column, label) pairs of the models to compare for object files of
    the given architecture: the columns that do not depend on the
    architecture and those for [arch]."""
    models = []
    for column in columns:
        for candidate in ARCHES:
            if column.endswith("_" + candidate):
                if candidate == arch:
                    name = column[: -len(candidate) - 1]
                    description = DESCRIPTIONS.get(name)
                    detail = f"{description}, {arch}" if description else arch
                    models.append((column, f"{name} ({detail})"))
                break
        else:
            description = DESCRIPTIONS.get(column)
            models.append(
                (column, f"{column} ({description})" if description else column)
            )
    return models


def unit_name(csv_path):
    """The name of the compilation unit whose sizes are in [csv_path], as it
    appears in its symbols (e.g. Flambda2_terms__Code_size)."""
    stem = os.path.basename(csv_path)[: -len(".code_sizes.csv")]
    return stem[:1].upper() + stem[1:]


SUFFIX_RE = re.compile(r"_\d+_\d+_code$")


def dedupe(rows, columns):
    """Keep one of each set of identical copies of a function: rows whose
    symbols are the same once the compilation unit and the numeric suffixes
    are removed and whose sizes are all the same."""
    seen = set()
    kept = []
    for r in rows:
        name = r["symbol"]
        prefix = "caml" + unit_name(r["file"]) + "__"
        if name.startswith(prefix):
            name = name[len(prefix) :]
        elif name.startswith("caml"):
            name = name[len("caml") :]
        name = SUFFIX_RE.sub("", name)
        key = (
            name,
            *(r["estimates"][c] for c in columns),
            r["actual"],
            r["bytes"],
        )
        if key not in seen:
            seen.add(key)
            kept.append(r)
    return kept


def object_file_for(csv_path):
    prefix = csv_path[: -len(".code_sizes.csv")]
    return prefix + ".o"


def is_dune_alias_module(prefix):
    """Whether the object [prefix].o is the alias module of a wrapped dune
    library: dune generates its source, <lib>.ml-gen in the library's
    directory, and compiles it with its own flags rather than the ocamlopt
    flags of the workspace, so it gets no code_sizes.csv file; it defines no
    functions anyway."""
    objs_dir, name = os.path.split(prefix)
    if os.path.basename(objs_dir) != "native":
        return False
    objs_dir = os.path.dirname(objs_dir)
    if not os.path.basename(objs_dir).endswith(".objs"):
        return False
    lib_dir = os.path.dirname(objs_dir)
    return os.path.exists(os.path.join(lib_dir, name + ".ml-gen"))


def units_without_csv(roots):
    """The OCaml object files (those with a .cmx next to them) under [roots]
    that have no code_sizes.csv file: units compiled without -dcode-sizes, or
    whose file was deleted afterwards (dune does not know about these files
    and removes them when it next loads the rules of their directory, unless
    the unit is recompiled)."""
    missing = []
    for root in roots:
        if os.path.isfile(root):
            continue
        for dirpath, _dirs, files in os.walk(root):
            for name in sorted(files):
                if not name.endswith(".cmx"):
                    continue
                prefix = os.path.join(dirpath, name[: -len(".cmx")])
                if (
                    os.path.exists(prefix + ".o")
                    and not os.path.exists(prefix + ".code_sizes.csv")
                    and not is_dune_alias_module(prefix)
                ):
                    missing.append(prefix + ".o")
    return missing


def find_csvs(roots):
    for root in roots:
        if os.path.isfile(root):
            yield root
        else:
            # os.walk rather than glob: dune keeps objects in dot-directories
            for dirpath, _dirs, files in os.walk(root):
                for name in sorted(files):
                    if name.endswith(".code_sizes.csv"):
                        yield os.path.join(dirpath, name)


def statistics(logs):
    """Summary statistics of a list of log2(estimate / emitted) values."""
    n = len(logs)
    ordered = sorted(logs)

    def within(fraction):
        bound = math.log2(1 + fraction)
        return sum(1 for v in ordered if abs(v) <= bound) / n

    return {
        "n": n,
        "median": ordered[n // 2],
        "mean_abs": sum(abs(v) for v in ordered) / n,
        "rms": math.sqrt(sum(v * v for v in ordered) / n),
        "within": {f: within(f) for f in (0.10, 0.25, 0.50, 1.0)},
    }


def bin_counts(logs, bin_width, lo, hi):
    """Counts below [lo], in each [bin_width]-wide bin of [lo, hi), and at or
    above [hi]."""
    num_bins = int(round((hi - lo) / bin_width))
    bins = [0] * num_bins
    under = over = 0
    for v in logs:
        if v < lo:
            under += 1
        elif v >= hi:
            over += 1
        else:
            bins[min(num_bins - 1, int((v - lo) / bin_width))] += 1
    return under, bins, over


def summarise(label, logs, bin_width, lo, hi, measure):
    n = len(logs)
    if n == 0:
        print(f"\n{label}: no data")
        return
    st = statistics(logs)
    w = st["within"]
    print(f"\n{label}: log2(estimate / {measure}), n = {n}")
    print(
        f"  median {st['median']:+.3f}   mean |log2| {st['mean_abs']:.3f}"
        f"   rms {st['rms']:.3f}"
        f"   within 10%: {w[0.10]:.0%}   25%: {w[0.25]:.0%}"
        f"   50%: {w[0.50]:.0%}   2x: {w[1.0]:.0%}"
    )
    under, bins, over = bin_counts(logs, bin_width, lo, hi)
    scale = 60.0 / max(1, max(bins))
    print(f"  {'log2':>16s} {'ratio':>6s} {'count':>6s} {'%':>6s}")
    if under:
        print(
            f"  {'< %+.3f' % lo:>16s} {'':>6s} {under:6d} {100*under/n:5.1f}%"
        )
    for i, count in enumerate(bins):
        a = lo + i * bin_width
        edge = f"[{a:+.3f},{a + bin_width:+.3f})"
        ratio = f"{2 ** a:5.2f}x" if i % 2 == 0 else ""
        mark = "|" if abs(a) < 1e-9 else " "
        bar = "#" * int(round(count * scale))
        print(
            f"  {edge:>16s} {ratio:>6s} {count:6d} {100*count/n:5.1f}%"
            f" {mark}{bar}"
        )
    if over:
        print(f"  {'>= %+.3f' % hi:>16s} {'':>6s} {over:6d} {100*over/n:5.1f}%")


def plot(path, series, bin_width, lo, hi, subtitle, measure, footer):
    """Draw the overlaid histograms of [series] (a list of (label, short label,
    colour, logs) tuples) to an image file (format chosen from the
    extension)."""
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import MultipleLocator, PercentFormatter

    plt.rcParams.update(
        {
            "font.family": "sans-serif",
            "font.sans-serif": [
                "Helvetica Neue",
                "Helvetica",
                "Arial",
                "DejaVu Sans",
            ],
            "axes.unicode_minus": True,
            "axes.spines.top": False,
            "axes.spines.right": False,
        }
    )
    fig, ax = plt.subplots(figsize=(15, 8.5))
    fig.subplots_adjust(left=0.07, right=0.98, top=0.835, bottom=0.14)

    num_bins = int(round((hi - lo) / bin_width))
    centres = [lo + (i + 0.5) * bin_width for i in range(num_bins)]
    under_x, over_x = lo - bin_width / 2, hi + bin_width / 2

    # Reference band and line: exact estimates, and within +/- 25%.
    band = math.log2(1.25)
    ax.axvspan(-band, band, color="#000000", alpha=0.045, lw=0, zorder=0)
    ax.axvline(0, color="#333333", lw=1.0, ls=(0, (4, 3)), zorder=1)

    top = 0
    for label, _short, colour, logs in series:
        n = len(logs)
        under, bins, over = bin_counts(logs, bin_width, lo, hi)
        pct = [100.0 * c / n for c in bins]
        top = max(top, max(pct))
        ax.bar(
            centres,
            pct,
            width=bin_width,
            color=colour,
            alpha=0.42,
            lw=0,
            zorder=2,
            label=label,
        )
        ax.step(
            [lo] + [lo + (i + 1) * bin_width for i in range(num_bins)],
            [pct[0]] + pct,
            where="pre",
            color=colour,
            lw=1.6,
            zorder=4,
        )
        # Open-ended end bins, hatched to mark them as such.
        for x, count in ((under_x, under), (over_x, over)):
            if count:
                ax.bar(
                    x,
                    100.0 * count / n,
                    width=bin_width,
                    facecolor=colour,
                    alpha=0.42,
                    edgecolor=colour,
                    hatch="///",
                    lw=0.8,
                    zorder=3,
                )
        st = statistics(logs)
        ax.axvline(
            st["median"], color=colour, lw=1.3, ls=(0, (1.5, 2.5)), zorder=5
        )

    ax.set_xlim(under_x - bin_width, over_x + bin_width)
    ax.set_ylim(0, top * 1.3)
    ax.xaxis.set_major_locator(MultipleLocator(0.5))
    ax.xaxis.set_minor_locator(MultipleLocator(bin_width))
    ax.yaxis.set_major_formatter(PercentFormatter(decimals=0))
    ax.grid(axis="y", color="#dddddd", lw=0.8, zorder=0)
    ax.set_axisbelow(True)
    ax.tick_params(axis="both", labelsize=11)
    ax.set_xlabel(
        (
            f"log₂(estimated size / {measure}), buckets of width"
            f" {bin_width:g}  —  hatched end bars collect everything below"
            f" {lo:+g} and at or above {hi:+g}"
        ).replace("-", "−"),
        fontsize=12,
        labelpad=10,
    )
    ax.set_ylabel("Share of functions", fontsize=12, labelpad=10)

    # Ratio labels along the top.
    ratio_ticks = [t / 2 for t in range(int(2 * lo), int(2 * hi) + 1)]
    top_ax = ax.secondary_xaxis("top", functions=(lambda x: x, lambda x: x))
    top_ax.set_xticks(ratio_ticks)
    top_ax.set_xticklabels(
        [f"{2 ** t:.2g}×" for t in ratio_ticks], fontsize=10.5, color="#444444"
    )
    top_ax.tick_params(length=3, color="#888888")
    top_ax.spines["top"].set_visible(False)
    top_ax.set_xlabel(
        "estimate as a multiple of the emitted size",
        fontsize=10,
        color="#666666",
        labelpad=5,
    )

    ax.annotate(
        "exact",
        xy=(0, top * 1.27),
        ha="center",
        va="top",
        fontsize=10,
        color="#333333",
    )
    ax.annotate(
        "within ±25%",
        xy=(band, top * 1.27),
        xytext=(4, 0),
        textcoords="offset points",
        ha="left",
        va="top",
        fontsize=10,
        color="#666666",
    )
    ax.annotate(
        r"under-estimated  $\leftarrow$",
        xy=(0, 0),
        xytext=(-8, -52),
        textcoords="offset points",
        xycoords=("data", "axes fraction"),
        ha="right",
        va="top",
        fontsize=10.5,
        color="#666666",
    )
    ax.annotate(
        r"$\rightarrow$  over-estimated",
        xy=(0, 0),
        xytext=(8, -52),
        textcoords="offset points",
        xycoords=("data", "axes fraction"),
        ha="left",
        va="top",
        fontsize=10.5,
        color="#666666",
    )

    # Summary table, in the empty upper-right part of the plot.
    header = [
        "model",
        "functions",
        "median log₂",
        "mean |log₂|",
        "rms",
        "within 10%",
        "within 25%",
        "within 50%",
        "within 2×",
    ]
    rows, row_colours = [], []
    for _label, short, colour, logs in series:
        st = statistics(logs)
        w = st["within"]
        rows.append(
            [
                short,
                f"{st['n']:,}",
                f"{st['median']:+.3f}",
                f"{st['mean_abs']:.3f}",
                f"{st['rms']:.3f}",
                f"{w[0.10]:.0%}",
                f"{w[0.25]:.0%}",
                f"{w[0.50]:.0%}",
                f"{w[1.0]:.0%}",
            ]
        )
        row_colours.append(colour)
    table = ax.table(
        cellText=rows,
        colLabels=header,
        cellLoc="center",
        colWidths=[
            0.16,
            0.10,
            0.115,
            0.115,
            0.08,
            0.1075,
            0.1075,
            0.1075,
            0.1075,
        ],
        bbox=[0.425, 0.70, 0.57, 0.185],
        zorder=6,
    )
    table.auto_set_font_size(False)
    table.set_fontsize(10)
    for (r, c), cell in table.get_celld().items():
        cell.set_edgecolor("#cccccc")
        cell.set_linewidth(0.6)
        if r == 0:
            cell.set_text_props(weight="bold", color="#333333")
            cell.set_facecolor("#f2f2f2")
        elif c == 0:
            cell.set_text_props(weight="bold", color="white")
            cell.set_facecolor(row_colours[r - 1])
        else:
            cell.set_facecolor("white")

    from matplotlib.lines import Line2D

    handles, labels = ax.get_legend_handles_labels()
    handles.append(Line2D([], [], color="#555555", lw=1.3, ls=(0, (1.5, 2.5))))
    labels.append("median of each model")
    ax.legend(
        handles,
        labels,
        loc="upper left",
        frameon=False,
        fontsize=12,
        bbox_to_anchor=(0.005, 0.99),
    )
    fig.suptitle(
        "Flambda 2 code size estimates versus emitted machine code",
        fontsize=19,
        fontweight="bold",
        x=0.07,
        ha="left",
        y=0.975,
    )
    fig.text(0.07, 0.925, subtitle, fontsize=12, color="#444444", ha="left")
    fig.text(
        0.98,
        0.02,
        "Estimates written by ocamlopt -dcode-sizes; emitted sizes measured"
        " with objdump on the .o files. " + footer,
        fontsize=9,
        color="#888888",
        ha="right",
    )
    fig.savefig(path, dpi=300)
    print(f"wrote {path}")


def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "roots", nargs="*", help="directories (or CSV files) to process"
    )
    parser.add_argument(
        "--objdump",
        default=None,
        help="disassembler to use (default: llvm-objdump if on the PATH,"
        " else objdump)",
    )
    parser.add_argument(
        "--arch",
        choices=["auto", *ARCHES],
        default="auto",
        help="the architecture of the object files (default: detected from"
        " them); selects the architecture-specific estimates and the"
        " default --unit",
    )
    parser.add_argument(
        "--unit",
        choices=["auto", "bytes", "instructions"],
        default="auto",
        help="how to measure the emitted code (default: bytes on x86-64,"
        " instructions on arm64)",
    )
    parser.add_argument(
        "--bytes-per-insn",
        type=float,
        default=None,
        metavar="B",
        help="with --unit bytes, the number of bytes per estimated"
        " instruction (default: the mean instruction length of the data"
        " set)",
    )
    parser.add_argument(
        "--bin",
        type=float,
        default=0.125,
        help="histogram bin width in log2 units",
    )
    parser.add_argument(
        "--range",
        type=float,
        nargs=2,
        default=(-1.5, 2.5),
        metavar=("LO", "HI"),
        help="histogram range in log2 units",
    )
    parser.add_argument(
        "--csv", help="write the joined per-function data to this file"
    )
    parser.add_argument(
        "--worst",
        type=int,
        default=0,
        metavar="N",
        help="list the N worst over- and under-estimates of a model (see"
        " --worst-model)",
    )
    parser.add_argument(
        "--worst-model",
        metavar="COLUMN",
        default=None,
        help="the model whose worst estimates --worst lists (default: the"
        " last column)",
    )
    parser.add_argument(
        "--from-csv",
        metavar="FILE",
        help="reuse the joined per-function data written by --csv instead"
        " of disassembling (ROOT is then ignored)",
    )
    parser.add_argument(
        "--plot",
        metavar="FILE",
        help="also draw the histograms of the models, overlaid, to this"
        " image file (needs matplotlib)",
    )
    parser.add_argument(
        "--plot-subtitle",
        default=None,
        help="subtitle for the plot (default: describes the data set)",
    )
    parser.add_argument(
        "--no-dedupe",
        dest="dedupe",
        action="store_false",
        help="count every copy of a function inlined into several"
        " compilation units, rather than one of each set of identical"
        " copies",
    )
    args = parser.parse_args()

    joined = []
    columns = None
    arch_seen = None
    missing_objects = unmatched = 0
    objdump = None if args.from_csv else find_objdump(args.objdump)
    if args.from_csv:
        with open(args.from_csv, newline="") as f:
            reader = csv.DictReader(f)
            fields = reader.fieldnames or []
            try:
                columns = fields[
                    fields.index("debuginfo") + 1 : fields.index("actual")
                ]
            except ValueError:
                sys.exit(f"error: {args.from_csv} was not written by --csv")
            for row in reader:
                joined.append(
                    {
                        "file": row["file"],
                        "symbol": row["symbol"],
                        "debuginfo": row["debuginfo"],
                        "estimates": {c: int(row[c]) for c in columns},
                        "actual": int(row["actual"]),
                        "bytes": (
                            int(row["bytes"]) if row.get("bytes") else None
                        ),
                    }
                )
    elif not args.roots:
        parser.error("ROOT is required unless --from-csv is given")
    for csv_path in [] if args.from_csv else find_csvs(args.roots):
        obj = object_file_for(csv_path)
        if not os.path.exists(obj):
            missing_objects += 1
            continue
        file_columns, rows = read_estimates(csv_path)
        if not file_columns:
            print(
                f"warning: {csv_path} is not a code_sizes.csv file",
                file=sys.stderr,
            )
            continue
        if columns is None:
            columns = file_columns
        elif file_columns != columns:
            sys.exit(
                f"error: {csv_path} has the columns {file_columns} but"
                f" earlier files have {columns}"
            )
        arch, counts = disassemble(objdump, obj)
        if arch_seen is None:
            arch_seen = arch
        for row in rows:
            symbol = row["symbol"]
            measured = counts.get(symbol)
            if measured is None:
                unmatched += 1
                continue
            actual, num_bytes = measured
            joined.append(
                {
                    "file": os.path.relpath(csv_path),
                    "symbol": symbol,
                    "debuginfo": row["debuginfo"],
                    "estimates": row["estimates"],
                    "actual": actual,
                    "bytes": num_bytes,
                }
            )
    without_csv = [] if args.from_csv else units_without_csv(args.roots)
    if without_csv:
        print(
            f"warning: {len(without_csv)} OCaml object files have no"
            " code_sizes.csv file (compiled without -dcode-sizes, or the file"
            " was deleted by a later dune run), e.g."
            f" {os.path.relpath(without_csv[0])}; they are not in the report",
            file=sys.stderr,
        )
    if columns is None:
        sys.exit("error: no code_sizes.csv files with an object file found")

    if args.from_csv and args.arch == "auto":
        arch_seen = (
            "arm64"
            if platform.machine().lower() in ("arm64", "aarch64")
            else "x86_64"
        )
    arch = args.arch if args.arch != "auto" else (arch_seen or "x86_64")
    models = models_for(columns, arch)
    if not models:
        sys.exit(f"error: no estimates for {arch} among the columns {columns}")
    model_columns = [column for column, _label in models]
    disassembler = "" if objdump is None else f", {objdump}"
    print(
        f"matched {len(joined)} functions (arch {arch}{disassembler});"
        f" {unmatched} estimates without a symbol;"
        f" {missing_objects} CSV files without an object file"
    )

    if args.csv:
        with open(args.csv, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(
                ["file", "symbol", "debuginfo", *columns, "actual", "bytes"]
            )
            for r in joined:
                writer.writerow(
                    [
                        r["file"],
                        r["symbol"],
                        r["debuginfo"],
                        *(r["estimates"][c] for c in columns),
                        r["actual"],
                        r["bytes"],
                    ]
                )

    unit = (
        args.unit
        if args.unit != "auto"
        else ("bytes" if arch == "x86_64" else "instructions")
    )
    if unit == "bytes" and any(r["bytes"] is None for r in joined):
        sys.exit(
            "error: the data has no sizes in bytes (written by an older"
            " version of this script?); use --unit instructions"
        )
    usable = [
        r
        for r in joined
        if r["actual"] > 0 and all(r["estimates"][c] > 0 for c in model_columns)
    ]
    skipped = len(joined) - len(usable)
    if skipped:
        print(f"skipped {skipped} functions with a zero estimate or no code")
    if not usable:
        sys.exit("error: no function has both an estimate and emitted code")
    num_copies = len(usable)
    if args.dedupe:
        usable = dedupe(usable, columns)
        print(
            f"{len(usable):,} distinct functions after collapsing identical"
            f" copies ({num_copies - len(usable):,} copies)"
        )
    if unit == "bytes":
        total_insns = sum(r["actual"] for r in usable)
        total_bytes = sum(r["bytes"] for r in usable)
        mean_length = total_bytes / max(1, total_insns)
        typical_length = math.exp(
            sum(math.log(r["bytes"] / r["actual"]) for r in usable)
            / max(1, len(usable))
        )
        bytes_per_insn = args.bytes_per_insn or typical_length
        print(
            f"emitted code: {total_insns:,} instructions, {total_bytes:,}"
            f" bytes; mean instruction length {mean_length:.3f} bytes"
            f" overall, {typical_length:.3f} bytes per function (geometric"
            f" mean); one estimated instruction is taken to be"
            f" {bytes_per_insn:.3f} bytes"
        )
        for r in usable:
            r["emitted"] = r["bytes"] / bytes_per_insn
        measure = f"(emitted bytes / {bytes_per_insn:.2f})"
        typical = (
            " (the typical instruction length)"
            if args.bytes_per_insn is None
            else ""
        )
        footer = (
            f"Emitted sizes are in bytes, divided by {bytes_per_insn:.2f}"
            f"{typical}; estimates are in instructions."
        )
    else:
        for r in usable:
            r["emitted"] = r["actual"]
        measure = "emitted instructions"
        footer = "Both are in machine instructions."
    lo, hi = args.range
    series = []
    for i, (column, label) in enumerate(models):
        logs = [
            math.log2(r["estimates"][column] / r["emitted"]) for r in usable
        ]
        summarise(label, logs, args.bin, lo, hi, measure)
        series.append((label, column, COLOURS[i % len(COLOURS)], logs))

    if args.plot:
        subtitle = args.plot_subtitle
        if subtitle is None:
            what = (
                "size in bytes of the code actually emitted"
                if unit == "bytes"
                else "number of instructions actually emitted"
            )
            copies = (
                f" ({num_copies:,} including identical inlined copies)"
                if args.dedupe
                else ""
            )
            subtitle = (
                f"{len(usable):,} functions{copies}, {arch} code  — "
                f" per-function ratio of the estimate to the {what} (from"
                " the .o files)"
            )
        plot(args.plot, series, args.bin, lo, hi, subtitle, measure, footer)

    if args.worst:
        column = args.worst_model or model_columns[-1]
        if column not in model_columns:
            sys.exit(f"error: --worst-model must be one of {model_columns}")
        for r in usable:
            r["log2"] = math.log2(r["estimates"][column] / r["emitted"])
        for title, ordered in (
            (
                f"worst {column} over-estimates",
                sorted(usable, key=lambda r: -r["log2"]),
            ),
            (
                f"worst {column} under-estimates",
                sorted(usable, key=lambda r: r["log2"]),
            ),
        ):
            print(f"\n{title}:")
            for r in ordered[: args.worst]:
                size = f"  bytes {r['bytes']:6d}" if unit == "bytes" else ""
                estimates = "  ".join(
                    f"{c} {r['estimates'][c]:5d}" for c in model_columns
                )
                print(
                    f"  {r['symbol'][:60]:60s} {estimates}"
                    f"  insns {r['actual']:5d}{size}"
                    f"  x{r['estimates'][column] / r['emitted']:.2f}"
                )


if __name__ == "__main__":
    main()
