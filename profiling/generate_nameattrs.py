import argparse
import re
from pathlib import Path
from typing import Optional, Sequence

GC_KEYWORDS = [
    "gc",
    "alloc",
    "oldify",
    "major_collection",
    "minor_heap",
    "mark_",
    "sweep",
    "empty_minor_heap",
    "do_some_marking",
    "bf_allocate",
]

GC_COLOR = "#8e44ad"
OTHER_COLOR = "#a6a6a6"
PALETTE = [
    "#4c8bf5",
    "#f2842d",
    "#2ca58d",
    "#d7263d",
    "#b5179e",
    "#ff6f61",
    "#00a896",
    "#f9c80e",
    "#3a86ff",
    "#8338ec",
    "#0ead69",
]
IKINDS_COLOR = "#f2842d"
NON_IKINDS_COLOR = "#4c8bf5"


def is_gc_frame(frame: str) -> bool:
    lower = frame.lower()
    return any(keyword in lower for keyword in GC_KEYWORDS)


def extract_symbol(frame: str) -> str:
    if "`" in frame:
        return frame.split("`", 1)[1]
    return frame


def extract_prefix(frame: str) -> Optional[str]:
    symbol = extract_symbol(frame)
    if "__" not in symbol:
        return None
    prefix, _ = symbol.split("__", 1)
    if prefix.startswith("caml"):
        prefix = prefix[4:]
    prefix = prefix.strip('_')
    return prefix or None


def sanitize_class(prefix: str) -> str:
    sanitized = re.sub(r"[^a-z0-9_-]+", "-", prefix.lower()).strip("-")
    return f"mod-{sanitized}" if sanitized else "other"


def compute_prefix_colors(functions: Sequence[str]) -> dict[str, str]:
    prefixes = sorted({
        prefix
        for func in functions
        if not is_gc_frame(func)
        for prefix in [extract_prefix(func)]
        if prefix
    })
    colors: dict[str, str] = {}
    for idx, prefix in enumerate(prefixes):
        colors[prefix] = PALETTE[idx % len(PALETTE)]
    return colors


def extract_functions(folded_path: Path) -> set[str]:
    functions: set[str] = set()
    with folded_path.open() as folded:
        for raw in folded:
            line = raw.strip()
            if not line:
                continue
            stack, *_ = line.rsplit(" ", 1)
            for frame in stack.split(";"):
                frame = frame.strip()
                if not frame:
                    continue
                functions.add(frame)
    return functions


def gather_ikinds_modules(root: Path) -> set[str]:
    modules: set[str] = set()
    for path in root.rglob("*"):
        if path.suffix not in {".ml", ".mli"}:
            continue
        stem = path.stem
        if not stem:
            continue
        module = stem[0].upper() + stem[1:]
        modules.add(module)
    return modules


def categorize(
    func: str,
    scheme: str,
    prefix_colors: Optional[dict[str, str]] = None,
    ikinds_modules: Optional[set[str]] = None,
) -> tuple[str, str]:
    if is_gc_frame(func):
        return "gc", GC_COLOR
    prefix = extract_prefix(func)
    if scheme == "ikinds":
        if prefix is not None and ikinds_modules and prefix in ikinds_modules:
            return "ikinds", IKINDS_COLOR
        return "non-ikinds", NON_IKINDS_COLOR
    if prefix is None or prefix_colors is None:
        return "other", OTHER_COLOR
    color = prefix_colors.get(prefix, OTHER_COLOR)
    return sanitize_class(prefix), color


def write_nameattrs(
    functions: set[str],
    dest: Path,
    scheme: str,
    ikinds_modules: Optional[set[str]] = None,
) -> None:
    prefix_colors = (
        compute_prefix_colors(sorted(functions)) if scheme == "prefix" else None
    )
    lines = []
    for func in sorted(functions):
        category, color = categorize(func, scheme, prefix_colors, ikinds_modules)
        attrs = [f"class={category}", f"g_extra=style=\"--fg-color:{color}\""]
        lines.append("\t".join([func, *attrs]))
    dest.write_text("\n".join(lines) + "\n")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate nameattr mapping for flamegraph coloring"
    )
    parser.add_argument("folded", type=Path, help="Path to collapsed stack file")
    parser.add_argument("nameattrs", type=Path, help="Output nameattr file")
    parser.add_argument(
        "--scheme",
        choices=["prefix", "ikinds"],
        default="prefix",
        help="Coloring scheme to use",
    )
    parser.add_argument(
        "--ikinds-dir",
        type=Path,
        help="Path to the typing/ikinds directory (required for --scheme ikinds)",
    )
    args = parser.parse_args()

    functions = extract_functions(args.folded)
    ikinds_modules = None
    if args.scheme == "ikinds":
        if args.ikinds_dir is None:
            parser.error("--ikinds-dir is required when using --scheme ikinds")
        ikinds_modules = gather_ikinds_modules(args.ikinds_dir)
    write_nameattrs(functions, args.nameattrs, args.scheme, ikinds_modules)


if __name__ == "__main__":
    main()
