from pathlib import Path
import sys

CSS_SNIPPET = """
#frames > g[class] > rect,
#frames > a[class] > rect {
  fill: var(--fg-color, #a6a6a6) !important;
}

#frames > g.gc > text,
#frames > a.gc > text {
  fill: #3b1b4f;
}
""".strip()


def inject_css(svg_path: Path) -> None:
    content = svg_path.read_text()
    if "--fg-color" in content and "#frames > g.mode" in content:
        return
    marker = "<style type=\"text/css\">"
    idx = content.find(marker)
    if idx == -1:
        raise SystemExit("Could not find <style> block in SVG")
    idx += len(marker)
    new_content = content[:idx] + "\n" + CSS_SNIPPET + "\n" + content[idx:]
    svg_path.write_text(new_content)


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: inject_flamegraph_css.py SVG_PATH")
    inject_css(Path(sys.argv[1]))


if __name__ == "__main__":
    main()
