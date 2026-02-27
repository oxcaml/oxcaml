#!/usr/bin/env python3

import argparse
import copy
import json
import os

TRACE_PREFIX = "SOLVER_TRACE "


def parse_args():
    parser = argparse.ArgumentParser(
        description="Render solver graph traces into a standalone HTML timeline."
    )
    parser.add_argument("--input", required=True, help="Input compiler log file")
    parser.add_argument("--output", required=True, help="Output HTML file")
    parser.add_argument(
        "--trace",
        type=int,
        default=None,
        help="Initially selected event id",
    )
    return parser.parse_args()


def parse_trace_line(raw_line):
    if not raw_line.startswith(TRACE_PREFIX):
        return None
    payload = raw_line[len(TRACE_PREFIX) :].strip()
    if not payload:
        return None
    try:
        return json.loads(payload)
    except json.JSONDecodeError:
        return None


def load_trace(path):
    events = []
    step_results = {}
    synthetic_event_id = 0
    supported_event_kinds = {
        "trace_delta",
        "trace_var_create",
        "trace_expr_enter",
        "trace_expr_exit",
    }

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            event = parse_trace_line(line)
            if event is None:
                continue
            kind = event.get("kind")
            if kind == "trace_step_end":
                step_id = event.get("step_id")
                if isinstance(step_id, int):
                    step_results[step_id] = event.get("result")
                continue
            if kind not in supported_event_kinds:
                continue

            event_id = event.get("event_id")
            if isinstance(event_id, int):
                synthetic_event_id = max(synthetic_event_id, event_id)
            else:
                synthetic_event_id += 1
                event_id = synthetic_event_id
            event["_event_id"] = event_id
            events.append(event)

    events.sort(key=lambda e: e["_event_id"])
    return events, step_results


def ensure_node(state, var_id):
    node = state.get(var_id)
    if node is not None:
        return node
    node = {
        "level": None,
        "lower": None,
        "upper": None,
        "vlower": [],
        "vupper": [],
        "gencopy": None,
    }
    state[var_id] = node
    return node


def normalize_edge_refs(values):
    if not isinstance(values, list):
        return []
    normalized = []
    for value in values:
        if isinstance(value, int):
            normalized.append({"var_id": value, "modality": None})
        elif isinstance(value, dict):
            var_id = value.get("var_id")
            if not isinstance(var_id, int):
                continue
            modality = value.get("modality")
            if modality is not None and not isinstance(modality, str):
                modality = str(modality)
            normalized.append({"var_id": var_id, "modality": modality})
    normalized.sort(key=lambda edge: (edge["var_id"], edge.get("modality") or ""))
    return normalized


def apply_delta(state, delta):
    var_id = delta["var_id"]
    field = delta["field"]
    node = ensure_node(state, var_id)
    if field in {"vlower", "vupper"}:
        node[field] = normalize_edge_refs(delta.get("new"))
    else:
        node[field] = delta.get("new")


def apply_creation(state, creation):
    var_id = creation["var_id"]
    node = ensure_node(state, var_id)
    node["level"] = creation.get("level")
    node["lower"] = creation.get("lower")
    node["upper"] = creation.get("upper")
    node["vlower"] = normalize_edge_refs(creation.get("vlower"))
    node["vupper"] = normalize_edge_refs(creation.get("vupper"))
    node["provenance"] = creation.get("provenance")
    node["related_var_ids"] = creation.get("related_var_ids") or []


def edge_changes_for_delta(delta):
    field = delta["field"]
    var_id = delta["var_id"]
    old_values = normalize_edge_refs(delta.get("old"))
    new_values = normalize_edge_refs(delta.get("new"))
    if field not in {"vlower", "vupper"}:
        return [], []
    if field == "vlower":
        old_edges = {
            (edge["var_id"], var_id, "vlower", edge.get("modality"))
            for edge in old_values
        }
        new_edges = {
            (edge["var_id"], var_id, "vlower", edge.get("modality"))
            for edge in new_values
        }
    else:
        old_edges = {
            (var_id, edge["var_id"], "vupper", edge.get("modality"))
            for edge in old_values
        }
        new_edges = {
            (var_id, edge["var_id"], "vupper", edge.get("modality"))
            for edge in new_values
        }
    added = sorted(new_edges - old_edges)
    removed = sorted(old_edges - new_edges)
    return added, removed


def edge_changes_for_creation(creation):
    var_id = creation["var_id"]
    vlower = normalize_edge_refs(creation.get("vlower"))
    vupper = normalize_edge_refs(creation.get("vupper"))
    added = {
        (edge["var_id"], var_id, "vlower", edge.get("modality"))
        for edge in vlower
    }
    added.update(
        (var_id, edge["var_id"], "vupper", edge.get("modality"))
        for edge in vupper
    )
    return sorted(added), []


def normalize_location(raw_loc):
    if not isinstance(raw_loc, dict):
        return None
    start = raw_loc.get("start")
    end = raw_loc.get("end")
    file = raw_loc.get("file")
    if not isinstance(file, str):
        return None
    if not isinstance(start, dict) or not isinstance(end, dict):
        return None
    start_line = start.get("line")
    start_col = start.get("col")
    start_cnum = start.get("cnum")
    end_line = end.get("line")
    end_col = end.get("col")
    end_cnum = end.get("cnum")
    ints = [start_line, start_col, start_cnum, end_line, end_col, end_cnum]
    if not all(isinstance(v, int) for v in ints):
        return None
    return {
        "file": file,
        "start": {"line": start_line, "col": start_col, "cnum": start_cnum},
        "end": {"line": end_line, "col": end_col, "cnum": end_cnum},
    }


def pop_expr_stack(stack, expr_id):
    if stack and stack[-1] == expr_id:
        stack.pop()
        return
    for i in range(len(stack) - 1, -1, -1):
        if stack[i] == expr_id:
            del stack[i]
            return


def build_timeline(events, step_results):
    state = {}
    timeline = {}
    order = []
    expr_stack = []
    expr_context = {}

    for event in events:
        event_id = event["_event_id"]
        order.append(event_id)
        changed_nodes = {event.get("var_id")}
        changed_nodes.discard(None)
        added_edges = []
        removed_edges = []
        kind = event.get("kind")

        if kind == "trace_var_create":
            added_edges, removed_edges = edge_changes_for_creation(event)
            apply_creation(state, event)
            op = "create"
        elif kind == "trace_delta":
            added_edges, removed_edges = edge_changes_for_delta(event)
            apply_delta(state, event)
            op = event.get("op") or "apply"
        elif kind == "trace_expr_enter":
            expr_id = event.get("expr_id")
            loc = normalize_location(event.get("loc"))
            if isinstance(expr_id, int):
                expr_stack.append(expr_id)
                expr_context[expr_id] = {
                    "expr_id": expr_id,
                    "loc": loc,
                    "parent_expr_id": event.get("parent_expr_id"),
                }
            op = "enter"
        elif kind == "trace_expr_exit":
            expr_id = event.get("expr_id")
            if isinstance(expr_id, int):
                pop_expr_stack(expr_stack, expr_id)
                expr_context.pop(expr_id, None)
            op = "exit"
        else:
            op = "unknown"

        step_id = event.get("step_id")
        step_result = (
            step_results.get(step_id) if isinstance(step_id, int) else None
        )
        active_expr = None
        if expr_stack:
            active_expr = copy.deepcopy(expr_context.get(expr_stack[-1]))

        timeline[event_id] = {
            "event": event,
            "op": op,
            "step_id": step_id,
            "step_result": step_result,
            "snapshot": copy.deepcopy(state),
            "changed_nodes": sorted(changed_nodes),
            "changed_edges": {
                "added": [
                    {"src": src, "dst": dst, "label": label, "modality": modality}
                    for src, dst, label, modality in added_edges
                ],
                "removed": [
                    {"src": src, "dst": dst, "label": label, "modality": modality}
                    for src, dst, label, modality in removed_edges
                ],
            },
            "active_expr": active_expr,
        }

    return timeline, order


def collect_sources(events, base_dir):
    files = set()
    for event in events:
        if event.get("kind") != "trace_expr_enter":
            continue
        loc = normalize_location(event.get("loc"))
        if loc is None:
            continue
        file = loc["file"]
        if not file:
            continue
        files.add(file)
    sources = {}
    for file in sorted(files):
        candidates = [file]
        if not os.path.isabs(file):
            candidates.append(os.path.normpath(os.path.join(base_dir, file)))
        for path in candidates:
            if not os.path.exists(path):
                continue
            if not os.path.isfile(path):
                continue
            try:
                with open(path, "r", encoding="utf-8") as f:
                    sources[file] = f.read()
                break
            except OSError:
                continue
            except UnicodeDecodeError:
                continue
    return sources


def html_document(data, initial_event):
    data_json = json.dumps(data)
    initial_event_json = json.dumps(initial_event)
    return f"""<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
<title>Solver Graph Trace Viewer</title>
<style>
:root {{
  --bg: #f7f5ee;
  --panel: #fffaf1;
  --ink: #1c1c1c;
  --muted: #6d675d;
  --accent: #0f766e;
  --danger: #b42318;
  --line: #dad2c3;
}}
body {{
  margin: 0;
  font-family: "Iosevka", "Menlo", "Consolas", monospace;
  color: var(--ink);
  background: radial-gradient(circle at 10% 10%, #fff9e8, var(--bg));
}}
.layout {{
  display: grid;
  grid-template-columns: minmax(280px, 380px) 1fr;
  gap: 12px;
  padding: 12px;
}}
@media (max-width: 900px) {{
  .layout {{ grid-template-columns: 1fr; }}
}}
.panel {{
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 10px;
  padding: 10px;
}}
.panel-graph {{
  position: relative;
}}
h1 {{
  margin: 0 0 10px 0;
  font-size: 16px;
}}
.meta {{
  color: var(--muted);
  font-size: 12px;
  margin-bottom: 8px;
}}
label {{
  font-size: 12px;
  color: var(--muted);
}}
select, input[type="range"] {{
  width: 100%;
  margin-top: 6px;
  margin-bottom: 10px;
}}
.result-ok {{ color: var(--accent); }}
.result-error {{ color: var(--danger); }}
.event-card {{
  border: 1px solid var(--line);
  border-radius: 8px;
  background: #fffdf7;
  padding: 8px;
}}
.event-title {{
  font-size: 13px;
  font-weight: 700;
  margin-bottom: 6px;
}}
.event-meaning {{
  font-size: 12px;
  color: var(--muted);
  margin-bottom: 8px;
}}
.event-chips {{
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  margin-bottom: 8px;
}}
.chip {{
  font-size: 11px;
  border: 1px solid var(--line);
  border-radius: 999px;
  padding: 1px 7px;
  background: #f7f1e3;
}}
.event-grid {{
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 4px 8px;
  font-size: 12px;
  margin-bottom: 8px;
}}
.event-key {{
  color: var(--muted);
}}
.event-val {{
  white-space: pre-wrap;
  word-break: break-word;
}}
.event-lists {{
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px;
}}
@media (max-width: 560px) {{
  .event-lists {{ grid-template-columns: 1fr; }}
}}
.event-list-block {{
  border: 1px solid var(--line);
  border-radius: 6px;
  background: #fff;
  padding: 6px 8px;
}}
.event-list-title {{
  font-size: 11px;
  color: var(--muted);
  margin-bottom: 4px;
  text-transform: uppercase;
  letter-spacing: 0.03em;
}}
.event-list {{
  margin: 0;
  padding-left: 16px;
  font-size: 11px;
  line-height: 1.35;
}}
.event-list li {{
  margin: 0 0 3px 0;
  word-break: break-word;
}}
.event-empty {{
  color: var(--muted);
  list-style: none;
  margin-left: -16px;
}}
.event-list-added .event-list-title {{
  color: #166534;
}}
.event-list-removed .event-list-title {{
  color: #b42318;
}}
.source-card {{
  margin-top: 10px;
}}
.source-meta {{
  color: var(--muted);
  font-size: 12px;
  margin-bottom: 6px;
}}
.source-view {{
  margin: 0;
  font-size: 11px;
  line-height: 1.35;
  border: 1px solid var(--line);
  border-radius: 6px;
  background: #fff;
  padding: 8px;
  max-height: 32vh;
  overflow: auto;
  white-space: pre-wrap;
  word-break: break-word;
}}
.source-hi {{
  background: #ffe9a8;
  box-shadow: 0 0 0 1px #f59e0b inset;
}}
.source-node {{
  background: #f5d0fe;
  box-shadow: 0 0 0 1px #c026d3 inset;
}}
.source-sel {{
  background: #dbeafe;
  box-shadow: 0 0 0 1px #60a5fa inset;
}}
.source-hi.source-sel {{
  background: #dff6c8;
  box-shadow:
    0 0 0 1px #f59e0b inset,
    0 0 0 2px #60a5fa inset;
}}
.source-hi.source-node {{
  background: #fde68a;
  box-shadow:
    0 0 0 1px #f59e0b inset,
    0 0 0 2px #c026d3 inset;
}}
.source-node.source-sel {{
  background: #e9d5ff;
  box-shadow:
    0 0 0 1px #c026d3 inset,
    0 0 0 2px #60a5fa inset;
}}
.tok-kw {{
  color: #9d174d;
  font-weight: 600;
}}
.tok-str {{
  color: #0f766e;
}}
.tok-num {{
  color: #1d4ed8;
}}
.tok-comment {{
  color: #6b7280;
  font-style: italic;
}}
.tok-ctor {{
  color: #7c3aed;
}}
#graph {{
  width: 100%;
  min-height: 70vh;
  border: 1px solid var(--line);
  border-radius: 8px;
  background: #fff;
}}
.legend {{
  font-size: 12px;
  color: var(--muted);
  margin-top: 8px;
}}
button {{
  font-family: inherit;
  font-size: 11px;
  border: 1px solid var(--line);
  border-radius: 6px;
  background: #fff;
  color: var(--ink);
  padding: 3px 8px;
  cursor: pointer;
}}
button:hover {{
  background: #f8f2e5;
}}
.node-ops {{
  border: 1px solid var(--line);
  border-radius: 6px;
  background: #fff;
  max-height: 28vh;
  overflow: auto;
  font-size: 11px;
  line-height: 1.35;
}}
.node-op {{
  padding: 6px 8px;
  border-bottom: 1px solid #eee6d8;
}}
.node-op:last-child {{
  border-bottom: none;
}}
.node-op-step {{
  color: var(--muted);
  margin-bottom: 2px;
}}
.node-op-body {{
  color: var(--ink);
  white-space: pre-wrap;
  word-break: break-word;
}}
.backtrace-toolbar {{
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  margin-top: 8px;
  margin-bottom: 10px;
}}
.backtrace-toolbar .meta {{
  margin: 0;
}}
.backtrace-overlay {{
  position: fixed;
  inset: 0;
  background: rgba(28, 28, 28, 0.35);
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  z-index: 30;
}}
.backtrace-overlay[hidden] {{
  display: none;
}}
.backtrace-dialog {{
  width: min(1100px, 96vw);
  max-height: 84vh;
  border: 1px solid var(--line);
  border-radius: 10px;
  padding: 0;
  background: #fffdf7;
  color: var(--ink);
}}
.backtrace-body {{
  padding: 10px;
}}
.backtrace-head {{
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 10px;
  margin-bottom: 8px;
}}
.backtrace-view {{
  margin: 0;
  border: 1px solid var(--line);
  border-radius: 6px;
  background: #fff;
  padding: 8px;
  max-height: 66vh;
  overflow: auto;
  font-size: 11px;
  line-height: 1.3;
  white-space: pre-wrap;
  word-break: break-word;
}}
</style>
</head>
<body>
  <div class=\"layout\">
    <section class=\"panel\">
      <h1>Solver Graph Trace Viewer</h1>
      <div class=\"meta\" id=\"summary\"></div>
      <label for=\"event-select\">Step</label>
      <select id=\"event-select\"></select>
      <label for=\"event-range\">Step Timeline</label>
      <input id=\"event-range\" type=\"range\" min=\"0\" max=\"0\" value=\"0\" />
      <div class=\"backtrace-toolbar\">
        <button id=\"backtrace-open\" type=\"button\">View Backtrace (b)</button>
        <div class=\"meta\" id=\"result\"></div>
      </div>
      <div class=\"event-card source-card\">
        <div class=\"event-title\">Source Focus</div>
        <div id=\"source-meta\" class=\"source-meta\"></div>
        <pre id=\"source-view\" class=\"source-view\"></pre>
      </div>
      <div class=\"event-card source-card\">
        <div class=\"event-title\">Creation Range Filter</div>
        <div id=\"source-selection-meta\" class=\"source-meta\"></div>
        <button id=\"source-selection-clear\" type=\"button\">
          Clear Selection
        </button>
      </div>
      <div id=\"step-event\" class=\"event-card\"></div>
      <div id=\"node-trace\" class=\"event-card source-card\">
        <div class=\"event-title\">Selected Node Trace</div>
        <div id=\"node-trace-meta\" class=\"source-meta\"></div>
        <div id=\"node-trace-ops\" class=\"node-ops\"></div>
      </div>
    </section>
    <section class=\"panel panel-graph\">
      <svg id=\"graph\" viewBox=\"0 0 1200 800\" preserveAspectRatio=\"xMidYMid meet\">
      </svg>
      <div class=\"legend\">
        Node border in orange means changed in selected step.
        Node color is keyed by node prefix letter.
        Edge in orange means edge change in selected step.
        Node subtitle shows current level.
        Edge arrows indicate direction.
        Hover a source or destination node to show that edge's modality label.
        Click a node to select it and inspect its history.
      </div>
    </section>
  </div>
  <div id=\"backtrace-overlay\" class=\"backtrace-overlay\" hidden>
    <div id=\"backtrace-dialog\" class=\"backtrace-dialog\" role=\"dialog\" aria-modal=\"true\" aria-label=\"Event Backtrace\">
      <div class=\"backtrace-body\">
        <div class=\"backtrace-head\">
          <div>
            <div class=\"event-title\">Event Backtrace</div>
            <div id=\"backtrace-meta\" class=\"source-meta\"></div>
          </div>
          <button id=\"backtrace-close\" type=\"button\">Close</button>
        </div>
        <pre id=\"backtrace-view\" class=\"backtrace-view\"></pre>
      </div>
    </div>
  </div>
<script>
const EVENT_DATA = {data_json};
const INITIAL_EVENT = {initial_event_json};

const eventOrder = EVENT_DATA.event_order;
const events = EVENT_DATA.events;
const sourceFiles = EVENT_DATA.sources || {{}};
const sourceFileOrder = Object.keys(sourceFiles).sort();

const select = document.getElementById("event-select");
const range = document.getElementById("event-range");
const summary = document.getElementById("summary");
const resultEl = document.getElementById("result");
const stepEvent = document.getElementById("step-event");
const sourceMeta = document.getElementById("source-meta");
const sourceView = document.getElementById("source-view");
const sourceSelectionMeta = document.getElementById("source-selection-meta");
const sourceSelectionClear = document.getElementById("source-selection-clear");
const nodeTraceMeta = document.getElementById("node-trace-meta");
const nodeTraceOps = document.getElementById("node-trace-ops");
const graph = document.getElementById("graph");
const backtraceOpen = document.getElementById("backtrace-open");
const backtraceOverlay = document.getElementById("backtrace-overlay");
const backtraceDialog = document.getElementById("backtrace-dialog");
const backtraceClose = document.getElementById("backtrace-close");
const backtraceMeta = document.getElementById("backtrace-meta");
const backtraceView = document.getElementById("backtrace-view");
const LAYOUT_WIDTH = 1200;
const LAYOUT_HEIGHT = 800;
const LAYOUT_PAD = 96;
const LAYOUT_MIN_SEP = 76;
const EDGE_TARGET = 108;
const NODE_RADIUS = 24;
const LEVEL_INFINITY_BASE = 100000000;
const DEFAULT_NODE_PALETTE = {{
  fill: "#f4f1eb",
  stroke: "#5c5548",
  text: "#2d2923",
  level: "#5f5648",
}};
const NODE_PREFIX_PALETTE = {{
  C: {{ fill: "#d9ecff", stroke: "#2f6fae", text: "#1b4976", level: "#2f6fae" }},
  M: {{ fill: "#dff8ea", stroke: "#237a52", text: "#145338", level: "#237a52" }},
  a: {{ fill: "#ffe7c6", stroke: "#b46412", text: "#7d430c", level: "#a65a10" }},
  c: {{ fill: "#f9d8d8", stroke: "#b24652", text: "#7f2231", level: "#a23a45" }},
  f: {{ fill: "#eadcff", stroke: "#6c4aa5", text: "#4b2f78", level: "#64479a" }},
  l: {{ fill: "#fff4c7", stroke: "#9e7a16", text: "#6f550d", level: "#907116" }},
  p: {{ fill: "#d9f2ff", stroke: "#1e7899", text: "#14516a", level: "#1f6f8c" }},
  s: {{ fill: "#ffdede", stroke: "#a84a4a", text: "#743232", level: "#9a4343" }},
  u: {{ fill: "#ece7ff", stroke: "#5a52a3", text: "#3f3875", level: "#534b95" }},
  v: {{ fill: "#e1f0d6", stroke: "#53803a", text: "#355626", level: "#4f7738" }},
  y: {{ fill: "#ffe6a6", stroke: "#9b6a08", text: "#6c4a08", level: "#8d620d" }},
}};

let unionBasePositions = null;
const layoutCache = new Map();
let layoutComputedUpto = -1;
let currentIndex = 0;
const nodeKindById = new Map();
const nodeCreationById = new Map();
let lastSourceFile = sourceFileOrder.length > 0 ? sourceFileOrder[0] : null;
let lastSourceStart = 0;
let lastSourceEnd = 0;
let currentSourceFile = null;
let selectedSourceRange = null;
let selectedNodeId = null;
let hoveredNodeId = null;
let lastSelectionCaptureMs = 0;

function edgeKey(edge) {{
  return `${{edge.src}}|${{edge.dst}}|${{edge.label}}|${{edge.modality || ""}}`;
}}

function normalizeEdgeRef(ref) {{
  if (typeof ref === "number") {{
    return {{ var_id: ref, modality: null }};
  }}
  if (ref && typeof ref === "object" && typeof ref.var_id === "number") {{
    return {{
      var_id: ref.var_id,
      modality: typeof ref.modality === "string" ? ref.modality : null,
    }};
  }}
  return null;
}}

function edgeGroupKey(edge) {{
  return `${{edge.src}}|${{edge.dst}}`;
}}

function reverseEdgeGroupKey(edge) {{
  return `${{edge.dst}}|${{edge.src}}`;
}}

function edgePairSign(edge) {{
  if (edge.src === edge.dst) return 0;
  return edge.src < edge.dst ? 1 : -1;
}}

function splitConstTokens(raw) {{
  if (typeof raw !== "string" || raw.length === 0) return [];
  return raw.split(",").map((s) => s.trim()).filter((s) => s.length > 0);
}}

function classifyNodeKind(lowerRaw, upperRaw) {{
  const lower = splitConstTokens(lowerRaw);
  if (lowerRaw && String(lowerRaw).includes(";")) return "full";
  if (lower.length === 6) return "comonadic";
  if (lower.length === 4) return "monadic";
  if (lower.length === 1) {{
    const token = lower[0];
    switch (token) {{
      case "global":
      case "local":
      case "regional":
        return "areality";
      case "many":
      case "once":
        return "linearity";
      case "portable":
      case "shareable":
      case "nonportable":
        return "portability";
      case "forkable":
      case "unforkable":
        return "forkable";
      case "yielding":
      case "unyielding":
        return "yielding";
      case "stateless":
      case "stateful":
        return "statefulness";
      case "aliased":
      case "unique":
        return "uniqueness";
      case "uncontended":
      case "shared":
      case "contended":
        return "contention";
      case "immutable":
      case "read":
      case "read_write":
        return "visibility";
      case "dynamic":
      case "static":
        return "staticity";
      default:
        return "single";
    }}
  }}
  return "unknown";
}}

function kindPrefix(kind) {{
  switch (kind) {{
    case "comonadic":
      return "C";
    case "monadic":
      return "M";
    case "full":
      return "f";
    case "areality":
      return "a";
    case "linearity":
      return "l";
    case "portability":
      return "p";
    case "forkable":
      return "f";
    case "yielding":
      return "y";
    case "statefulness":
      return "s";
    case "uniqueness":
      return "u";
    case "contention":
      return "c";
    case "visibility":
      return "v";
    case "staticity":
      return "s";
    case "single":
      return "s";
    default:
      return "u";
  }}
}}

function nodeKindFromNode(node) {{
  if (!node || typeof node !== "object") return "unknown";
  return classifyNodeKind(node.lower, node.upper);
}}

function maybeIndexNodeKind(id, lower, upper) {{
  if (nodeKindById.has(id)) return;
  nodeKindById.set(id, classifyNodeKind(lower, upper));
}}

function initNodeKindIndex() {{
  eventOrder.forEach((eventId) => {{
    const item = events[String(eventId)];
    const event = (item && item.event) || {{}};
    if (
      event.kind === "trace_var_create" &&
      typeof event.var_id === "number"
    ) {{
      maybeIndexNodeKind(event.var_id, event.lower, event.upper);
    }}
    const snapshot = (item && item.snapshot) || {{}};
    Object.entries(snapshot).forEach(([rawId, node]) => {{
      const id = Number(rawId);
      if (!Number.isFinite(id)) return;
      maybeIndexNodeKind(id, node.lower, node.upper);
    }});
  }});
}}

function initNodeCreationIndex() {{
  eventOrder.forEach((eventId) => {{
    const item = events[String(eventId)];
    if (!item) return;
    const event = item.event || {{}};
    if (event.kind !== "trace_var_create") return;
    if (typeof event.var_id !== "number") return;
    if (nodeCreationById.has(event.var_id)) return;
    const activeExpr =
      item.active_expr && typeof item.active_expr === "object"
        ? item.active_expr
        : null;
    const loc =
      activeExpr && activeExpr.loc && typeof activeExpr.loc === "object"
        ? activeExpr.loc
        : null;
    nodeCreationById.set(event.var_id, {{
      event_id: eventId,
      step_id: item.step_id,
      provenance: event.provenance || null,
      expr_id:
        activeExpr && typeof activeExpr.expr_id === "number"
          ? activeExpr.expr_id
          : null,
      loc,
    }});
  }});
}}

function shortFileName(file) {{
  if (typeof file !== "string" || file.length === 0) return "?";
  const parts = file.split("/");
  if (parts.length <= 2) return file;
  return `.../${{parts.slice(-2).join("/")}}`;
}}

function formatLocationCompact(loc) {{
  if (!loc || typeof loc !== "object") return "unknown";
  const file = shortFileName(loc.file || "");
  const start = loc.start || {{}};
  const end = loc.end || {{}};
  const sl = Number.isFinite(start.line) ? start.line : "?";
  const sc = Number.isFinite(start.col) ? start.col : "?";
  const el = Number.isFinite(end.line) ? end.line : "?";
  const ec = Number.isFinite(end.col) ? end.col : "?";
  return `${{file}}:${{sl}}:${{sc}}-${{el}}:${{ec}}`;
}}

function rangesOverlap(a0, a1, b0, b1) {{
  return a0 < b1 && b0 < a1;
}}

function nodeIdsCreatedInRange(rangeSpec) {{
  const ids = new Set();
  if (!rangeSpec || typeof rangeSpec !== "object") return ids;
  const file = rangeSpec.file;
  const start = rangeSpec.start;
  const end = rangeSpec.end;
  if (typeof file !== "string") return ids;
  if (!Number.isFinite(start) || !Number.isFinite(end) || end <= start) return ids;
  nodeCreationById.forEach((creation, nodeId) => {{
    const loc = creation && creation.loc;
    if (!loc || typeof loc !== "object") return;
    if (loc.file !== file) return;
    const ls = loc.start && loc.start.cnum;
    const le = loc.end && loc.end.cnum;
    if (!Number.isFinite(ls) || !Number.isFinite(le)) return;
    if (rangesOverlap(start, end, ls, le)) ids.add(nodeId);
  }});
  return ids;
}}

function varLabel(id, node = null) {{
  let kind = nodeKindById.get(id);
  if (!kind && node) {{
    kind = nodeKindFromNode(node);
    nodeKindById.set(id, kind);
  }}
  if (!kind) kind = "unknown";
  return `${{kindPrefix(kind)}}${{id}}`;
}}

function varPrefix(id, node = null) {{
  let kind = nodeKindById.get(id);
  if (!kind && node) {{
    kind = nodeKindFromNode(node);
    nodeKindById.set(id, kind);
  }}
  if (!kind) kind = "unknown";
  return kindPrefix(kind);
}}

function nodePalette(prefix) {{
  return NODE_PREFIX_PALETTE[prefix] || DEFAULT_NODE_PALETTE;
}}

function varLabelFromEvent(event) {{
  if (!event || typeof event.var_id !== "number") return "x?";
  if (event.kind === "trace_var_create") {{
    const kind = classifyNodeKind(event.lower, event.upper);
    return `${{kindPrefix(kind)}}${{event.var_id}}`;
  }}
  return varLabel(event.var_id);
}}

function clampIndex(index) {{
  if (eventOrder.length === 0) return 0;
  return Math.max(0, Math.min(index, eventOrder.length - 1));
}}

function shortActionForItem(item) {{
  const event = item.event || {{}};
  if (event.kind === "trace_var_create") {{
    return `create ${{varLabelFromEvent(event)}}`;
  }}
  if (event.kind === "trace_expr_enter") {{
    const exprId =
      typeof event.expr_id === "number" ? String(event.expr_id) : "?";
    return `enter expr#${{exprId}}`;
  }}
  if (event.kind === "trace_expr_exit") {{
    const exprId =
      typeof event.expr_id === "number" ? String(event.expr_id) : "?";
    const result = event.result || "unknown";
    return `exit expr#${{exprId}} (${{result}})`;
  }}
  const field = event.field || "?";
  const op = item.op || event.op || "apply";
  return `${{varLabelFromEvent(event)}}.${{field}} (${{op}})`;
}}

function stepLabelFor(item, eventId) {{
  if (item.step_id === null || item.step_id === undefined) {{
    return `step ? - ${{shortActionForItem(item)}}`;
  }}
  return `step ${{item.step_id}} - ${{shortActionForItem(item)}}`;
}}

function isTypingTarget(target) {{
  if (!target || !(target instanceof HTMLElement)) return false;
  if (target.isContentEditable) return true;
  const tag = target.tagName;
  return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT";
}}

function setIndex(nextIndex) {{
  const clamped = clampIndex(nextIndex);
  currentIndex = clamped;
  select.value = String(clamped);
  range.value = String(clamped);
  render(clamped);
}}

function initControls() {{
  eventOrder.forEach((eventId, index) => {{
    const item = events[String(eventId)];
    const option = document.createElement("option");
    option.value = String(index);
    option.textContent = stepLabelFor(item, eventId);
    select.appendChild(option);
  }});
  range.max = String(Math.max(0, eventOrder.length - 1));
  const wanted = eventOrder.indexOf(INITIAL_EVENT);
  const selected = wanted >= 0 ? wanted : Math.max(eventOrder.length - 1, 0);
  currentIndex = selected;
  select.value = String(currentIndex);
  range.value = String(currentIndex);
  select.addEventListener("change", () => {{
    setIndex(Number(select.value));
  }});
  range.addEventListener("input", () => {{
    setIndex(Number(range.value));
  }});
}}

function currentBacktrace(item) {{
  const event = (item && item.event) || {{}};
  return typeof event.backtrace === "string" ? event.backtrace : "";
}}

function updateBacktracePanel(item, eventId) {{
  if (!item) {{
    backtraceMeta.textContent = "";
    backtraceView.textContent = "";
    return;
  }}
  const event = item.event || {{}};
  const backtrace = currentBacktrace(item);
  backtraceMeta.textContent =
    `${{stepLabelFor(item, eventId)}} | ${{event.kind || "event"}}`;
  backtraceView.textContent =
    backtrace.length > 0 ? backtrace : "No backtrace recorded for this event.";
}}

function isBacktraceOpen() {{
  return !backtraceOverlay.hasAttribute("hidden");
}}

function openBacktraceDialog() {{
  backtraceOverlay.removeAttribute("hidden");
}}

function closeBacktraceDialog() {{
  backtraceOverlay.setAttribute("hidden", "");
}}

function initKeyboardNavigation() {{
  document.addEventListener("keydown", (event) => {{
    if (event.key === "Escape" && isBacktraceOpen()) {{
      event.preventDefault();
      closeBacktraceDialog();
      return;
    }}

    if (
      !event.altKey &&
      !event.ctrlKey &&
      !event.metaKey &&
      event.key.toLowerCase() === "b"
    ) {{
      if (isTypingTarget(event.target)) return;
      event.preventDefault();
      if (isBacktraceOpen()) {{
        closeBacktraceDialog();
      }} else {{
        openBacktraceDialog();
      }}
      return;
    }}

    if (eventOrder.length === 0) return;
    if (event.altKey || event.ctrlKey || event.metaKey) return;
    if (isTypingTarget(event.target)) return;

    if (event.key === "ArrowLeft" || event.key === "PageUp" || event.key === "k") {{
      event.preventDefault();
      setIndex(currentIndex - 1);
      return;
    }}
    if (
      event.key === "ArrowRight" ||
      event.key === "PageDown" ||
      event.key === "j"
    ) {{
      event.preventDefault();
      setIndex(currentIndex + 1);
      return;
    }}
    if (event.key === "Home") {{
      event.preventDefault();
      setIndex(0);
      return;
    }}
    if (event.key === "End") {{
      event.preventDefault();
      setIndex(eventOrder.length - 1);
    }}
  }});
}}

function formatValue(value) {{
  if (value === null || value === undefined) return "null";
  if (typeof value === "string") return value;
  return JSON.stringify(value);
}}

function formatLevelValue(value) {{
  if (value === null || value === undefined) return "null";
  if (typeof value !== "number" || !Number.isFinite(value)) {{
    return String(value);
  }}
  if (!Number.isInteger(value)) return String(value);
  if (value < LEVEL_INFINITY_BASE) return String(value);
  const extra = value - LEVEL_INFINITY_BASE;
  if (extra === 0) return "\\u221E";
  return `\\u221E+${{extra}}`;
}}

function formatFieldValue(field, value) {{
  return field === "level" ? formatLevelValue(value) : formatValue(value);
}}

function formatLocation(loc) {{
  if (!loc || typeof loc !== "object") return "unknown";
  const file = typeof loc.file === "string" ? loc.file : "?";
  const start = loc.start || {{}};
  const end = loc.end || {{}};
  const sl = Number.isFinite(start.line) ? start.line : "?";
  const sc = Number.isFinite(start.col) ? start.col : "?";
  const el = Number.isFinite(end.line) ? end.line : "?";
  const ec = Number.isFinite(end.col) ? end.col : "?";
  return `${{file}}:${{sl}}:${{sc}}-${{el}}:${{ec}}`;
}}

function normalizeRange(startRaw, endRaw, maxLen) {{
  let start = Number.isFinite(startRaw) ? startRaw : 0;
  let end = Number.isFinite(endRaw) ? endRaw : start;
  start = Math.max(0, Math.min(start, maxLen));
  end = Math.max(0, Math.min(end, maxLen));
  if (end < start) {{
    const tmp = start;
    start = end;
    end = tmp;
  }}
  return {{ start, end }};
}}

function renderSourceWindow(
  source,
  focusStartRaw,
  focusEndRaw,
  highlightFocus,
  selectedRangeRaw,
  nodeRangeRaw
) {{
  const focus = normalizeRange(focusStartRaw, focusEndRaw, source.length);
  const selected = selectedRangeRaw
    ? normalizeRange(selectedRangeRaw.start, selectedRangeRaw.end, source.length)
    : null;
  const nodeRange = nodeRangeRaw
    ? normalizeRange(nodeRangeRaw.start, nodeRangeRaw.end, source.length)
    : null;
  const cuts = new Set([0, source.length, focus.start, focus.end]);
  if (selected) {{
    cuts.add(selected.start);
    cuts.add(selected.end);
  }}
  if (nodeRange) {{
    cuts.add(nodeRange.start);
    cuts.add(nodeRange.end);
  }}
  const sortedCuts = Array.from(cuts).sort((a, b) => a - b);
  sourceView.replaceChildren();
  for (let i = 0; i + 1 < sortedCuts.length; i += 1) {{
    const start = sortedCuts[i];
    const end = sortedCuts[i + 1];
    if (end <= start) continue;
    const text = source.slice(start, end);
    const classes = [];
    if (
      highlightFocus &&
      focus.end > focus.start &&
      start >= focus.start &&
      end <= focus.end
    ) {{
      classes.push("source-hi");
    }}
    if (
      selected &&
      selected.end > selected.start &&
      start >= selected.start &&
      end <= selected.end
    ) {{
      classes.push("source-sel");
    }}
    if (
      nodeRange &&
      nodeRange.end > nodeRange.start &&
      start >= nodeRange.start &&
      end <= nodeRange.end
    ) {{
      classes.push("source-node");
    }}
    appendSourceCode(sourceView, text, classes);
  }}
}}

function renderSourceForItem(item) {{
  const focusedNodeId =
    selectedNodeId !== null && selectedNodeId !== undefined
      ? selectedNodeId
      : hoveredNodeId;
  const focusedCreation =
    focusedNodeId !== null && focusedNodeId !== undefined
      ? nodeCreationById.get(focusedNodeId)
      : null;
  const focusedLoc =
    focusedCreation &&
    focusedCreation.loc &&
    typeof focusedCreation.loc === "object"
      ? focusedCreation.loc
      : null;
  const focusedFile =
    focusedLoc && typeof focusedLoc.file === "string" ? focusedLoc.file : null;
  const focusedSource =
    focusedFile && typeof sourceFiles[focusedFile] === "string"
      ? sourceFiles[focusedFile]
      : null;
  const focusedStart = focusedLoc && focusedLoc.start && focusedLoc.start.cnum;
  const focusedEnd = focusedLoc && focusedLoc.end && focusedLoc.end.cnum;
  const focusedRange =
    Number.isFinite(focusedStart) && Number.isFinite(focusedEnd)
      ? {{ start: focusedStart, end: focusedEnd }}
      : null;

  const active = item && item.active_expr ? item.active_expr : null;
  if (!active || !active.loc) {{
    const fallbackFile =
      focusedFile ||
      lastSourceFile ||
      (sourceFileOrder.length > 0 ? sourceFileOrder[0] : null);
    if (!fallbackFile) {{
      sourceMeta.textContent = "";
      sourceView.textContent = "";
      currentSourceFile = null;
      return;
    }}
    const fallbackSource =
      fallbackFile === focusedFile && focusedSource
        ? focusedSource
        : sourceFiles[fallbackFile];
    if (typeof fallbackSource !== "string") {{
      sourceMeta.textContent = "";
      sourceView.textContent = "";
      currentSourceFile = null;
      return;
    }}
    sourceMeta.textContent = "";
    currentSourceFile = fallbackFile;
    renderSourceWindow(
      fallbackSource,
      lastSourceStart,
      lastSourceEnd,
      false,
      selectedSourceRange && selectedSourceRange.file === fallbackFile
        ? selectedSourceRange
        : null,
      focusedRange && fallbackFile === focusedFile ? focusedRange : null
    );
    return;
  }}
  const loc = active.loc;
  sourceMeta.textContent = "";
  const file = typeof loc.file === "string" ? loc.file : "";
  const useFocusedFile = focusedRange && focusedFile && focusedSource;
  const sourceFile = useFocusedFile ? focusedFile : file;
  const source = useFocusedFile ? focusedSource : sourceFiles[file];
  if (typeof source !== "string") {{
    sourceMeta.textContent = "";
    currentSourceFile = null;
    return;
  }}
  const start = useFocusedFile ? focusedStart : loc.start && loc.start.cnum;
  const end = useFocusedFile ? focusedEnd : loc.end && loc.end.cnum;
  lastSourceFile = sourceFile;
  lastSourceStart = Number.isFinite(start) ? start : 0;
  lastSourceEnd =
    Number.isFinite(end) ? end : Number.isFinite(start) ? start : 0;
  currentSourceFile = sourceFile;
  renderSourceWindow(
    source,
    lastSourceStart,
    lastSourceEnd,
    !useFocusedFile,
    selectedSourceRange && selectedSourceRange.file === sourceFile
      ? selectedSourceRange
      : null,
    focusedRange && sourceFile === focusedFile ? focusedRange : null
  );
}}

function getSelectionOffsetsInSourceView() {{
  const selection = window.getSelection();
  if (!selection || selection.rangeCount === 0 || selection.isCollapsed) {{
    return null;
  }}
  const rangeSel = selection.getRangeAt(0);
  if (!sourceView.contains(rangeSel.startContainer)) return null;
  if (!sourceView.contains(rangeSel.endContainer)) return null;
  const startRange = document.createRange();
  startRange.selectNodeContents(sourceView);
  startRange.setEnd(rangeSel.startContainer, rangeSel.startOffset);
  const endRange = document.createRange();
  endRange.selectNodeContents(sourceView);
  endRange.setEnd(rangeSel.endContainer, rangeSel.endOffset);
  let start = startRange.toString().length;
  let end = endRange.toString().length;
  if (end < start) {{
    const tmp = start;
    start = end;
    end = tmp;
  }}
  return {{ start, end }};
}}

function updateSourceSelectionMeta() {{
  sourceSelectionMeta.textContent = "";
}}

function maybeCaptureSourceSelection() {{
  const offsets = getSelectionOffsetsInSourceView();
  if (!offsets) return;
  if (!currentSourceFile || !sourceFiles[currentSourceFile]) return;
  if (offsets.end <= offsets.start) return;
  selectedSourceRange = {{
    file: currentSourceFile,
    start: offsets.start,
    end: offsets.end,
  }};
  lastSelectionCaptureMs = Date.now();
  render(currentIndex);
}}

function scheduleCaptureSourceSelection() {{
  window.setTimeout(maybeCaptureSourceSelection, 0);
}}

const OCAML_KEYWORDS = new Set([
  "and", "as", "assert", "begin", "class", "constraint", "do", "done",
  "downto", "else", "end", "exception", "external", "false", "for", "fun",
  "function", "functor", "if", "in", "include", "inherit", "initializer",
  "lazy", "let", "match", "method", "module", "mutable", "new", "nonrec",
  "object", "of", "open", "or", "private", "rec", "sig", "struct", "then",
  "to", "true", "try", "type", "val", "virtual", "when", "while", "with",
]);

const OCAML_TOKEN_RE = new RegExp(
  String.raw`\\(\\*[\\s\\S]*?\\*\\)|"(?:\\\\.|[^"\\\\])*"|'(?:\\\\.|[^'\\\\])'|\\b\\d[\\d_]*\\b|\\b[A-Za-z_][A-Za-z0-9_']*\\b`,
  "g"
);

function sourceTokenClass(token) {{
  if (token.startsWith("(*")) return "tok-comment";
  if (token.startsWith('"') || token.startsWith("'")) return "tok-str";
  if (/^\\d/.test(token)) return "tok-num";
  if (OCAML_KEYWORDS.has(token)) return "tok-kw";
  if (/^[A-Z]/.test(token)) return "tok-ctor";
  return "";
}}

function appendSourceChunk(container, text, classes) {{
  if (!text) return;
  if (classes.length === 0) {{
    container.appendChild(document.createTextNode(text));
    return;
  }}
  const span = document.createElement("span");
  span.className = classes.join(" ");
  span.textContent = text;
  container.appendChild(span);
}}

function appendSourceCode(container, text, baseClassesRaw) {{
  if (!text) return;
  const baseClasses = Array.isArray(baseClassesRaw) ? baseClassesRaw : [];
  OCAML_TOKEN_RE.lastIndex = 0;
  let last = 0;
  while (true) {{
    const match = OCAML_TOKEN_RE.exec(text);
    if (!match) break;
    const start = match.index;
    if (start > last) {{
      appendSourceChunk(container, text.slice(last, start), baseClasses);
    }}
    const token = match[0];
    const tokenClass = sourceTokenClass(token);
    const classes = tokenClass ? [...baseClasses, tokenClass] : baseClasses;
    appendSourceChunk(container, token, classes);
    last = OCAML_TOKEN_RE.lastIndex;
  }}
  if (last < text.length) {{
    appendSourceChunk(container, text.slice(last), baseClasses);
  }}
}}

function edgeRefLabel(rawRef) {{
  const ref = normalizeEdgeRef(rawRef);
  if (!ref) return null;
  const name = varLabel(ref.var_id);
  return ref.modality ? `${{name}} (m=${{ref.modality}})` : name;
}}

function edgeRefList(rawValue) {{
  if (!Array.isArray(rawValue)) return [];
  const labels = [];
  rawValue.forEach((ref) => {{
    const label = edgeRefLabel(ref);
    if (label !== null) labels.push(label);
  }});
  labels.sort();
  return labels;
}}

function fieldLabel(field) {{
  switch (field) {{
    case "level":
      return "level";
    case "lower":
      return "lower bound";
    case "upper":
      return "upper bound";
    case "vlower":
      return "lower edges";
    case "vupper":
      return "upper edges";
    case "gencopy":
      return "gencopy";
    default:
      return field || "?";
  }}
}}

function opLabel(op) {{
  return op === "undo" ? "Rollback" : "Apply";
}}

function summarizeList(items, maxItems = 6) {{
  if (items.length === 0) return "[]";
  if (items.length <= maxItems) return items.join(", ");
  const shown = items.slice(0, maxItems).join(", ");
  return `${{shown}}, ... (+${{items.length - maxItems}})`;
}}

function listDiff(oldItems, newItems) {{
  const oldSet = new Set(oldItems);
  const newSet = new Set(newItems);
  const added = [];
  const removed = [];
  newItems.forEach((x) => {{
    if (!oldSet.has(x)) added.push(x);
  }});
  oldItems.forEach((x) => {{
    if (!newSet.has(x)) removed.push(x);
  }});
  return {{ added, removed }};
}}

function edgeRefItems(rawValue, field, ownerVarId) {{
  if (!Array.isArray(rawValue)) return [];
  const items = [];
  rawValue.forEach((rawRef) => {{
    const ref = normalizeEdgeRef(rawRef);
    if (!ref) return;
    const src = field === "vlower" ? ref.var_id : ownerVarId;
    const dst = field === "vlower" ? ownerVarId : ref.var_id;
    const modality =
      typeof ref.modality === "string" && ref.modality.length > 0
        ? ref.modality
        : null;
    const key = `${{src}}|${{dst}}|${{modality || ""}}`;
    const text =
      `${{varLabel(src)}} \u2192 ${{varLabel(dst)}}` +
      `${{modality ? ` (m=${{modality}})` : ""}}`;
    items.push({{ key, src, dst, modality, text }});
  }});
  items.sort((a, b) => {{
    if (a.src !== b.src) return a.src - b.src;
    if (a.dst !== b.dst) return a.dst - b.dst;
    return (a.modality || "").localeCompare(b.modality || "");
  }});
  return items;
}}

function diffEdgeItems(oldItems, newItems) {{
  const oldMap = new Map(oldItems.map((item) => [item.key, item]));
  const newMap = new Map(newItems.map((item) => [item.key, item]));
  const added = [];
  const removed = [];
  newItems.forEach((item) => {{
    if (!oldMap.has(item.key)) added.push(item.text);
  }});
  oldItems.forEach((item) => {{
    if (!newMap.has(item.key)) removed.push(item.text);
  }});
  return {{ added, removed }};
}}

function diffEdgeObjects(oldItems, newItems) {{
  const oldMap = new Map(oldItems.map((item) => [item.key, item]));
  const newMap = new Map(newItems.map((item) => [item.key, item]));
  const added = [];
  const removed = [];
  newItems.forEach((item) => {{
    if (!oldMap.has(item.key)) added.push(item);
  }});
  oldItems.forEach((item) => {{
    if (!newMap.has(item.key)) removed.push(item);
  }});
  return {{ added, removed }};
}}

function truncateText(text, maxLen = 120) {{
  const s = String(text);
  if (s.length <= maxLen) return s;
  return `${{s.slice(0, maxLen - 3)}}...`;
}}

function describeNodeEvent(nodeId, eventId) {{
  const item = events[String(eventId)];
  if (!item) return null;
  const event = item.event || {{}};
  if (!event.kind) return null;
  const step = stepLabelFor(item, eventId);
  if (event.kind === "trace_var_create") {{
    if (event.var_id === nodeId) {{
      const creation = nodeCreationById.get(nodeId);
      const locText =
        creation && creation.loc
          ? formatLocation(creation.loc)
          : "unknown expression range";
      const provenance = event.provenance || "unknown";
      return {{
        step,
        body:
          `create ${{varLabel(nodeId)}}` +
          ` (provenance=${{provenance}}, range=${{locText}})`,
      }};
    }}
    const lowerEdges = edgeRefItems(event.vlower, "vlower", event.var_id);
    const upperEdges = edgeRefItems(event.vupper, "vupper", event.var_id);
    const touches =
      lowerEdges
        .concat(upperEdges)
        .filter((e) => e.src === nodeId || e.dst === nodeId);
    if (touches.length === 0) return null;
    return {{
      step,
      body:
        `create ${{varLabel(event.var_id)}} with incident edge` +
        ` ${{truncateText(touches.map((t) => t.text).join("; "), 150)}}`,
    }};
  }}
  if (event.kind !== "trace_delta") return null;
  const field = event.field || "?";
  const op = item.op || event.op || "apply";
  if (field === "vlower" || field === "vupper") {{
    const oldItems = edgeRefItems(event.old, field, event.var_id);
    const newItems = edgeRefItems(event.new, field, event.var_id);
    const diff = diffEdgeObjects(oldItems, newItems);
    const touches = diff.added
      .concat(diff.removed)
      .filter((e) => e.src === nodeId || e.dst === nodeId);
    if (event.var_id !== nodeId && touches.length === 0) return null;
    const added = diff.added.map((e) => e.text);
    const removed = diff.removed.map((e) => e.text);
    if (event.var_id === nodeId) {{
      return {{
        step,
        body:
          `${{opLabel(op)}} ${{fieldLabel(field)}} on ${{varLabel(nodeId)}}` +
          ` (+${{added.length}}/-${{removed.length}})` +
          `${{added.length ? ` added: ${{truncateText(added.join('; '), 140)}}` : ""}}` +
          `${{removed.length ? ` removed: ${{truncateText(removed.join('; '), 140)}}` : ""}}`,
      }};
    }}
    return {{
      step,
      body:
        `${{opLabel(op)}} ${{fieldLabel(field)}} on ${{varLabel(event.var_id)}}` +
        ` touching ${{varLabel(nodeId)}}: ` +
        truncateText(touches.map((t) => t.text).join("; "), 150),
    }};
  }}
  if (event.var_id !== nodeId) return null;
  return {{
    step,
    body:
      `${{opLabel(op)}} ${{fieldLabel(field)}}: ` +
      `${{truncateText(formatFieldValue(field, event.old), 80)}} -> ` +
      `${{truncateText(formatFieldValue(field, event.new), 80)}}`,
  }};
}}

function renderSelectedNodeTrace() {{
  nodeTraceOps.replaceChildren();
  if (selectedNodeId === null || selectedNodeId === undefined) {{
    nodeTraceMeta.textContent = "No node selected.";
    return;
  }}
  const creation = nodeCreationById.get(selectedNodeId);
  if (creation && creation.loc) {{
    nodeTraceMeta.textContent =
      `${{varLabel(selectedNodeId)}} created at ${{formatLocation(creation.loc)}}`;
  }} else {{
    nodeTraceMeta.textContent =
      `${{varLabel(selectedNodeId)}} (creation range unavailable)`;
  }}
  const ops = [];
  eventOrder.forEach((eventId) => {{
    const summary = describeNodeEvent(selectedNodeId, eventId);
    if (summary) ops.push(summary);
  }});
  if (ops.length === 0) {{
    const empty = document.createElement("div");
    empty.className = "node-op";
    empty.textContent = "No operations recorded for this node.";
    nodeTraceOps.appendChild(empty);
    return;
  }}
  ops.forEach((entry) => {{
    const row = document.createElement("div");
    row.className = "node-op";
    const step = document.createElement("div");
    step.className = "node-op-step";
    step.textContent = entry.step;
    const body = document.createElement("div");
    body.className = "node-op-body";
    body.textContent = entry.body;
    row.append(step, body);
    nodeTraceOps.appendChild(row);
  }});
}}

function appendEventRow(grid, key, value) {{
  const keyEl = document.createElement("div");
  keyEl.className = "event-key";
  keyEl.textContent = key;
  const valEl = document.createElement("div");
  valEl.className = "event-val";
  valEl.textContent = value;
  grid.append(keyEl, valEl);
}}

function appendEventList(parent, title, items, klass) {{
  const block = document.createElement("div");
  block.className = `event-list-block ${{klass}}`;
  const heading = document.createElement("div");
  heading.className = "event-list-title";
  heading.textContent = title;
  const list = document.createElement("ul");
  list.className = "event-list";
  if (items.length === 0) {{
    const li = document.createElement("li");
    li.className = "event-empty";
    li.textContent = "none";
    list.appendChild(li);
  }} else {{
    items.forEach((item) => {{
      const li = document.createElement("li");
      li.textContent = item;
      list.appendChild(li);
    }});
  }}
  block.append(heading, list);
  parent.appendChild(block);
}}

function fieldMeaning(field, op) {{
  const isUndo = op === "undo";
  const verb = isUndo ? "Rollback" : "Apply";
  switch (field) {{
    case "level":
      return `${{verb}} variable level (scope/genericity level).`;
    case "lower":
      return `${{verb}} conservative lower bound of the variable.`;
    case "upper":
      return `${{verb}} conservative upper bound of the variable.`;
    case "vlower":
      return `${{verb}} outgoing lower constraints (edges f(u) <= v).`;
    case "vupper":
      return `${{verb}} outgoing upper constraints (edges v <= f(u)).`;
    case "gencopy":
      return `${{verb}} generalized-copy pointer cache for the variable.`;
    default:
      return `${{verb}} solver field update.`;
  }}
}}

function renderStepEvent(item, eventId) {{
  stepEvent.innerHTML = "";
  const event = item.event || {{}};

  const title = document.createElement("div");
  title.className = "event-title";
  const meaning = document.createElement("div");
  meaning.className = "event-meaning";
  const chips = document.createElement("div");
  chips.className = "event-chips";
  const grid = document.createElement("div");
  grid.className = "event-grid";

  function addChip(text) {{
    const chip = document.createElement("span");
    chip.className = "chip";
    chip.textContent = text;
    chips.appendChild(chip);
  }}

  if (event.kind === "trace_expr_enter") {{
    const exprId =
      typeof event.expr_id === "number" ? `expr#${{event.expr_id}}` : "expr#?";
    title.textContent = `Enter ${{exprId}}`;
    meaning.textContent =
      "Type checker starts typing this expression node.";
    addChip("enter");
    addChip(exprId);
    appendEventRow(grid, "step", stepLabelFor(item, eventId));
    appendEventRow(grid, "location", formatLocation(event.loc));
    appendEventRow(
      grid,
      "parent",
      event.parent_expr_id === null || event.parent_expr_id === undefined
        ? "none"
        : `expr#${{event.parent_expr_id}}`
    );
  }} else if (event.kind === "trace_expr_exit") {{
    const exprId =
      typeof event.expr_id === "number" ? `expr#${{event.expr_id}}` : "expr#?";
    const exitResult = event.result || "unknown";
    title.textContent = `Exit ${{exprId}}`;
    meaning.textContent =
      "Type checker finished this expression node.";
    addChip("exit");
    addChip(exprId);
    addChip(`result:${{exitResult}}`);
    appendEventRow(grid, "step", stepLabelFor(item, eventId));
    appendEventRow(grid, "result", exitResult);
  }} else if (event.kind === "trace_var_create") {{
    title.textContent = `Create solver node ${{varLabelFromEvent(event)}}`;
    meaning.textContent =
      "A fresh mode variable was created with initial bounds, level, and graph links.";
    addChip("create");
    addChip(varLabelFromEvent(event));
    addChip(`provenance: ${{event.provenance || "unknown"}}`);
    appendEventRow(grid, "step", stepLabelFor(item, eventId));
    appendEventRow(grid, "level", formatLevelValue(event.level));
    appendEventRow(grid, "lower", formatValue(event.lower));
    appendEventRow(grid, "upper", formatValue(event.upper));
    appendEventRow(grid, "vlower", summarizeList(edgeRefList(event.vlower)));
    appendEventRow(grid, "vupper", summarizeList(edgeRefList(event.vupper)));
    appendEventRow(
      grid,
      "related vars",
      summarizeList((event.related_var_ids || []).map((id) => varLabel(id)))
    );
  }} else {{
    const field = event.field || "?";
    const op = item.op || event.op || "apply";
    const varName = varLabelFromEvent(event);
    title.textContent =
      `${{opLabel(op)}} ${{fieldLabel(field)}} on ${{varName}}`;
    meaning.textContent = fieldMeaning(field, op);
    addChip(stepLabelFor(item, eventId));
    addChip(op);
    addChip(varName);
    addChip(fieldLabel(field));

    if (field === "vlower" || field === "vupper") {{
      const oldItems = edgeRefItems(event.old, field, event.var_id);
      const newItems = edgeRefItems(event.new, field, event.var_id);
      const diff = diffEdgeItems(oldItems, newItems);
      const addedCount = diff.added.length;
      const removedCount = diff.removed.length;
      const oldCount = oldItems.length;
      const newCount = newItems.length;
      appendEventRow(
        grid,
        "edge count",
        `${{oldCount}} \u2192 ${{newCount}} (+${{addedCount}} / -${{removedCount}})`
      );
      appendEventRow(grid, "before", summarizeList(oldItems.map((i) => i.text)));
      appendEventRow(grid, "after", summarizeList(newItems.map((i) => i.text)));
      const lists = document.createElement("div");
      lists.className = "event-lists";
      appendEventList(lists, "Added", diff.added, "event-list-added");
      appendEventList(lists, "Removed", diff.removed, "event-list-removed");
      stepEvent.append(title, meaning, chips, grid, lists);
      return;
    }}

    appendEventRow(grid, "before", formatFieldValue(field, event.old));
    appendEventRow(grid, "after", formatFieldValue(field, event.new));
    if (event.old === event.new) {{
      appendEventRow(grid, "change", "no-op value write");
    }}
  }}

  stepEvent.append(title, meaning, chips, grid);
}}

function snapshotFor(eventId) {{
  const item = events[String(eventId)];
  return item ? item.snapshot : {{}};
}}

function edgesFromSnapshot(snapshot) {{
  const edges = new Map();
  Object.entries(snapshot).forEach(([varId, node]) => {{
    (node.vlower || []).forEach((rawChild) => {{
      const child = normalizeEdgeRef(rawChild);
      if (!child) return;
      const edge = {{
        src: child.var_id,
        dst: Number(varId),
        label: "vlower",
        modality: child.modality,
      }};
      edges.set(edgeKey(edge), edge);
    }});
    (node.vupper || []).forEach((rawParent) => {{
      const parent = normalizeEdgeRef(rawParent);
      if (!parent) return;
      const edge = {{
        src: Number(varId),
        dst: parent.var_id,
        label: "vupper",
        modality: parent.modality,
      }};
      edges.set(edgeKey(edge), edge);
    }});
  }});
  return Array.from(edges.values());
}}

function clearGraph() {{
  while (graph.firstChild) graph.removeChild(graph.firstChild);
}}

function seededUnit(seed) {{
  const x = Math.sin(seed * 12.9898 + 78.233) * 43758.5453;
  return x - Math.floor(x);
}}

function nodeIdsForLayout(item) {{
  const snapshot = item.snapshot || {{}};
  const ids = new Set(Object.keys(snapshot).map(Number));
  const changed = item.changed_edges || {{ added: [], removed: [] }};
  (changed.added || []).forEach((edge) => {{
    ids.add(Number(edge.src));
    ids.add(Number(edge.dst));
  }});
  (changed.removed || []).forEach((edge) => {{
    ids.add(Number(edge.src));
    ids.add(Number(edge.dst));
  }});
  (item.changed_nodes || []).forEach((id) => ids.add(Number(id)));
  return Array.from(ids).sort((a, b) => a - b);
}}

function allNodeIdsAcrossTimeline() {{
  const ids = new Set();
  eventOrder.forEach((eventId) => {{
    const item = events[String(eventId)];
    nodeIdsForLayout(item).forEach((id) => ids.add(id));
  }});
  return Array.from(ids).sort((a, b) => a - b);
}}

function layoutEdgesFor(item) {{
  const snapshotEdges = edgesFromSnapshot(item.snapshot || {{}});
  const changed = item.changed_edges || {{ added: [], removed: [] }};
  const union = new Map();
  snapshotEdges.forEach((edge) => union.set(edgeKey(edge), edge));
  (changed.added || []).forEach((edge) => union.set(edgeKey(edge), edge));
  (changed.removed || []).forEach((edge) => union.set(edgeKey(edge), edge));
  return Array.from(union.values());
}}

function allEdgesEverForLayout() {{
  const union = new Map();
  eventOrder.forEach((eventId) => {{
    const item = events[String(eventId)];
    layoutEdgesFor(item).forEach((edge) => {{
      union.set(edgeKey(edge), edge);
    }});
  }});
  return Array.from(union.values());
}}

function initialGridPositions(nodeIds) {{
  const positions = new Map();
  const nodeCount = Math.max(nodeIds.length, 1);
  const cols = Math.max(2, Math.ceil(Math.sqrt(nodeCount)));
  const rows = Math.max(1, Math.ceil(nodeCount / cols));
  nodeIds.forEach((id, index) => {{
    const col = index % cols;
    const row = Math.floor(index / cols);
    const gx = (col + 0.5) / cols;
    const gy = (row + 0.5) / rows;
    const jitterX = (seededUnit(id * 11) - 0.5) * 26;
    const jitterY = (seededUnit(id * 29) - 0.5) * 26;
    positions.set(id, {{
      x: LAYOUT_PAD + gx * (LAYOUT_WIDTH - 2 * LAYOUT_PAD) + jitterX,
      y: LAYOUT_PAD + gy * (LAYOUT_HEIGHT - 2 * LAYOUT_PAD) + jitterY,
    }});
  }});
  return positions;
}}

function springsFromEdges(nodeIds, edges) {{
  const allowed = new Set(nodeIds);
  const undirected = new Map();
  edges.forEach((edge) => {{
    const a = Number(edge.src);
    const b = Number(edge.dst);
    if (!allowed.has(a) || !allowed.has(b) || a === b) return;
    const key = a < b ? `${{a}}|${{b}}` : `${{b}}|${{a}}`;
    undirected.set(key, (undirected.get(key) || 0) + 1);
  }});
  return Array.from(undirected.entries()).map(([key, weight]) => {{
    const [a, b] = key.split("|").map((s) => Number(s));
    return {{ a, b, weight }};
  }});
}}

function boundaryForce(p) {{
  let outX = 0;
  let outY = 0;
  if (p.x < LAYOUT_PAD) outX = LAYOUT_PAD - p.x;
  else if (p.x > LAYOUT_WIDTH - LAYOUT_PAD) {{
    outX = (LAYOUT_WIDTH - LAYOUT_PAD) - p.x;
  }}
  if (p.y < LAYOUT_PAD) outY = LAYOUT_PAD - p.y;
  else if (p.y > LAYOUT_HEIGHT - LAYOUT_PAD) {{
    outY = (LAYOUT_HEIGHT - LAYOUT_PAD) - p.y;
  }}
  return {{
    x: outX * 0.55,
    y: outY * 0.55,
  }};
}}

function applySpringForces(disp, positions, springs, targetEdge) {{
  springs.forEach((spring) => {{
    const pa = positions.get(spring.a);
    const pb = positions.get(spring.b);
    if (!pa || !pb) return;
    let dx = pb.x - pa.x;
    let dy = pb.y - pa.y;
    let dist = Math.hypot(dx, dy);
    if (dist < 1e-4) {{
      const nudge = (seededUnit((spring.a + 3) * (spring.b + 5)) - 0.5) * 0.1;
      dx = 0.02 + nudge;
      dy = -0.02 + nudge;
      dist = Math.hypot(dx, dy);
    }}
    const ux = dx / dist;
    const uy = dy / dist;
    const stretch = dist - targetEdge;
    const stiffness = 0.055 + 0.016 * Math.min(3, spring.weight);
    const force = stiffness * stretch;
    const da = disp.get(spring.a);
    const db = disp.get(spring.b);
    da.x += ux * force;
    da.y += uy * force;
    db.x -= ux * force;
    db.y -= uy * force;
  }});
}}

function applyCollisionForces(disp, positions, nodeIds) {{
  for (let i = 0; i < nodeIds.length; i += 1) {{
    const a = nodeIds[i];
    const pa = positions.get(a);
    for (let j = i + 1; j < nodeIds.length; j += 1) {{
      const b = nodeIds[j];
      const pb = positions.get(b);
      let dx = pa.x - pb.x;
      let dy = pa.y - pb.y;
      let dist = Math.hypot(dx, dy);
      if (dist < 1e-4) {{
        const nudge = (seededUnit((a + 1) * (b + 7)) - 0.5) * 0.1;
        dx = 0.02 + nudge;
        dy = 0.02 - nudge;
        dist = Math.hypot(dx, dy);
      }}
      if (dist >= LAYOUT_MIN_SEP) continue;
      const overlap = LAYOUT_MIN_SEP - dist;
      const ux = dx / dist;
      const uy = dy / dist;
      const force = overlap * 0.24;
      const da = disp.get(a);
      const db = disp.get(b);
      da.x += ux * force;
      da.y += uy * force;
      db.x -= ux * force;
      db.y -= uy * force;
    }}
  }}
}}

function relaxAllNodes(
  positions,
  nodeIds,
  springs,
  iterations,
  targetEdge = EDGE_TARGET
) {{
  let step = 1.3;
  for (let it = 0; it < iterations; it += 1) {{
    const disp = new Map();
    nodeIds.forEach((id) => disp.set(id, {{ x: 0, y: 0 }}));

    applySpringForces(disp, positions, springs, targetEdge);
    applyCollisionForces(disp, positions, nodeIds);

    nodeIds.forEach((id) => {{
      const d = disp.get(id);
      const p = positions.get(id);
      const border = boundaryForce(p);
      d.x += border.x;
      d.y += border.y;
      p.x += d.x * step;
      p.y += d.y * step;
    }});
    step *= 0.984;
  }}
}}

function clonePositionMap3D(positions) {{
  const out = new Map();
  positions.forEach((p, id) => {{
    out.set(id, {{
      x: p.x,
      y: p.y,
      z: p.z === undefined || p.z === null ? 0 : p.z,
    }});
  }});
  return out;
}}

function dropZPositions(positions) {{
  const out = new Map();
  positions.forEach((p, id) => {{
    out.set(id, {{ x: p.x, y: p.y }});
  }});
  return out;
}}

function buildNeighborSets(nodeIds, springs) {{
  const neighbors = new Map();
  nodeIds.forEach((id) => neighbors.set(id, new Set()));
  springs.forEach((spring) => {{
    const na = neighbors.get(spring.a);
    const nb = neighbors.get(spring.b);
    if (!na || !nb) return;
    na.add(spring.b);
    nb.add(spring.a);
  }});
  return neighbors;
}}

function componentIndexByNode(nodeIds, springs) {{
  const neighbors = buildNeighborSets(nodeIds, springs);
  const componentById = new Map();
  let nextComponent = 0;
  nodeIds.forEach((startId) => {{
    if (componentById.has(startId)) return;
    const stack = [startId];
    componentById.set(startId, nextComponent);
    while (stack.length > 0) {{
      const id = stack.pop();
      const adj = neighbors.get(id) || new Set();
      adj.forEach((other) => {{
        if (componentById.has(other)) return;
        componentById.set(other, nextComponent);
        stack.push(other);
      }});
    }}
    nextComponent += 1;
  }});
  return componentById;
}}

function layoutEnergy3D(
  nodeIds,
  springs,
  positions,
  neighbors,
  componentById,
  targetEdge,
  zPenalty
) {{
  let energy = 0;
  springs.forEach((spring) => {{
    const pa = positions.get(spring.a);
    const pb = positions.get(spring.b);
    if (!pa || !pb) return;
    const dx = pb.x - pa.x;
    const dy = pb.y - pa.y;
    const dz = (pb.z || 0) - (pa.z || 0);
    const dist = Math.max(Math.hypot(dx, dy, dz), 1e-6);
    const stretch = dist - targetEdge;
    energy += stretch * stretch * (1 + 0.3 * (spring.weight - 1));
  }});

  for (let i = 0; i < nodeIds.length; i += 1) {{
    const a = nodeIds[i];
    const pa = positions.get(a);
    if (!pa) continue;
    for (let j = i + 1; j < nodeIds.length; j += 1) {{
      const b = nodeIds[j];
      const pb = positions.get(b);
      if (!pb) continue;
      const dx = pa.x - pb.x;
      const dy = pa.y - pb.y;
      const dz = (pa.z || 0) - (pb.z || 0);
      const dist = Math.max(Math.hypot(dx, dy, dz), 1e-6);
      if (dist >= LAYOUT_MIN_SEP) continue;
      const overlap = LAYOUT_MIN_SEP - dist;
      energy += 6 * overlap * overlap;
    }}
  }}

  for (let i = 0; i < nodeIds.length; i += 1) {{
    const a = nodeIds[i];
    const pa = positions.get(a);
    if (!pa) continue;
    const aNeighbors = neighbors.get(a) || new Set();
    for (let j = i + 1; j < nodeIds.length; j += 1) {{
      const b = nodeIds[j];
      if (aNeighbors.has(b)) continue;
      const pb = positions.get(b);
      if (!pb) continue;
      const dx = pa.x - pb.x;
      const dy = pa.y - pb.y;
      const dz = (pa.z || 0) - (pb.z || 0);
      const dist = Math.max(Math.hypot(dx, dy, dz), 1e-6);
      const softSep = LAYOUT_MIN_SEP * 1.45;
      if (dist >= softSep) continue;
      const gap = softSep - dist;
      const crossComponent =
        componentById.get(a) !== componentById.get(b);
      const weight = crossComponent ? 1.0 : 0.45;
      energy += weight * 0.52 * gap * gap;

      const planar = Math.max(Math.hypot(dx, dy), 1e-6);
      const planarMin = LAYOUT_MIN_SEP * 0.92;
      if (planar < planarMin) {{
        const planarGap = planarMin - planar;
        energy += weight * 0.11 * planarGap * planarGap;
      }}
    }}
  }}

  nodeIds.forEach((id) => {{
    const p = positions.get(id);
    if (!p) return;
    let outX = 0;
    let outY = 0;
    if (p.x < LAYOUT_PAD) outX = LAYOUT_PAD - p.x;
    else if (p.x > LAYOUT_WIDTH - LAYOUT_PAD) {{
      outX = p.x - (LAYOUT_WIDTH - LAYOUT_PAD);
    }}
    if (p.y < LAYOUT_PAD) outY = LAYOUT_PAD - p.y;
    else if (p.y > LAYOUT_HEIGHT - LAYOUT_PAD) {{
      outY = p.y - (LAYOUT_HEIGHT - LAYOUT_PAD);
    }}
    energy += 10 * (outX * outX + outY * outY);
    const z = p.z || 0;
    energy += zPenalty * z * z;
  }});

  return energy;
}}

function perturbPositions3D(
  positions,
  nodeIds,
  temp,
  phaseKey,
  componentById,
  zScale
) {{
  const componentNudges = new Map();
  nodeIds.forEach((id) => {{
    const comp = componentById.get(id);
    if (comp === undefined || componentNudges.has(comp)) return;
    const aSeed = phaseKey * 9109 + comp * 193;
    const mSeed = phaseKey * 3067 + comp * 389;
    const angle = 2 * Math.PI * seededUnit(aSeed);
    const mag = temp * (0.24 + 0.76 * seededUnit(mSeed));
    const zSeed = phaseKey * 11939 + comp * 577;
    componentNudges.set(comp, {{
      x: Math.cos(angle) * mag * 0.52,
      y: Math.sin(angle) * mag * 0.52,
      z: (seededUnit(zSeed) - 0.5) * 2 * mag * 0.52 * zScale,
    }});
  }});

  nodeIds.forEach((id) => {{
    const p = positions.get(id);
    if (!p) return;
    const aSeed = phaseKey * 10007 + id * 137;
    const mSeed = phaseKey * 6263 + id * 211;
    const angle = 2 * Math.PI * seededUnit(aSeed);
    const mag = temp * (0.2 + 0.8 * seededUnit(mSeed));
    const zSeed = phaseKey * 13007 + id * 419;
    p.x += Math.cos(angle) * mag;
    p.y += Math.sin(angle) * mag;
    p.z = (p.z || 0) + (seededUnit(zSeed) - 0.5) * 2 * mag * zScale;
    const comp = componentById.get(id);
    const drift = comp === undefined ? null : componentNudges.get(comp);
    if (drift) {{
      p.x += drift.x;
      p.y += drift.y;
      p.z += drift.z;
    }}
  }});
}}

function relaxAllNodes3D(
  positions,
  nodeIds,
  springs,
  iterations,
  targetEdge,
  zPenalty
) {{
  let step = 1.16;
  for (let it = 0; it < iterations; it += 1) {{
    const disp = new Map();
    nodeIds.forEach((id) => disp.set(id, {{ x: 0, y: 0, z: 0 }}));

    springs.forEach((spring) => {{
      const pa = positions.get(spring.a);
      const pb = positions.get(spring.b);
      if (!pa || !pb) return;
      let dx = pb.x - pa.x;
      let dy = pb.y - pa.y;
      let dz = (pb.z || 0) - (pa.z || 0);
      let dist = Math.hypot(dx, dy, dz);
      if (dist < 1e-4) {{
        const nudge = (
          seededUnit((spring.a + 3) * (spring.b + 5)) - 0.5
        ) * 0.1;
        dx = 0.02 + nudge;
        dy = -0.02 + nudge;
        dz = 0.02 - nudge;
        dist = Math.hypot(dx, dy, dz);
      }}
      const ux = dx / dist;
      const uy = dy / dist;
      const uz = dz / dist;
      const stretch = dist - targetEdge;
      const stiffness = 0.053 + 0.017 * Math.min(3, spring.weight);
      const force = stiffness * stretch;
      const da = disp.get(spring.a);
      const db = disp.get(spring.b);
      da.x += ux * force;
      da.y += uy * force;
      da.z += uz * force;
      db.x -= ux * force;
      db.y -= uy * force;
      db.z -= uz * force;
    }});

    for (let i = 0; i < nodeIds.length; i += 1) {{
      const a = nodeIds[i];
      const pa = positions.get(a);
      for (let j = i + 1; j < nodeIds.length; j += 1) {{
        const b = nodeIds[j];
        const pb = positions.get(b);
        let dx = pa.x - pb.x;
        let dy = pa.y - pb.y;
        let dz = (pa.z || 0) - (pb.z || 0);
        let dist = Math.hypot(dx, dy, dz);
        if (dist < 1e-4) {{
          const nudge = (
            seededUnit((a + 1) * (b + 7)) - 0.5
          ) * 0.1;
          dx = 0.02 + nudge;
          dy = 0.02 - nudge;
          dz = -0.02 + nudge;
          dist = Math.hypot(dx, dy, dz);
        }}
        if (dist >= LAYOUT_MIN_SEP) continue;
        const overlap = LAYOUT_MIN_SEP - dist;
        const ux = dx / dist;
        const uy = dy / dist;
        const uz = dz / dist;
        const force = overlap * 0.23;
        const da = disp.get(a);
        const db = disp.get(b);
        da.x += ux * force;
        da.y += uy * force;
        da.z += uz * force;
        db.x -= ux * force;
        db.y -= uy * force;
        db.z -= uz * force;
      }}
    }}

    nodeIds.forEach((id) => {{
      const d = disp.get(id);
      const p = positions.get(id);
      const border = boundaryForce(p);
      d.x += border.x;
      d.y += border.y;
      d.z += -(p.z || 0) * (0.08 + 0.45 * zPenalty);
      p.x += d.x * step;
      p.y += d.y * step;
      p.z = (p.z || 0) + d.z * step;
      if (p.z > 500) p.z = 500;
      else if (p.z < -500) p.z = -500;
    }});
    step *= 0.986;
  }}
}}

function annealUnionLayout(initial, nodeIds, springs) {{
  const neighbors = buildNeighborSets(nodeIds, springs);
  const componentById = componentIndexByNode(nodeIds, springs);
  const clusterTarget = Math.max(20, EDGE_TARGET * 0.23);

  const restartCount = nodeIds.length > 170 ? 3 : 5;
  const rounds = nodeIds.length > 170 ? 18 : 24;
  const baseTemp = Math.max(
    30,
    Math.min(150, 42 + 4.2 * Math.sqrt(nodeIds.length))
  );

  let best = null;
  let bestScore = Infinity;

  for (let restart = 0; restart < restartCount; restart += 1) {{
    let current = clonePositionMap3D(initial);
    if (restart > 0) {{
      const shakeTemp = baseTemp * (0.9 + 0.25 * restart);
      perturbPositions3D(
        current,
        nodeIds,
        shakeTemp,
        17003 + restart * 101,
        componentById,
        1.0
      );
      relaxAllNodes3D(
        current,
        nodeIds,
        springs,
        110,
        clusterTarget,
        0.06
      );
    }}

    const initScore = layoutEnergy3D(
      nodeIds,
      springs,
      current,
      neighbors,
      componentById,
      EDGE_TARGET,
      1.9
    );
    if (initScore < bestScore) {{
      best = clonePositionMap3D(current);
      bestScore = initScore;
    }}

    for (let round = 0; round < rounds; round += 1) {{
      const progress = round / Math.max(1, rounds - 1);
      const temp = baseTemp * Math.pow(0.86, round);
      const proposals = round < 7 ? 3 : 2;
      const zPenalty = 0.01 + 1.55 * progress * progress;
      const edgeProgress = Math.pow(progress, 0.78);
      const targetEdge =
        clusterTarget + (EDGE_TARGET - clusterTarget) * edgeProgress;
      let currentEnergy = layoutEnergy3D(
        nodeIds,
        springs,
        current,
        neighbors,
        componentById,
        targetEdge,
        zPenalty
      );
      for (let proposal = 0; proposal < proposals; proposal += 1) {{
        const trial = clonePositionMap3D(current);
        const phaseKey =
          30011 + restart * 1009 + round * 47 + proposal * 13;
        const tempScale = proposal === 0 ? 1.0 : 0.62;
        const zScale = 1.0 - 0.82 * progress;
        perturbPositions3D(
          trial,
          nodeIds,
          temp * tempScale,
          phaseKey,
          componentById,
          zScale
        );
        const localIters = Math.max(18, Math.round(68 - 2.1 * round));
        relaxAllNodes3D(
          trial,
          nodeIds,
          springs,
          localIters,
          targetEdge,
          zPenalty
        );
        const trialEnergy = layoutEnergy3D(
          nodeIds,
          springs,
          trial,
          neighbors,
          componentById,
          targetEdge,
          zPenalty
        );
        const delta = trialEnergy - currentEnergy;
        const denom = Math.max(1e-6, temp * temp * 1.8);
        const boltz = Math.exp(-delta / denom);
        const acceptSeed = phaseKey * 53 + nodeIds.length * 173;
        const accept = delta < 0 || boltz > seededUnit(acceptSeed);
        if (accept) {{
          current = trial;
          currentEnergy = trialEnergy;
        }}
        const scoreForFinal = layoutEnergy3D(
          nodeIds,
          springs,
          trial,
          neighbors,
          componentById,
          EDGE_TARGET,
          1.9
        );
        if (scoreForFinal < bestScore) {{
          best = clonePositionMap3D(trial);
          bestScore = scoreForFinal;
        }}
      }}
    }}
  }}

  const collapsed = clonePositionMap3D(best || initial);
  for (let i = 0; i < 6; i += 1) {{
    const zPenalty = 1.8 + i * 0.6;
    const iters = 44 + i * 8;
    const progress = (i + 1) / 6;
    const targetEdge =
      clusterTarget + (EDGE_TARGET - clusterTarget) * progress;
    relaxAllNodes3D(
      collapsed,
      nodeIds,
      springs,
      iters,
      targetEdge,
      zPenalty
    );
  }}
  collapsed.forEach((p) => {{
    p.z = 0;
  }});
  const polished = dropZPositions(collapsed);
  relaxAllNodes(polished, nodeIds, springs, 120);
  const polishedEnergy = layoutEnergy3D(
    nodeIds,
    springs,
    clonePositionMap3D(polished),
    neighbors,
    componentById,
    EDGE_TARGET,
    2.0
  );
  return polishedEnergy < bestScore ? polished : dropZPositions(best || collapsed);
}}

function computeUnionBaseLayout() {{
  if (unionBasePositions !== null) return;
  const ids = allNodeIdsAcrossTimeline();
  if (ids.length === 0) {{
    unionBasePositions = new Map();
    return;
  }}
  const unionEdges = allEdgesEverForLayout();
  const springs = springsFromEdges(ids, unionEdges);
  const positions = initialGridPositions(ids);
  const clusterTarget = Math.max(20, EDGE_TARGET * 0.23);
  const warmup = Math.min(220, 90 + ids.length * 2);
  relaxAllNodes(positions, ids, springs, warmup, clusterTarget);
  unionBasePositions = annealUnionLayout(positions, ids, springs);
}}

function basePositionForNode(id) {{
  computeUnionBaseLayout();
  const existing = unionBasePositions.get(id);
  if (existing) return existing;
  const spanX = LAYOUT_WIDTH - 2 * LAYOUT_PAD;
  const spanY = LAYOUT_HEIGHT - 2 * LAYOUT_PAD;
  return {{
    x: LAYOUT_PAD + spanX * seededUnit(id * 17 + 3),
    y: LAYOUT_PAD + spanY * seededUnit(id * 31 + 7),
  }};
}}

function clonePositionsForIds(previous, nodeIds) {{
  const next = new Map();
  nodeIds.forEach((id) => {{
    const p = previous && previous.get(id);
    if (p) {{
      next.set(id, {{ x: p.x, y: p.y }});
      return;
    }}
    const base = basePositionForNode(id);
    next.set(id, {{ x: base.x, y: base.y }});
  }});
  return next;
}}

function placeNewNodeLocally(newId, positions, fixedIds, springs) {{
  if (!positions.has(newId)) {{
    const base = basePositionForNode(newId);
    positions.set(newId, {{ x: base.x, y: base.y }});
  }}
  const p0 = positions.get(newId);
  const neighbors = [];
  springs.forEach((spring) => {{
    if (spring.a === newId && positions.has(spring.b)) neighbors.push(spring.b);
    else if (spring.b === newId && positions.has(spring.a)) {{
      neighbors.push(spring.a);
    }}
  }});
  if (neighbors.length > 0) {{
    let x = 0;
    let y = 0;
    neighbors.forEach((id) => {{
      const p = positions.get(id);
      x += p.x;
      y += p.y;
    }});
    const cx = x / neighbors.length;
    const cy = y / neighbors.length;
    p0.x = 0.65 * p0.x + 0.35 * cx;
    p0.y = 0.65 * p0.y + 0.35 * cy;
  }}

  let step = 1.1;
  for (let it = 0; it < 24; it += 1) {{
    const p = positions.get(newId);
    if (!p) return;
    const d = {{ x: 0, y: 0 }};

    springs.forEach((spring) => {{
      let other = null;
      if (spring.a === newId) other = spring.b;
      else if (spring.b === newId) other = spring.a;
      if (other === null) return;
      const po = positions.get(other);
      if (!po) return;
      let dx = po.x - p.x;
      let dy = po.y - p.y;
      let dist = Math.hypot(dx, dy);
      if (dist < 1e-4) {{
        const nudge = (seededUnit((newId + 3) * (other + 11)) - 0.5) * 0.1;
        dx = 0.02 + nudge;
        dy = -0.02 + nudge;
        dist = Math.hypot(dx, dy);
      }}
      const ux = dx / dist;
      const uy = dy / dist;
      const stretch = dist - EDGE_TARGET;
      const stiffness = 0.07 + 0.018 * Math.min(3, spring.weight);
      const force = stiffness * stretch;
      d.x += ux * force;
      d.y += uy * force;
    }});

    fixedIds.forEach((other) => {{
      if (other === newId) return;
      const po = positions.get(other);
      if (!po) return;
      let dx = p.x - po.x;
      let dy = p.y - po.y;
      let dist = Math.hypot(dx, dy);
      if (dist < 1e-4) {{
        const nudge = (seededUnit((newId + 5) * (other + 17)) - 0.5) * 0.1;
        dx = 0.02 + nudge;
        dy = 0.02 - nudge;
        dist = Math.hypot(dx, dy);
      }}
      if (dist >= LAYOUT_MIN_SEP) return;
      const overlap = LAYOUT_MIN_SEP - dist;
      const ux = dx / dist;
      const uy = dy / dist;
      const force = overlap * 0.32;
      d.x += ux * force;
      d.y += uy * force;
    }});

    const border = boundaryForce(p);
    d.x += border.x;
    d.y += border.y;
    p.x += d.x * step;
    p.y += d.y * step;
    step *= 0.965;
  }}
}}

function ensureLayoutComputed(index) {{
  computeUnionBaseLayout();
  const clamped = Math.max(0, Math.min(index, eventOrder.length - 1));
  if (clamped <= layoutComputedUpto) return;

  for (let i = layoutComputedUpto + 1; i <= clamped; i += 1) {{
    const eventId = eventOrder[i];
    const item = events[String(eventId)];
    const orderedIds = nodeIdsForLayout(item);
    const layoutEdges = layoutEdgesFor(item);
    const springs = springsFromEdges(orderedIds, layoutEdges);

    let positions;
    if (i === 0) {{
      positions = new Map();
      orderedIds.forEach((id) => {{
        const base = basePositionForNode(id);
        positions.set(id, {{ x: base.x, y: base.y }});
      }});
      relaxAllNodes(positions, orderedIds, springs, 52);
    }} else {{
      const previous = layoutCache.get(eventOrder[i - 1]) || new Map();
      positions = clonePositionsForIds(previous, orderedIds);
      const existingIds = orderedIds.filter((id) => previous.has(id));
      const newIds = orderedIds.filter((id) => !previous.has(id));
      newIds.forEach((newId) => {{
        placeNewNodeLocally(newId, positions, existingIds, springs);
      }});
      relaxAllNodes(positions, orderedIds, springs, 24);
    }}

    layoutCache.set(eventId, positions);
  }}
  layoutComputedUpto = clamped;
}}

function drawGraph(eventId, index) {{
  clearGraph();
  hoveredNodeId = null;
  graph.onclick = () => {{
    if (selectedNodeId !== null) {{
      selectedNodeId = null;
      render(currentIndex);
    }}
  }};
  const item = events[String(eventId)];
  const snapshot = snapshotFor(eventId);
  const changed = item.changed_edges || {{ added: [], removed: [] }};
  const changedAdded = changed.added || [];
  const changedRemoved = changed.removed || [];
  const changedNodes = new Set(item.changed_nodes || []);
  const createdInSelectedRange = nodeIdsCreatedInRange(selectedSourceRange);
  const focusNodeIds = new Set(createdInSelectedRange);
  if (selectedNodeId !== null && selectedNodeId !== undefined) {{
    focusNodeIds.add(selectedNodeId);
  }}
  const hasFocusSelection = focusNodeIds.size > 0;
  const orderedIds = nodeIdsForLayout(item);
  if (orderedIds.length === 0) return;

  ensureLayoutComputed(index);
  const edges = edgesFromSnapshot(snapshot);
  const positions = layoutCache.get(eventId) || new Map();

  const changedEdgeKeys = new Set(
    changedAdded.concat(changedRemoved).map(edgeKey)
  );

  const defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
  const marker = document.createElementNS("http://www.w3.org/2000/svg", "marker");
  marker.setAttribute("id", "arrow");
  marker.setAttribute("markerWidth", "8");
  marker.setAttribute("markerHeight", "8");
  marker.setAttribute("refX", "6.3");
  marker.setAttribute("refY", "2.5");
  marker.setAttribute("orient", "auto");
  const markerPath = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "path"
  );
  markerPath.setAttribute("d", "M0,0 L0,5 L7,2.5 z");
  markerPath.setAttribute("fill", "#7a756d");
  marker.appendChild(markerPath);
  defs.appendChild(marker);
  graph.appendChild(defs);

  const edgeLabelEntries = [];
  const nodePopupEntries = [];
  function updateEdgeLabelVisibility() {{
    const focusIds = new Set();
    if (hoveredNodeId !== null) focusIds.add(hoveredNodeId);
    if (selectedNodeId !== null) focusIds.add(selectedNodeId);
    edgeLabelEntries.forEach((entry) => {{
      const show =
        focusIds.size > 0 &&
        (focusIds.has(entry.src) || focusIds.has(entry.dst));
      entry.group.style.display = show ? "" : "none";
    }});
    nodePopupEntries.forEach((entry) => {{
      const show = focusIds.has(entry.nodeId);
      entry.group.style.display = show ? "" : "none";
    }});
  }}

  const edgeGroups = new Map();
  const allEdgesForBends = edges.concat(changedRemoved);
  allEdgesForBends.forEach((edge) => {{
    const groupKey = edgeGroupKey(edge);
    const group = edgeGroups.get(groupKey);
    if (group) group.push(edge);
    else edgeGroups.set(groupKey, [edge]);
  }});

  function edgeLaneNudge(edge) {{
    const token = `${{edge.label}}|${{edge.modality || ""}}`;
    let hash = 0;
    for (let i = 0; i < token.length; i += 1) {{
      hash = ((hash * 33) ^ token.charCodeAt(i)) >>> 0;
    }}
    return ((hash % 9) - 4) * 1.1;
  }}

  const edgeBends = new Map();
  edgeGroups.forEach((group, groupKey) => {{
    group.sort((a, b) => {{
      const labelCmp = String(a.label).localeCompare(String(b.label));
      if (labelCmp !== 0) return labelCmp;
      return String(a.modality || "").localeCompare(String(b.modality || ""));
    }});
    const reverse = edgeGroups.get(reverseEdgeGroupKey(group[0])) || [];
    const baseBend = edgePairSign(group[0]) * 8;
    const pairBend = reverse.length > 0 ? edgePairSign(group[0]) * 20 : baseBend;
    const count = group.length;
    group.forEach((edge, index) => {{
      const spread = (index - (count - 1) / 2) * 26;
      const nudge = count > 1 ? edgeLaneNudge(edge) : 0;
      edgeBends.set(edgeKey(edge), pairBend + spread + nudge);
    }});
  }});

  function edgeColor(edge, removed) {{
    if (removed) return "#b42318";
    if (changedEdgeKeys.has(edgeKey(edge))) return "#d97706";
    return edge.label === "vlower" ? "#15803d" : "#a21caf";
  }}

  function nodeOpacity(id) {{
    if (!hasFocusSelection) return 1;
    return focusNodeIds.has(id) ? 1 : 0.5;
  }}

  function edgeOpacity(edge) {{
    if (!hasFocusSelection) return 1;
    return focusNodeIds.has(edge.src) || focusNodeIds.has(edge.dst) ? 1 : 0.5;
  }}

  function edgeText(edge) {{
    const rawModality = edge.modality;
    const modality =
      rawModality === null || rawModality === undefined
        ? "null"
        : String(rawModality);
    const shortModality =
      modality.length > 30 ? `${{modality.slice(0, 27)}}...` : modality;
    return shortModality;
  }}

  function drawEdge(edge, removed) {{
    const src = positions.get(edge.src) || basePositionForNode(edge.src);
    const dst = positions.get(edge.dst) || basePositionForNode(edge.dst);
    if (!src || !dst) return;
    const stroke = edgeColor(edge, removed);
    const bend = edgeBends.get(edgeKey(edge)) || 0;
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    let labelX = (src.x + dst.x) / 2;
    let labelY = (src.y + dst.y) / 2;
    if (edge.src === edge.dst) {{
      const startAngle = -0.17 * Math.PI;
      const endAngle = 0.17 * Math.PI;
      const startX = src.x + NODE_RADIUS * Math.cos(startAngle);
      const startY = src.y + NODE_RADIUS * Math.sin(startAngle);
      const endX = src.x + NODE_RADIUS * Math.cos(endAngle);
      const endY = src.y + NODE_RADIUS * Math.sin(endAngle);
      const nx0 = Math.cos(startAngle);
      const ny0 = Math.sin(startAngle);
      const nx1 = Math.cos(endAngle);
      const ny1 = Math.sin(endAngle);
      const loopOut = NODE_RADIUS * 2.1 + 12 + Math.abs(bend) * 0.35;
      const loopShift = bend * 0.45;
      const c1x = startX + nx0 * loopOut;
      const c1y = startY + ny0 * loopOut + loopShift;
      const c2x = endX + nx1 * loopOut;
      const c2y = endY + ny1 * loopOut + loopShift;
      path.setAttribute(
        "d",
        `M ${{startX}} ${{startY}} C ${{c1x}} ${{c1y}} ${{c2x}} ${{c2y}} ${{endX}} ${{endY}}`
      );
      const t = 0.28;
      const mt = 1 - t;
      labelX =
        mt * mt * mt * startX +
        3 * mt * mt * t * c1x +
        3 * mt * t * t * c2x +
        t * t * t * endX;
      labelY =
        mt * mt * mt * startY +
        3 * mt * mt * t * c1y +
        3 * mt * t * t * c2y +
        t * t * t * endY;
    }} else {{
      const canonSrc = edge.src < edge.dst ? src : dst;
      const canonDst = edge.src < edge.dst ? dst : src;
      const dx = canonDst.x - canonSrc.x;
      const dy = canonDst.y - canonSrc.y;
      const len = Math.max(Math.hypot(dx, dy), 1e-6);
      const nx = -dy / len;
      const ny = dx / len;
      const laneOffset = bend * 0.36;
      const offsetX = nx * laneOffset;
      const offsetY = ny * laneOffset;
      const cx = (src.x + dst.x) / 2 + nx * bend + offsetX;
      const cy = (src.y + dst.y) / 2 + ny * bend + offsetY;
      let sdx = cx - src.x;
      let sdy = cy - src.y;
      let slen = Math.hypot(sdx, sdy);
      if (slen < 1e-6) {{
        sdx = dst.x - src.x;
        sdy = dst.y - src.y;
        slen = Math.max(Math.hypot(sdx, sdy), 1e-6);
      }}
      const sux = sdx / slen;
      const suy = sdy / slen;
      let edx = dst.x - cx;
      let edy = dst.y - cy;
      let elen = Math.hypot(edx, edy);
      if (elen < 1e-6) {{
        edx = dst.x - src.x;
        edy = dst.y - src.y;
        elen = Math.max(Math.hypot(edx, edy), 1e-6);
      }}
      const eux = edx / elen;
      const euy = edy / elen;
      const startX = src.x + sux * NODE_RADIUS + offsetX;
      const startY = src.y + suy * NODE_RADIUS + offsetY;
      const endX = dst.x - eux * NODE_RADIUS + offsetX;
      const endY = dst.y - euy * NODE_RADIUS + offsetY;
      path.setAttribute(
        "d",
        `M ${{startX}} ${{startY}} Q ${{cx}} ${{cy}} ${{endX}} ${{endY}}`
      );
      const t = 0.28;
      const mt = 1 - t;
      labelX = mt * mt * startX + 2 * mt * t * cx + t * t * endX;
      labelY = mt * mt * startY + 2 * mt * t * cy + t * t * endY;
    }}
    path.setAttribute("fill", "none");
    path.setAttribute("stroke", stroke);
    path.setAttribute(
      "stroke-width",
      removed ? "1.8" : changedEdgeKeys.has(edgeKey(edge)) ? "2.2" : "1.3"
    );
    path.setAttribute("opacity", String(edgeOpacity(edge)));
    if (removed) {{
      path.setAttribute("stroke-dasharray", "6,4");
    }} else if (edge.modality === "id") {{
      path.setAttribute("stroke-dasharray", "4,3");
    }}
    path.setAttribute("marker-end", "url(#arrow)");
    path.setAttribute("pointer-events", "none");
    graph.appendChild(path);

    if (removed) return;
    const labelGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    labelGroup.setAttribute("pointer-events", "none");
    labelGroup.style.display = "none";
    const label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttribute("x", String(labelX));
    label.setAttribute("y", String(labelY));
    label.setAttribute("text-anchor", "middle");
    label.setAttribute("font-size", "9");
    label.setAttribute("fill", stroke);
    label.textContent = edgeText(edge);
    labelGroup.setAttribute("opacity", String(edgeOpacity(edge)));
    labelGroup.appendChild(label);
    graph.appendChild(labelGroup);
    const bbox = label.getBBox();
    const bg = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    bg.setAttribute("x", String(bbox.x - 2));
    bg.setAttribute("y", String(bbox.y - 1));
    bg.setAttribute("width", String(bbox.width + 4));
    bg.setAttribute("height", String(bbox.height + 2));
    bg.setAttribute("fill", "#fffdf6");
    bg.setAttribute("fill-opacity", "1");
    bg.setAttribute("stroke", "none");
    labelGroup.insertBefore(bg, label);
    edgeLabelEntries.push({{ group: labelGroup, src: edge.src, dst: edge.dst }});
  }}

  function drawNodes() {{
    orderedIds.forEach((id) => {{
    const pos = positions.get(id) || basePositionForNode(id);
    const node = snapshot[String(id)] || {{
      level: null,
      lower: null,
      upper: null,
      vlower: [],
      vupper: [],
      gencopy: null,
      provenance: null,
      related_var_ids: [],
    }};
    const prefix = varPrefix(id, node);
    const palette = nodePalette(prefix);
    const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
    group.setAttribute("tabindex", "0");
    group.setAttribute("opacity", String(nodeOpacity(id)));
    group.addEventListener("click", (event) => {{
      event.stopPropagation();
      selectedNodeId = selectedNodeId === id ? null : id;
      render(currentIndex);
    }});
    group.addEventListener("keydown", (event) => {{
      if (event.key !== "Enter" && event.key !== " ") return;
      event.preventDefault();
      event.stopPropagation();
      selectedNodeId = selectedNodeId === id ? null : id;
      render(currentIndex);
    }});
    const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    circle.setAttribute("cx", String(pos.x));
    circle.setAttribute("cy", String(pos.y));
    circle.setAttribute("r", String(NODE_RADIUS));
    circle.setAttribute("fill", palette.fill);
    const isSelected = selectedNodeId === id;
    const isChanged = changedNodes.has(id);
    const isInRange = createdInSelectedRange.has(id);
    let stroke = palette.stroke;
    let strokeWidth = 1.2;
    if (isInRange) {{
      stroke = "#0f766e";
      strokeWidth = 2.0;
    }}
    if (isChanged) {{
      stroke = "#d97706";
      strokeWidth = 2.2;
    }}
    if (isSelected) {{
      stroke = "#111827";
      strokeWidth = 2.6;
    }}
    circle.setAttribute("stroke", stroke);
    circle.setAttribute("stroke-width", String(strokeWidth));
    group.addEventListener("mouseenter", () => {{
      hoveredNodeId = id;
      updateEdgeLabelVisibility();
      renderSourceForItem(item);
    }});
    group.addEventListener("mouseleave", () => {{
      if (hoveredNodeId === id) hoveredNodeId = null;
      updateEdgeLabelVisibility();
      renderSourceForItem(item);
    }});
    group.addEventListener("focus", () => {{
      hoveredNodeId = id;
      updateEdgeLabelVisibility();
      renderSourceForItem(item);
    }});
    group.addEventListener("blur", () => {{
      if (hoveredNodeId === id) hoveredNodeId = null;
      updateEdgeLabelVisibility();
      renderSourceForItem(item);
    }});
    const label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttribute("x", String(pos.x));
    label.setAttribute("y", String(pos.y - 2));
    label.setAttribute("text-anchor", "middle");
    label.setAttribute("font-size", "11");
    label.setAttribute("fill", palette.text);
    label.textContent = varLabel(id, node);
    const levelLabel = document.createElementNS(
      "http://www.w3.org/2000/svg",
      "text"
    );
    levelLabel.setAttribute("x", String(pos.x));
    levelLabel.setAttribute("y", String(pos.y + 10));
    levelLabel.setAttribute("text-anchor", "middle");
    levelLabel.setAttribute("font-size", "9");
    levelLabel.setAttribute("fill", palette.level);
    const levelText =
      node.level === null || node.level === undefined
        ? "?"
        : formatLevelValue(node.level);
    levelLabel.textContent = `L=${{levelText}}`;
    group.append(circle, label, levelLabel);
    graph.appendChild(group);

    const popupGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    popupGroup.setAttribute("pointer-events", "none");
    popupGroup.style.display = "none";
    const popupX = pos.x + NODE_RADIUS + 7;
    const popupY = pos.y - 4;
    const lowerText = document.createElementNS("http://www.w3.org/2000/svg", "text");
    lowerText.setAttribute("x", String(popupX));
    lowerText.setAttribute("y", String(popupY));
    lowerText.setAttribute("text-anchor", "start");
    lowerText.setAttribute("font-size", "8.5");
    lowerText.setAttribute("fill", "#3f3a33");
    lowerText.textContent = `lower: ${{formatValue(node.lower)}}`;
    const upperText = document.createElementNS("http://www.w3.org/2000/svg", "text");
    upperText.setAttribute("x", String(popupX));
    upperText.setAttribute("y", String(popupY + 10));
    upperText.setAttribute("text-anchor", "start");
    upperText.setAttribute("font-size", "8.5");
    upperText.setAttribute("fill", "#3f3a33");
    upperText.textContent = `upper: ${{formatValue(node.upper)}}`;
    const creation = nodeCreationById.get(id);
    const creationText = document.createElementNS("http://www.w3.org/2000/svg", "text");
    creationText.setAttribute("x", String(popupX));
    creationText.setAttribute("y", String(popupY + 20));
    creationText.setAttribute("text-anchor", "start");
    creationText.setAttribute("font-size", "8.5");
    creationText.setAttribute("fill", "#3f3a33");
    creationText.textContent =
      `created: ${{creation && creation.loc ? formatLocationCompact(creation.loc) : "unknown"}}`;
    popupGroup.append(lowerText, upperText, creationText);
    graph.appendChild(popupGroup);
    const popupBox = popupGroup.getBBox();
    const popupBg = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    popupBg.setAttribute("x", String(popupBox.x - 3));
    popupBg.setAttribute("y", String(popupBox.y - 2));
    popupBg.setAttribute("width", String(popupBox.width + 6));
    popupBg.setAttribute("height", String(popupBox.height + 4));
    popupBg.setAttribute("fill", "#fffdf6");
    popupBg.setAttribute("fill-opacity", "1");
    popupBg.setAttribute("stroke", "#cfc5b5");
    popupBg.setAttribute("stroke-width", "0.9");
    popupGroup.insertBefore(popupBg, lowerText);
    popupGroup.setAttribute("opacity", String(nodeOpacity(id)));
    nodePopupEntries.push({{ group: popupGroup, nodeId: id }});
    }});
  }}

  drawNodes();
  edges.forEach((edge) => drawEdge(edge, false));
  changedRemoved.forEach((edge) => drawEdge(edge, true));
  nodePopupEntries.forEach((entry) => {{
    graph.appendChild(entry.group);
  }});
  updateEdgeLabelVisibility();
}}

function render(index) {{
  if (eventOrder.length === 0) {{
    summary.textContent = "No solver trace events found.";
    resultEl.textContent = "";
    stepEvent.textContent = "";
    sourceMeta.textContent = "";
    sourceView.textContent = "";
    sourceSelectionMeta.textContent = "";
    nodeTraceMeta.textContent = "";
    nodeTraceOps.textContent = "";
    backtraceMeta.textContent = "";
    backtraceView.textContent = "";
    closeBacktraceDialog();
    clearGraph();
    return;
  }}
  const eventId = eventOrder[index];
  const item = events[String(eventId)];
  summary.textContent =
    `Step ${{index + 1}} / ${{eventOrder.length}}` +
    " | keys: \u2190/\u2192, j/k, Home/End";
  const result = item.step_result || "unknown";
  resultEl.textContent = `Result: ${{result}}`;
  resultEl.className = result === "error" ? "meta result-error" : "meta result-ok";
  renderStepEvent(item, eventId);
  updateBacktracePanel(item, eventId);
  renderSourceForItem(item);
  updateSourceSelectionMeta();
  renderSelectedNodeTrace();
  drawGraph(eventId, index);
}}

initNodeKindIndex();
initNodeCreationIndex();
initControls();
initKeyboardNavigation();
sourceView.addEventListener("mouseup", scheduleCaptureSourceSelection);
sourceView.addEventListener("keyup", scheduleCaptureSourceSelection);
sourceView.addEventListener("click", () => {{
  if (Date.now() - lastSelectionCaptureMs < 250) return;
  const selection = window.getSelection();
  const hasExpandedSelection =
    selection &&
    selection.rangeCount > 0 &&
    !selection.isCollapsed &&
    sourceView.contains(selection.anchorNode) &&
    sourceView.contains(selection.focusNode);
  if (hasExpandedSelection) return;
  if (!selectedSourceRange) return;
  selectedSourceRange = null;
  render(currentIndex);
}});
sourceSelectionClear.addEventListener("click", () => {{
  selectedSourceRange = null;
  render(currentIndex);
}});
backtraceOpen.addEventListener("click", openBacktraceDialog);
backtraceClose.addEventListener("click", closeBacktraceDialog);
backtraceOverlay.addEventListener("click", (event) => {{
  if (event.target === backtraceOverlay) closeBacktraceDialog();
}});
setIndex(Number(select.value || 0));
</script>
</body>
</html>
"""


def main():
    args = parse_args()
    events, step_results = load_trace(args.input)
    timeline, order = build_timeline(events, step_results)
    base_dir = os.path.dirname(os.path.abspath(args.input))
    sources = collect_sources(events, base_dir)
    data = {
        "event_order": order,
        "events": {str(event_id): timeline[event_id] for event_id in order},
        "sources": sources,
    }
    html = html_document(data, args.trace)
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(html)


if __name__ == "__main__":
    main()
