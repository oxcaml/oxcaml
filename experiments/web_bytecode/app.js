import {
  defaultSample,
  getSampleById,
  getVisibleSamplesByTopic,
} from "./sample_catalog.js";
import {
  EditorState,
  RangeSetBuilder,
  StateEffect,
  StateField,
} from "@codemirror/state";
import {
  Decoration,
  EditorView,
  drawSelection,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import {
  bracketMatching,
  indentOnInput,
} from "@codemirror/language";
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
} from "@codemirror/commands";
import { Parser, Language, Query } from "./vendor/tree_sitter/web-tree-sitter.js";

const autoRunDelayMs = 360;
const buildBase = "../../_build/default/experiments/web_bytecode";
const treeSitterCoreWasmUrl = new URL("./vendor/tree_sitter/web-tree-sitter.wasm", import.meta.url).href;
const treeSitterOcamlWasmUrl = new URL("./vendor/tree_sitter/tree-sitter-ocaml.wasm", import.meta.url).href;
const treeSitterOcamlHighlightsUrl =
  new URL("./vendor/tree_sitter/ocaml-highlights.scm", import.meta.url).href;

const editorHostEl = document.getElementById("editor");
const outputEl = document.getElementById("output");
const outputLabelEl = document.getElementById("output-label");
const outputPanelEl = document.getElementById("output-panel");
const statusEl = document.getElementById("status");
const statusTextEl = statusEl?.querySelector(".status-text") ?? null;
const samplePickerEl = document.getElementById("sample-picker");
const fullUi = Boolean(
  editorHostEl &&
  outputLabelEl &&
  outputPanelEl &&
  samplePickerEl,
);

let currentFilename = "snippet.ml";
let currentSampleId = null;
let pendingRunTimer = null;
let currentRevision = 0;
let editorMarkers = [];
let editorView = null;
let suppressEditorChanges = false;
const loadedScriptUrls = new Map();
let browserFsPromise = null;
const browserFsConcurrency = 4;
const browserFsFetchRetries = 4;
let bootStatusActive = false;
let treeSitterBackendPromise = null;
let syntaxRequestRevision = 0;

const setDiagnosticsEffect = StateEffect.define();
const setSyntaxDecorationsEffect = StateEffect.define();

const treeSitterCaptureClass = new Map([
  ["keyword", "tok-keyword"],
  ["operator", "tok-operator"],
  ["punctuation.delimiter", "tok-operator"],
  ["punctuation.bracket", "tok-operator"],
  ["punctuation.special", "tok-operator"],
  ["string", "tok-string"],
  ["string.special", "tok-string"],
  ["escape", "tok-string"],
  ["comment", "tok-comment"],
  ["number", "tok-number"],
  ["module", "tok-module"],
  ["function", "tok-function"],
  ["function.method", "tok-function"],
  ["function.builtin", "tok-function"],
  ["variable.parameter", "tok-parameter"],
  ["type", "tok-type"],
  ["type.builtin", "tok-type"],
  ["constructor", "tok-constructor"],
  ["constant", "tok-constructor"],
  ["property", "tok-property"],
  ["tag", "tok-tag"],
]);

const oxcamlIdentifierNames = new Set(["local_", "stack_", "exclave_"]);
const packageModuleNames = new Set(["Base", "Core", "Stdlib_stable"]);

function buildDiagnosticDecorations(doc, markers) {
  if (!markers.length) {
    return Decoration.none;
  }
  const builder = new RangeSetBuilder();
  for (const marker of markers) {
    const range = markerDocRange(doc, marker);
    if (!range) {
      continue;
    }
    builder.add(
      range.from,
      range.to,
      Decoration.mark({
        class: marker.severity === "warning" ? "cm-diagnostic-warning" : "cm-diagnostic-error",
      }),
    );
  }
  return builder.finish();
}

const diagnosticField = StateField.define({
  create() {
    return Decoration.none;
  },
  update(decorations, tr) {
    let next = tr.docChanged ? Decoration.none : decorations.map(tr.changes);
    for (const effect of tr.effects) {
      if (effect.is(setDiagnosticsEffect)) {
        next = buildDiagnosticDecorations(tr.state.doc, effect.value);
      }
    }
    return next;
  },
  provide: (field) => EditorView.decorations.from(field),
});

const syntaxField = StateField.define({
  create() {
    return Decoration.none;
  },
  update(decorations, tr) {
    let next = tr.docChanged ? decorations.map(tr.changes) : decorations;
    for (const effect of tr.effects) {
      if (effect.is(setSyntaxDecorationsEffect)) {
        next = effect.value;
      }
    }
    return next;
  },
  provide: (field) => EditorView.decorations.from(field),
});

const diagnosticHover = hoverTooltip((view, pos) => {
  const match = markerAtPosition(view, pos);
  if (!match) {
    return null;
  }
  return {
    pos: match.range.from,
    end: match.range.to,
    above: true,
    create() {
      const dom = document.createElement("div");
      dom.className = `cm-diagnostic-tooltip ${match.marker.severity}`;
      dom.textContent = match.marker.message || "Diagnostic";
      return { dom };
    },
  };
});

function buildByteOffsetTable(source) {
  const table = [0];
  let byteIndex = 0;
  for (let index = 0; index < source.length; ) {
    const codePoint = source.codePointAt(index);
    const codeUnitLength = codePoint > 0xffff ? 2 : 1;
    let utf8Length = 1;
    if (codePoint > 0x7f) utf8Length = codePoint <= 0x7ff ? 2 : codePoint <= 0xffff ? 3 : 4;
    byteIndex += utf8Length;
    while (table.length <= byteIndex) {
      table.push(index + codeUnitLength);
    }
    index += codeUnitLength;
  }
  return table;
}

function byteIndexToOffset(table, byteIndex, fallback) {
  if (byteIndex < 0) {
    return 0;
  }
  if (byteIndex < table.length) {
    return table[byteIndex];
  }
  return fallback;
}

function previousNonWhitespace(source, offset) {
  for (let index = offset - 1; index >= 0; index -= 1) {
    const char = source[index];
    if (!/\s/.test(char)) {
      return char;
    }
  }
  return "";
}

function nextNonWhitespaceWord(source, offset) {
  let index = offset;
  while (index < source.length && /\s/.test(source[index])) {
    index += 1;
  }
  const match = /^[A-Za-z_][A-Za-z0-9_']*/.exec(source.slice(index));
  return match ? match[0] : "";
}

function classifyCaptureClasses(capture, source, from, to, baseClassName) {
  const text = source.slice(from, to);
  const classes = [baseClassName];

  if (oxcamlIdentifierNames.has(text)) {
    classes.unshift("tok-oxcaml");
  }

  if (text === "local" && previousNonWhitespace(source, from) === "@") {
    classes.unshift("tok-annotation");
  }

  if (packageModuleNames.has(text)) {
    classes.unshift("tok-package");
  }

  if (baseClassName === "tok-property" && previousNonWhitespace(source, from) === "~") {
    classes.unshift("tok-label");
  }

  if (text === "open") {
    const openedModule = nextNonWhitespaceWord(source, to);
    if (packageModuleNames.has(openedModule)) {
      classes.unshift("tok-package-open");
    }
  }

  return Array.from(new Set(classes)).join(" ");
}

function collectSupplementalSyntaxRanges(source) {
  const ranges = [];
  const pushMatches = (regex, className) => {
    let match;
    while ((match = regex.exec(source)) !== null) {
      const from = match.index;
      const to = from + match[0].length;
      if (from < to) {
        ranges.push({ from, to, className });
      }
    }
  };

  pushMatches(/@[ \t\r\n]*local\b/g, "tok-annotation");
  pushMatches(/~[A-Za-z_][A-Za-z0-9_']*/g, "tok-label");
  return ranges;
}

function buildSyntaxDecorations(source, captures) {
  const byteOffsets = buildByteOffsetTable(source);
  const ranges = [];
  for (const capture of captures) {
    const className = treeSitterCaptureClass.get(capture.name);
    if (!className) {
      continue;
    }
    const from = byteIndexToOffset(byteOffsets, capture.node.startIndex, source.length);
    const to = byteIndexToOffset(byteOffsets, capture.node.endIndex, source.length);
    if (from >= to) {
      continue;
    }
    ranges.push({
      from,
      to,
      className: classifyCaptureClasses(capture, source, from, to, className),
    });
  }
  ranges.push(...collectSupplementalSyntaxRanges(source));
  ranges.sort(
    (left, right) =>
      left.from - right.from ||
      left.to - right.to ||
      left.className.localeCompare(right.className),
  );
  const builder = new RangeSetBuilder();
  for (const { from, to, className } of ranges) {
    builder.add(from, to, Decoration.mark({ class: className }));
  }
  return builder.finish();
}

async function ensureTreeSitterBackend() {
  if (treeSitterBackendPromise) {
    return treeSitterBackendPromise;
  }
  treeSitterBackendPromise = (async () => {
    await Parser.init({
      locateFile(scriptName) {
        if (scriptName === "tree-sitter.wasm" || scriptName === "web-tree-sitter.wasm") {
          return treeSitterCoreWasmUrl;
        }
        return scriptName;
      },
    });
    const [language, querySource] = await Promise.all([
      Language.load(treeSitterOcamlWasmUrl),
      fetchText(treeSitterOcamlHighlightsUrl),
    ]);
    const parser = new Parser();
    parser.setLanguage(language);
    const query = new Query(language, querySource);
    return { parser, query };
  })();
  return treeSitterBackendPromise;
}

function scheduleSyntaxRefresh() {
  if (!editorView) {
    return;
  }
  const request = ++syntaxRequestRevision;
  const source = sourceText();
  ensureTreeSitterBackend()
    .then(({ parser, query }) => {
      if (!editorView || request !== syntaxRequestRevision) {
        return;
      }
      const tree = parser.parse(source);
      if (!tree) {
        return;
      }
      const captures = query.captures(tree.rootNode);
      const decorations = buildSyntaxDecorations(source, captures);
      tree.delete();
      if (!editorView || request !== syntaxRequestRevision) {
        return;
      }
      editorView.dispatch({
        effects: setSyntaxDecorationsEffect.of(decorations),
      });
    })
    .catch((error) => {
      console.warn("Tree-sitter highlighting failed", error);
    });
}

function clearEditorMarkers() {
  editorMarkers = [];
  if (!editorView) {
    return;
  }
  editorView.dispatch({
    effects: setDiagnosticsEffect.of([]),
  });
}

function createEditor() {
  if (!editorHostEl) {
    return null;
  }
  return new EditorView({
    parent: editorHostEl,
    state: EditorState.create({
      doc: "",
      extensions: [
        EditorState.tabSize.of(2),
        lineNumbers(),
        history(),
        drawSelection(),
        highlightActiveLine(),
        highlightActiveLineGutter(),
        indentOnInput(),
        bracketMatching(),
        keymap.of([indentWithTab, ...defaultKeymap, ...historyKeymap]),
        EditorView.contentAttributes.of({
          spellcheck: "false",
          autocorrect: "off",
          autocapitalize: "off",
          "aria-label": "Source code",
        }),
        diagnosticField,
        syntaxField,
        diagnosticHover,
        EditorView.updateListener.of((update) => {
          if (!update.docChanged || suppressEditorChanges) {
            return;
          }
          scheduleSyntaxRefresh();
          currentRevision += 1;
          editorMarkers = [];
          schedulePipeline();
        }),
      ],
    }),
  });
}

function sourceText() {
  return editorView ? editorView.state.doc.toString() : "";
}

function replaceEditorSource(source) {
  if (!editorView) {
    return;
  }
  suppressEditorChanges = true;
  editorView.dispatch({
    changes: {
      from: 0,
      to: editorView.state.doc.length,
      insert: source,
    },
    effects: setDiagnosticsEffect.of([]),
  });
  suppressEditorChanges = false;
  scheduleSyntaxRefresh();
}

function loadScript(url) {
  const href = url instanceof URL ? url.href : String(url);
  const existing = loadedScriptUrls.get(href);
  if (existing) {
    return existing;
  }
  const promise = new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = href;
    script.async = false;
    script.onload = () => resolve();
    script.onerror = () => reject(new Error(`failed to load ${href}`));
    document.head.appendChild(script);
  });
  loadedScriptUrls.set(href, promise);
  return promise;
}

function installGlobalScriptEvaluator() {
  globalThis.__oxcamlEvalGlobalScript = (source, label = "oxcaml-runtime.js") => {
    const script = document.createElement("script");
    script.textContent = `${String(source)}\n//# sourceURL=${String(label)}`;
    document.head.appendChild(script);
    script.remove();
  };
}

function buildAssetUrl(path) {
  return new URL(`${buildBase}/${path}`, import.meta.url);
}

async function fetchJson(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}`);
  }
  return response.json();
}

async function readBlobAsBinaryString(blob) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () => reject(reader.error ?? new Error("failed to read blob"));
    reader.onload = () => {
      if (typeof reader.result !== "string") {
        reject(new Error("expected binary string from FileReader"));
        return;
      }
      resolve(reader.result);
    };
    reader.readAsBinaryString(blob);
  });
}

async function fetchBinaryString(url, compression) {
  let lastError = null;
  for (let attempt = 0; attempt < browserFsFetchRetries; attempt += 1) {
    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`failed to fetch ${url}`);
      }
      if (compression === "gzip") {
        if (typeof DecompressionStream !== "function") {
          throw new Error("gzip-compressed browser assets require DecompressionStream support");
        }
        if (!response.body) {
          throw new Error(`missing response body for ${url}`);
        }
        const stream = response.body.pipeThrough(new DecompressionStream("gzip"));
        return readBlobAsBinaryString(await new Response(stream).blob());
      }
      return readBlobAsBinaryString(await response.blob());
    } catch (error) {
      lastError = error;
      if (attempt + 1 >= browserFsFetchRetries) {
        break;
      }
      await new Promise((resolve) => {
        window.setTimeout(resolve, 40 * (attempt + 1));
      });
    }
  }
  throw lastError ?? new Error(`failed to fetch ${url}`);
}

async function fetchText(url, compression) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}`);
  }
  if (compression === "gzip") {
    if (typeof DecompressionStream !== "function") {
      throw new Error("gzip-compressed browser assets require DecompressionStream support");
    }
    if (!response.body) {
      throw new Error(`missing response body for ${url}`);
    }
    const stream = response.body.pipeThrough(new DecompressionStream("gzip"));
    return new Response(stream).text();
  }
  return response.text();
}

function formatByteCount(byteCount) {
  if (!Number.isFinite(byteCount) || byteCount < 0) {
    return "";
  }
  if (byteCount < 1024 * 1024) {
    return `${Math.round(byteCount / 1024)} KB`;
  }
  return `${(byteCount / (1024 * 1024)).toFixed(1)} MB`;
}

async function fetchTextWithProgress(url, compression, onProgress) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}`);
  }
  if (!response.body) {
    if (onProgress) {
      onProgress(null);
    }
    return fetchText(url, compression);
  }

  const totalHeader = response.headers.get("content-length");
  const totalBytes = totalHeader ? Number.parseInt(totalHeader, 10) : null;
  const reader = response.body.getReader();
  const chunks = [];
  let receivedBytes = 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) {
      break;
    }
    chunks.push(value);
    receivedBytes += value.byteLength;
    if (onProgress) {
      onProgress({ receivedBytes, totalBytes });
    }
  }

  const buffer = new Uint8Array(receivedBytes);
  let offset = 0;
  for (const chunk of chunks) {
    buffer.set(chunk, offset);
    offset += chunk.byteLength;
  }

  if (compression === "gzip") {
    if (typeof DecompressionStream !== "function") {
      throw new Error("gzip-compressed browser assets require DecompressionStream support");
    }
    const stream = new Blob([buffer]).stream().pipeThrough(new DecompressionStream("gzip"));
    return new Response(stream).text();
  }

  return new TextDecoder().decode(buffer);
}

function setBootStatus(text) {
  bootStatusActive = true;
  setStatus("loading", text);
}

function clearBootStatus() {
  bootStatusActive = false;
}

async function ensureBrowserFsLoaded() {
  if (browserFsPromise) {
    return browserFsPromise;
  }
  browserFsPromise = (async () => {
    if (typeof globalThis.jsoo_create_file !== "function") {
      throw new Error("js_of_ocaml filesystem initializer is not ready");
    }
    try {
      const bundle = JSON.parse(
        await fetchTextWithProgress(
          buildAssetUrl("browser_fs_bundle.json.gz"),
          "gzip",
          (progress) => {
            if (!bootStatusActive) {
              return;
            }
            if (!progress) {
              setBootStatus("loading runtime");
              return;
            }
            const { receivedBytes, totalBytes } = progress;
            if (Number.isFinite(totalBytes) && totalBytes > 0) {
              const percent = Math.max(
                0,
                Math.min(100, Math.round((receivedBytes / totalBytes) * 100)),
              );
              setBootStatus(`loading runtime ${percent}%`);
              return;
            }
            setBootStatus(`loading runtime ${formatByteCount(receivedBytes)}`);
          },
        ),
      );
      setBootStatus("starting runtime");
      for (const entry of bundle) {
        globalThis.jsoo_create_file(entry.fs_path, atob(entry.content_base64));
      }
      return;
    } catch (error) {
      console.warn("OxCaml browser_fs bundle load failed; falling back to manifest", error);
    }
    const manifest = await fetchJson(buildAssetUrl("browser_fs_manifest.json"));
    let nextIndex = 0;
    const concurrency = Math.min(browserFsConcurrency, manifest.length || 1);
    async function worker() {
      while (nextIndex < manifest.length) {
        const entry = manifest[nextIndex];
        nextIndex += 1;
        const content = await fetchBinaryString(
          buildAssetUrl(entry.asset_path),
          entry.compression,
        );
        globalThis.jsoo_create_file(entry.fs_path, content);
      }
    }
    await Promise.all(Array.from({ length: concurrency }, () => worker()));
  })();
  return browserFsPromise;
}

const ready = (async () => {
  setBootStatus("loading runtime");
  await loadScript(new URL("./runtime_shims.js", import.meta.url));
  setBootStatus("loading compiler");
  await loadScript(buildAssetUrl("web_bytecode_js.bc.js"));
  setBootStatus("loading standard library");
  installGlobalScriptEvaluator();
  await ensureBrowserFsLoaded();
  setBootStatus("starting compiler");
  const backend = window.WebBytecodeJs;
  if (
    !backend ||
    typeof backend.checkString !== "function" ||
    typeof backend.runString !== "function"
  ) {
    throw new Error("static OxCaml backend failed to initialize");
  }
  return backend;
})();

export async function checkString(filename, source) {
  const backend = await ready;
  return backend.checkString(filename, source);
}

export async function runString(filename, source) {
  const backend = await ready;
  return backend.runString(filename, source);
}

export async function checkFile(file) {
  const source = await file.text();
  return checkString(file.name, source);
}

export async function runFile(file) {
  const source = await file.text();
  return runString(file.name, source);
}

window.webBytecode = { checkString, runString, checkFile, runFile };

function escapeHtml(text) {
  return text.replace(/[&<>"]/g, (char) => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    "\"": "&quot;",
  }[char]));
}

function parseDiagnosticMarkers(text, filename) {
  const lines = text.replace(/\r\n/g, "\n").split("\n");
  const markers = [];
  for (let index = 0; index < lines.length; index += 1) {
    const match = /^File "([^"]+)", line (\d+), characters (\d+)-(\d+):$/.exec(lines[index]);
    if (!match) {
      continue;
    }
    const [, diagnosticFilename, lineText, startText, endText] = match;
    if (diagnosticFilename !== filename) {
      continue;
    }
    let blockEnd = index + 1;
    while (blockEnd < lines.length && !/^File /.test(lines[blockEnd])) {
      blockEnd += 1;
    }
    let severity = "error";
    for (let lookahead = index + 1; lookahead < Math.min(blockEnd, index + 8); lookahead += 1) {
      const line = lines[lookahead];
      if (/^Warning\b|^Alert\b/.test(line)) {
        severity = "warning";
        break;
      }
      if (/^Error:|^Exception:/.test(line)) {
        severity = "error";
        break;
      }
    }
    const start = Number.parseInt(startText, 10);
    const end = Math.max(Number.parseInt(endText, 10), start + 1);
    const message = lines
      .slice(index + 1, blockEnd)
      .filter((line) => line !== "")
      .join("\n")
      .trim();
    markers.push({
      line: Math.max(Number.parseInt(lineText, 10) - 1, 0),
      start,
      end,
      severity,
      message,
      blockStartLine: index,
      blockEndLine: Math.max(index, blockEnd - 1),
    });
    index = blockEnd - 1;
  }
  return markers;
}

function markerDocRange(doc, marker) {
  try {
    const line = doc.line(marker.line + 1);
    const lineEnd = line.to;
    const from = Math.min(line.from + marker.start, lineEnd);
    const to = Math.min(Math.max(line.from + marker.end, from + 1), lineEnd);
    if (from >= to) {
      return null;
    }
    return { from, to };
  } catch {
    return null;
  }
}

function markerAtPosition(view, pos) {
  let bestMatch = null;
  for (const marker of editorMarkers) {
    const range = markerDocRange(view.state.doc, marker);
    if (!range) {
      continue;
    }
    if (pos < range.from || pos > range.to) {
      continue;
    }
    if (
      bestMatch === null ||
      range.to - range.from < bestMatch.range.to - bestMatch.range.from
    ) {
      bestMatch = { marker, range };
    }
  }
  return bestMatch;
}

function jumpToMarker(marker) {
  if (!editorView) {
    return;
  }
  const range = markerDocRange(editorView.state.doc, marker);
  if (!range) {
    return;
  }
  editorView.dispatch({
    selection: { anchor: range.from, head: range.to },
    effects: EditorView.scrollIntoView(range.from, { y: "center" }),
  });
  editorView.focus();
}

function setStatus(state, text = "") {
  if (!statusEl || !statusTextEl) {
    return;
  }
  statusEl.dataset.state = state;
  statusTextEl.textContent = text;
}

function setOutputState(state) {
  if (!outputPanelEl || !outputLabelEl) {
    return;
  }
  outputPanelEl.dataset.state = state;
  outputLabelEl.textContent = "Output";
}

function setOutputBusy(isBusy) {
  if (!outputPanelEl) {
    return;
  }
  outputPanelEl.dataset.busy = isBusy ? "true" : "false";
}

function renderEmptyOutput() {
  if (!fullUi || !outputEl) {
    return;
  }
  setOutputBusy(false);
  setOutputState("idle");
  outputEl.innerHTML = '<div class="output-empty"></div>';
}

function classifyTranscriptLine(line, inDiagnosticBlock, forceDiagnostics) {
  const isFile = /^File /.test(line);
  const isWarning = /^Warning\b/.test(line) || /^Alert\b/.test(line);
  const isError = /^Error:/.test(line);
  const isException = /^Exception:/.test(line);
  const isHint = /^\s*Hint:/.test(line);
  const isTrace = /^(Raised at|Called from|Re-raised at)/.test(line);
  const isCaret = /^\s*\^+/.test(line);
  const isCode = /^\d+\s+\|/.test(line);
  const isDetail =
    /^\s{2,}\S/.test(line) ||
    /^\s+This /.test(line) ||
    /^\s+The /.test(line) ||
    /^\s+Hint:/.test(line) ||
    /^\s+but /.test(line);

  const isDiagnostic =
    forceDiagnostics ||
    isFile ||
    isWarning ||
    isError ||
    isException ||
    isHint ||
    isTrace ||
    isCaret ||
    isCode ||
    (inDiagnosticBlock && (line === "" || isDetail));

  if (!isDiagnostic) {
    return { cls: "stream", nextDiagnosticBlock: false, hasWarning: false, hasError: false };
  }

  let cls = "detail";
  if (isFile) cls = "file";
  else if (isWarning) cls = "warning";
  else if (isError) cls = "error";
  else if (isException) cls = "exception";
  else if (isHint) cls = "hint";
  else if (isTrace) cls = "trace";
  else if (isCaret) cls = "caret";
  else if (isCode) cls = "code";

  return {
    cls,
    nextDiagnosticBlock: true,
    hasWarning: isWarning,
    hasError: isError || isException,
    hasException: isException,
    hasCompilerError: isError,
  };
}

function buildDiagnosticLineMarkerMap(markers) {
  const lineToMarker = new Map();
  markers.forEach((marker, index) => {
    for (let line = marker.blockStartLine; line <= marker.blockEndLine; line += 1) {
      lineToMarker.set(line, index);
    }
  });
  return lineToMarker;
}

function buildTranscript(text, { emptyPlaceholder = null, forceDiagnostics = false } = {}) {
  const normalized = text.replace(/\r\n/g, "\n");
  if (normalized === "" && emptyPlaceholder !== null) {
    return {
      hasWarning: false,
      hasError: false,
      hasException: false,
      hasCompilerError: false,
      tone: "output",
      html: `<pre class="transcript"><span class="transcript-line stream placeholder">${escapeHtml(emptyPlaceholder)}</span></pre>`,
    };
  }

  const lines = normalized.replace(/\n$/, "").split("\n");
  const markerByLine = buildDiagnosticLineMarkerMap(editorMarkers);
  let hasWarning = false;
  let hasError = false;
  let hasException = false;
  let hasCompilerError = false;
  let inDiagnosticBlock = forceDiagnostics;
  const body = lines
    .map((line, lineIndex) => {
      const info = classifyTranscriptLine(line, inDiagnosticBlock, forceDiagnostics);
      inDiagnosticBlock = info.nextDiagnosticBlock;
      hasWarning ||= info.hasWarning;
      hasError ||= info.hasError;
      hasException ||= info.hasException;
      hasCompilerError ||= info.hasCompilerError;
      const markerIndex = markerByLine.get(lineIndex);
      const attrs =
        markerIndex === undefined
          ? ""
          : ` data-marker-index="${markerIndex}" tabindex="0" role="button"`;
      const clickableClass = markerIndex === undefined ? "" : " clickable";
      return `<span class="transcript-line ${info.cls}${clickableClass}"${attrs}>${escapeHtml(line || " ")}\n</span>`;
    })
    .join("");

  return {
    hasWarning,
    hasError,
    hasException,
    hasCompilerError,
    tone: hasError ? "error" : hasWarning ? "warning" : "output",
    html: `<pre class="transcript">${body}</pre>`,
  };
}

function renderTranscript(text, options) {
  if (!fullUi || !outputEl) {
    return { hasWarning: false, hasError: false, tone: "idle", html: "" };
  }
  const transcript = buildTranscript(text, options);
  setOutputBusy(false);
  setOutputState(transcript.tone);
  outputEl.innerHTML = transcript.html;
  return transcript;
}

function updateEditorMarkers(_source, diagnostics) {
  editorMarkers = diagnostics ? parseDiagnosticMarkers(diagnostics, currentFilename) : [];
  if (!editorView) {
    return;
  }
  editorView.dispatch({
    effects: setDiagnosticsEffect.of(editorMarkers),
  });
}

function currentSourceRevision() {
  return currentRevision;
}

function setSource(source, filename, sampleId = null) {
  if (!fullUi) {
    return;
  }
  currentFilename = filename;
  currentSampleId = sampleId;
  currentRevision += 1;
  clearEditorMarkers();
  replaceEditorSource(source);
  if (samplePickerEl.value !== (sampleId || "")) {
    samplePickerEl.value = sampleId || "";
  }
  schedulePipeline();
}

function clearPendingWork() {
  if (pendingRunTimer !== null) {
    clearTimeout(pendingRunTimer);
    pendingRunTimer = null;
  }
}

function schedulePipeline() {
  clearPendingWork();
  const revision = currentSourceRevision();
  setOutputBusy(true);
  setStatus("running", "running");
  pendingRunTimer = window.setTimeout(() => {
    pendingRunTimer = null;
    void runCurrentSource({ revision });
  }, autoRunDelayMs);
}

async function runCurrentSource({ revision = currentSourceRevision() } = {}) {
  try {
    setStatus("running", "running");
    await ready;
    if (revision !== currentSourceRevision()) {
      return;
    }
    const output = await runString(currentFilename, sourceText());
    if (revision !== currentSourceRevision()) {
      return;
    }
    updateEditorMarkers(sourceText(), output);
    const transcript = renderTranscript(output, { emptyPlaceholder: "(no output)" });
    if (transcript.hasException) {
      setStatus("error", "exception");
    } else if (transcript.hasCompilerError) {
      setStatus("error", "error");
    } else if (transcript.hasWarning) {
      setStatus("warning", "warnings");
    } else {
      setStatus("ready", "ok");
    }
  } catch (error) {
    setOutputBusy(false);
    renderTranscript(String(error), { forceDiagnostics: true });
    setStatus("error", "offline");
  }
}

function populateSamples() {
  samplePickerEl.innerHTML = getVisibleSamplesByTopic()
    .map(({ topic, samples }) => {
      const options = samples
        .map(
          (sample) =>
            `<option value="${escapeHtml(sample.id)}">${escapeHtml(sample.label)}</option>`,
        )
        .join("");
      return `<optgroup label="${escapeHtml(topic)}">${options}</optgroup>`;
    })
    .join("");
}

if (fullUi) {
  editorView = createEditor();

  samplePickerEl.addEventListener("change", () => {
    const sample = getSampleById(samplePickerEl.value);
    if (!sample) {
      return;
    }
    setSource(sample.source, sample.filename, sample.id);
  });

  outputEl?.addEventListener("click", (event) => {
    const target = event.target instanceof Element
      ? event.target.closest("[data-marker-index]")
      : null;
    if (!target) {
      return;
    }
    const markerIndex = Number.parseInt(target.getAttribute("data-marker-index") || "", 10);
    const marker = editorMarkers[markerIndex];
    if (marker) {
      jumpToMarker(marker);
    }
  });

  outputEl?.addEventListener("keydown", (event) => {
    if (!(event.target instanceof Element)) {
      return;
    }
    if (event.key !== "Enter" && event.key !== " ") {
      return;
    }
    const target = event.target.closest("[data-marker-index]");
    if (!target) {
      return;
    }
    event.preventDefault();
    const markerIndex = Number.parseInt(target.getAttribute("data-marker-index") || "", 10);
    const marker = editorMarkers[markerIndex];
    if (marker) {
      jumpToMarker(marker);
    }
  });

  populateSamples();
  renderEmptyOutput();
  if (defaultSample) {
    setSource(defaultSample.source, defaultSample.filename, defaultSample.id);
  }

  ready.then(
    () => {
      if (bootStatusActive) {
        clearBootStatus();
        if (statusEl?.dataset.state === "loading") {
          setStatus("ready", "ok");
        }
      }
    },
    (error) => {
      clearBootStatus();
      renderTranscript(String(error), { forceDiagnostics: true });
      setStatus("error", "offline");
    },
  );
}
