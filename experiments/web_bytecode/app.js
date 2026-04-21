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

const autoRunDelayMs = 360;
const buildBase = "../../_build/default/experiments/web_bytecode";
const storagePrefix = "oxcaml-playground:v1";

const editorHostEl = document.getElementById("editor");
const outputEl = document.getElementById("output");
const outputLabelEl = document.getElementById("output-label");
const outputPanelEl = document.getElementById("output-panel");
const statusEl = document.getElementById("status");
const statusTextEl = statusEl?.querySelector(".status-text") ?? null;
const samplePickerEl = document.getElementById("sample-picker");
const resetButtonEl = document.getElementById("reset-source");
const fullUi = Boolean(
  editorHostEl &&
  outputLabelEl &&
  outputPanelEl &&
  samplePickerEl,
);

let currentFilename = "snippet.ml";
let currentOriginalSource = "";
let currentSampleId = null;
let currentStorageKey = null;
let pendingRunTimer = null;
let currentRevision = 0;
let editorMarkers = [];
let editorView = null;
let suppressEditorChanges = false;
const loadedScriptUrls = new Map();
let browserFsManifestPromise = null;
let browserFsSeedPromise = null;
const browserFsLoadedPaths = new Set();
const browserFsLoadingPaths = new Map();
const browserFsConcurrency = 4;
const browserFsFetchRetries = 4;
const browserFsRetryLimit = 32;
const browserFsSeedPaths = [
  "/static/cmis/stdlib.cmi",
  "/static/cmis/stdlib__List.cmi",
];
let bootStatusActive = false;

const setDiagnosticsEffect = StateEffect.define();
const setSyntaxDecorationsEffect = StateEffect.define();

function pageStorageScope() {
  try {
    const url = new URL(window.location.href);
    url.hash = "";
    return url.href;
  } catch {
    return "";
  }
}

function stableHash(text) {
  let hash = 2166136261;
  for (let index = 0; index < text.length; index += 1) {
    hash ^= text.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0).toString(36);
}

function storageKeyForSource(source) {
  return `${storagePrefix}:${pageStorageScope()}:${source.length}:${stableHash(source)}`;
}

function readStoredSource(storageKey) {
  try {
    return window.localStorage.getItem(storageKey);
  } catch {
    return null;
  }
}

function writeStoredSource(storageKey, source) {
  try {
    window.localStorage.setItem(storageKey, source);
  } catch {
    // Storage can be unavailable or full; the playground should still run normally.
  }
}

function removeStoredSource(storageKey) {
  try {
    window.localStorage.removeItem(storageKey);
  } catch {
    // Ignore storage failures.
  }
}

const oxcamlIdentifierNames = new Set(["local_", "stack_", "exclave_"]);
const packageModuleNames = new Set(["Base", "Core", "Stdlib_stable"]);
const primitiveTypeNames = new Set([
  "array",
  "bool",
  "bytes",
  "char",
  "exn",
  "float",
  "int",
  "list",
  "nativeint",
  "option",
  "ref",
  "result",
  "string",
  "unit",
]);
const keywordTokens = new Set([
  "and",
  "as",
  "assert",
  "begin",
  "class",
  "constraint",
  "do",
  "done",
  "downto",
  "else",
  "end",
  "exception",
  "external",
  "false",
  "for",
  "fun",
  "function",
  "functor",
  "if",
  "in",
  "include",
  "inherit",
  "initializer",
  "lazy",
  "let",
  "match",
  "method",
  "module",
  "mutable",
  "new",
  "nonrec",
  "object",
  "of",
  "open",
  "or",
  "private",
  "rec",
  "sig",
  "struct",
  "then",
  "to",
  "true",
  "try",
  "type",
  "val",
  "virtual",
  "when",
  "while",
  "with",
]);
const moduleIntroducers = new Set(["open", "include", "module", "functor", "inherit"]);
const declarationIntroducers = new Set(["let", "and", "external", "method", "val"]);
const parameterIntroducers = new Set(["fun", "function"]);
const typeIntroducers = new Set(["type", "of", "constraint"]);
const punctuationChars = new Set(["(", ")", "[", "]", "{", "}", ";", ","]);
const operatorChars = new Set(["!", "$", "%", "&", "*", "+", "-", ".", "/", ":", "<", "=", ">", "@", "^", "|", "~", "?"]);

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
  return ranges;
}

function isWhitespace(char) {
  return /\s/.test(char);
}

function isIdentifierStart(char) {
  return /[A-Za-z_]/.test(char);
}

function isIdentifierChar(char) {
  return /[A-Za-z0-9_']/.test(char);
}

function isOperatorChar(char) {
  return operatorChars.has(char);
}

function nextNonWhitespaceChar(source, offset) {
  let index = offset;
  while (index < source.length && isWhitespace(source[index])) {
    index += 1;
  }
  return source[index] ?? "";
}

function previousToken(tokens, index) {
  for (let cursor = index - 1; cursor >= 0; cursor -= 1) {
    if (tokens[cursor].type !== "comment") {
      return tokens[cursor];
    }
  }
  return null;
}

function nextToken(tokens, index) {
  for (let cursor = index + 1; cursor < tokens.length; cursor += 1) {
    if (tokens[cursor].type !== "comment") {
      return tokens[cursor];
    }
  }
  return null;
}

function pushToken(tokens, from, to, type, source) {
  if (to > from) {
    tokens.push({ from, to, type, text: source.slice(from, to) });
  }
}

function tokenizeSyntax(source) {
  const tokens = [];
  let index = 0;
  while (index < source.length) {
    const char = source[index];
    if (isWhitespace(char)) {
      index += 1;
      continue;
    }
    if (char === "(" && source[index + 1] === "*") {
      const start = index;
      index += 2;
      let depth = 1;
      while (index < source.length && depth > 0) {
        if (source[index] === "(" && source[index + 1] === "*") {
          depth += 1;
          index += 2;
        } else if (source[index] === "*" && source[index + 1] === ")") {
          depth -= 1;
          index += 2;
        } else {
          index += 1;
        }
      }
      pushToken(tokens, start, index, "comment", source);
      continue;
    }
    if (char === "\"") {
      const start = index;
      index += 1;
      while (index < source.length) {
        if (source[index] === "\\") {
          index += 2;
        } else if (source[index] === "\"") {
          index += 1;
          break;
        } else {
          index += 1;
        }
      }
      pushToken(tokens, start, index, "string", source);
      continue;
    }
    if (char === "'" && /[A-Za-z_]/.test(source[index + 1] ?? "")) {
      const start = index;
      index += 2;
      while (index < source.length && isIdentifierChar(source[index])) {
        index += 1;
      }
      pushToken(tokens, start, index, "typevar", source);
      continue;
    }
    if (char === "'") {
      const start = index;
      index += 1;
      while (index < source.length) {
        if (source[index] === "\\") {
          index += 2;
        } else if (source[index] === "'") {
          index += 1;
          break;
        } else {
          index += 1;
        }
      }
      pushToken(tokens, start, index, "string", source);
      continue;
    }
    if ((char === "~" || char === "?") && isIdentifierStart(source[index + 1] ?? "")) {
      const start = index;
      index += 2;
      while (index < source.length && isIdentifierChar(source[index])) {
        index += 1;
      }
      if (source[index] === ":") {
        index += 1;
      }
      pushToken(tokens, start, index, "label", source);
      continue;
    }
    if (/[0-9]/.test(char)) {
      const start = index;
      const match = /^(?:0[xX][0-9A-Fa-f_]+|0[oO][0-7_]+|0[bB][01_]+|[0-9][0-9_]*(?:\.[0-9_]+)?(?:[eE][+-]?[0-9_]+)?)/.exec(source.slice(index));
      index += match ? match[0].length : 1;
      pushToken(tokens, start, index, "number", source);
      continue;
    }
    if (isIdentifierStart(char)) {
      const start = index;
      index += 1;
      while (index < source.length && isIdentifierChar(source[index])) {
        index += 1;
      }
      const text = source.slice(start, index);
      pushToken(tokens, start, index, keywordTokens.has(text) ? "keyword" : "identifier", source);
      continue;
    }
    if (punctuationChars.has(char)) {
      pushToken(tokens, index, index + 1, "operator", source);
      index += 1;
      continue;
    }
    if (isOperatorChar(char)) {
      const start = index;
      index += 1;
      while (
        index < source.length &&
        isOperatorChar(source[index]) &&
        !(source[index] === "(" && source[index + 1] === "*")
      ) {
        index += 1;
      }
      pushToken(tokens, start, index, "operator", source);
      continue;
    }
    index += 1;
  }
  return tokens;
}

function classifySyntaxToken(tokens, index, source) {
  const token = tokens[index];
  const prev = previousToken(tokens, index);
  const next = nextToken(tokens, index);
  const nextChar = nextNonWhitespaceChar(source, token.to);
  const classes = [];

  switch (token.type) {
    case "comment":
      classes.push("tok-comment");
      break;
    case "string":
      classes.push("tok-string");
      break;
    case "number":
      classes.push("tok-number");
      break;
    case "typevar":
      classes.push("tok-type");
      break;
    case "label":
      classes.push("tok-label");
      break;
    case "operator":
      classes.push("tok-operator");
      break;
    case "keyword":
      classes.push("tok-keyword");
      if (oxcamlIdentifierNames.has(token.text)) {
        classes.unshift("tok-oxcaml");
      }
      break;
    case "identifier":
      if (oxcamlIdentifierNames.has(token.text)) {
        classes.push("tok-oxcaml");
      }
      if (packageModuleNames.has(token.text)) {
        classes.push("tok-package");
        if (prev?.text === "open") {
          classes.push("tok-package-open");
        }
      }
      if (/^[A-Z]/.test(token.text)) {
        classes.push(
          moduleIntroducers.has(prev?.text ?? "") || nextChar === "." || token.text.includes("__")
            ? "tok-module"
            : "tok-constructor",
        );
      } else if (primitiveTypeNames.has(token.text)) {
        classes.push("tok-type");
      } else if (parameterIntroducers.has(prev?.text ?? "")) {
        classes.push("tok-parameter");
      } else if (declarationIntroducers.has(prev?.text ?? "")) {
        classes.push("tok-function");
      } else if (typeIntroducers.has(prev?.text ?? "") || next?.text === ":") {
        classes.push("tok-type");
      }
      break;
    default:
      break;
  }

  if (!classes.length) {
    return null;
  }
  return Array.from(new Set(classes)).join(" ");
}

function buildSyntaxDecorations(source) {
  const tokens = tokenizeSyntax(source);
  const ranges = [];
  for (let index = 0; index < tokens.length; index += 1) {
    const token = tokens[index];
    const className = classifySyntaxToken(tokens, index, source);
    if (!className) {
      continue;
    }
    ranges.push({ from: token.from, to: token.to, className });
  }
  ranges.push(...collectSupplementalSyntaxRanges(source));
  const builder = new RangeSetBuilder();
  for (const { from, to, className } of ranges) {
    builder.add(from, to, Decoration.mark({ class: className }));
  }
  return builder.finish();
}

function highlightedSyntaxHtml(source) {
  const tokens = tokenizeSyntax(source);
  let cursor = 0;
  let html = "";
  for (let index = 0; index < tokens.length; index += 1) {
    const token = tokens[index];
    const className = classifySyntaxToken(tokens, index, source);
    if (!className) {
      continue;
    }
    html += escapeHtml(source.slice(cursor, token.from));
    html += `<span class="${className}">${escapeHtml(source.slice(token.from, token.to))}</span>`;
    cursor = token.to;
  }
  html += escapeHtml(source.slice(cursor));
  return html;
}

function outcomeStartIndex(line) {
  const match = /(?:val\s+[A-Za-z_][A-Za-z0-9_']*\s*:|-\s*:|type\b|module(?:\s+type)?\b|exception\b|external\b|class(?:\s+type)?\b)/.exec(line);
  return match ? match.index : -1;
}

function splitOutcomeTypeAndValue(text) {
  const separator = " = ";
  const index = text.indexOf(separator);
  if (index === -1) {
    return { typeText: text, valueText: null };
  }
  return {
    typeText: text.slice(0, index),
    valueText: text.slice(index + separator.length),
  };
}

function highlightedOutcomeHtml(line) {
  const leadingMatch = /^(\s*)/.exec(line);
  const leading = leadingMatch ? leadingMatch[1] : "";
  const body = line.slice(leading.length);
  const valueMatch = /^(val)\s+([A-Za-z_][A-Za-z0-9_']*)\s*:\s*(.+)$/.exec(body);
  const expressionMatch = /^(-)\s*:\s*(.+)$/.exec(body);
  if (!valueMatch && !expressionMatch) {
    return escapeHtml(leading) + highlightedSyntaxHtml(body || " ");
  }
  const keyword = valueMatch ? valueMatch[1] : expressionMatch[1];
  const name = valueMatch ? valueMatch[2] : null;
  const rest = valueMatch ? valueMatch[3] : expressionMatch[2];
  const { typeText, valueText } = splitOutcomeTypeAndValue(rest);
  const valueHtml = valueText === null
    ? ""
    : ` <span class="utop-outcome__equals">=</span> <span class="utop-outcome__value">${highlightedSyntaxHtml(valueText)}</span>`;
  return (
    `${escapeHtml(leading)}<span class="utop-outcome__keyword">${escapeHtml(keyword)}</span>` +
    (name === null ? "" : ` <span class="utop-outcome__name">${escapeHtml(name)}</span>`) +
    ` <span class="utop-outcome__punctuation">:</span> ` +
    `<span class="utop-outcome__type">${highlightedSyntaxHtml(typeText.trim())}</span>` +
    valueHtml
  );
}

function transcriptLineHtml(cls, clickableClass, attrs, lineHtml) {
  return `<span class="transcript-line ${cls}${clickableClass}"${attrs}>${lineHtml}\n</span>`;
}

function streamLineHtml(line, cls, clickableClass, attrs, { utopMode = false } = {}) {
  if (!utopMode) {
    return transcriptLineHtml(cls, clickableClass, attrs, escapeHtml(line || " "));
  }
  const index = outcomeStartIndex(line);
  if (index === 0) {
    return transcriptLineHtml(
      `${cls} utop-outcome-line`,
      clickableClass,
      attrs,
      highlightedOutcomeHtml(line || " "),
    );
  }
  if (index > 0) {
    const stdoutHtml = `<span class="utop-stdout">${escapeHtml(line.slice(0, index))}</span>`;
    const outcomeHtml = highlightedOutcomeHtml(line.slice(index));
    return (
      transcriptLineHtml(cls, clickableClass, attrs, stdoutHtml) +
      transcriptLineHtml(`${cls} utop-outcome-line`, "", "", outcomeHtml)
    );
  }
  return transcriptLineHtml(
    `${cls} utop-stdout-line`,
    clickableClass,
    attrs,
    `<span class="utop-stdout">${escapeHtml(line || " ")}</span>`,
  );
}

function scheduleSyntaxRefresh() {
  if (!editorView) {
    return;
  }
  try {
    editorView.dispatch({
      effects: setSyntaxDecorationsEffect.of(buildSyntaxDecorations(sourceText())),
    });
  } catch (error) {
    console.warn("Local syntax highlighting failed", error);
  }
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
          persistCurrentSource();
          schedulePipeline();
        }),
      ],
    }),
  });
}

function sourceText() {
  return editorView ? editorView.state.doc.toString() : "";
}

function updateResetState() {
  if (!resetButtonEl) {
    return;
  }
  resetButtonEl.hidden = sourceText() === currentOriginalSource;
}

function persistCurrentSource() {
  if (!currentStorageKey) {
    return;
  }
  const source = sourceText();
  if (source === currentOriginalSource) {
    removeStoredSource(currentStorageKey);
  } else {
    writeStoredSource(currentStorageKey, source);
  }
  updateResetState();
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
  updateResetState();
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

function setBootStatus(text) {
  bootStatusActive = true;
  setStatus("loading", text);
}

function clearBootStatus() {
  bootStatusActive = false;
}

function browserFsBasename(fsPath) {
  const slashIndex = fsPath.lastIndexOf("/");
  return slashIndex >= 0 ? fsPath.slice(slashIndex + 1) : fsPath;
}

function browserFsEntryPriority(fsPath) {
  if (fsPath.startsWith("/static/cmis/")) {
    return 0;
  }
  if (fsPath.startsWith("/static/packages/opam/")) {
    return 1;
  }
  if (fsPath.startsWith("/static/packages/install/")) {
    return 2;
  }
  return 3;
}

async function ensureBrowserFsManifest() {
  if (browserFsManifestPromise) {
    return browserFsManifestPromise;
  }
  browserFsManifestPromise = (async () => {
    const manifest = await fetchJson(buildAssetUrl("browser_fs_manifest.json"));
    const entriesByPath = new Map();
    const entriesByBasename = new Map();

    for (const entry of manifest) {
      entriesByPath.set(entry.fs_path, entry);

      const basename = browserFsBasename(entry.fs_path);
      const basenameEntries = entriesByBasename.get(basename) ?? [];
      basenameEntries.push(entry);
      entriesByBasename.set(basename, basenameEntries);
    }

    for (const entries of entriesByBasename.values()) {
      entries.sort(
        (left, right) =>
          browserFsEntryPriority(left.fs_path) - browserFsEntryPriority(right.fs_path) ||
          left.fs_path.localeCompare(right.fs_path),
      );
    }

    return {
      manifest,
      entriesByPath,
      entriesByBasename,
    };
  })();
  return browserFsManifestPromise;
}

async function ensureBrowserFsEntryLoaded(entry) {
  if (browserFsLoadedPaths.has(entry.fs_path)) {
    return;
  }
  const existing = browserFsLoadingPaths.get(entry.fs_path);
  if (existing) {
    return existing;
  }
  const promise = (async () => {
    if (typeof globalThis.jsoo_create_file !== "function") {
      throw new Error("js_of_ocaml filesystem initializer is not ready");
    }
    const content = await fetchBinaryString(
      buildAssetUrl(entry.asset_path),
      entry.compression,
    );
    globalThis.jsoo_create_file(entry.fs_path, content);
    browserFsLoadedPaths.add(entry.fs_path);
  })();
  browserFsLoadingPaths.set(entry.fs_path, promise);
  try {
    await promise;
  } finally {
    browserFsLoadingPaths.delete(entry.fs_path);
  }
}

async function ensureBrowserFsEntriesLoaded(entries) {
  let nextIndex = 0;
  const concurrency = Math.min(browserFsConcurrency, entries.length || 1);
  async function worker() {
    while (nextIndex < entries.length) {
      const entry = entries[nextIndex];
      nextIndex += 1;
      await ensureBrowserFsEntryLoaded(entry);
    }
  }
  await Promise.all(Array.from({ length: concurrency }, () => worker()));
}

async function ensureBrowserFsSeedLoaded() {
  if (browserFsSeedPromise) {
    return browserFsSeedPromise;
  }
  browserFsSeedPromise = (async () => {
    const manifest = await ensureBrowserFsManifest();
    const seedEntries = browserFsSeedPaths
      .map((fsPath) => manifest.entriesByPath.get(fsPath))
      .filter(Boolean);
    if (bootStatusActive) {
      setBootStatus("loading standard library");
    }
    await ensureBrowserFsEntriesLoaded(seedEntries);
  })();
  return browserFsSeedPromise;
}

async function resolveBrowserFsEntry(filename) {
  const manifest = await ensureBrowserFsManifest();
  if (filename.startsWith("/")) {
    return manifest.entriesByPath.get(filename) ?? null;
  }
  return manifest.entriesByBasename.get(filename)?.[0] ?? null;
}

async function ensureBrowserFsForMissingFilename(filename) {
  const entry = await resolveBrowserFsEntry(filename);
  if (!entry) {
    return false;
  }
  await ensureBrowserFsEntryLoaded(entry);
  return true;
}

function trimDiagnosticDelimiters(text) {
  return text.trim().replace(/^["'`]+|["'`]+$/g, "");
}

function lowercaseFirst(text) {
  if (!text) {
    return text;
  }
  return `${text[0].toLowerCase()}${text.slice(1)}`;
}

function flattenModulePath(name) {
  const parts = name.split(".").filter(Boolean);
  if (!parts.length) {
    return name;
  }
  let flattened = parts[0];
  for (let index = 1; index < parts.length; index += 1) {
    const part = parts[index];
    flattened += flattened.endsWith("__") || part.startsWith("__") ? part : `__${part}`;
  }
  return flattened;
}

function flattenModulePathPrefixes(name) {
  const parts = name.split(".").filter(Boolean);
  if (!parts.length) {
    return [];
  }
  const prefixes = [];
  let flattened = parts[0];
  prefixes.push(flattened);
  for (let index = 1; index < parts.length; index += 1) {
    const part = parts[index];
    flattened += flattened.endsWith("__") || part.startsWith("__") ? part : `__${part}`;
    prefixes.push(flattened);
  }
  return prefixes;
}

function modulePathToCmiCandidates(name) {
  if (typeof name !== "string" || name.length === 0) {
    return [];
  }
  const trimmed = trimDiagnosticDelimiters(name);
  if (!trimmed) {
    return [];
  }
  const basename = browserFsBasename(trimmed);
  if (basename.endsWith(".cmi")) {
    return [basename];
  }
  if (basename.endsWith(".ml") || basename.endsWith(".mli")) {
    return [`${basename.replace(/\.(?:ml|mli)$/, "")}.cmi`];
  }
  const prefixes = flattenModulePathPrefixes(trimmed).map((prefix) => `${lowercaseFirst(prefix)}.cmi`);
  return Array.from(new Set([
    ...prefixes,
    `${lowercaseFirst(basename)}.cmi`,
  ]));
}

function legacyMissingCmiCandidates(output) {
  if (typeof output !== "string") {
    return [];
  }
  const extractedNames = [];
  const patterns = [
    /Could not find the \.cmi file for interface\s+["'`]?([A-Za-z0-9_'.\/-]+)["'`]?\./,
    /The compiled interface for module\s+["'`]?([A-Za-z0-9_'.]+)["'`]?\s+was not found\./,
    /Unbound module\s+["'`]?([A-Z][A-Za-z0-9_'.]*)["'`]?(?:\s+in instance\b|$)/m,
    /This is an alias for module\s+["'`]?([A-Za-z0-9_'.]+)["'`]?,\s+which is missing/,
    /The module\s+["'`]?[A-Za-z0-9_'.]+["'`]?\s+is an alias for module\s+["'`]?([A-Za-z0-9_'.]+)["'`]?,\s+which is missing/,
    /The type of this packed module refers to\s+["'`]?([A-Za-z0-9_'.]+)["'`]?,\s+which is missing/,
  ];
  for (const pattern of patterns) {
    const match = pattern.exec(output);
    if (match?.[1]) {
      extractedNames.push(match[1]);
    }
  }
  return Array.from(new Set(extractedNames.flatMap(modulePathToCmiCandidates)));
}

async function legacyMissingCmiResult(output) {
  const candidates = legacyMissingCmiCandidates(output);
  if (!candidates.length) {
    return null;
  }
  const manifest = await ensureBrowserFsManifest();
  let firstLoadedMatch = null;
  for (const candidate of candidates) {
    const entry = manifest.entriesByBasename.get(candidate)?.[0];
    if (entry) {
      if (!browserFsLoadedPaths.has(entry.fs_path)) {
        return { kind: "missing_cmi", filename: entry.fs_path };
      }
      if (!firstLoadedMatch) {
        firstLoadedMatch = entry.fs_path;
      }
    }
  }
  if (firstLoadedMatch) {
    return { kind: "missing_cmi", filename: firstLoadedMatch };
  }
  return null;
}

async function normalizeBackendResult(result) {
  if (typeof result === "string") {
    return (await legacyMissingCmiResult(result)) ?? { kind: "ok", output: result };
  }
  return result;
}

async function runBackendWithLazyFs(methodName, filename, source) {
  const backend = await ready;
  let previousMissingFilename = null;
  for (let attempt = 0; attempt < browserFsRetryLimit; attempt += 1) {
    const result = await normalizeBackendResult(backend[methodName](filename, source));
    if (!result || result.kind === "ok") {
      return result?.output ?? "";
    }
    if (result.kind !== "missing_cmi" || typeof result.filename !== "string") {
      throw new Error(`unexpected backend result from ${methodName}`);
    }
    if (result.filename === previousMissingFilename) {
      throw new Error(`lazy filesystem stalled while loading ${result.filename}`);
    }
    previousMissingFilename = result.filename;
    setStatus("loading", `loading ${result.filename}`);
    const loaded = await ensureBrowserFsForMissingFilename(result.filename);
    if (!loaded) {
      throw new Error(`missing browser filesystem asset for ${result.filename}`);
    }
  }
  throw new Error(`lazy filesystem retry limit exceeded for ${filename}`);
}

const ready = (async () => {
  setBootStatus("loading runtime");
  await loadScript(new URL("./runtime_shims.js", import.meta.url));
  setBootStatus("loading compiler");
  await loadScript(buildAssetUrl("web_bytecode_js.bc.js"));
  installGlobalScriptEvaluator();
  await ensureBrowserFsSeedLoaded();
  setBootStatus("starting compiler");
  const backend = window.WebBytecodeJs;
  if (
    !backend ||
    typeof backend.checkString !== "function" ||
    typeof backend.runString !== "function" ||
    typeof backend.utopString !== "function"
  ) {
    throw new Error("static OxCaml backend failed to initialize");
  }
  return backend;
})();

export async function checkString(filename, source) {
  return runBackendWithLazyFs("checkString", filename, source);
}

export async function interfaceString(filename, source) {
  const backend = await ready;
  if (typeof backend.interfaceString !== "function") {
    return undefined;
  }
  return runBackendWithLazyFs("interfaceString", filename, source);
}

export async function runString(filename, source) {
  return runBackendWithLazyFs("runString", filename, source);
}

export async function utopString(filename, source) {
  return runBackendWithLazyFs("utopString", filename, source);
}

export async function checkFile(file) {
  const source = await file.text();
  return checkString(file.name, source);
}

export async function runFile(file) {
  const source = await file.text();
  return runString(file.name, source);
}

window.webBytecode = {
  checkString,
  interfaceString,
  runString,
  utopString,
  checkFile,
  runFile,
};

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

function buildTranscript(text, { emptyPlaceholder = null, forceDiagnostics = false, utopMode = false } = {}) {
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
      let lineHtml = escapeHtml(line || " ");
      if (info.cls === "code") {
        const match = /^(\d+\s+\|\s?)(.*)$/.exec(line);
        if (match) {
          lineHtml =
            `<span class="diagnostic-code-prefix">${escapeHtml(match[1])}</span>` +
            highlightedSyntaxHtml(match[2]);
        }
      } else if (info.cls === "stream") {
        return streamLineHtml(line, info.cls, clickableClass, attrs, { utopMode });
      }
      return transcriptLineHtml(info.cls, clickableClass, attrs, lineHtml);
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

function buildInterfaceHtml(text) {
  const trimmed = text.replace(/\r\n/g, "\n").trim();
  if (trimmed === "") {
    return "";
  }
  return (
    '<div class="interface-output"><div class="interface-output__label">Inferred types</div>' +
    `<pre class="interface-output__body">${highlightedSyntaxHtml(trimmed)}</pre></div>`
  );
}

function renderTranscript(text, options) {
  if (!fullUi || !outputEl) {
    return { hasWarning: false, hasError: false, tone: "idle", html: "" };
  }
  const transcript = buildTranscript(text, options);
  setOutputBusy(false);
  setOutputState(transcript.tone);
  outputEl.innerHTML =
    transcript.html + (options?.interfaceText === undefined ? "" : buildInterfaceHtml(options.interfaceText));
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
  currentOriginalSource = source;
  currentSampleId = sampleId;
  currentStorageKey = storageKeyForSource(source);
  currentRevision += 1;
  clearEditorMarkers();
  replaceEditorSource(readStoredSource(currentStorageKey) ?? source);
  if (samplePickerEl.value !== (sampleId || "")) {
    samplePickerEl.value = sampleId || "";
  }
  updateResetState();
  schedulePipeline();
}

function resetCurrentSource() {
  if (!currentStorageKey) {
    return;
  }
  removeStoredSource(currentStorageKey);
  clearPendingWork();
  currentRevision += 1;
  clearEditorMarkers();
  replaceEditorSource(currentOriginalSource);
  updateResetState();
  schedulePipeline();
  editorView?.focus();
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
    const source = sourceText();
    const output = await runString(currentFilename, source);
    if (revision !== currentSourceRevision()) {
      return;
    }
    updateEditorMarkers(source, output);
    const transcriptPreview = buildTranscript(output, { emptyPlaceholder: "(no output)" });
    const showInterface =
      !transcriptPreview.hasException && !transcriptPreview.hasCompilerError;
    const interfaceOutput = showInterface
      ? await interfaceString(currentFilename, source)
      : undefined;
    if (revision !== currentSourceRevision()) {
      return;
    }
    const transcript = renderTranscript(output, {
      emptyPlaceholder: "(no output)",
      interfaceText: interfaceOutput,
    });
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

  resetButtonEl?.addEventListener("click", resetCurrentSource);

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
