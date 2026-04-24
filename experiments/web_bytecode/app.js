import {
  defaultSample,
  getSampleById,
  getVisibleSamplesByTopic,
} from "./sample_catalog.js";
import {
  addBackendStatusListener,
  checkFile as backendCheckFile,
  checkString as backendCheckString,
  interfaceString as backendInterfaceString,
  ready as backendReady,
  runFile as backendRunFile,
  runString as backendRunString,
  utopString as backendUtopString,
} from "./backend.js?v=20260424-multicore-shim";
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

const autoRunDelayMs = 90;
const maxCheckedSourceLength = 100000;
const maxCheckedNumericLiteralLength = 80;
const maxBrowserDecimalIntLiteral = "2147483648";
const htmlOutputBeginMarker = "%%OXCAML_HTML_BEGIN%%";
const htmlOutputEndMarker = "%%OXCAML_HTML_END%%";
const storagePrefix = "oxcaml-playground:v1";
const clearStorageParam = "clear";

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
let runningPipeline = false;
let queuedPipelineRevision = null;
let currentRevision = 0;
let editorMarkers = [];
let editorView = null;
let suppressEditorChanges = false;
let bootStatusActive = false;
const ready = backendReady;

const setDiagnosticsEffect = StateEffect.define();
const setSyntaxDecorationsEffect = StateEffect.define();

function pageStorageScope() {
  try {
    const url = new URL(window.location.href);
    url.hash = "";
    url.searchParams.delete(clearStorageParam);
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

function shouldClearStoredSources() {
  try {
    return new URL(window.location.href).searchParams.has(clearStorageParam);
  } catch {
    return false;
  }
}

function clearStoredSourcesForRequest() {
  if (!shouldClearStoredSources()) {
    return;
  }
  try {
    const storage = window.localStorage;
    const pagePrefix = `${storagePrefix}:${pageStorageScope()}:`;
    for (let index = storage.length - 1; index >= 0; index -= 1) {
      const key = storage.key(index);
      if (!key) {
        continue;
      }
      if (key.startsWith(pagePrefix)) {
        storage.removeItem(key);
      }
    }
  } catch {
    // Ignore storage failures.
  }
}

clearStoredSourcesForRequest();

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
  ranges.sort((left, right) => left.from - right.from || left.to - right.to);
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

function htmlOutputFrameHtml(htmlSource) {
  return (
    '<div class="html-output">' +
    `<iframe class="html-output__frame" sandbox srcdoc="${escapeHtml(htmlSource)}"></iframe>` +
    "</div>"
  );
}

function parsedTranscriptParts(text) {
  const lines = text.replace(/\n$/, "").split("\n");
  const parts = [];
  let textLines = [];
  let htmlLines = null;

  const flushText = () => {
    if (textLines.length > 0) {
      parts.push({ kind: "text", lines: textLines });
      textLines = [];
    }
  };

  for (const line of lines) {
    if (htmlLines !== null) {
      if (line === htmlOutputEndMarker) {
        parts.push({ kind: "html", html: htmlLines.join("\n") });
        htmlLines = null;
      } else {
        htmlLines.push(line);
      }
    } else if (line === htmlOutputBeginMarker) {
      flushText();
      htmlLines = [];
    } else {
      textLines.push(line);
    }
  }

  if (htmlLines !== null) {
    textLines.push(htmlOutputBeginMarker, ...htmlLines);
  }
  flushText();
  return parts;
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

function sourcePosition(source, offset) {
  const prefix = source.slice(0, offset);
  const line = (prefix.match(/\n/g) || []).length;
  const lineStart = prefix.lastIndexOf("\n") + 1;
  return { line, character: offset - lineStart };
}

function sourceDiagnostic(filename, line, start, end, message) {
  return `File "${filename}", line ${line + 1}, characters ${start}-${Math.max(end, start + 1)}:\nError: ${message}`;
}

function unsafeBrowserIntLiteral(token) {
  if (!/^[0-9][0-9_]*$/.test(token)) {
    return false;
  }
  const digits = token.replace(/_/g, "");
  if (digits.length > maxBrowserDecimalIntLiteral.length) {
    return true;
  }
  return (
    digits.length === maxBrowserDecimalIntLiteral.length &&
    digits > maxBrowserDecimalIntLiteral
  );
}

function findNumericLiteralPreflight(source) {
  let index = 0;
  let line = 0;
  let lineStart = 0;
  while (index < source.length) {
    const char = source[index];
    if (char === "\n") {
      index += 1;
      line += 1;
      lineStart = index;
      continue;
    }
    if (isWhitespace(char)) {
      index += 1;
      continue;
    }
    if (char === "(" && source[index + 1] === "*") {
      index += 2;
      let depth = 1;
      while (index < source.length && depth > 0) {
        if (source[index] === "\n") {
          index += 1;
          line += 1;
          lineStart = index;
        } else if (source[index] === "(" && source[index + 1] === "*") {
          depth += 1;
          index += 2;
        } else if (source[index] === "*" && source[index + 1] === ")") {
          depth -= 1;
          index += 2;
        } else {
          index += 1;
        }
      }
      continue;
    }
    if (char === "\"" || char === "'") {
      const quote = char;
      index += 1;
      while (index < source.length) {
        if (source[index] === "\n") {
          index += 1;
          line += 1;
          lineStart = index;
        } else if (source[index] === "\\") {
          index += 2;
        } else if (source[index] === quote) {
          index += 1;
          break;
        } else {
          index += 1;
        }
      }
      continue;
    }
    if (/[0-9]/.test(char)) {
      const start = index;
      const startLine = line;
      const startCharacter = index - lineStart;
      index += 1;
      while (index < source.length && /[A-Za-z0-9_'.]/.test(source[index])) {
        index += 1;
      }
      const token = source.slice(start, index);
      const length = index - start;
      if (unsafeBrowserIntLiteral(token)) {
        return {
          kind: "int_overflow",
          line: startLine,
          start: startCharacter,
          end: startCharacter + length,
        };
      }
      if (length > maxCheckedNumericLiteralLength) {
        return {
          kind: "oversized",
          line: startLine,
          start: startCharacter,
          end: startCharacter + Math.min(length, maxCheckedNumericLiteralLength),
          length,
        };
      }
      continue;
    }
    index += 1;
  }
  return null;
}

function sourcePreflightDiagnostic(filename, source) {
  if (source.length > maxCheckedSourceLength) {
    const position = sourcePosition(source, maxCheckedSourceLength);
    return sourceDiagnostic(
      filename,
      position.line,
      position.character,
      position.character + 1,
      `This playground snippet is too large to check as you type. Keep snippets under ${maxCheckedSourceLength.toLocaleString()} characters.`,
    );
  }
  const numericLiteral = findNumericLiteralPreflight(source);
  if (numericLiteral) {
    if (numericLiteral.kind === "int_overflow") {
      return sourceDiagnostic(
        filename,
        numericLiteral.line,
        numericLiteral.start,
        numericLiteral.end,
        "Integer literal exceeds the range of representable integers of type int",
      );
    }
    return sourceDiagnostic(
      filename,
      numericLiteral.line,
      numericLiteral.start,
      numericLiteral.end,
      `This numeric literal has ${numericLiteral.length.toLocaleString()} characters, which is too large for browser auto-checking. Shorten it or put the digits in a string.`,
    );
  }
  return null;
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

function setBootStatus(text) {
  bootStatusActive = true;
  setStatus("loading", text);
}

function clearBootStatus() {
  bootStatusActive = false;
}

export async function checkString(filename, source) {
  return backendCheckString(filename, source);
}

export async function interfaceString(filename, source) {
  return backendInterfaceString(filename, source);
}

export async function runString(filename, source) {
  return backendRunString(filename, source);
}

export async function utopString(filename, source) {
  return backendUtopString(filename, source);
}

export async function checkFile(file) {
  return backendCheckFile(file);
}

export async function runFile(file) {
  return backendRunFile(file);
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

  const parts = parsedTranscriptParts(normalized);
  const markerByLine = buildDiagnosticLineMarkerMap(editorMarkers);
  let hasWarning = false;
  let hasError = false;
  let hasException = false;
  let hasCompilerError = false;
  let inDiagnosticBlock = forceDiagnostics;
  let globalLineIndex = 0;
  const body = parts
    .map((part) => {
      if (part.kind === "html") {
        globalLineIndex += part.html === "" ? 2 : part.html.split("\n").length + 2;
        return htmlOutputFrameHtml(part.html);
      }
      const preBody = part.lines
        .map((line) => {
          const lineIndex = globalLineIndex;
          globalLineIndex += 1;
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
      return preBody === "" ? "" : `<pre class="transcript">${preBody}</pre>`;
    })
    .join("");

  return {
    hasWarning,
    hasError,
    hasException,
    hasCompilerError,
    tone: hasError ? "error" : hasWarning ? "warning" : "output",
    html: body,
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
  queuedPipelineRevision = null;
}

function schedulePipeline() {
  clearPendingWork();
  const revision = currentSourceRevision();
  setOutputBusy(true);
  setStatus("running", "running");
  pendingRunTimer = window.setTimeout(() => {
    pendingRunTimer = null;
    void requestPipelineRun(revision);
  }, autoRunDelayMs);
}

async function requestPipelineRun(revision) {
  if (runningPipeline) {
    queuedPipelineRevision = revision;
    return;
  }
  await runCurrentSource({ revision });
}

async function runCurrentSource({ revision = currentSourceRevision() } = {}) {
  runningPipeline = true;
  try {
    setStatus("running", "running");
    const source = sourceText();
    const preflightDiagnostic = sourcePreflightDiagnostic(currentFilename, source);
    if (preflightDiagnostic) {
      updateEditorMarkers(source, preflightDiagnostic);
      const transcript = renderTranscript(preflightDiagnostic, { forceDiagnostics: true });
      setStatus(transcript.hasWarning ? "warning" : "error", "error");
      return;
    }
    await ready;
    if (revision !== currentSourceRevision()) {
      return;
    }
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
  } finally {
    runningPipeline = false;
    if (queuedPipelineRevision !== null) {
      const nextRevision = queuedPipelineRevision;
      queuedPipelineRevision = null;
      window.setTimeout(() => {
        void requestPipelineRun(nextRevision);
      }, 0);
    }
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

addBackendStatusListener(({ state, text }) => {
  if (!fullUi) {
    return;
  }
  if (state === "ready") {
    clearBootStatus();
    return;
  }
  if (state === "loading") {
    setBootStatus(text || "loading compiler");
  }
});

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
