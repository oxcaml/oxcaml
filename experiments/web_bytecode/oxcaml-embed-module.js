import {
  addBackendStatusListener,
  interfaceString,
  ready,
  runString,
  utopString,
} from "./backend.js?v=20260421-utop-output";
import {
  EditorState,
  RangeSetBuilder,
  StateEffect,
  StateField,
} from "https://esm.sh/@codemirror/state@6.5.2/es2022/state.mjs";
import {
  Decoration,
  EditorView,
  drawSelection,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
} from "https://esm.sh/@codemirror/view@6.38.6?deps=@codemirror/state@6.5.2";
import {
  bracketMatching,
  indentOnInput,
} from "https://esm.sh/@codemirror/language@6.11.3?deps=@codemirror/state@6.5.2,@codemirror/view@6.38.6";
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
} from "https://esm.sh/@codemirror/commands@6.8.1?deps=@codemirror/state@6.5.2,@codemirror/view@6.38.6,@codemirror/language@6.11.3";

const autoRunDelayMs = 360;
const storagePrefix = "oxcaml-editor:v1";
const mountedEditors = new Set();
const setDiagnosticsEffect = StateEffect.define();
const setSyntaxDecorationsEffect = StateEffect.define();

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
  "and", "as", "assert", "begin", "class", "constraint", "do", "done",
  "downto", "else", "end", "exception", "external", "false", "for", "fun",
  "function", "functor", "if", "in", "include", "inherit", "initializer",
  "lazy", "let", "match", "method", "module", "mutable", "new", "nonrec",
  "object", "of", "open", "or", "private", "rec", "sig", "struct", "then",
  "to", "true", "try", "type", "val", "virtual", "when", "while", "with",
]);
const moduleIntroducers = new Set(["open", "include", "module", "functor", "inherit"]);
const declarationIntroducers = new Set(["let", "and", "external", "method", "val"]);
const parameterIntroducers = new Set(["fun", "function"]);
const typeIntroducers = new Set(["type", "of", "constraint"]);
const punctuationChars = new Set(["(", ")", "[", "]", "{", "}", ";", ","]);
const operatorChars = new Set([
  "!", "$", "%", "&", "*", "+", "-", ".", "/", ":", "<", "=", ">", "@",
  "^", "|", "~", "?",
]);

function escapeHtml(text) {
  return text.replace(/[&<>"]/g, (char) => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    "\"": "&quot;",
  }[char]));
}

function dedent(source) {
  const withoutOuterBlankLines = source.replace(/^\n+|\s+$/g, "");
  const lines = withoutOuterBlankLines.split("\n");
  const indents = lines
    .filter((line) => line.trim() !== "")
    .map((line) => /^ */.exec(line)?.[0].length ?? 0);
  const minIndent = indents.length ? Math.min(...indents) : 0;
  return lines.map((line) => line.slice(minIndent)).join("\n");
}

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

function storageKeyForSource(source, duplicateIndex = 0) {
  const duplicateSuffix = duplicateIndex > 0 ? `:${duplicateIndex}` : "";
  return `${storagePrefix}:${pageStorageScope()}:${source.length}:${stableHash(source)}${duplicateSuffix}`;
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
    // Storage can be unavailable or full; the editor should still run normally.
  }
}

function removeStoredSource(storageKey) {
  try {
    window.localStorage.removeItem(storageKey);
  } catch {
    // Ignore storage failures.
  }
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
        class: marker.severity === "warning"
          ? "cm-diagnostic-warning"
          : "cm-diagnostic-error",
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

function markerAtPosition(editor, view, pos) {
  let bestMatch = null;
  for (const marker of editor.markers) {
    const range = markerDocRange(view.state.doc, marker);
    if (!range || pos < range.from || pos > range.to) {
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

function diagnosticHover(editor) {
  return hoverTooltip((view, pos) => {
    const match = markerAtPosition(editor, view, pos);
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
          moduleIntroducers.has(prev?.text ?? "") ||
          nextChar === "." ||
          token.text.includes("__")
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
    if (className) {
      ranges.push({ from: token.from, to: token.to, className });
    }
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

function sourceText(editor) {
  return editor.view ? editor.view.state.doc.toString() : "";
}

function updateResetState(editor) {
  if (!editor.resetButtonEl) {
    return;
  }
  const isModified = sourceText(editor) !== editor.originalSource;
  editor.resetButtonEl.hidden = !isModified;
  editor.root.dataset.modified = isModified ? "true" : "false";
}

function persistEditorSource(editor) {
  const source = sourceText(editor);
  if (source === editor.originalSource) {
    removeStoredSource(editor.storageKey);
  } else {
    writeStoredSource(editor.storageKey, source);
  }
  updateResetState(editor);
}

function replaceEditorSource(editor, source) {
  if (!editor.view) {
    return;
  }
  editor.suppressEditorChanges = true;
  editor.view.dispatch({
    changes: {
      from: 0,
      to: editor.view.state.doc.length,
      insert: source,
    },
    effects: setDiagnosticsEffect.of([]),
  });
  editor.suppressEditorChanges = false;
  scheduleSyntaxRefresh(editor);
}

function scheduleSyntaxRefresh(editor) {
  if (!editor.view) {
    return;
  }
  try {
    editor.view.dispatch({
      effects: setSyntaxDecorationsEffect.of(buildSyntaxDecorations(sourceText(editor))),
    });
  } catch (error) {
    console.warn("Local syntax highlighting failed", error);
  }
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

function buildDiagnosticLineMarkerMap(markers) {
  const lineToMarker = new Map();
  markers.forEach((marker, index) => {
    for (let line = marker.blockStartLine; line <= marker.blockEndLine; line += 1) {
      lineToMarker.set(line, index);
    }
  });
  return lineToMarker;
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

function buildTranscript(editor, text, { emptyPlaceholder = null, forceDiagnostics = false, utopMode = false } = {}) {
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
  const markerByLine = buildDiagnosticLineMarkerMap(editor.markers);
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
  const body = trimmed === ""
    ? '<pre class="interface-output__body placeholder">(no exported values)</pre>'
    : `<pre class="interface-output__body">${highlightedSyntaxHtml(trimmed)}</pre>`;
  return `<div class="interface-output"><div class="interface-output__label">Inferred types</div>${body}</div>`;
}

function modeForElement(element, options = {}) {
  if (options.mode !== undefined) {
    return options.mode === "utop" ? "utop" : "run";
  }
  return element.hasAttribute("utop") ? "utop" : "run";
}

function injectStyles() {
  if (document.getElementById("oxcaml-embed-styles")) {
    return;
  }
  const style = document.createElement("style");
  style.id = "oxcaml-embed-styles";
  style.textContent = `
    .oxcaml-embed {
      --oxcaml-bg: #fffdf9;
      --oxcaml-ink: #1c2530;
      --oxcaml-muted: #687586;
      --oxcaml-border: rgba(21, 32, 46, 0.16);
      --oxcaml-editor: #12161d;
      --oxcaml-editor-ink: #edf4ff;
      --oxcaml-output-bg: #f1f5f7;
      --oxcaml-output-border: rgba(21, 32, 46, 0.12);
      --oxcaml-accent: #bf4f2d;
      --oxcaml-ok: #0f7b5f;
      --oxcaml-warn: #b36b00;
      --oxcaml-error: #b23b2c;
      border: 1px solid var(--oxcaml-border);
      border-radius: 8px;
      background: var(--oxcaml-bg);
      color: var(--oxcaml-ink);
      font-family: Avenir Next, Segoe UI, system-ui, sans-serif;
      margin: 1rem 0;
      overflow: hidden;
    }

    .oxcaml-embed__status {
      font-size: 0.76rem;
      font-weight: 650;
      letter-spacing: 0.08em;
      line-height: 1;
      text-transform: uppercase;
    }

    .oxcaml-embed__status {
      position: absolute;
      right: 0.85rem;
      top: 0.72rem;
      color: var(--oxcaml-muted);
      z-index: 1;
      white-space: nowrap;
    }

    .oxcaml-embed__reset {
      position: absolute;
      right: 0.7rem;
      top: 0.62rem;
      z-index: 5;
      min-height: 1.75rem;
      border: 1px solid rgba(255, 255, 255, 0.15);
      border-radius: 6px;
      background: rgba(18, 22, 29, 0.78);
      color: #d8e3f2;
      cursor: pointer;
      font: 650 0.72rem/1 Avenir Next, Segoe UI, system-ui, sans-serif;
      padding: 0.32rem 0.5rem;
      box-shadow: 0 8px 20px rgba(0, 0, 0, 0.22);
      backdrop-filter: blur(8px);
    }

    .oxcaml-embed__reset:hover,
    .oxcaml-embed__reset:focus-visible {
      background: rgba(28, 35, 47, 0.94);
      border-color: rgba(255, 181, 126, 0.48);
      color: #ffcfb4;
      outline: none;
    }

    .oxcaml-embed__reset[hidden] {
      display: none;
    }

    .oxcaml-embed[data-state="running"] .oxcaml-embed__status,
    .oxcaml-embed[data-state="loading"] .oxcaml-embed__status {
      color: var(--oxcaml-accent);
    }

    .oxcaml-embed[data-state="ready"] .oxcaml-embed__status {
      color: var(--oxcaml-ok);
    }

    .oxcaml-embed[data-state="warning"] .oxcaml-embed__status {
      color: var(--oxcaml-warn);
    }

    .oxcaml-embed[data-state="error"] .oxcaml-embed__status {
      color: var(--oxcaml-error);
    }

    .oxcaml-embed__editor-host {
      position: relative;
      background: var(--oxcaml-editor);
    }

    .oxcaml-embed__editor-host .cm-editor {
      min-height: 9rem;
      background: var(--oxcaml-editor);
      color: var(--oxcaml-editor-ink);
      font: 0.92rem/1.55 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    }

    .oxcaml-embed__editor-host .cm-scroller {
      min-height: 9rem;
      max-height: min(38rem, 72vh);
      overflow: auto;
      line-height: 1.55;
    }

    .oxcaml-embed__editor-host .cm-gutters {
      background: #0e131a;
      border-right: 1px solid rgba(255, 255, 255, 0.08);
      color: #6e7a8d;
    }

    .oxcaml-embed__editor-host .cm-gutterElement {
      min-width: 2.4rem;
      padding: 0 0.65rem 0 0.45rem;
    }

    .oxcaml-embed__editor-host .cm-content {
      caret-color: #ffffff;
      padding: 0.85rem 5.9rem 0.85rem 0.95rem;
    }

    .oxcaml-embed__editor-host .cm-line {
      padding: 0 0.3rem;
    }

    .oxcaml-embed__editor-host .cm-activeLine,
    .oxcaml-embed__editor-host .cm-activeLineGutter {
      background: rgba(255, 255, 255, 0.06);
    }

    .oxcaml-embed__editor-host .cm-selectionBackground {
      background: rgba(110, 169, 255, 0.34) !important;
    }

    .oxcaml-embed__editor-host .cm-cursor,
    .oxcaml-embed__editor-host .cm-dropCursor {
      border-left-color: #ffffff;
    }

    .tok-keyword { color: #ffb57e; }
    .tok-module { color: #8ed2ff; }
    .tok-string { color: #b6f09c; }
    .tok-comment { color: #6a768c; }
    .tok-number { color: #f7cd74; }
    .tok-operator { color: #f0f5ff; }
    .tok-function { color: #9fd8ff; }
    .tok-parameter { color: #ffd7a1; }
    .tok-type { color: #88c6ff; }
    .tok-constructor { color: #f2c572; }
    .tok-oxcaml { color: #ff9f7a; font-weight: 700; }
    .tok-annotation { color: #ffd28f; font-style: italic; }
    .tok-package { color: #7fd6c2; font-weight: 700; }
    .tok-package-open { color: #a8bcff; }
    .tok-label { color: #f4b5ff; }

    .cm-diagnostic-error {
      border-bottom: 2px solid rgba(255, 92, 92, 0.95);
      background: rgba(255, 92, 92, 0.13);
      border-radius: 2px;
    }

    .cm-diagnostic-warning {
      border-bottom: 2px solid rgba(255, 202, 102, 0.92);
      background: rgba(255, 202, 102, 0.13);
      border-radius: 2px;
    }

    .cm-tooltip.cm-tooltip-hover,
    .cm-diagnostic-tooltip {
      max-width: min(520px, calc(100vw - 48px));
      border: 1px solid rgba(21, 32, 46, 0.18);
      border-radius: 8px;
      background: #fffdf9;
      color: #1d2733;
      box-shadow: 0 14px 32px rgba(16, 24, 35, 0.18);
      font: 0.82rem/1.45 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      padding: 0.65rem 0.75rem;
      white-space: pre-wrap;
    }

    .cm-diagnostic-tooltip.warning {
      border-color: rgba(179, 107, 0, 0.34);
    }

    .cm-diagnostic-tooltip.error {
      border-color: rgba(178, 59, 44, 0.34);
    }

    .oxcaml-embed__output {
      position: relative;
      min-height: 2.75rem;
      margin: 0;
      overflow: auto;
      border-top: 1px solid var(--oxcaml-output-border);
      background: var(--oxcaml-output-bg);
      color: var(--oxcaml-ink);
    }

    .oxcaml-embed[data-busy="true"] .oxcaml-embed__output {
      opacity: 0.72;
    }

    .transcript {
      margin: 0;
      padding: 0.75rem 5.5rem 0.75rem 0.95rem;
      font: 0.86rem/1.48 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      white-space: pre-wrap;
    }

    .transcript-line {
      display: block;
      min-height: 1.35em;
    }

    .transcript-line.clickable {
      cursor: pointer;
      border-radius: 4px;
    }

    .transcript-line.clickable:hover,
    .transcript-line.clickable:focus-visible {
      background: rgba(191, 79, 45, 0.12);
      outline: none;
    }

    .transcript-line.stream {
      color: #243142;
    }

    .transcript-line.placeholder {
      color: var(--oxcaml-muted);
    }

    .transcript-line.file {
      color: #49627d;
      font-weight: 650;
    }

    .transcript-line.warning {
      color: var(--oxcaml-warn);
      font-weight: 700;
    }

    .transcript-line.error,
    .transcript-line.exception {
      color: var(--oxcaml-error);
      font-weight: 700;
    }

    .transcript-line.hint {
      color: #725c9e;
    }

    .transcript-line.caret {
      color: var(--oxcaml-error);
      font-weight: 700;
    }

    .transcript-line.code {
      color: #2e4053;
    }

    .diagnostic-code-prefix {
      color: #718095;
    }

    .transcript-line.detail,
    .transcript-line.trace {
      color: #526274;
    }

    .utop-outcome__keyword {
      color: #98521a;
      font-weight: 750;
    }

    .utop-outcome__name {
      color: #0b638a;
      font-weight: 700;
    }

    .utop-outcome__punctuation,
    .utop-outcome__equals {
      color: #718095;
    }

    .utop-outcome__type {
      color: var(--oxcaml-ok);
    }

    .utop-outcome__value {
      color: #263341;
    }

    .utop-stdout {
      color: #334155;
    }

    .interface-output {
      margin: 0 5.5rem 0.85rem 0.95rem;
      border: 1px solid rgba(15, 123, 95, 0.18);
      border-left: 4px solid var(--oxcaml-ok);
      border-radius: 7px;
      background: rgba(224, 245, 238, 0.62);
      padding: 0.68rem 0.8rem 0.75rem;
    }

    .interface-output__label {
      margin-bottom: 0.45rem;
      color: var(--oxcaml-ok);
      font: 700 0.68rem/1 Avenir Next, Segoe UI, system-ui, sans-serif;
      letter-spacing: 0.12em;
      text-transform: uppercase;
    }

    .interface-output__body {
      margin: 0;
      color: #173829;
      font: 0.84rem/1.48 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      white-space: pre-wrap;
    }

    .interface-output__body.placeholder {
      color: var(--oxcaml-muted);
    }

    .oxcaml-embed__output .tok-keyword { color: #98521a; font-weight: 650; }
    .oxcaml-embed__output .tok-module { color: #0d638c; }
    .oxcaml-embed__output .tok-string { color: #276a3f; }
    .oxcaml-embed__output .tok-comment { color: #6d7988; }
    .oxcaml-embed__output .tok-number { color: #846000; }
    .oxcaml-embed__output .tok-operator { color: #4f5c6b; }
    .oxcaml-embed__output .tok-function { color: #0d638c; }
    .oxcaml-embed__output .tok-parameter { color: #805514; }
    .oxcaml-embed__output .tok-type { color: #0d638c; }
    .oxcaml-embed__output .tok-constructor { color: #8a5a00; }
    .oxcaml-embed__output .tok-oxcaml { color: #b8462b; font-weight: 700; }
    .oxcaml-embed__output .tok-annotation { color: #8c5f16; font-style: italic; }
    .oxcaml-embed__output .tok-package { color: #007260; font-weight: 700; }
    .oxcaml-embed__output .tok-package-open { color: #475fa5; }
    .oxcaml-embed__output .tok-label { color: #8b3aa8; }
  `;
  document.head.appendChild(style);
}

function setStatus(editor, state, text) {
  editor.root.dataset.state = state;
  editor.statusEl.textContent = text;
}

function setOutputBusy(editor, isBusy) {
  editor.root.dataset.busy = isBusy ? "true" : "false";
}

function renderTranscript(editor, text, options) {
  const transcript = buildTranscript(editor, text, options);
  setOutputBusy(editor, false);
  editor.transcriptEl.innerHTML =
    transcript.html + (options?.interfaceText === undefined ? "" : buildInterfaceHtml(options.interfaceText));
  return transcript;
}

function updateEditorMarkers(editor, diagnostics) {
  editor.markers = diagnostics ? parseDiagnosticMarkers(diagnostics, editor.filename) : [];
  if (!editor.view) {
    return;
  }
  editor.view.dispatch({
    effects: setDiagnosticsEffect.of(editor.markers),
  });
}

function jumpToMarker(editor, marker) {
  if (!editor.view) {
    return;
  }
  const range = markerDocRange(editor.view.state.doc, marker);
  if (!range) {
    return;
  }
  editor.view.dispatch({
    selection: { anchor: range.from, head: range.to },
    effects: EditorView.scrollIntoView(range.from, { y: "center" }),
  });
  editor.view.focus();
}

function clearPendingWork(editor) {
  if (editor.pendingTimer !== null) {
    window.clearTimeout(editor.pendingTimer);
    editor.pendingTimer = null;
  }
}

function resetEditor(editor) {
  removeStoredSource(editor.storageKey);
  clearPendingWork(editor);
  editor.markers = [];
  editor.revision += 1;
  replaceEditorSource(editor, editor.originalSource);
  updateResetState(editor);
  scheduleRun(editor);
}

function scheduleRun(editor) {
  clearPendingWork(editor);
  editor.revision += 1;
  const revision = editor.revision;
  setOutputBusy(editor, true);
  setStatus(editor, "running", "running");
  editor.pendingTimer = window.setTimeout(() => {
    editor.pendingTimer = null;
    void runEditor(editor, revision);
  }, autoRunDelayMs);
}

async function runEditor(editor, revision = editor.revision) {
  try {
    setStatus(editor, "running", "running");
    await ready;
    if (revision !== editor.revision) {
      return;
    }
    const source = sourceText(editor);
    const output =
      editor.mode === "utop"
        ? await utopString(editor.filename, source)
        : await runString(editor.filename, source);
    if (revision !== editor.revision) {
      return;
    }
    updateEditorMarkers(editor, output);
    const transcriptPreview = buildTranscript(editor, output, {
      emptyPlaceholder: "(no output)",
      utopMode: editor.mode === "utop",
    });
    const showInterface =
      editor.mode !== "utop" &&
      !transcriptPreview.hasException && !transcriptPreview.hasCompilerError;
    const interfaceOutput = showInterface
      ? await interfaceString(editor.filename, source)
      : undefined;
    if (revision !== editor.revision) {
      return;
    }
    const transcript = renderTranscript(editor, output, {
      emptyPlaceholder: "(no output)",
      interfaceText: interfaceOutput,
      utopMode: editor.mode === "utop",
    });
    if (transcript.hasException) {
      setStatus(editor, "error", "exception");
    } else if (transcript.hasCompilerError) {
      setStatus(editor, "error", "error");
    } else if (transcript.hasWarning) {
      setStatus(editor, "warning", "warnings");
    } else {
      setStatus(editor, "ready", "ok");
    }
  } catch (error) {
    if (revision !== editor.revision) {
      return;
    }
    setOutputBusy(editor, false);
    renderTranscript(editor, String(error && error.stack ? error.stack : error), {
      forceDiagnostics: true,
    });
    setStatus(editor, "error", "offline");
  }
}

function createEditorView(editor, source) {
  return new EditorView({
    parent: editor.editorHostEl,
    state: EditorState.create({
      doc: source,
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
          "aria-label": "OxCaml source code",
        }),
        diagnosticField,
        syntaxField,
        diagnosticHover(editor),
        EditorView.updateListener.of((update) => {
          if (!update.docChanged || editor.suppressEditorChanges) {
            return;
          }
          scheduleSyntaxRefresh(editor);
          editor.markers = [];
          editor.revision += 1;
          persistEditorSource(editor);
          scheduleRun(editor);
        }),
      ],
    }),
  });
}

function createEditorElement() {
  const root = document.createElement("div");
  root.className = "oxcaml-embed";
  root.dataset.state = "loading";

  const statusEl = document.createElement("div");
  statusEl.className = "oxcaml-embed__status";
  statusEl.textContent = "loading";

  const resetButtonEl = document.createElement("button");
  resetButtonEl.type = "button";
  resetButtonEl.className = "oxcaml-embed__reset";
  resetButtonEl.textContent = "Reset";
  resetButtonEl.title = "Reset to the original source";
  resetButtonEl.hidden = true;

  const editorHostEl = document.createElement("div");
  editorHostEl.className = "oxcaml-embed__editor-host";

  const outputEl = document.createElement("div");
  outputEl.className = "oxcaml-embed__output";

  const transcriptEl = document.createElement("div");
  transcriptEl.className = "oxcaml-embed__transcript";
  transcriptEl.innerHTML =
    '<pre class="transcript"><span class="transcript-line stream placeholder">loading</span></pre>';

  editorHostEl.append(resetButtonEl);
  outputEl.append(transcriptEl, statusEl);
  root.append(editorHostEl, outputEl);

  return {
    root,
    statusEl,
    resetButtonEl,
    editorHostEl,
    outputEl,
    transcriptEl,
  };
}

export function mount(element, options = {}) {
  injectStyles();
  const originalSource = options.source ?? dedent(element.textContent ?? "");
  const storageKey =
    options.storageKey ??
    storageKeyForSource(originalSource, options.duplicateIndex ?? 0);
  const source = readStoredSource(storageKey) ?? originalSource;
  const explicitFilename =
    options.filename ??
    element.getAttribute("filename") ??
    element.getAttribute("data-filename");
  const filename = explicitFilename ?? `snippet_${mountedEditors.size + 1}.ml`;
  const mode = modeForElement(element, options);
  const editor = {
    ...createEditorElement(),
    filename,
    markers: [],
    mode,
    originalSource,
    pendingTimer: null,
    revision: 0,
    storageKey,
    suppressEditorChanges: false,
    view: null,
  };

  element.replaceWith(editor.root);
  editor.view = createEditorView(editor, source);
  mountedEditors.add(editor);
  scheduleSyntaxRefresh(editor);
  updateResetState(editor);

  editor.resetButtonEl.addEventListener("click", () => {
    resetEditor(editor);
    editor.view?.focus();
  });

  editor.outputEl.addEventListener("click", (event) => {
    const target = event.target instanceof Element
      ? event.target.closest("[data-marker-index]")
      : null;
    if (!target) {
      return;
    }
    const markerIndex = Number.parseInt(target.getAttribute("data-marker-index") || "", 10);
    const marker = editor.markers[markerIndex];
    if (marker) {
      jumpToMarker(editor, marker);
    }
  });

  editor.outputEl.addEventListener("keydown", (event) => {
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
    const marker = editor.markers[markerIndex];
    if (marker) {
      jumpToMarker(editor, marker);
    }
  });

  scheduleRun(editor);
  return editor;
}

export function processOxcamlTags(root = document) {
  const elements = Array.from(root.querySelectorAll("oxcaml"));
  const sources = elements.map((element) => dedent(element.textContent ?? ""));
  const sourceCounts = new Map();
  for (const source of sources) {
    sourceCounts.set(source, (sourceCounts.get(source) ?? 0) + 1);
  }
  const seenCounts = new Map();
  return elements.map((element, index) => {
    const source = sources[index];
    const seen = (seenCounts.get(source) ?? 0) + 1;
    seenCounts.set(source, seen);
    return mount(element, {
      source,
      duplicateIndex: sourceCounts.get(source) > 1 ? seen : 0,
    });
  });
}

addBackendStatusListener(({ state, text }) => {
  for (const editor of mountedEditors) {
    if (editor.root.dataset.state === "loading" || editor.root.dataset.state === "running") {
      setStatus(editor, state === "ready" ? "running" : state, text);
    }
  }
});

function processWhenReady() {
  injectStyles();
  processOxcamlTags();
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", processWhenReady, { once: true });
} else {
  processWhenReady();
}

const existingApi = window.OxCamlPlayground;
window.OxCamlPlayground = {
  ...(existingApi && typeof existingApi === "object" ? existingApi : {}),
  mount,
  processOxcamlTags,
  interfaceString,
  ready,
  runString,
  utopString,
};
