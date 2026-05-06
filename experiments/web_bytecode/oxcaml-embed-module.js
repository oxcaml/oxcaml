import {
  addBackendStatusListener,
  checkString,
  interfaceString,
  ready,
  runString,
  utopString,
} from "./backend.js?v=20260427-worker-blob";
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

const autoRunDelayMs = 90;
const maxCheckedSourceLength = 100000;
const maxCheckedNumericLiteralLength = 80;
const maxBrowserDecimalIntLiteral = "2147483648";
const htmlOutputBeginMarker = "%%OXCAML_HTML_BEGIN%%";
const htmlOutputEndMarker = "%%OXCAML_HTML_END%%";
const storagePrefix = "oxcaml-editor:v1";
const clearStorageParam = "clear";
const mountedEditors = new Set();
let editorRunQueue = Promise.resolve();
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

function sourceText(editor) {
  return editor.view ? editor.view.state.doc.toString() : "";
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
      `This playground snippet is too large for browser checking. Keep snippets under ${maxCheckedSourceLength.toLocaleString()} characters.`,
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
      `This numeric literal has ${numericLiteral.length.toLocaleString()} characters, which is too large for browser checking. Shorten it or put the digits in a string.`,
    );
  }
  return null;
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
  if (normalized === "") {
    return {
      hasWarning: false,
      hasError: false,
      hasException: false,
      hasCompilerError: false,
      tone: "output",
      isEmpty: true,
      html: emptyPlaceholder === null
        ? ""
        : `<pre class="transcript"><span class="transcript-line stream placeholder">${escapeHtml(emptyPlaceholder)}</span></pre>`,
    };
  }

  const parts = parsedTranscriptParts(normalized);
  const markerByLine = buildDiagnosticLineMarkerMap(editor.markers);
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
    isEmpty: false,
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

function modeForElement(element, options = {}) {
  if (options.mode !== undefined) {
    return options.mode === "utop" || options.mode === "check" ? options.mode : "run";
  }
  if (element.hasAttribute("utop")) {
    return "utop";
  }
  if (element.hasAttribute("check")) {
    return "check";
  }
  return "run";
}

function normalizeRunTrigger(value) {
  const normalized = String(value ?? "auto").trim().toLowerCase();
  if (normalized === "manual" || normalized === "manual-after-initial") {
    return normalized;
  }
  return "auto";
}

function runTriggerForElement(element, options = {}) {
  if (options.runTrigger !== undefined) {
    return normalizeRunTrigger(options.runTrigger);
  }
  return normalizeRunTrigger(element.getAttribute("data-oxcaml-run-trigger"));
}

function normalizeEmptyOutput(value) {
  return value === "hide" ? "hide" : "show";
}

function emptyOutputForElement(element, options = {}) {
  if (options.emptyOutput !== undefined) {
    return normalizeEmptyOutput(options.emptyOutput);
  }
  return normalizeEmptyOutput(element.getAttribute("data-oxcaml-empty-output"));
}

function runActionLabel(mode) {
  return mode === "check" ? "Check" : "Run";
}

function runShortcutLabel() {
  const platform =
    navigator.userAgentData?.platform ??
    navigator.platform ??
    "";
  return /Mac|iPhone|iPad|iPod/i.test(platform) ? "⌘↵" : "Ctrl+Enter";
}

function runButtonText(mode) {
  return runActionLabel(mode);
}

function runButtonTooltip(mode) {
  return `${runShortcutLabel()} to ${runActionLabel(mode).toLowerCase()}`;
}

function runTriggerIsManual(editor) {
  return editor.runTrigger === "manual" ||
    editor.runTrigger === "manual-after-initial";
}

function injectStyles() {
  if (document.getElementById("oxcaml-embed-styles")) {
    return;
  }
  const style = document.createElement("style");
  style.id = "oxcaml-embed-styles";
  style.textContent = `
    .oxcaml-embed {
      --_oxcaml-color-scheme: var(--oxcaml-color-scheme, auto);
      --_oxcaml-margin-block: var(--oxcaml-margin-block, 1rem);
      container-name: oxcaml-embed;
      font-family: var(--oxcaml-font-family, Avenir Next, Segoe UI, system-ui, sans-serif);
      margin: var(--_oxcaml-margin-block) 0;
    }

    .oxcaml-embed__surface {
      color-scheme: light;
      --_oxcaml-bg: var(--oxcaml-bg, #fffdf9);
      --_oxcaml-ink: var(--oxcaml-ink, #1c2530);
      --_oxcaml-muted: var(--oxcaml-muted, #687586);
      --_oxcaml-border: var(--oxcaml-border, rgba(21, 32, 46, 0.16));
      --_oxcaml-editor: var(--oxcaml-editor, #fbfcfd);
      --_oxcaml-editor-ink: var(--oxcaml-editor-ink, #1d2733);
      --_oxcaml-editor-gutter: var(--oxcaml-editor-gutter, #eef2f6);
      --_oxcaml-editor-gutter-ink: var(--oxcaml-editor-gutter-ink, #687586);
      --_oxcaml-editor-gutter-border: var(--oxcaml-editor-gutter-border, rgba(21, 32, 46, 0.1));
      --_oxcaml-editor-active: var(--oxcaml-editor-active, rgba(21, 32, 46, 0.055));
      --_oxcaml-editor-selection: var(--oxcaml-editor-selection, rgba(15, 123, 95, 0.18));
      --_oxcaml-editor-cursor: var(--oxcaml-editor-cursor, #0f7b5f);
      --_oxcaml-output-bg: var(--oxcaml-output-bg, #f1f5f7);
      --_oxcaml-output-border: var(--oxcaml-output-border, rgba(21, 32, 46, 0.12));
      --_oxcaml-accent: var(--oxcaml-accent, #bf4f2d);
      --_oxcaml-ok: var(--oxcaml-ok, #0f7b5f);
      --_oxcaml-warn: var(--oxcaml-warn, #b36b00);
      --_oxcaml-error: var(--oxcaml-error, #b23b2c);
      --_oxcaml-tooltip-bg: var(--oxcaml-tooltip-bg, #fffdf9);
      --_oxcaml-tooltip-ink: var(--oxcaml-tooltip-ink, #1d2733);
      --_oxcaml-stream: var(--oxcaml-stream, #243142);
      --_oxcaml-code: var(--oxcaml-code, #2e4053);
      --_oxcaml-detail: var(--oxcaml-detail, #526274);
      --_oxcaml-prefix: var(--oxcaml-prefix, #718095);
      --_oxcaml-interface-bg: var(--oxcaml-interface-bg, rgba(224, 245, 238, 0.62));
      --_oxcaml-interface-ink: var(--oxcaml-interface-ink, #173829);
      --_syntax-keyword: var(--syntax-keyword, #98521a);
      --_syntax-module: var(--syntax-module, #0d638c);
      --_syntax-string: var(--syntax-string, #276a3f);
      --_syntax-comment: var(--syntax-comment, #394756);
      --_syntax-number: var(--syntax-number, #846000);
      --_syntax-operator: var(--syntax-operator, #4f5c6b);
      --_syntax-function: var(--syntax-function, #0d638c);
      --_syntax-parameter: var(--syntax-parameter, #805514);
      --_syntax-type: var(--syntax-type, #0d638c);
      --_syntax-constructor: var(--syntax-constructor, #8a5a00);
      --_syntax-annotation: var(--syntax-annotation, #8c5f16);
      --_syntax-package: var(--syntax-package, #007260);
      --_syntax-package-open: var(--syntax-package-open, #475fa5);
      --_syntax-label: var(--syntax-label, #8b3aa8);
      --_oxcaml-radius: var(--oxcaml-radius, 8px);
      --_oxcaml-font-family: inherit;
      --_oxcaml-mono-font-family: var(--oxcaml-mono-font-family, ui-monospace, SFMono-Regular, Menlo, Consolas, monospace);
      --_oxcaml-editor-font-size: var(--oxcaml-editor-font-size, 0.92rem);
      --_oxcaml-output-font-size: var(--oxcaml-output-font-size, 0.86rem);
      --_oxcaml-editor-min-height: var(--oxcaml-editor-min-height, 9rem);
      --_oxcaml-editor-max-height: var(--oxcaml-editor-max-height, min(38rem, 72vh));
      --_oxcaml-output-min-height: var(--oxcaml-output-min-height, 2.75rem);
      --_oxcaml-html-output-height: var(--oxcaml-html-output-height, 320px);
      border: 1px solid var(--_oxcaml-border);
      border-radius: var(--_oxcaml-radius);
      background: var(--_oxcaml-bg);
      color: var(--_oxcaml-ink);
      font-family: var(--_oxcaml-font-family);
      overflow: hidden;
    }

    @media (prefers-color-scheme: dark) {
      @container oxcaml-embed style(--_oxcaml-color-scheme: auto) {
        .oxcaml-embed__surface {
          color-scheme: dark;
          --_oxcaml-bg: var(--oxcaml-bg, #17202a);
          --_oxcaml-ink: var(--oxcaml-ink, #eef4fb);
          --_oxcaml-muted: var(--oxcaml-muted, #9aa7b8);
          --_oxcaml-border: var(--oxcaml-border, rgba(215, 226, 240, 0.16));
          --_oxcaml-editor: var(--oxcaml-editor, #10151d);
          --_oxcaml-editor-ink: var(--oxcaml-editor-ink, #ebf3ff);
          --_oxcaml-editor-gutter: var(--oxcaml-editor-gutter, #0c1118);
          --_oxcaml-editor-gutter-ink: var(--oxcaml-editor-gutter-ink, #78879c);
          --_oxcaml-editor-gutter-border: var(--oxcaml-editor-gutter-border, rgba(255, 255, 255, 0.08));
          --_oxcaml-editor-active: var(--oxcaml-editor-active, rgba(255, 255, 255, 0.06));
          --_oxcaml-editor-selection: var(--oxcaml-editor-selection, rgba(110, 169, 255, 0.34));
          --_oxcaml-editor-cursor: var(--oxcaml-editor-cursor, #ffd28f);
          --_oxcaml-output-bg: var(--oxcaml-output-bg, #141c25);
          --_oxcaml-output-border: var(--oxcaml-output-border, rgba(215, 226, 240, 0.14));
          --_oxcaml-accent: var(--oxcaml-accent, #ff9f7a);
          --_oxcaml-ok: var(--oxcaml-ok, #7fd6c2);
          --_oxcaml-warn: var(--oxcaml-warn, #f0bd64);
          --_oxcaml-error: var(--oxcaml-error, #ff7f68);
          --_oxcaml-tooltip-bg: var(--oxcaml-tooltip-bg, #202b37);
          --_oxcaml-tooltip-ink: var(--oxcaml-tooltip-ink, #eef4fb);
          --_oxcaml-stream: var(--oxcaml-stream, #dce6f2);
          --_oxcaml-code: var(--oxcaml-code, #d6e1ef);
          --_oxcaml-detail: var(--oxcaml-detail, #a9b7c8);
          --_oxcaml-prefix: var(--oxcaml-prefix, #8e9caf);
          --_oxcaml-interface-bg: var(--oxcaml-interface-bg, rgba(35, 70, 62, 0.46));
          --_oxcaml-interface-ink: var(--oxcaml-interface-ink, #d8f5ea);
          --_syntax-keyword: var(--syntax-keyword, #ffb57e);
          --_syntax-module: var(--syntax-module, #8ed2ff);
          --_syntax-string: var(--syntax-string, #b6f09c);
          --_syntax-comment: var(--syntax-comment, #dce6f2);
          --_syntax-number: var(--syntax-number, #f7cd74);
          --_syntax-operator: var(--syntax-operator, #f0f5ff);
          --_syntax-function: var(--syntax-function, #9fd8ff);
          --_syntax-parameter: var(--syntax-parameter, #ffd7a1);
          --_syntax-type: var(--syntax-type, #88c6ff);
          --_syntax-constructor: var(--syntax-constructor, #f2c572);
          --_syntax-annotation: var(--syntax-annotation, #ffd28f);
          --_syntax-package: var(--syntax-package, #7fd6c2);
          --_syntax-package-open: var(--syntax-package-open, #a8bcff);
          --_syntax-label: var(--syntax-label, #f4b5ff);
        }
      }
    }

    @container oxcaml-embed style(--_oxcaml-color-scheme: dark) {
      .oxcaml-embed__surface {
        color-scheme: dark;
        --_oxcaml-bg: var(--oxcaml-bg, #17202a);
        --_oxcaml-ink: var(--oxcaml-ink, #eef4fb);
        --_oxcaml-muted: var(--oxcaml-muted, #9aa7b8);
        --_oxcaml-border: var(--oxcaml-border, rgba(215, 226, 240, 0.16));
        --_oxcaml-editor: var(--oxcaml-editor, #10151d);
        --_oxcaml-editor-ink: var(--oxcaml-editor-ink, #ebf3ff);
        --_oxcaml-editor-gutter: var(--oxcaml-editor-gutter, #0c1118);
        --_oxcaml-editor-gutter-ink: var(--oxcaml-editor-gutter-ink, #78879c);
        --_oxcaml-editor-gutter-border: var(--oxcaml-editor-gutter-border, rgba(255, 255, 255, 0.08));
        --_oxcaml-editor-active: var(--oxcaml-editor-active, rgba(255, 255, 255, 0.06));
        --_oxcaml-editor-selection: var(--oxcaml-editor-selection, rgba(110, 169, 255, 0.34));
        --_oxcaml-editor-cursor: var(--oxcaml-editor-cursor, #ffd28f);
        --_oxcaml-output-bg: var(--oxcaml-output-bg, #141c25);
        --_oxcaml-output-border: var(--oxcaml-output-border, rgba(215, 226, 240, 0.14));
        --_oxcaml-accent: var(--oxcaml-accent, #ff9f7a);
        --_oxcaml-ok: var(--oxcaml-ok, #7fd6c2);
        --_oxcaml-warn: var(--oxcaml-warn, #f0bd64);
        --_oxcaml-error: var(--oxcaml-error, #ff7f68);
        --_oxcaml-tooltip-bg: var(--oxcaml-tooltip-bg, #202b37);
        --_oxcaml-tooltip-ink: var(--oxcaml-tooltip-ink, #eef4fb);
        --_oxcaml-stream: var(--oxcaml-stream, #dce6f2);
        --_oxcaml-code: var(--oxcaml-code, #d6e1ef);
        --_oxcaml-detail: var(--oxcaml-detail, #a9b7c8);
        --_oxcaml-prefix: var(--oxcaml-prefix, #8e9caf);
        --_oxcaml-interface-bg: var(--oxcaml-interface-bg, rgba(35, 70, 62, 0.46));
        --_oxcaml-interface-ink: var(--oxcaml-interface-ink, #d8f5ea);
        --_syntax-keyword: var(--syntax-keyword, #ffb57e);
        --_syntax-module: var(--syntax-module, #8ed2ff);
        --_syntax-string: var(--syntax-string, #b6f09c);
        --_syntax-comment: var(--syntax-comment, #dce6f2);
        --_syntax-number: var(--syntax-number, #f7cd74);
        --_syntax-operator: var(--syntax-operator, #f0f5ff);
        --_syntax-function: var(--syntax-function, #9fd8ff);
        --_syntax-parameter: var(--syntax-parameter, #ffd7a1);
        --_syntax-type: var(--syntax-type, #88c6ff);
        --_syntax-constructor: var(--syntax-constructor, #f2c572);
        --_syntax-annotation: var(--syntax-annotation, #ffd28f);
        --_syntax-package: var(--syntax-package, #7fd6c2);
        --_syntax-package-open: var(--syntax-package-open, #a8bcff);
        --_syntax-label: var(--syntax-label, #f4b5ff);
      }
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
      color: var(--_oxcaml-muted);
      z-index: 1;
      white-space: nowrap;
    }

    .oxcaml-embed__controls {
      position: absolute;
      right: 0.58rem;
      top: 0.54rem;
      display: flex;
      gap: 0.26rem;
      align-items: center;
      z-index: 5;
    }

    .oxcaml-embed__run,
    .oxcaml-embed__reset {
      min-height: 1.34rem;
      border: 1px solid rgba(255, 255, 255, 0.15);
      border-radius: 4px;
      background: rgba(18, 22, 29, 0.78);
      color: #d8e3f2;
      cursor: pointer;
      font: 650 0.61rem/1 var(--_oxcaml-font-family);
      padding: 0.2rem 0.34rem;
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.16);
      backdrop-filter: blur(8px);
      white-space: nowrap;
    }

    .oxcaml-embed__run {
      position: relative;
      background: var(--_oxcaml-accent);
      border-color: color-mix(in srgb, var(--_oxcaml-accent), #000 16%);
      color: #fff;
    }

    .oxcaml-embed__run::after {
      content: attr(data-tooltip);
      position: absolute;
      right: 0;
      top: calc(100% + 0.32rem);
      z-index: 20;
      min-width: max-content;
      max-width: 14rem;
      border: 1px solid var(--_oxcaml-border);
      border-radius: 5px;
      background: var(--_oxcaml-tooltip-bg);
      color: var(--_oxcaml-tooltip-ink);
      box-shadow: 0 8px 24px rgba(19, 31, 47, 0.16);
      font: 650 0.66rem/1.25 var(--_oxcaml-font-family);
      opacity: 0;
      padding: 0.28rem 0.42rem;
      pointer-events: none;
      text-align: center;
      white-space: nowrap;
    }

    .oxcaml-embed__run:hover::after,
    .oxcaml-embed__run:focus-visible::after {
      opacity: 1;
    }

    .oxcaml-embed__run:hover,
    .oxcaml-embed__run:focus-visible,
    .oxcaml-embed__reset:hover,
    .oxcaml-embed__reset:focus-visible {
      background: rgba(28, 35, 47, 0.94);
      border-color: rgba(255, 181, 126, 0.48);
      color: #ffcfb4;
      outline: none;
    }

    .oxcaml-embed__run[hidden],
    .oxcaml-embed__reset[hidden] {
      display: none;
    }

    .oxcaml-embed[data-run-trigger="manual"] .cm-content,
    .oxcaml-embed[data-run-trigger="manual-after-initial"] .cm-content {
      padding-right: 13rem;
    }

    .oxcaml-embed[data-state="running"] .oxcaml-embed__status,
    .oxcaml-embed[data-state="loading"] .oxcaml-embed__status {
      color: var(--_oxcaml-accent);
    }

    .oxcaml-embed[data-state="ready"] .oxcaml-embed__status {
      color: var(--_oxcaml-ok);
    }

    .oxcaml-embed[data-state="warning"] .oxcaml-embed__status {
      color: var(--_oxcaml-warn);
    }

    .oxcaml-embed[data-state="error"] .oxcaml-embed__status {
      color: var(--_oxcaml-error);
    }

    .oxcaml-embed__editor-host {
      position: relative;
      background: var(--_oxcaml-editor);
    }

    .oxcaml-embed__editor-host .cm-editor {
      min-height: var(--_oxcaml-editor-min-height);
      background: var(--_oxcaml-editor);
      color: var(--_oxcaml-editor-ink);
      font: var(--_oxcaml-editor-font-size)/1.55 var(--_oxcaml-mono-font-family);
    }

    .oxcaml-embed__editor-host .cm-scroller {
      min-height: var(--_oxcaml-editor-min-height);
      max-height: var(--_oxcaml-editor-max-height);
      overflow: auto;
      line-height: 1.55;
    }

    .oxcaml-embed__editor-host .cm-gutters {
      background: var(--_oxcaml-editor-gutter);
      border-right: 1px solid var(--_oxcaml-editor-gutter-border);
      color: var(--_oxcaml-editor-gutter-ink);
    }

    .oxcaml-embed__editor-host .cm-gutterElement {
      min-width: 2.4rem;
      padding: 0 0.65rem 0 0.45rem;
    }

    .oxcaml-embed__editor-host .cm-content {
      caret-color: var(--_oxcaml-editor-cursor);
      padding: 0.85rem 5.9rem 0.85rem 0.95rem;
    }

    .oxcaml-embed__editor-host .cm-line {
      padding: 0 0.3rem;
    }

    .oxcaml-embed__editor-host .cm-activeLine,
    .oxcaml-embed__editor-host .cm-activeLineGutter {
      background: var(--_oxcaml-editor-active);
    }

    .oxcaml-embed__editor-host .cm-selectionBackground {
      background: var(--_oxcaml-editor-selection) !important;
    }

    .oxcaml-embed__editor-host .cm-cursor,
    .oxcaml-embed__editor-host .cm-dropCursor {
      border-left-color: var(--_oxcaml-editor-cursor);
    }

    .tok-keyword { color: var(--_syntax-keyword); }
    .tok-module { color: var(--_syntax-module); }
    .tok-string { color: var(--_syntax-string); }
    .tok-comment { color: var(--_syntax-comment); font-style: italic; font-weight: 650; }
    .tok-number { color: var(--_syntax-number); }
    .tok-operator { color: var(--_syntax-operator); }
    .tok-function { color: var(--_syntax-function); }
    .tok-parameter { color: var(--_syntax-parameter); }
    .tok-type { color: var(--_syntax-type); }
    .tok-constructor { color: var(--_syntax-constructor); }
    .tok-oxcaml { color: var(--_oxcaml-accent); font-weight: 700; }
    .tok-annotation { color: var(--_syntax-annotation); font-style: italic; }
    .tok-package { color: var(--_syntax-package); font-weight: 700; }
    .tok-package-open { color: var(--_syntax-package-open); }
    .tok-label { color: var(--_syntax-label); }

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
      border-radius: var(--_oxcaml-radius);
      background: var(--_oxcaml-tooltip-bg);
      color: var(--_oxcaml-tooltip-ink);
      box-shadow: 0 14px 32px rgba(16, 24, 35, 0.18);
      font: 0.82rem/1.45 var(--_oxcaml-mono-font-family);
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
      min-height: var(--_oxcaml-output-min-height);
      margin: 0;
      overflow: auto;
      border-top: 1px solid var(--_oxcaml-output-border);
      background: var(--_oxcaml-output-bg);
      color: var(--_oxcaml-ink);
    }

    .oxcaml-embed[data-busy="true"] .oxcaml-embed__output {
      opacity: 0.72;
    }

    .transcript {
      margin: 0;
      padding: 0.75rem 5.5rem 0.75rem 0.95rem;
      font: var(--_oxcaml-output-font-size)/1.48 var(--_oxcaml-mono-font-family);
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
      color: var(--_oxcaml-stream);
    }

    .transcript-line.placeholder {
      color: var(--_oxcaml-muted);
    }

    .html-output {
      margin: 0.75rem 0;
      border: 1px solid var(--_oxcaml-output-border);
      border-radius: var(--_oxcaml-radius);
      background: #fff;
      overflow: hidden;
    }

    .html-output__frame {
      display: block;
      width: 100%;
      height: var(--_oxcaml-html-output-height);
      border: 0;
      background: #fff;
    }

    .transcript-line.file {
      color: #49627d;
      font-weight: 650;
    }

    .transcript-line.warning {
      color: var(--_oxcaml-warn);
      font-weight: 700;
    }

    .transcript-line.error,
    .transcript-line.exception {
      color: var(--_oxcaml-error);
      font-weight: 700;
    }

    .transcript-line.hint {
      color: var(--_syntax-label);
    }

    .transcript-line.caret {
      color: var(--_oxcaml-error);
      font-weight: 700;
    }

    .transcript-line.code {
      color: var(--_oxcaml-code);
    }

    .diagnostic-code-prefix {
      color: var(--_oxcaml-prefix);
    }

    .transcript-line.detail,
    .transcript-line.trace {
      color: var(--_oxcaml-detail);
    }

    .utop-outcome__keyword {
      color: var(--_syntax-keyword);
      font-weight: 750;
    }

    .utop-outcome__name {
      color: var(--_syntax-module);
      font-weight: 700;
    }

    .utop-outcome__punctuation,
    .utop-outcome__equals {
      color: var(--_oxcaml-prefix);
    }

    .utop-outcome__type {
      color: var(--_oxcaml-ok);
    }

    .utop-outcome__value {
      color: var(--_oxcaml-code);
    }

    .utop-stdout {
      color: var(--_oxcaml-detail);
    }

    .interface-output {
      margin: 0 5.5rem 0.85rem 0.95rem;
      border: 1px solid rgba(15, 123, 95, 0.18);
      border-left: 4px solid var(--_oxcaml-ok);
      border-radius: 7px;
      background: var(--_oxcaml-interface-bg);
      padding: 0.68rem 0.8rem 0.75rem;
    }

    .interface-output__label {
      margin-bottom: 0.45rem;
      color: var(--_oxcaml-ok);
      font: 700 0.68rem/1 var(--_oxcaml-font-family);
      letter-spacing: 0.12em;
      text-transform: uppercase;
    }

    .interface-output__body {
      margin: 0;
      color: var(--_oxcaml-interface-ink);
      font: 0.84rem/1.48 var(--_oxcaml-mono-font-family);
      white-space: pre-wrap;
    }

    .oxcaml-embed__output .tok-keyword { color: var(--_syntax-keyword); font-weight: 650; }
    .oxcaml-embed__output .tok-module { color: var(--_syntax-module); }
    .oxcaml-embed__output .tok-string { color: var(--_syntax-string); }
    .oxcaml-embed__output .tok-comment { color: var(--_syntax-comment); font-style: italic; font-weight: 650; }
    .oxcaml-embed__output .tok-number { color: var(--_syntax-number); }
    .oxcaml-embed__output .tok-operator { color: var(--_syntax-operator); }
    .oxcaml-embed__output .tok-function { color: var(--_syntax-function); }
    .oxcaml-embed__output .tok-parameter { color: var(--_syntax-parameter); }
    .oxcaml-embed__output .tok-type { color: var(--_syntax-type); }
    .oxcaml-embed__output .tok-constructor { color: var(--_syntax-constructor); }
    .oxcaml-embed__output .tok-oxcaml { color: var(--_oxcaml-accent); font-weight: 700; }
    .oxcaml-embed__output .tok-annotation { color: var(--_syntax-annotation); font-style: italic; }
    .oxcaml-embed__output .tok-package { color: var(--_syntax-package); font-weight: 700; }
    .oxcaml-embed__output .tok-package-open { color: var(--_syntax-package-open); }
    .oxcaml-embed__output .tok-label { color: var(--_syntax-label); }
  `;
  document.head.appendChild(style);
}

function setStatus(editor, state, text) {
  editor.root.dataset.state = state;
  editor.statusEl.textContent = text;
}

function setOutputBusy(editor, isBusy) {
  editor.root.dataset.busy = isBusy ? "true" : "false";
  if (isBusy) {
    editor.outputEl.hidden = false;
  }
}

function renderTranscript(editor, text, options) {
  const transcript = buildTranscript(editor, text, options);
  const interfaceHtml =
    options?.interfaceText === undefined ? "" : buildInterfaceHtml(options.interfaceText);
  setOutputBusy(editor, false);
  editor.transcriptEl.innerHTML = transcript.html + interfaceHtml;
  editor.outputEl.hidden =
    editor.emptyOutput === "hide" && transcript.isEmpty && interfaceHtml === "";
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
  editor.queuedRevision = null;
}

function markManualRunPending(editor, { initial = false } = {}) {
  clearPendingWork(editor);
  setOutputBusy(editor, false);
  setStatus(editor, "idle", initial ? "idle" : "edited");
  if (initial) {
    editor.transcriptEl.innerHTML = "";
    editor.outputEl.hidden = true;
  }
}

function requestImmediateRun(editor) {
  clearPendingWork(editor);
  editor.revision += 1;
  const revision = editor.revision;
  setOutputBusy(editor, true);
  setStatus(editor, "running", "running");
  void requestEditorRun(editor, revision);
}

function resetEditor(editor) {
  removeStoredSource(editor.storageKey);
  clearPendingWork(editor);
  editor.markers = [];
  editor.revision += 1;
  replaceEditorSource(editor, editor.originalSource);
  updateResetState(editor);
  if (runTriggerIsManual(editor)) {
    markManualRunPending(editor, { initial: true });
  } else {
    scheduleRun(editor);
  }
}

function scheduleRun(editor) {
  clearPendingWork(editor);
  editor.revision += 1;
  const revision = editor.revision;
  setOutputBusy(editor, true);
  setStatus(editor, "running", "running");
  editor.pendingTimer = window.setTimeout(() => {
    editor.pendingTimer = null;
    void requestEditorRun(editor, revision);
  }, autoRunDelayMs);
}

async function requestEditorRun(editor, revision) {
  if (editor.running) {
    editor.queuedRevision = revision;
    return;
  }
  const run = () => runEditor(editor, revision);
  const queuedRun = editorRunQueue.then(run, run);
  editorRunQueue = queuedRun.catch(() => {});
  await queuedRun;
}

async function runEditor(editor, revision = editor.revision) {
  if (revision !== editor.revision) {
    return;
  }
  editor.running = true;
  try {
    setStatus(editor, "running", "running");
    const source = sourceText(editor);
    const preflightDiagnostic = sourcePreflightDiagnostic(editor.filename, source);
    if (preflightDiagnostic) {
      updateEditorMarkers(editor, preflightDiagnostic);
      const transcript = renderTranscript(editor, preflightDiagnostic, {
        forceDiagnostics: true,
      });
      setStatus(editor, transcript.hasWarning ? "warning" : "error", "error");
      return;
    }
    await ready;
    if (revision !== editor.revision) {
      return;
    }
    let output;
    if (editor.mode === "utop") {
      const checkOutput = await checkString(editor.filename, source);
      if (revision !== editor.revision) {
        return;
      }
      if (checkOutput.trim() !== "") {
        updateEditorMarkers(editor, checkOutput);
        const transcript = renderTranscript(editor, checkOutput, {
          forceDiagnostics: true,
          utopMode: true,
        });
        setStatus(editor, transcript.hasWarning ? "warning" : "error", "error");
        return;
      }
      output = await utopString(editor.filename, source);
    } else if (editor.mode === "check") {
      output = await checkString(editor.filename, source);
    } else {
      output = await runString(editor.filename, source);
    }
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
      editor.mode !== "check" &&
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
  } finally {
    editor.running = false;
    if (editor.queuedRevision !== null) {
      const nextRevision = editor.queuedRevision;
      editor.queuedRevision = null;
      window.setTimeout(() => {
        void requestEditorRun(editor, nextRevision);
      }, 0);
    }
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
        keymap.of([
          {
            key: "Mod-Enter",
            run() {
              if (!runTriggerIsManual(editor)) {
                return false;
              }
              requestImmediateRun(editor);
              return true;
            },
          },
          indentWithTab,
          ...defaultKeymap,
          ...historyKeymap,
        ]),
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
          if (runTriggerIsManual(editor)) {
            markManualRunPending(editor);
          } else {
            scheduleRun(editor);
          }
        }),
      ],
    }),
  });
}

function copyPresentationAttributes(source, root) {
  const originalClass = source.getAttribute("class");
  if (originalClass) {
    root.className = `oxcaml-embed ${originalClass}`;
  }
  const id = source.getAttribute("id");
  if (id) {
    root.id = id;
  }
  const style = source.getAttribute("style");
  if (style) {
    root.setAttribute("style", style);
  }
  for (const attribute of source.attributes) {
    const name = attribute.name.toLowerCase();
    if (name === "data-oxcaml-theme") {
      continue;
    }
    if (
      name.startsWith("data-") ||
      name.startsWith("aria-") ||
      name === "title"
    ) {
      root.setAttribute(attribute.name, attribute.value);
    }
  }
}

function createEditorElement() {
  const root = document.createElement("div");
  root.className = "oxcaml-embed";
  root.dataset.state = "loading";
  const surfaceEl = document.createElement("div");
  surfaceEl.className = "oxcaml-embed__surface";

  const statusEl = document.createElement("div");
  statusEl.className = "oxcaml-embed__status";
  statusEl.textContent = "loading";

  const controlsEl = document.createElement("div");
  controlsEl.className = "oxcaml-embed__controls";

  const runButtonEl = document.createElement("button");
  runButtonEl.type = "button";
  runButtonEl.className = "oxcaml-embed__run";
  runButtonEl.hidden = true;

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

  controlsEl.append(resetButtonEl, runButtonEl);
  editorHostEl.append(controlsEl);
  outputEl.append(transcriptEl, statusEl);
  surfaceEl.append(editorHostEl, outputEl);
  root.append(surfaceEl);

  return {
    root,
    surfaceEl,
    controlsEl,
    runButtonEl,
    statusEl,
    resetButtonEl,
    editorHostEl,
    outputEl,
    transcriptEl,
  };
}

function mountEditor(element, options, { copyAttributes, placeRoot }) {
  injectStyles();
  const originalSource = options.source === undefined
    ? dedent(element.textContent ?? "")
    : String(options.source);
  const storageKey =
    options.storageKey ??
    storageKeyForSource(originalSource, options.duplicateIndex ?? 0);
  const source = readStoredSource(storageKey) ?? originalSource;
  const explicitFilename =
    options.filename ??
    element.getAttribute("data-filename");
  const filename = explicitFilename ?? `snippet_${mountedEditors.size + 1}.ml`;
  const mode = modeForElement(element, options);
  const runTrigger = runTriggerForElement(element, options);
  const emptyOutput = emptyOutputForElement(element, options);
  const elements = createEditorElement();
  if (copyAttributes) {
    copyPresentationAttributes(element, elements.root);
  }
  const editor = {
    ...elements,
    emptyOutput,
    filename,
    markers: [],
    mode,
    originalSource,
    pendingTimer: null,
    queuedRevision: null,
    running: false,
    runTrigger,
    revision: 0,
    storageKey,
    suppressEditorChanges: false,
    view: null,
  };
  editor.root.dataset.runTrigger = runTrigger;
  editor.runButtonEl.textContent = runButtonText(mode);
  editor.runButtonEl.dataset.tooltip = runButtonTooltip(mode);
  editor.runButtonEl.setAttribute(
    "aria-label",
    `${runActionLabel(mode)} source (${runShortcutLabel()})`,
  );
  editor.runButtonEl.hidden = !runTriggerIsManual(editor);

  placeRoot(editor.root);
  editor.view = createEditorView(editor, source);
  mountedEditors.add(editor);
  scheduleSyntaxRefresh(editor);
  updateResetState(editor);

  editor.resetButtonEl.addEventListener("click", () => {
    resetEditor(editor);
    editor.view?.focus();
  });

  editor.runButtonEl.addEventListener("click", () => {
    requestImmediateRun(editor);
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

  if (editor.runTrigger === "manual") {
    markManualRunPending(editor, { initial: true });
  } else {
    scheduleRun(editor);
  }
  return editor;
}

export function mount(target, options = {}) {
  if (!(target instanceof Element)) {
    throw new TypeError("mount target must be a DOM element");
  }
  const isOxcamlTag = target.tagName.toLowerCase() === "oxcaml";
  if (!isOxcamlTag && options.source === undefined) {
    throw new TypeError("mount requires options.source when target is not an <oxcaml> tag");
  }
  return mountEditor(target, options, {
    copyAttributes: isOxcamlTag,
    placeRoot(root) {
      if (isOxcamlTag) {
        target.replaceWith(root);
      } else {
        target.replaceChildren(root);
      }
    },
  });
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
