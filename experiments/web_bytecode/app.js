import {
  defaultSample,
  getSampleById,
  getVisibleSamplesByTopic,
} from "./sample_catalog.js";

const autoCheckDelayMs = 280;
const autoRunDelayMs = 420;
const buildBase = "../../_build/default/experiments/web_bytecode";

const keywordSet = new Set([
  "and", "as", "assert", "begin", "class", "constraint", "do", "done",
  "downto", "else", "end", "exception", "external", "false", "for",
  "fun", "function", "functor", "if", "in", "include", "inherit",
  "initializer", "lazy", "let", "match", "method", "module", "mutable",
  "new", "nonrec", "object", "of", "open", "or", "private", "rec",
  "sig", "struct", "then", "to", "true", "try", "type", "val",
  "virtual", "when", "while", "with",
]);

const sourceEl = document.getElementById("source");
const highlightEl = document.getElementById("highlight");
const outputEl = document.getElementById("output");
const outputLabelEl = document.getElementById("output-label");
const outputPanelEl = document.getElementById("output-panel");
const statusEl = document.getElementById("status");
const statusTextEl = statusEl?.querySelector(".status-text") ?? null;
const samplePickerEl = document.getElementById("sample-picker");
const fullUi = Boolean(
  sourceEl &&
  highlightEl &&
  outputLabelEl &&
  outputPanelEl &&
  samplePickerEl,
);

let currentFilename = "snippet.ml";
let currentSampleId = null;
let pendingCheckTimer = null;
let pendingRunTimer = null;
let currentRevision = 0;
let lastCompletedCheck = {
  revision: -1,
  result: { diagnostics: "", hasError: false, hasWarning: false },
};
let runRevision = -1;
let editorMarkers = [];
const loadedScriptUrls = new Map();
let browserFsPromise = null;

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
}

async function ensureBrowserFsLoaded() {
  if (browserFsPromise) {
    return browserFsPromise;
  }
  browserFsPromise = (async () => {
    const manifest = await fetchJson(buildAssetUrl("browser_fs_manifest.json"));
    if (typeof globalThis.jsoo_create_file !== "function") {
      throw new Error("js_of_ocaml filesystem initializer is not ready");
    }
    let nextIndex = 0;
    const concurrency = Math.min(6, manifest.length || 1);
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
  await loadScript(new URL("./runtime_shims.js", import.meta.url));
  await loadScript(buildAssetUrl("web_bytecode_js.bc.js"));
  installGlobalScriptEvaluator();
  await ensureBrowserFsLoaded();
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

function readIdentifier(source, start) {
  let end = start + 1;
  while (end < source.length && /[A-Za-z0-9_']/.test(source[end])) {
    end += 1;
  }
  return source.slice(start, end);
}

function markRange(target, start, end, value) {
  for (let index = start; index < end; index += 1) {
    target[index] = value;
  }
}

function buildLineStarts(source) {
  const starts = [0];
  for (let index = 0; index < source.length; index += 1) {
    if (source[index] === "\n") {
      starts.push(index + 1);
    }
  }
  return starts;
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
    let severity = "error";
    for (let lookahead = index + 1; lookahead < Math.min(lines.length, index + 8); lookahead += 1) {
      const line = lines[lookahead];
      if (/^File /.test(line)) {
        break;
      }
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
    markers.push({
      line: Math.max(Number.parseInt(lineText, 10) - 1, 0),
      start,
      end,
      severity,
    });
  }
  return markers;
}

function markerRangesToClasses(source, markers) {
  const markerClasses = new Array(source.length).fill("");
  const lineStarts = buildLineStarts(source);
  for (const marker of markers) {
    const lineStart = lineStarts[marker.line];
    if (lineStart === undefined) {
      continue;
    }
    const lineEndWithNewline =
      marker.line + 1 < lineStarts.length ? lineStarts[marker.line + 1] : source.length;
    const lineEnd = source[lineEndWithNewline - 1] === "\n"
      ? lineEndWithNewline - 1
      : lineEndWithNewline;
    const absoluteStart = Math.min(lineStart + marker.start, lineEnd);
    const absoluteEnd = Math.min(Math.max(lineStart + marker.end, absoluteStart + 1), lineEnd);
    if (absoluteStart >= absoluteEnd) {
      continue;
    }
    markRange(
      markerClasses,
      absoluteStart,
      absoluteEnd,
      marker.severity === "warning" ? "diag-warning" : "diag-error",
    );
  }
  return markerClasses;
}

function highlightSource(source) {
  if (!highlightEl) {
    return;
  }
  const syntaxClasses = new Array(source.length).fill("");
  let index = 0;
  while (index < source.length) {
    if (source.startsWith("(*", index)) {
      const close = source.indexOf("*)", index + 2);
      const end = close === -1 ? source.length : close + 2;
      markRange(syntaxClasses, index, end, "tok-comment");
      index = end;
      continue;
    }
    if (source[index] === "\"") {
      let end = index + 1;
      while (end < source.length) {
        if (source[end] === "\\") {
          end += 2;
          continue;
        }
        if (source[end] === "\"") {
          end += 1;
          break;
        }
        end += 1;
      }
      markRange(syntaxClasses, index, end, "tok-string");
      index = end;
      continue;
    }
    if (/[A-Za-z_]/.test(source[index])) {
      const word = readIdentifier(source, index);
      if (keywordSet.has(word)) {
        markRange(syntaxClasses, index, index + word.length, "tok-keyword");
      } else if (/^[A-Z]/.test(word)) {
        markRange(syntaxClasses, index, index + word.length, "tok-module");
      }
      index += word.length;
      continue;
    }
    if (/[0-9]/.test(source[index])) {
      let end = index + 1;
      while (end < source.length && /[0-9_]/.test(source[end])) {
        end += 1;
      }
      markRange(syntaxClasses, index, end, "tok-number");
      index = end;
      continue;
    }
    if (/[:=<>|@.^+\-*/?!$~]/.test(source[index])) {
      syntaxClasses[index] = "tok-operator";
      index += 1;
      continue;
    }
    index += 1;
  }
  const markerClasses = markerRangesToClasses(source, editorMarkers);
  let html = "";
  index = 0;
  while (index < source.length) {
    const syntaxClass = syntaxClasses[index];
    const markerClass = markerClasses[index];
    let end = index + 1;
    while (
      end < source.length &&
      syntaxClasses[end] === syntaxClass &&
      markerClasses[end] === markerClass
    ) {
      end += 1;
    }
    const classes = [];
    if (syntaxClass) {
      classes.push(syntaxClass);
    }
    if (markerClass) {
      classes.push(markerClass);
    }
    const content = escapeHtml(source.slice(index, end));
    html += classes.length > 0
      ? `<span class="${classes.join(" ")}">${content}</span>`
      : content;
    index = end;
  }
  if (source.endsWith("\n")) {
    html += "\n";
  }
  highlightEl.innerHTML = html || " ";
}

function syncEditorScroll() {
  if (!highlightEl || !sourceEl) {
    return;
  }
  highlightEl.scrollTop = sourceEl.scrollTop;
  highlightEl.scrollLeft = sourceEl.scrollLeft;
}

function setStatus(state, text = "") {
  if (!statusEl || !statusTextEl) {
    return;
  }
  statusEl.dataset.state = state;
  statusTextEl.textContent = text;
}

function setOutputState(state, label = "Output", meta = "") {
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
  setOutputState("idle", "Output", "");
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
  };
}

function buildTranscript(text, { emptyPlaceholder = null, forceDiagnostics = false } = {}) {
  const normalized = text.replace(/\r\n/g, "\n");
  if (normalized === "" && emptyPlaceholder !== null) {
    return {
      hasWarning: false,
      hasError: false,
      tone: "output",
      html: `<pre class="transcript"><span class="transcript-line stream placeholder">${escapeHtml(emptyPlaceholder)}</span></pre>`,
    };
  }

  const lines = normalized.replace(/\n$/, "").split("\n");
  let hasWarning = false;
  let hasError = false;
  let inDiagnosticBlock = forceDiagnostics;
  const body = lines
    .map((line) => {
      const info = classifyTranscriptLine(line, inDiagnosticBlock, forceDiagnostics);
      inDiagnosticBlock = info.nextDiagnosticBlock;
      hasWarning ||= info.hasWarning;
      hasError ||= info.hasError;
      return `<span class="transcript-line ${info.cls}">${escapeHtml(line || " ")}\n</span>`;
    })
    .join("");

  return {
    hasWarning,
    hasError,
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
  setOutputState(transcript.tone, "Output", "");
  outputEl.innerHTML = transcript.html;
  return transcript;
}

function updateEditorMarkers(source, diagnostics) {
  editorMarkers = diagnostics ? parseDiagnosticMarkers(diagnostics, currentFilename) : [];
  highlightSource(source);
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
  runRevision = -1;
  editorMarkers = [];
  sourceEl.value = source;
  highlightSource(source);
  syncEditorScroll();
  if (samplePickerEl.value !== (sampleId || "")) {
    samplePickerEl.value = sampleId || "";
  }
  lastCompletedCheck = {
    revision: -1,
    result: { diagnostics: "", hasError: false, hasWarning: false },
  };
  schedulePipeline();
}

async function performCheck(revision, { renderSuccess = false } = {}) {
  const source = sourceEl.value;
  const diagnostics = await checkString(currentFilename, source);
  if (revision !== currentSourceRevision()) {
    return null;
  }
  updateEditorMarkers(source, diagnostics);
  const transcript =
    diagnostics === ""
      ? { hasError: false, hasWarning: false }
      : renderTranscript(diagnostics, { forceDiagnostics: true });
  const result = {
    diagnostics,
    hasError: transcript.hasError,
    hasWarning: transcript.hasWarning,
  };
  lastCompletedCheck = { revision, result };
  if (diagnostics) {
    if (result.hasError) {
      setStatus("error", "type error");
    } else {
      setStatus("warning", "warnings");
    }
  } else if (renderSuccess || runRevision !== revision) {
    setStatus("ready", "ok");
  }
  return result;
}

function clearPendingWork() {
  if (pendingCheckTimer !== null) {
    clearTimeout(pendingCheckTimer);
    pendingCheckTimer = null;
  }
  if (pendingRunTimer !== null) {
    clearTimeout(pendingRunTimer);
    pendingRunTimer = null;
  }
}

function schedulePipeline() {
  clearPendingWork();
  const revision = currentSourceRevision();
  setOutputBusy(true);
  setStatus("checking", "checking");
  pendingCheckTimer = window.setTimeout(async () => {
    pendingCheckTimer = null;
    try {
      await ready;
      const checkResult = await performCheck(revision);
      if (revision !== currentSourceRevision() || checkResult?.hasError) {
        return;
      }
      setOutputBusy(true);
      setStatus("running", "running");
      pendingRunTimer = window.setTimeout(() => {
        pendingRunTimer = null;
        void runCurrentSource({ skipCheck: true, revision });
      }, autoRunDelayMs);
    } catch (error) {
      if (revision !== currentSourceRevision()) {
        return;
      }
      setOutputBusy(false);
      renderTranscript(String(error), { forceDiagnostics: true });
      setStatus("error", "offline");
    }
  }, autoCheckDelayMs);
}

async function ensureFreshCheck() {
  const revision = currentSourceRevision();
  clearPendingWork();
  if (lastCompletedCheck.revision === revision) {
    return lastCompletedCheck.result;
  }
  return performCheck(revision, { renderSuccess: true });
}

async function runCurrentSource({ skipCheck = false, revision = currentSourceRevision() } = {}) {
  try {
    setStatus("running", "running");
    const checkResult = skipCheck
      ? lastCompletedCheck.result
      : await ensureFreshCheck();
    if (checkResult?.hasError) {
      return;
    }
    if (revision !== currentSourceRevision()) {
      return;
    }
    const output = await runString(currentFilename, sourceEl.value);
    if (revision !== currentSourceRevision()) {
      return;
    }
    runRevision = revision;
    updateEditorMarkers(sourceEl.value, output);
    const transcript = renderTranscript(output, { emptyPlaceholder: "(no output)" });
    if (transcript.hasError) {
      setStatus("error", "exception");
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
  sourceEl.addEventListener("input", () => {
    currentRevision += 1;
    runRevision = -1;
    editorMarkers = [];
    highlightSource(sourceEl.value);
    schedulePipeline();
  });

  sourceEl.addEventListener("scroll", syncEditorScroll);

  samplePickerEl.addEventListener("change", () => {
    const sample = getSampleById(samplePickerEl.value);
    if (!sample) {
      return;
    }
    setSource(sample.source, sample.filename, sample.id);
  });

  populateSamples();
  if (defaultSample) {
    setSource(defaultSample.source, defaultSample.filename, defaultSample.id);
  }
  renderEmptyOutput();

  ready.then(
    (_backend) => {
      setStatus("ready", "ok");
    },
    (error) => {
      renderTranscript(String(error), { forceDiagnostics: true });
      setStatus("error", "offline");
    });
}
