import {
  stripPlaygroundPreludeInterface,
  withPlaygroundPrelude,
} from "./playground_prelude.js";

const buildBase = "./build";
const compilerAssetVersion = "20260803-dox-persistent-cache-7";
const artifactDatabaseName = `dox-compiler-artifacts-${compilerAssetVersion}`;
const artifactStoreName = "artifacts";
const artifactRecordLimit = 160;
const artifactSuffixes = [".cmo", ".cmi", ".dox-cache", ".dox-constructs"];

const loadedScriptUrls = new Map();
let browserFsManifestPromise = null;
let browserFsSeedPromise = null;
const browserFsLoadedPaths = new Set();
const browserFsLoadingPaths = new Map();
const browserFsConcurrency = 4;
const browserFsFetchRetries = 4;
const browserFsRetryLimit = 96;
const browserFsSeedPaths = [
  "/static/cmis/stdlib.cmi",
  "/static/cmis/stdlib__List.cmi",
];
const statusListeners = new Set();
let artifactDatabasePromise = null;

function openArtifactDatabase() {
  if (artifactDatabasePromise) return artifactDatabasePromise;
  if (typeof indexedDB === "undefined") return Promise.resolve(null);
  artifactDatabasePromise = new Promise((resolve) => {
    const request = indexedDB.open(artifactDatabaseName, 1);
    request.onupgradeneeded = () => {
      if (!request.result.objectStoreNames.contains(artifactStoreName)) {
        const store = request.result.createObjectStore(artifactStoreName);
        store.createIndex("savedAt", "savedAt");
      }
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => resolve(null);
    request.onblocked = () => resolve(null);
  });
  return artifactDatabasePromise;
}

function databaseRequest(request) {
  return new Promise((resolve, reject) => {
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

function databaseTransactionDone(transaction) {
  return new Promise((resolve, reject) => {
    transaction.oncomplete = () => resolve();
    transaction.onerror = () => reject(transaction.error);
    transaction.onabort = () => reject(transaction.error);
  });
}

async function sourceDigest(source) {
  const bytes = new TextEncoder().encode(source);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return Array.from(new Uint8Array(digest), (byte) =>
    byte.toString(16).padStart(2, "0")).join("");
}

function unitOutputPrefix(cacheDirectory, filename) {
  const suffix = filename.endsWith(".ml.md")
    ? ".ml.md"
    : filename.includes(".")
      ? filename.slice(filename.lastIndexOf("."))
      : "";
  const basename = suffix ? filename.slice(0, -suffix.length) : filename;
  return `${cacheDirectory}/${basename}`;
}

function artifactKey(filename, digest, suffix) {
  return `${filename}\x1f${digest}\x1f${suffix}`;
}

function artifactContent(record) {
  if (typeof record === "string") return record;
  return typeof record?.content === "string" ? record.content : null;
}

async function trimArtifactDatabase(database) {
  const countTransaction = database.transaction(artifactStoreName, "readonly");
  const count = await databaseRequest(countTransaction.objectStore(artifactStoreName).count());
  if (count <= artifactRecordLimit) return;
  const transaction = database.transaction(artifactStoreName, "readwrite");
  const store = transaction.objectStore(artifactStoreName);
  const index = store.index("savedAt");
  let remaining = count - artifactRecordLimit;
  await new Promise((resolve, reject) => {
    const request = index.openCursor();
    request.onerror = () => reject(request.error);
    request.onsuccess = () => {
      const cursor = request.result;
      if (!cursor || remaining <= 0) {
        resolve();
        return;
      }
      store.delete(cursor.primaryKey);
      remaining -= 1;
      cursor.continue();
    };
  });
  await databaseTransactionDone(transaction);
}

function readRuntimeFile(path) {
  try {
    const runtime = globalThis.jsoo_runtime;
    const mlPath = runtime.caml_string_of_jsbytes(path);
    return runtime.caml_jsbytes_of_string(runtime.caml_read_file_content(mlPath));
  } catch (_error) {
    return null;
  }
}

async function restoreCompilerArtifacts(cacheDirectory, units) {
  const database = await openArtifactDatabase();
  if (!database) return { restoredUnits: 0, restoreMs: 0 };
  const startedAt = performance.now();
  try {
    const unitDigests = await Promise.all(
      units.map(async (unit) => [unit, await sourceDigest(unit.source)]),
    );
    const transaction = database.transaction(artifactStoreName, "readonly");
    const store = transaction.objectStore(artifactStoreName);
    const order = artifactContent(await databaseRequest(store.get("compilation-order")));
    if (order) {
      globalThis.jsoo_create_file(`${cacheDirectory}/order`, order);
    }
    let restoredUnits = 0;
    for (const [unit, digest] of unitDigests) {
      const prefix = unitOutputPrefix(cacheDirectory, unit.filename);
      let required = 0;
      let restored = 0;
      for (const suffix of artifactSuffixes) {
        if (suffix === ".dox-constructs" && !unit.filename.endsWith(".ml.md")) continue;
        required += 1;
        const content = artifactContent(await databaseRequest(
          store.get(artifactKey(unit.filename, digest, suffix)),
        ));
        if (content !== null) {
          globalThis.jsoo_create_file(`${prefix}${suffix}`, content);
          restored += 1;
        }
      }
      if (restored === required) restoredUnits += 1;
    }
    return { restoredUnits, restoreMs: Math.round(performance.now() - startedAt) };
  } catch (_error) {
    return { restoredUnits: 0, restoreMs: Math.round(performance.now() - startedAt) };
  }
}

async function persistCompilerArtifacts(cacheDirectory, units) {
  const database = await openArtifactDatabase();
  if (!database) return { persistedUnits: 0, persistMs: 0 };
  const startedAt = performance.now();
  try {
    const unitDigests = await Promise.all(
      units.map(async (unit) => [unit, await sourceDigest(unit.source)]),
    );
    const transaction = database.transaction(artifactStoreName, "readwrite");
    const store = transaction.objectStore(artifactStoreName);
    const savedAt = Date.now();
    const order = readRuntimeFile(`${cacheDirectory}/order`);
    if (typeof order === "string") {
      store.put({ content: order, savedAt }, "compilation-order");
    }
    let persistedUnits = 0;
    for (const [unit, digest] of unitDigests) {
      const prefix = unitOutputPrefix(cacheDirectory, unit.filename);
      let complete = true;
      for (const suffix of artifactSuffixes) {
        if (suffix === ".dox-constructs" && !unit.filename.endsWith(".ml.md")) continue;
        const content = readRuntimeFile(`${prefix}${suffix}`);
        if (typeof content !== "string") {
          complete = false;
          continue;
        }
        store.put(
          { content, savedAt },
          artifactKey(unit.filename, digest, suffix),
        );
      }
      if (complete) persistedUnits += 1;
    }
    await databaseTransactionDone(transaction);
    await trimArtifactDatabase(database);
    return { persistedUnits, persistMs: Math.round(performance.now() - startedAt) };
  } catch (_error) {
    return { persistedUnits: 0, persistMs: Math.round(performance.now() - startedAt) };
  }
}

export function addBackendStatusListener(listener) {
  statusListeners.add(listener);
  return () => {
    statusListeners.delete(listener);
  };
}

function emitStatus(state, text = "") {
  for (const listener of statusListeners) {
    try {
      listener({ state, text });
    } catch (error) {
      console.warn("OxCaml backend status listener failed", error);
    }
  }
}

function loadScript(url) {
  const href = url instanceof URL ? url.href : String(url);
  const existing = loadedScriptUrls.get(href);
  if (existing) {
    return existing;
  }
  if (typeof document === "undefined") {
    const promise = Promise.resolve().then(() => {
      if (typeof globalThis.importScripts === "function") {
        globalThis.importScripts(href);
        return;
      }
      return fetch(href)
        .then((response) => {
          if (!response.ok) {
            throw new Error(`failed to fetch ${href}`);
          }
          return response.text();
        })
        .then((source) => {
          (0, eval)(`${source}\n//# sourceURL=${href}`);
        });
    });
    loadedScriptUrls.set(href, promise);
    return promise;
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
    (0, eval)(`${String(source)}\n//# sourceURL=${String(label)}`);
  };
}

function buildAssetUrl(path) {
  const url = new URL(`${buildBase}/${path}`, import.meta.url);
  url.searchParams.set("v", compilerAssetVersion);
  return url;
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
        globalThis.setTimeout(resolve, 40 * (attempt + 1));
      });
    }
  }
  throw lastError ?? new Error(`failed to fetch ${url}`);
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
    const seedEntries = [
      ...browserFsSeedPaths
        .map((fsPath) => manifest.entriesByPath.get(fsPath))
        .filter(Boolean),
      ...manifest.manifest.filter((entry) =>
        /^\/static\/cmis\/(?:stdlib|camlinternal).*\.cmi$/i.test(entry.fs_path),
      ),
    ].filter(
      (entry, index, entries) =>
        entries.findIndex((candidate) => candidate.fs_path === entry.fs_path) === index,
    );
    emitStatus("loading", "loading the OCaml standard library");
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
  const prefixes = flattenModulePathPrefixes(trimmed)
    .map((prefix) => `${lowercaseFirst(prefix)}.cmi`);
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
  const effectiveSource =
    methodName === "utopString" ? source : withPlaygroundPrelude(filename, source);
  let previousMissingFilename = null;
  for (let attempt = 0; attempt < browserFsRetryLimit; attempt += 1) {
    const result = await normalizeBackendResult(backend[methodName](filename, effectiveSource));
    if (!result || result.kind === "ok") {
      const output = result?.output ?? "";
      return methodName === "interfaceString"
        ? stripPlaygroundPreludeInterface(output)
        : output;
    }
    if (result.kind !== "missing_cmi" || typeof result.filename !== "string") {
      throw new Error(`unexpected backend result from ${methodName}`);
    }
    if (result.filename === previousMissingFilename) {
      throw new Error(`lazy filesystem stalled while loading ${result.filename}`);
    }
    previousMissingFilename = result.filename;
    emitStatus("loading", `loading ${result.filename}`);
    const loaded = await ensureBrowserFsForMissingFilename(result.filename);
    if (!loaded) {
      throw new Error(`missing browser filesystem asset for ${result.filename}`);
    }
  }
  throw new Error(`lazy filesystem retry limit exceeded for ${filename}`);
}

export const ready = (async () => {
  emitStatus("loading", "loading runtime");
  await loadScript(
    new URL("./runtime_shims.js?v=20260803-dox-persistent-cache-7", import.meta.url),
  );
  emitStatus("loading", "loading compiler");
  await loadScript(buildAssetUrl("web_bytecode_js.bc.js"));
  globalThis.installDoxRuntimePrimitives?.();
  installGlobalScriptEvaluator();
  await ensureBrowserFsSeedLoaded();
  emitStatus("loading", "starting compiler");
  const backend = globalThis.WebBytecodeJs;
  if (
    !backend ||
    typeof backend.checkString !== "function" ||
    typeof backend.runString !== "function" ||
    typeof backend.utopString !== "function"
  ) {
    throw new Error("static OxCaml backend failed to initialize");
  }
  emitStatus("ready", "ready");
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

export async function runDoxProject(request) {
  const backend = await ready;
  if (typeof backend.runDoxProject !== "function") {
    throw new Error("Dox project evaluation is unavailable in this compiler bundle");
  }
  const parsedRequest = typeof request === "string" ? JSON.parse(request) : request;
  const units = Array.isArray(parsedRequest?.units) ? parsedRequest.units : [];
  const encoded = typeof request === "string" ? request : JSON.stringify(request);
  const cacheDirectory = typeof backend.doxCacheDirectory === "function"
    ? backend.doxCacheDirectory()
    : null;
  const restored = cacheDirectory
    ? await restoreCompilerArtifacts(cacheDirectory, units)
    : { restoredUnits: 0, restoreMs: 0 };
  let previousMissingFilename = null;
  for (let attempt = 0; attempt < browserFsRetryLimit; attempt += 1) {
    const result = JSON.parse(backend.runDoxProject(encoded));
    if (result?.kind !== "missing_cmi") {
      const persisted = result?.kind === "ok" && cacheDirectory
        && Number(result?.cache?.compiledUnits || 0) > 0
        ? await persistCompilerArtifacts(cacheDirectory, units)
        : { persistedUnits: 0, persistMs: 0 };
      result.cache = {
        ...(result.cache || {}),
        persistentRestoredUnits: restored.restoredUnits,
        persistentPersistedUnits: persisted.persistedUnits,
      };
      result.timings = {
        ...(result.timings || {}),
        artifactRestoreMs: restored.restoreMs,
        artifactPersistMs: persisted.persistMs,
      };
      return result;
    }
    if (
      typeof result.filename !== "string" ||
      result.filename === previousMissingFilename
    ) {
      throw new Error(`lazy filesystem stalled while loading ${String(result?.filename)}`);
    }
    previousMissingFilename = result.filename;
    emitStatus("loading", `loading ${result.filename}`);
    if (!(await ensureBrowserFsForMissingFilename(result.filename))) {
      throw new Error(`missing browser filesystem asset for ${result.filename}`);
    }
  }
  throw new Error("lazy filesystem retry limit exceeded for Dox project");
}

export async function checkFile(file) {
  const source = await file.text();
  return checkString(file.name, source);
}

export async function runFile(file) {
  const source = await file.text();
  return runString(file.name, source);
}

if (typeof window !== "undefined") {
  window.webBytecode = {
    checkString,
    interfaceString,
    runString,
    runDoxProject,
    utopString,
    checkFile,
    runFile,
  };
}
