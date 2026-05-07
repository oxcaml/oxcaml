const statusListeners = new Set();
const pendingRequests = new Map();

let nextRequestId = 1;
let backendWorker = null;
let directBackendPromise = null;
let workerFailed = false;

const backendWorkerUrl = new URL(
  "./backend_worker.js?v=20260507-webkit-worker-fallback",
  import.meta.url,
).href;

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

function rejectPendingRequests(error) {
  for (const { reject } of pendingRequests.values()) {
    reject(error);
  }
  pendingRequests.clear();
}

function retryPendingRequestsDirect() {
  for (const { resolve, reject, methodName, args } of pendingRequests.values()) {
    callDirect(methodName, args).then(resolve, reject);
  }
  pendingRequests.clear();
}

function workerSupported() {
  return !workerFailed && typeof Worker === "function" && typeof URL === "function";
}

function createBlobWorker(workerUrl) {
  if (
    typeof Blob !== "function" ||
    typeof URL.createObjectURL !== "function" ||
    typeof URL.revokeObjectURL !== "function"
  ) {
    return null;
  }
  const source = `
const queuedMessages = [];
function queueMessage(event) {
  queuedMessages.push(event);
}
globalThis.onmessage = queueMessage;
import(${JSON.stringify(workerUrl)}).then(() => {
  const handler = globalThis.onmessage;
  if (handler === queueMessage || typeof handler !== "function") {
    throw new Error("OxCaml backend worker did not install a message handler");
  }
  for (const event of queuedMessages) {
    handler.call(globalThis, event);
  }
}).catch((error) => {
  globalThis.postMessage({
    type: "worker_boot_error",
    error: error?.stack || error?.message || String(error),
  });
});
`;
  const blobUrl = URL.createObjectURL(
    new Blob([source], { type: "application/javascript" }),
  );
  try {
    return new Worker(blobUrl);
  } finally {
    globalThis.setTimeout(() => {
      URL.revokeObjectURL(blobUrl);
    }, 0);
  }
}

function getBackendWorker() {
  if (backendWorker || !workerSupported()) {
    return backendWorker;
  }
  try {
    backendWorker = new Worker(backendWorkerUrl);
  } catch (error) {
    backendWorker = createBlobWorker(backendWorkerUrl);
    if (!backendWorker) {
      workerFailed = true;
      backendWorker = null;
      return null;
    }
  }
  backendWorker.onmessage = ({ data }) => {
    if (!data || typeof data !== "object") {
      return;
    }
    if (data.type === "worker_boot_error") {
      workerFailed = true;
      backendWorker?.terminate();
      backendWorker = null;
      retryPendingRequestsDirect();
      return;
    }
    if (data.type === "status") {
      emitStatus(data.state, data.text);
      return;
    }
    if (data.type !== "result") {
      return;
    }
    const pending = pendingRequests.get(data.id);
    if (!pending) {
      return;
    }
    if (!data.ok && pending.methodName === "ready") {
      workerFailed = true;
      backendWorker?.terminate();
      backendWorker = null;
      retryPendingRequestsDirect();
      return;
    }
    pendingRequests.delete(data.id);
    if (data.ok) {
      pending.resolve(data.value);
    } else {
      pending.reject(new Error(data.error || "OxCaml worker request failed"));
    }
  };
  backendWorker.onerror = (event) => {
    workerFailed = true;
    backendWorker?.terminate();
    backendWorker = null;
    retryPendingRequestsDirect();
  };
  backendWorker.onmessageerror = () => {
    workerFailed = true;
    backendWorker?.terminate();
    backendWorker = null;
    retryPendingRequestsDirect();
  };
  return backendWorker;
}

async function directBackend() {
  if (!directBackendPromise) {
    directBackendPromise = import("./backend_direct.js?v=20260507-webkit-worker-fallback");
  }
  return directBackendPromise;
}

async function callDirect(methodName, args) {
  const backend = await directBackend();
  if (methodName === "ready") {
    await backend.ready;
    return true;
  }
  return backend[methodName](...args);
}

function callWorker(methodName, args) {
  const worker = getBackendWorker();
  if (!worker) {
    return callDirect(methodName, args);
  }
  const id = nextRequestId;
  nextRequestId += 1;
  return new Promise((resolve, reject) => {
    pendingRequests.set(id, { resolve, reject, methodName, args });
    worker.postMessage({ id, methodName, args });
  });
}

export async function readyForOptions(options = {}) {
  return callWorker("ready", []);
}

export const ready = {
  then(resolve, reject) {
    return readyForOptions().then(resolve, reject);
  },
  catch(reject) {
    return readyForOptions().catch(reject);
  },
  finally(onFinally) {
    return readyForOptions().finally(onFinally);
  },
};

export async function checkString(filename, source, options = {}) {
  return callWorker("checkString", [filename, source]);
}

export async function interfaceString(filename, source, options = {}) {
  return callWorker("interfaceString", [filename, source]);
}

export async function runString(filename, source, options = {}) {
  return callWorker("runString", [filename, source]);
}

export async function utopString(filename, source, options = {}) {
  return callWorker("utopString", [filename, source]);
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
    readyForOptions,
    runString,
    utopString,
    checkFile,
    runFile,
  };
}
