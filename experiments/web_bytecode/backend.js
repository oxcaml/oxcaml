const statusListeners = new Set();
const pendingRequests = new Map();

let nextRequestId = 1;
let backendWorker = null;
let directBackendPromise = null;
let workerFailed = false;

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

function getBackendWorker() {
  if (backendWorker || !workerSupported()) {
    return backendWorker;
  }
  backendWorker = new Worker(new URL("./backend_worker.js", import.meta.url));
  backendWorker.onmessage = ({ data }) => {
    if (!data || typeof data !== "object") {
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
    directBackendPromise = import("./backend_direct.js?v=20260424-multicore-shim");
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

export const ready = callWorker("ready", []);

export async function checkString(filename, source) {
  return callWorker("checkString", [filename, source]);
}

export async function interfaceString(filename, source) {
  return callWorker("interfaceString", [filename, source]);
}

export async function runString(filename, source) {
  return callWorker("runString", [filename, source]);
}

export async function utopString(filename, source) {
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
    runString,
    utopString,
    checkFile,
    runFile,
  };
}
