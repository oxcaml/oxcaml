const backendPromise = import("./backend_direct.js?v=20260424-multicore-shim").then((backend) => {
  backend.addBackendStatusListener(({ state, text }) => {
    globalThis.postMessage({ type: "status", state, text });
  });
  return {
    async ready() {
      await backend.ready;
      return true;
    },
    checkString: backend.checkString,
    interfaceString: backend.interfaceString,
    runString: backend.runString,
    utopString: backend.utopString,
  };
});

let requestQueue = Promise.resolve();

globalThis.onmessage = ({ data }) => {
  if (!data || typeof data !== "object") {
    return;
  }
  const { id, methodName, args = [] } = data;
  requestQueue = requestQueue
    .catch(() => undefined)
    .then(async () => {
      try {
        const methods = await backendPromise;
        const method = methods[methodName];
        if (typeof method !== "function") {
          throw new Error(`unknown backend method ${String(methodName)}`);
        }
        const value = await method(...args);
        globalThis.postMessage({ type: "result", id, ok: true, value });
      } catch (error) {
        globalThis.postMessage({
          type: "result",
          id,
          ok: false,
          error: error?.stack || error?.message || String(error),
        });
      }
    });
};
