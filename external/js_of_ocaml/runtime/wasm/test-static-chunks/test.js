"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const source = fs.readFileSync(process.argv[2], "utf8");

function deferred() {
  let resolve, reject;
  const promise = new Promise((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
}

async function fixture(chunks, instantiate, enableJspi = false, eager = {}) {
  const events = [];
  const promisingCalls = [];
  const shared = new WebAssembly.Global(
    { value: "externref", mutable: true },
    { registrations: 0 },
  );
  const runtimeGlobal = new WebAssembly.Global({ value: "i32" }, 17);
  const mainFragments = { main: () => 1 };
  let load, sharedImports, sharedOptions;
  const runtime = vm.runInNewContext(source, {
    TextDecoder,
    TextEncoder,
    WeakRef,
    FinalizationRegistry,
    URL,
    document: { currentScript: { src: "https://example.invalid/app/loader.js" } },
    fetch: async (url) => {
      const name = new URL(url).pathname.split("/").pop();
      events.push("fetch:" + name);
      if (name === "fetch-failure.wasm") throw new Error("network failure");
      return { name };
    },
    WebAssembly: {
      Tag: WebAssembly.Tag,
      JSTag: WebAssembly.JSTag,
      promising: (f) => {
        promisingCalls.push(f);
        return (...args) => Promise.resolve().then(() => f(...args));
      },
      instantiateStreaming: async (response, imports, options) => {
        const { name } = await response;
        events.push("instantiate:" + name);
        if (sharedImports) {
          assert.equal(imports, sharedImports);
          assert.equal(options, sharedOptions);
        } else {
          sharedImports = imports;
          sharedOptions = options;
          assert.deepEqual(Array.from(options.builtins), [
            "js-string", "text-decoder", "text-encoder",
          ]);
          assert.equal(options.importedStringConstants, "str");
        }
        if (name === "runtime.wasm")
          return { instance: { exports: { runtimeGlobal } } };
        if (name !== "merged.wasm") assert.equal(imports.env.runtimeGlobal, runtimeGlobal);
        if (name === "main.wasm" || name === "merged.wasm") {
          assert.equal(imports["main.fragments"], mainFragments);
          return {
            instance: {
              exports: {
                ...(name === "merged.wasm" ? { runtimeGlobal } : {}),
                Shared: shared,
                _initialize: () => {
                  assert.equal(typeof load, "function");
                  events.push("initialize:main");
                  return eager.initialize?.();
                },
              },
            },
          };
        }
        assert.equal(imports.OCaml.Shared, shared);
        assert.equal(imports.env.Shared, shared);
        return { instance: { exports: instantiate(name, imports, events) } };
      },
    },
  });
  await runtime({})({
    link: eager.merged ? [["merged", 0]] : [["runtime", 0], ["main", 0]],
    merged_eager: eager.merged,
    src: "assets",
    generated: { "main.fragments": mainFragments },
    enable_jspi: enableJspi,
    static_chunks: chunks,
    install_static_loader: (loader) => {
      events.push("install");
      load = loader;
      eager.install?.(loader, events);
    },
  });
  const startupEvents = eager.merged
    ? ["fetch:merged.wasm", "instantiate:merged.wasm", "install", "initialize:main"]
    : ["fetch:runtime.wasm", "instantiate:runtime.wasm",
       "fetch:main.wasm", "instantiate:main.wasm", "install", "initialize:main"];
  assert.deepEqual(events.slice(0, startupEvents.length), startupEvents);
  return { load, events, shared, imports: sharedImports, promisingCalls };
}

async function main() {
  const fragmentsReady = deferred();
  const initializerReady = deferred();
  const initializerStarted = deferred();
  const fragments = { probe: () => 42 };
  let glueLoads = 0;
  const success = await fixture({
    lazy: {
      module: "lazy",
      initialize: "partition_lazy.init",
      generated: () => {
        glueLoads++;
        return fragmentsReady.promise;
      },
    },
  }, (name, imports, events) => {
    assert.equal(name, "lazy.wasm");
    assert.equal(imports["partition_lazy.fragments"], fragments);
    return {
      "partition_lazy.init": async () => {
        events.push("initialize:lazy");
        imports.OCaml.Shared.value.registrations++;
        initializerStarted.resolve();
        await initializerReady.promise;
      },
    };
  }, true);
  assert.equal(glueLoads, 0);
  assert.equal(success.promisingCalls.length, 1);
  const first = success.load("lazy");
  assert.equal(success.load("lazy"), first);
  let resolved = false;
  first.then(() => { resolved = true; });
  await new Promise(setImmediate);
  assert.equal(glueLoads, 1);
  assert.equal(success.events.includes("instantiate:lazy.wasm"), false);
  fragmentsReady.resolve({ "partition_lazy.fragments": fragments });
  await initializerStarted.promise;
  assert.equal(success.promisingCalls.length, 2);
  assert.equal(success.shared.value.registrations, 1);
  assert.equal(resolved, false);
  initializerReady.resolve();
  await first;
  assert.equal(resolved, true);
  assert.equal(success.load("lazy"), first);
  assert.equal(success.events.filter((e) => e === "fetch:lazy.wasm").length, 1);
  assert.equal(success.events.filter((e) => e === "initialize:lazy").length, 1);

  const rootGates = { A: deferred(), B: deferred(), C: deferred() };
  const rootStarted = { A: deferred(), B: deferred(), C: deferred() };
  const rootFragments = Object.fromEntries(["A", "B", "C"].map(root => [root, { root }]));
  const multiple = await fixture(Object.fromEntries(["A", "B", "C"].map(root => [root, {
    module: "lazy-" + root,
    namespace: "partition_lazy_" + root + ".fragments",
    initialize: "partition_lazy_" + root + ".init",
    generated: () => ({ ["partition_lazy_" + root + ".fragments"]: rootFragments[root] }),
  }])), (name, imports, events) => {
    const root = /^lazy-(.).wasm$/.exec(name)[1];
    assert.equal(imports["partition_lazy_" + root + ".fragments"], rootFragments[root]);
    return { ["partition_lazy_" + root + ".init"]: async () => {
      events.push("start:" + root);
      imports.OCaml.Shared.value.registrations++;
      rootStarted[root].resolve();
      await rootGates[root].promise;
      events.push("finish:" + root);
    } };
  }, true, { merged: true });
  const loadA = multiple.load("A");
  await rootStarted.A.promise;
  assert.equal(multiple.events.includes("fetch:lazy-B.wasm"), false);
  assert.equal(multiple.events.includes("fetch:lazy-C.wasm"), false);
  const loadB = multiple.load("B");
  await rootStarted.B.promise;
  assert.equal(multiple.load("A"), loadA);
  assert.equal(multiple.load("B"), loadB);
  rootGates.B.resolve();
  await loadB;
  assert.equal(multiple.events.includes("finish:A"), false);
  assert.equal(multiple.events.includes("fetch:lazy-C.wasm"), false);
  rootGates.A.resolve();
  await loadA;
  assert.deepEqual(multiple.events.filter(event => event.startsWith("finish:")), ["finish:B", "finish:A"]);
  const loadC = multiple.load("C");
  await rootStarted.C.promise;
  rootGates.C.resolve();
  await loadC;
  assert.equal(multiple.shared.value.registrations, 3);
  for (const root of ["A", "B", "C"]) {
    assert.equal(multiple.events.filter(event => event === "fetch:lazy-" + root + ".wasm").length, 1);
    assert.equal(multiple.imports["partition_lazy_" + root + ".fragments"], rootFragments[root]);
  }
  assert.equal(multiple.load("A"), loadA);
  assert.equal(multiple.load("B"), loadB);
  assert.equal(multiple.load("C"), loadC);

  const collision = await fixture({
    first: { module: "first", initialize: "first.init", namespace: "one.fragments", generated: () => ({ "one.fragments": {} }) },
    second: { module: "second", initialize: "second.init", namespace: "one.fragments", generated: () => ({ "one.fragments": {} }) },
    wrong: { module: "wrong", initialize: "wrong.init", namespace: "wrong.fragments", generated: () => ({ "other.fragments": {} }) },
  }, name => ({ [name.replace(".wasm", ".init")]: () => {} }));
  await collision.load("first");
  const originalFragments = collision.imports["one.fragments"];
  await assert.rejects(collision.load("second"), /Conflicting static Wasm import namespace/);
  await assert.rejects(collision.load("wrong"), /Unexpected static Wasm fragment namespace/);
  assert.equal(collision.imports["one.fragments"], originalFragments);
  assert.equal(Object.hasOwn(collision.imports, "other.fragments"), false);

  const failureReady = deferred();
  const failureStarted = deferred();
  const failure = await fixture({
    failure: { module: "failure", initialize: "failure.init" },
    network: { module: "fetch-failure", initialize: "failure.init" },
    glue: {
      module: "glue",
      initialize: "glue.init",
      generated: () => { throw new Error("glue failure"); },
    },
    namespace: {
      module: "namespace",
      initialize: "namespace.init",
      generated: () => ({ env: {} }),
    },
    export: { module: "export", initialize: "export.init" },
    missing: { module: "missing", initialize: "missing.init" },
    path: { module: "../outside", initialize: "bad.init" },
  }, (name, imports) => {
    if (name === "failure.wasm") return {
      "failure.init": async () => {
        imports.OCaml.Shared.value.registrations++;
        failureStarted.resolve();
        await failureReady.promise;
      },
    };
    if (name === "export.wasm") return {
      Shared: new WebAssembly.Global({ value: "externref", mutable: true }),
      "export.init": () => assert.fail("conflicting exports initialized"),
    };
    if (name === "missing.wasm") return {};
    assert.fail("unexpected instantiation: " + name);
  });
  const pending = failure.load("failure");
  const rejected = assert.rejects(pending, /failure after registration/);
  await failureStarted.promise;
  assert.equal(failure.shared.value.registrations, 1);
  failureReady.reject(new Error("failure after registration"));
  await rejected;
  assert.equal(failure.load("failure"), pending);
  assert.equal(failure.shared.value.registrations, 1);
  for (const [key, error] of [
    ["network", /network failure/],
    ["glue", /glue failure/],
    ["namespace", /Conflicting static Wasm import namespace: env/],
    ["export", /Conflicting static Wasm export: Shared/],
    ["missing", /Missing static Wasm initializer/],
    ["path", /Invalid static Wasm module name/],
    ["unknown", /Unknown static Wasm chunk/],
  ]) {
    const promise = failure.load(key);
    await assert.rejects(promise, error);
    assert.equal(failure.load(key), promise);
  }
  assert.equal(failure.imports.OCaml.Shared, failure.shared);
  assert.equal(failure.imports.env.Shared, failure.shared);
  assert.equal(failure.events.some((e) => e.includes("outside")), false);
  const eagerReady = deferred();
  const eagerStarted = deferred();
  let earlyLoad, earlyEvents, earlyGlueLoads = 0;
  const earlyBoot = fixture({
    early: {
      module: "early",
      initialize: "early.init",
      generated: () => { earlyGlueLoads++; },
    },
  }, () => ({ "early.init": () => {} }), true, {
    install: (load, events) => {
      earlyLoad = load("early");
      earlyEvents = events;
    },
    initialize: () => {
      eagerStarted.resolve();
      return eagerReady.promise;
    },
  });
  await eagerStarted.promise;
  await new Promise(setImmediate);
  assert.equal(earlyGlueLoads, 0);
  assert.equal(earlyEvents.includes("fetch:early.wasm"), false);
  eagerReady.resolve();
  const early = await earlyBoot;
  await earlyLoad;
  assert.equal(earlyGlueLoads, 1);
  assert.equal(early.load("early"), earlyLoad);

  const startupError = new Error("eager startup failed");
  let failedLoader, blockedLoad, blockedResult, blockedEvents, blockedGlueLoads = 0;
  const failedBoot = fixture({
    blocked: {
      module: "blocked",
      initialize: "blocked.init",
      generated: () => { blockedGlueLoads++; },
    },
  }, () => assert.fail("chunk instantiated after failed eager startup"), true, {
    install: (load, events) => {
      blockedEvents = events;
      failedLoader = load;
      blockedLoad = load("blocked");
      blockedResult = blockedLoad.catch((error) => error);
    },
    initialize: () => { throw startupError; },
  });
  await assert.rejects(failedBoot, (error) => error === startupError);
  assert.equal(await blockedResult, startupError);
  assert.equal(failedLoader("blocked"), blockedLoad);
  assert.equal(blockedGlueLoads, 0);
  assert.equal(blockedEvents.includes("fetch:blocked.wasm"), false);
  const merged = await fixture({
    lazy: { module: "merged-lazy", initialize: "merged_lazy.init" },
  }, (_name, imports) => {
    assert.equal(imports.env, imports.OCaml);
    return { "merged_lazy.init": () => { imports.OCaml.Shared.value.registrations++; } };
  }, false, { merged: true });
  await merged.load("lazy");
  assert.equal(merged.shared.value.registrations, 1);
  assert.equal(merged.imports.env.Shared, merged.shared);
  console.log("static Wasm chunks: independent roots, reversed completion, namespaces, singleton imports, and sticky failures passed");
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
